library("here")
library("fs")
library("DT")
library("glue")
library("janitor")
library("scales")
library("tidyverse")

data_dir <- "data"
file_name <- "medium_country.csv"

medium_country_raw <-
  read_csv(
    path_join(c(here(), data_dir, file_name)),
    skip = 6
  ) |>
  clean_names()

by_country <-
  medium_country_raw |>
  group_by(country) |>
  summarise(
    # summable-metrics
    across(
      c(users, new_users, sessions, transactions, revenue),
      sum
    ),
    # take medians for the rates, shares, durations
    across(
      c(bounce_rate, pages_session, avg_session_duration, ecommerce_conversion_rate),
      median
    )
  ) |>
  ungroup() |>
  # calculate new columns
  mutate(
    revenue_per_transaction = revenue / transactions,
    new_users_share = new_users / users,
  ) |>
  arrange(desc(revenue))

# filter only bigger volume countries
by_country <- by_country |>
  filter(
    (revenue > 20000) & (users > 500) & (transactions > 500)
  )

datatable(
  by_country,
  options = list(fixedColumns = list(leftColumns = 1))
)

by_country <-
  by_country |>
  rename_with(
    ~ ifelse(
      .x == "country",
      .x,
      glue("{.x}_country")
    )
  )
by_country

medium_country <-
  # filtering join on top-volume countries
  medium_country_raw |>
  inner_join(by_country, join_by(country)) |>
  # re-arrange a bit
  arrange(desc(revenue_country), country, desc(revenue)) |>
  relocate(country, .before = medium) |>
  # calculate share of channel within country for main metrics
  mutate(
    revenue_share = revenue / revenue_country,
    users_share = users / users_country,
    new_users_share = new_users / new_users_country,
    sessions_share = sessions / sessions_country,
    transactions_share = sessions / transactions_country
  ) |>
  # rank channels within country
  group_by(country) |>
  mutate(
    across(
      c(users, new_users, sessions, transactions, revenue),
      ~ dense_rank(desc(.x)),
      .names = "{col}_channel_within_country_rank"
    )
  ) |>
  ungroup()


# revenue abs
medium_country |>
  semi_join(
    by_country |> slice_max(order_by = revenue_country, n = 20),
    join_by(country)
  ) |>
  filter(country != "(not set)") |>
  mutate(
    medium = case_when(
      medium == "(none)"    ~ "unknown",
      medium == "(not set)" ~ "unknown",
      .default = medium
    ),
    medium = fct_lump(medium, n = 5, w = revenue),
    medium = fct_reorder(medium, revenue, mean),
    country = fct_reorder(country, revenue, sum)
  ) |>
  ggplot(aes(x = revenue, y = country, fill = medium)) +
  geom_col(position = "stack") +
  scale_x_continuous(
    labels = comma_format(),
    expand = c(0, 0.3)
  ) +
  # scale_fill_manual(
  #   values = RColorBrewer::brewer.pal(8, "Dark2")[-4]
  #   values = RColorBrewer::brewer.pal(8, "Set1")[-6]
  # ) +
  scale_fill_brewer(palette = "Dark2", direction = 1) +
  theme(
    legend.position = "bottom"
  ) +
  labs(
    y = NULL
  )


# revenue share
medium_country |>
  semi_join(
    by_country |> slice_max(order_by = revenue_country, n = 20),
    join_by(country)
  ) |>
  filter(country != "(not set)") |>
  mutate(
    medium = case_when(
      medium == "(none)"    ~ "unknown",
      medium == "(not set)" ~ "unknown",
      .default = medium
    ),
    medium = fct_lump(medium, n = 6, w = revenue_share),
    medium = fct_reorder(medium, revenue_share, mean),
    country = fct_reorder(country, revenue, sum)
  ) |>
  ggplot(aes(x = revenue_share, y = country, fill = medium)) +
  geom_col(position = "stack") +
  scale_x_continuous(
    labels = percent_format(),
    expand = c(0, 0.01)
  ) +
  # scale_fill_manual(
  #   values = RColorBrewer::brewer.pal(8, "Dark2")[-4]
  #   values = RColorBrewer::brewer.pal(8, "Set1")[-6]
  # ) +
  scale_fill_brewer(palette = "Dark2", direction = 1) +
  theme(
    legend.position = "bottom"
  ) +
  labs(
    y = NULL
  )



# distribution of revenue share - per medium - across all countries
medium_country |>
  semi_join(
    by_country |> slice_max(order_by = revenue_country, n = 50),
    join_by(country)
  ) |>
  # filter(country != "(not set)") |>
  mutate(
    medium = case_when(
      medium == "(none)"    ~ "unknown",
      medium == "(not set)" ~ "unknown",
      .default = medium
    ),
    medium = fct_lump(medium, n = 7, w = revenue),
    medium = fct_reorder(medium, revenue)
  ) |>
  select(country, medium, revenue, revenue_country, revenue_share) |>

  ggplot(aes(revenue_share)) +
  geom_histogram() +
  facet_wrap(~ medium, scales = "free_y", ncol = 1, strip.position = "right")

# like above in table form - with median values for revenue share
# across all countries
medium_country |>
  semi_join(
    by_country |> slice_max(order_by = revenue_country, n = 50),
    join_by(country)
  ) |>
  # filter(country != "(not set)") |>
  mutate(
    medium = case_when(
      medium == "(none)"    ~ "unknown",
      medium == "(not set)" ~ "unknown",
      .default = medium
    ),
    medium = fct_lump(medium, n = 7, w = revenue),
    medium = fct_reorder(medium, revenue, .desc = T)
  ) |>
  select(country, medium, revenue, revenue_country, revenue_share) |>
  group_by(medium) |>
  summarise(
    median_revenue_share = median(revenue_share),
    median_revenue_share = percent_format(accuracy = 0.01)(median_revenue_share)
  )


# revenue per transaction -----
medium_country |>
  select(country, medium, revenue, transactions) |>
  filter(
    revenue > 500,
    transactions > 30
  ) |>
  mutate(revenue_per_transaction = revenue / transactions) |>
  group_by(country) |>
  mutate(revenue_per_transaction_rank = dense_rank(desc(revenue_per_transaction))) |>
  ungroup() |>
  arrange(country, revenue_per_transaction_rank) |>
  mutate(
    medium = fct_lump(medium, n = 6, w = revenue_per_transaction),
    medium = fct_reorder(medium, revenue_per_transaction_rank, mean, .desc = T)
  ) |>
  # TODO re-summarise now duplicate Other factor rows per country-medium
  ggplot(aes(revenue_per_transaction)) +
  geom_histogram(binwidth = 25) +
  facet_wrap(~ medium, ncol = 1, scales = "free_y")


# tally top ranked mediums for revenue per transaction
# ~ same as above in more condensed table form
medium_country |>
  select(country, medium, revenue, transactions) |>
  filter(
    revenue > 500,
    transactions > 30
  ) |>
  mutate(revenue_per_transaction = revenue / transactions) |>
  group_by(country) |>
  mutate(revenue_per_transaction_rank = dense_rank(desc(revenue_per_transaction))) |>
  ungroup() |>
  arrange(country, revenue_per_transaction_rank) |>
  mutate(
    medium = fct_lump(medium, n = 6, w = revenue_per_transaction),
    medium = fct_reorder(medium, revenue_per_transaction_rank, mean, .desc = T)
  ) |>
  filter(revenue_per_transaction_rank == 1) |>
  count(medium, sort = T) |>
  mutate(perc = n / sum(n))


medium_country |>
  select(country, medium, revenue, transactions) |>
  filter(
    revenue > 500,
    transactions > 30
  ) |>
  mutate(revenue_per_transaction = revenue / transactions) |>
  group_by(country) |>
  mutate(revenue_per_transaction_rank = dense_rank(desc(revenue_per_transaction))) |>
  ungroup() |>
  mutate(
    medium = fct_lump(medium, n = 6, w = revenue_per_transaction),
    medium = fct_reorder(medium, revenue_per_transaction_rank, .desc = T)
  ) |>
  group_by(medium) |>
  summarise(
    median_revenue_per_transaction = median(revenue_per_transaction),
    iqr_revenue_per_transaction = IQR(revenue_per_transaction)
  ) |>
  arrange(desc(median_revenue_per_transaction))



# correlation revenue share - new users share ----
# TODO: questionable ... ask Tom what he thinks ?
#
# Drivers for growth - which mediums correlate most with new users share
#
# if referral is so big - then (if my understanding of referral is correct)
# then we should have high co
medium_country |>
  filter(
    revenue > 500,
    transactions > 30
  ) |>
  mutate(
    medium = case_when(
      medium == "(none)"    ~ "unknown",
      medium == "(not set)" ~ "unknown",
      .default = medium
    ),
    medium = fct_lump(medium, n = 7, w = revenue),
  ) |>
  select(country, medium, revenue_share, new_users_share) |>
  group_by(medium) |>
  # ? view-1
  filter(medium %in% c("organic", "referral", "cpc")) |>
  ggplot(aes(x = new_users_share, y = revenue_share)) +
  geom_point() +
  geom_smooth(method = "lm", se = F, aes(colour = medium)) +
  facet_wrap(~ medium, scales = "free_y", ncol = 1)
  # # ? view-2
  # summarise(
  #   revenue_new_users_share_corr = cor(revenue_share, new_users_share)
  # ) |>
  # arrange(desc(revenue_new_users_share_corr))



# top medium for new users share -----
medium_country |>
  filter(revenue > 500, transactions > 30) |>
  mutate(
    medium = case_when(
      medium == "(none)"    ~ "unknown",
      medium == "(not set)" ~ "unknown",
      .default = medium
    ),
    medium = fct_lump(medium, n = 7, w = new_users),
  ) |>
  select(country, medium, new_users_share) |>
  group_by(country) |>
  slice_max(order_by = new_users_share, n = 1) |>
  ungroup() |>
  count(medium)

# top medium for users share -----
medium_country |>
  filter(revenue > 500, transactions > 30) |>
  mutate(
    medium = case_when(
      medium == "(none)"    ~ "unknown",
      medium == "(not set)" ~ "unknown",
      .default = medium
    ),
    medium = fct_lump(medium, n = 7, w = users),
  ) |>
  select(country, medium, users_share, users_channel_within_country_rank) |>
  filter(users_channel_within_country_rank == 1) |>
  count(medium)


# ratio of revenue share - to - users share -----
# TODO: worth exploring ?
# i.e. if a channel brings:
#   50% of the users
#   70 of the revenue
# -> then it's quite profitable and worth expanding likely
medium_country |>
  filter(revenue > 500, transactions > 30) |>
  select(country, medium, users_share, revenue_share) |>
  mutate(
    revenue_to_users_share_ratio = revenue_share / users_share
  ) |>
  filter(revenue_to_users_share_ratio > 1) |>
  arrange(desc(revenue_to_users_share_ratio))



# ecomm conv.rate -------
medium_country |>
  filter(revenue > 500, transactions > 30) |>
  select(country, medium, ecommerce_conversion_rate) |>
  group_by(country) |>
  mutate(
    within_country_rank = dense_rank(desc(ecommerce_conversion_rate))
  ) |>
  ungroup() |>
  arrange(country, within_country_rank) |>
  filter(within_country_rank == 1) |>
  count(medium, sort = T)


# bounce rate
medium_country


# simulate potential funnel -------
# TODO: questionable - ask Tom ?
# if 10,000 people come to each (country, medium)
# and convert as per ecomm conv.rate --> customers
# and spend the average revenue per transaction for that (country, medium) --> revenue
# which medium would make the most sense ?
medium_country |>
  filter(revenue > 500, transactions > 30) |>
  select(country, medium, ecommerce_conversion_rate, revenue, transactions) |>
  mutate(
    revenue_per_transaction = revenue / transactions
  ) |>
  # select(-revenue, -transactions) |>
  mutate(n_potential_incoming_visitors = 10000) |>
  relocate(n_potential_incoming_visitors, .before = ecommerce_conversion_rate) |>
  mutate(
    potential_revenue = {
      n_potential_incoming_visitors * ecommerce_conversion_rate * revenue_per_transaction
    }
  ) |>
  group_by(country) |>
  mutate(
    across(
      contains("revenue"),
      ~ dense_rank(desc(.x)),
      .names = "{col}_rank"
    )
  ) |>
  select(country, medium, ends_with("rank")) |>
  filter(
    country == "Russia",
    potential_revenue_rank < revenue_rank
  )
