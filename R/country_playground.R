library("here")
library("fs")
library("DT")
library("janitor")
library("scales")
library("tidyverse")

data_dir <- "data"
file_name <- "medium_country.csv"

medium_country_raw <- read_csv(
  path_join(c(here(), data_dir, file_name)),
  skip = 6
) |>
  clean_names()

n_countries_total <- medium_country_raw$country |> unique() |> length()

medium_country_raw |>
  filter(country == "Croatia") |>
  arrange(desc(revenue)) |>
  datatable()

medium_country_raw |> colnames()

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
    new_users_share = new_users / users
  )



by_country |> filter(country == "Croatia")


quantile(by_country$revenue, seq(0, 1, 0.05))

by_country |>

quantile(by_country$users, seq(0, 1, 0.05))


by_country |> filter(revenue < 1000 | transactions < 500 | users < 500)


