library("here")
library("fs")
library("DT")
library("glue")
library("janitor")
library("scales")
library("tidyverse")

data_dir <- "data"
file_name <- "conversion_paths.csv"

conv_paths_raw <-
  read_csv(path_join(c(here(), data_dir, file_name))) |>
  clean_names()

# conversions are all 1s ? ----------------
conv_paths_raw |>
  slice_sample(n = 10) |>
  select(-medium_path)

summary(conv_paths_raw$conversions)
# Seems like most of conv. are 1s but still lots of other values

conv_paths_raw |> filter(conversions < 1)
# there are no 0-conversions

# ok seems that i can convert them to integer numbers.
conv_paths <- conv_paths_raw |>
  mutate(conversions = as.integer(conversions))


# value always in EUR ? ----------------
conv_paths <- conv_paths |>
  mutate(
    value_splits = str_split(value, " ", n = 2),
    currency = map_chr(value_splits, first),
    value = {
      map_chr(value_splits, nth, n = 2) |>
        str_remove_all(",") |>
        parse_number()
    },
    value_splits = NULL
  )

# is value always in EUR ?
conv_paths |> count(currency)
# yes - we can remove it
conv_paths <- conv_paths |> select(-currency)

# how does value look like ?
summary(conv_paths$value)
ggplot(conv_paths, aes(value)) + stat_ecdf(geom = "step")
# looks like we have suuuper heavy tail with some extreme values ?
quantile(conv_paths$value, seq(0.9, 1, 0.01)) |> prettyNum(scientific = F)

conv_paths |> filter(value > 20000)

# how do conversions-value pairs look like ?
ggplot(conv_paths, aes(conversions, value)) + geom_point()
# not sure what to think of this big hole in the middle, at first glance it looks bit unrealistic ?
ggplot(conv_paths, aes(conversions, value)) + geom_point() + scale_x_log10() + scale_y_log10()

# the last message indicates that there are zero value conversions ?
conv_paths |> filter(value < 1)
# hm yes ... not sure if this is a "hiccup" in the data / export ?
conv_paths |>
  count(value == 0) |>
  mutate(perc = n / sum(n))

conv_paths |>
  group_by(value == 0) |>
  summarise(n = sum(conversions)) |>
  mutate(perc = n / sum(n))

# since it's only 4% of the data and 0.12% in terms of conversions
# then i think it's safe to simply remove them
conv_paths <- conv_paths |> filter(value > 0)


# what are all possible mediums represented ? any weird ones ? ---------
mediums <- conv_paths |>
  select(medium_path) |>
  separate_wider_delim(
    medium_path,
    delim = " > ",
    too_few = "align_start",
    names_sep = "_"
  )

mediums |> map(unique)|> flatten_chr() |> unique() |> sort()

# distribution of lenghts of conv.paths ? --------------
# i want to look at distribution of path lengths and out of curiosity i want to
# check what is the most common first medium touchpoint usually
path_lengths <- conv_paths |>
  select(medium_path) |>
  mutate(
    path_splits = str_split(medium_path, " > "),
    path_length = map_int(path_splits, length),
    first_touchpoint_medium = map_chr(path_splits, first)
  ) |>
  select(path_length, first_touchpoint_medium)

# how does the distribution of path lengths look like ?
ggplot(path_lengths, aes(path_length)) + geom_histogram(binwidth = 1)
# heavily right skewed with extremely long path lengths

ggplot(path_lengths, aes(path_length)) +
  geom_histogram(binwidth = 1) +
  xlim(0, 50)
# centered around 7-9 ?

quantile(path_lengths$path_length, probs = seq(0.50, 1.00, 0.05))
# half of the conversions take 10-touchpoints to convers
# 90% of the conversions take <= 26 touchpoints/steps

# most common first medium ?
path_lengths |>
  count(first_touchpoint_medium, sort = T) |>
  mutate(perc = percent_format(accuracy = 0.001)(n / sum(n)))
# perhaps unsurprisingly as i saw in the country-medium analysis - referral is the 1st touchpoint for people
# ~45% of the time

# any duplicates ?
anyDuplicated(conv_paths$medium_path)

conv_paths$medium_path |> duplicated() |> sum()
# yes, i need to group-and-summarise

conv_paths <- conv_paths |>
  group_by(medium_path) |>
  summarise(across(c(conversions, value), sum)) |>
  ungroup()


# Markov models --------------
library("ChannelAttribution")

markov_mod <- markov_model(
  conv_paths,
  var_path = "medium_path",
  var_conv = "conversions",
  var_value = "value",
  out_more = T
)

markov_mod
# arrow::write_feather(conv_paths, "data/markov_mod_data.feather")

# removal effecits -------
as_tibble(markov_mod$removal_effects) |>
  arrange(desc(removal_effects_conversion_value))


# visualise the transition network ---
library("visNetwork")

# proof that channels are cross-referenced between
# result and transition_matrix
conv_paths |> filter(str_detect(medium_path, "zalo"))
markov_mod$transition_matrix |> tail(2)
conv_paths |> head(1)

# channel index <-> name
channel_index_to_name <- markov_mod$result |>
  as_tibble() |>
  select(channel_name) |>
  rownames_to_column("channel_index") |>
  deframe()


transition_matrix <- markov_mod$transition_matrix |>
  as_tibble() |>
  mutate(
    channel_from = ifelse(
      channel_from %in% c("(start)", "(conversion)"),
      channel_from,
      channel_index_to_name[ channel_from ]
    ),
    channel_to = ifelse(
      channel_to %in% c("(start)", "(conversion)"),
      channel_to,
      channel_index_to_name[ channel_to ]
    )
  )


transition_matrix_short <-
  transition_matrix |>
  arrange(desc(transition_probability)) |>
  filter(transition_probability > 0.7) |>
  mutate(
    label = percent_format(accuracy = 0.01)(transition_probability),
    arrows = "to",
    dashes = T
  ) |>
  rename(from = channel_from, to = channel_to)

nodes <-
  unique(c(transition_matrix_short$from, transition_matrix_short$to)) |>
  as_tibble()

nodes <- nodes |>
  mutate(id = value) |>
  rename(title = value) |>
  mutate(label = title)

visNetwork(
  nodes,
  transition_matrix_short,
  height = "500px",
  width = "100%"
)
