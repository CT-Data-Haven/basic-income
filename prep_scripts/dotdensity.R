library(dplyr)
library(sf)
library(camiller)
library(purrr)
library(future)


cores <- parallelly::availableCores()
plan(future.callr::callr, workers = cores - 2)

sf_use_s2(FALSE)

block_sf <- tigris::block_groups(state = "09", year = 2020) |>
  janitor::clean_names() |>
  filter(aland > 0) |>
  select(geoid) |>
  rmapshaper::ms_simplify() |>
  st_transform(2234)
pop <- tidycensus::get_decennial(
  geography = "block group",
  table = "P5",
  cache_table = TRUE,
  year = 2020,
  sumfile = "dhc",
  state = "09"
) |>
  cwi::label_decennial() |>
  janitor::clean_names() |>
  group_by(geoid) |>
  add_grps(list(total_pop = 1, white = 3, black = 4, latino = 10, asian = 6, other_race = c(5, 7:9)),
           group = label, value = value) |>
  rename(group = label)

dense_n <- 50

to_dense <- block_sf |>
  # filter(grepl("^090(01|03|09)", geoid)) |>
  left_join(pop, by = "geoid") |>
  filter(group != "total_pop") |>
  mutate(dots = floor(value / dense_n)) |>
  filter(dots > 0) |>
  mutate(across(where(is.factor), forcats::fct_drop)) |>
  split(~group)



tictoc::tic()
dense_sf <- imap(to_dense, function(df, grp) st_sample(df$geometry, df$dots)) |>
  map(st_sf) |>
  bind_rows(.id = "group") |>
  mutate(group = forcats::as_factor(group)) |>
  group_by(group) |>
  summarise() |>
  slice_sample(prop = 1, replace = FALSE)
tictoc::toc()
BRRR::skrrrahh("birdman1")

attr(dense_sf, "dense_n") <- dense_n

saveRDS(dense_sf, file.path("data", "dot_density_sf.rds"))

plan(sequential)



