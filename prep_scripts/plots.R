library(dplyr)
library(purrr)
library(sf)
library(ggplot2)
library(gt)
library(forcats)

source("utils/plotting_utils.R")
showtext::showtext_auto()
showtext::showtext_opts(dpi = 300)

regs <- list(
  Bridgeport = "Fairfield County",
  Hartford = "Greater Hartford",
  "New Haven" = "Greater New Haven"
)
locs <- c("Connecticut", unlist(regs), names(regs))
cities <- names(regs)
pumas <- cwi::xwalk |>
  distinct(town, puma_fips) |>
  filter(town %in% cities) |>
  tibble::deframe()

plots <- list()
tbls <- list()

## DOT DENSITY ----
set.seed(1)
dense_sf <- readRDS(file.path("data", "dot_density_sf.rds")) |>
  rename(geometry = .x..i..) |>
  sf::st_cast("POINT") |>
  slice_sample(prop = 1.0, replace = FALSE)
dense_n <- attr(dense_sf, "dense_n")

plots[["dot_density"]] <- dense_sf |>
  mutate(group = fct_relabel(group, camiller::clean_titles)) |>
  ggplot(aes(color = group)) +
  geom_sf(alpha = 0.1, size = 0.01) +
  scale_color_manual(values = race_pal,
                     guide = guide_legend(override.aes = list(size = 2, alpha = 1))) +
  labs(title = "Population by race/ethnicity, 2020",
       subtitle = stringr::str_glue("One dot = {dense_n} residents"), 
       color = NULL) +
  # theme_void() +
  coord_sf(ndiscr = 0) +
  theme(legend.position = "top",
        legend.justification = c(0, 0))


## TENURE BY RACE X AGE ----
tenure <- readr::read_csv(file.path("data", "tenure_by_race_x_age_2021.csv")) |>
  mutate(name = as_factor(name) |>
           fct_recode(!!!pumas)) |>
  filter(name %in% locs) |>
  filter(tenure == "owned") |>
  filter(race_eth != "other_race") |>
  mutate(dimension = ifelse(age_grp == "total", "all_ages", "by_age")) |>
  mutate(across(c(dimension, level, race_eth, age_grp), as_factor)) |>
  mutate(race_eth = fct_relabel(race_eth, camiller::clean_titles)) |>
  mutate(age_grp = fct_relabel(age_grp, clean_ages)) |>
  split(~dimension)

(plots[["tenure_by_race"]] <- tenure[["all_ages"]] |>
  filter(level %in% c("state", "region")) |>
  offset_lbls(value = share, fun = scales::label_percent(accuracy = 1)) |>
  ggplot(aes(x = race_eth, y = share, fill = race_eth)) +
  geom_col(width = 0.8) +
  geom_text(aes(y = y, label = lbl, vjust = just)) +
  facet_wrap(vars(name), nrow = 2) +
  scale_fill_manual(values = race_pal, guide = guide_none()) +
  scale_y_barcontinuous(breaks = NULL) +
  labs(x = NULL, y = NULL,
       title = "Homeownership rate by race/ethnicity\nof head of household",
       subtitle = "Share of households, 2021"))

(plots[["tenure_by_race_x_age"]] <- tenure[["by_age"]] |>
  filter(name == "Connecticut") |>
  offset_lbls(value = share, fun = scales::label_percent(accuracy = 1)) |>
  ggplot(aes(x = race_eth, y = share, fill = race_eth)) +
  geom_col(width = 0.8) +
  geom_text(aes(y = y, label = lbl, vjust = just)) +
  facet_wrap(vars(age_grp)) +
  scale_fill_manual(values = race_pal, guide = guide_none()) +
  scale_y_barcontinuous(breaks = NULL) +
  labs(x = NULL, y = NULL,
       title = "Homeownership rate by race/ethnicity and age of head of household",
       subtitle = "Share of households, Connecticut, 2021"))

(tbls[["tenure_by_race_x_age"]] <- tenure[["by_age"]] |>
  mutate(across(share, scales::label_percent(accuracy = 1))) |>
  filter(level %in% c("state", "region")) |>
  tidyr::pivot_wider(id_cols = c(name, age_grp), names_from = race_eth, values_from = share) |>
  gt(rowname_col = "age_grp", groupname_col = "name") |>
  cols_align("left", "age_grp") |>
  tab_options(
    row_group.font.weight = "bold"
  ) |>
  opt_table_font(
    font = list(
      google_font("Roboto Slab"),
      "sans-serif"
    )
  ))

## WAGE GAP ----
wages <- readr::read_csv("data/wage_gap_race_sex_2021.csv") |>
  mutate(name = as_factor(name) |>
           fct_recode(!!!pumas)) |>
  filter(name %in% locs) |>
  filter(indicator %in% c("by_sex", "by_race_x_sex")) |>
  filter(race_eth != "other_race") |>
  mutate(across(c(indicator, level, sex, race_eth), as_factor)) |>
  mutate(across(c(sex, race_eth), \(x) fct_relabel(x, camiller::clean_titles))) |>
  mutate(race_eth = fct_recode(race_eth, Asian = "Api")) |>
  mutate(sex = fct_recode(sex, Men = "Male", Women = "Female"))

(plots[["wages_by_sex"]] <- wages |>
  filter(indicator == "by_sex") |>
  # filter(name == "Connecticut") |>
  offset_lbls(value = median_earnings, fun = scales::label_dollar(accuracy = 1, scale = 1e-3, suffix = "k")) |>
  ggplot(aes(x = name, y = median_earnings, fill = sex, group = sex)) +
  geom_col(width = 0.8, position = position_dodge2(width = 0.8)) +
  geom_text(aes(y = y, label = lbl, vjust = just), position = position_dodge2(width = 0.8)) +
  # facet_wrap(vars(name), nrow = 1, labeller = labeller(.cols = label_wrap_gen())) +
  scale_x_discrete(labels = label_wrap_gen(12)) +
  scale_y_barcontinuous(breaks = NULL) +
  scale_fill_manual(values = gender_pal) +
  theme(legend.position = "bottom",
        legend.justification = c(0, 0)) +
  labs(x = NULL, y = NULL, fill = NULL,
       title = "Median (average) earnings by gender",
       subtitle = "Full-time, year-round workers ages 25+, 2021"))

wages |>
  # filter(indicator == "by_sex") |>
  filter(name == "Connecticut") |>
  mutate(baseline = sex == "Men" & race_eth == "Total") |>
  mutate(ratio_x_men = median_earnings / median_earnings[baseline])

(plots[["wages_by_sex_x_race"]] <- wages |>
  filter(name == "Connecticut") |>
  offset_lbls(value = median_earnings, fun = scales::label_dollar(accuracy = 1, scale = 1e-3, suffix = "k")) |>
  ggplot(aes(x = race_eth, y = median_earnings, fill = sex, group = sex)) +
  geom_col(width = 0.8, position = position_dodge2(width = 0.8)) +
  geom_text(aes(y = y, label = lbl, vjust = just), position = position_dodge2(width = 0.8)) +
  # facet_wrap(vars(name), nrow = 1, labeller = labeller(.cols = label_wrap_gen())) +
  scale_x_discrete(labels = label_wrap_gen(12)) +
  scale_y_barcontinuous(breaks = NULL) +
  scale_fill_manual(values = gender_pal) +
  theme(legend.position = "bottom",
        legend.justification = c(0, 0)) +
  labs(x = NULL, y = NULL, fill = NULL,
       title = "Median (average) earnings by gender and race/ethnicity",
       subtitle = "Full-time, year-round workers ages 25+, Connecticut, 2021"))

## EDUCATION ----
edu <- readr::read_csv(file.path("data", "adult_ed_2021.csv")) |>
  filter(name %in% locs) |>
  filter(!race %in% c("other_race")) |>
  mutate(across(c(level, name, race, group), as_factor)) |>
  mutate(level = fct_relabel(level, stringr::str_remove, "^\\d_")) |>
  mutate(across(c(race, group), \(x) fct_relabel(x, camiller::clean_titles))) |>
  mutate(race = fct_recode(race, "Native Amer." = "Aian")) |>
  mutate(group = fct_recode(group,
                            "No HS diploma" = "Less than hs",
                            # "High school diploma" = "High school",
                            "Some college / AA" = "Some college aa",
                            "Bachelor's or more" = "Bachelors plus")) |>
  filter((name == "Connecticut") | (name != "Connecticut" & race == "Total")) |>
  filter(!is.na(share)) |>
  filter(level %in% c("state", "town"))

(plots[["edu_by_race"]] <- edu |>
  group_by(name, race) |>
  # mutate(y = stack_lbls(share, fill = TRUE)) |>
  mutate(lbl = scales::label_percent()(share)) |>
  # mutate(race = fct_rev(race)) |>
  ggplot(aes(x = race, y = share, fill = group, group = group)) +
  geom_col(width = 0.8, position = position_fill(vjust = 0.5, reverse = TRUE)) +
  geom_text(aes(label = lbl), position = position_fill(vjust = 0.5, reverse = TRUE)) +
  # coord_flip() +
  scale_y_barcontinuous(breaks = NULL) +
  scale_x_discrete(labels = label_wrap_gen(12)) +
  scale_fill_manual(values = seq_pal, 
                    guide = guide_legend(reverse = TRUE)) +
  facet_grid(cols = vars(name), scales = "free_x", space = "free",
             labeller = labeller(.cols = label_wrap_gen(7))) +
  theme(legend.position = "right",
        # legend.justification = c(0, 0),
        strip.clip = "off") +
  labs(x = NULL, y = NULL, fill = NULL,
       title = "Highest level of education",
       subtitle = "Share of adults ages 25+, 2021"))



# imap(plots[1], function(p, id) {
#   fn <- file.path("plots", id)
#   ggsave(fn, p, width = 8, height = 7, bg = "white")
# })

params <- list(
  dot_density = list(w = 8, h = 7.5),
  tenure_by_race = list(w = 8, h = 8),
  tenure_by_race_x_age = list(w = 13, h = 5),
  wages_by_sex = list(w = 13, h = 6),
  wages_by_sex_x_race = list(w = 13, h = 6),
  edu_by_race = list(w = 13, h = 6)
)

imap(plots, function(p, id) {
  ps <- params[[id]]
  fn <- file.path("plots", id)
  ggsave(xfun::with_ext(fn, "png"), p, width = ps$w, height = ps$h, dpi = 300, bg = "white")
  ggsave(xfun::with_ext(fn, "svg"), p, width = ps$w, height = ps$h)
})
