library(tidyverse)
library(here)
library(janitor)
library(as.charts)

# Raw data
dat_le <- read_csv(
  file = here("data/nz-complete-cohort-life-tables-1876-2020.csv"),
  col_types = "iciciniinnnn"
) |>
  clean_names()

# Median additional life expectancy of people having birthdays in 2022
dat_birthday_le <- full_join(
  # Total life expectancy given their age in 2021
  dat_le |>
    filter(percentile == "median") |>
    select(yearofbirth, sex, age, ex) |>
    mutate(age_in_2021 = 2021L - yearofbirth) |>
    filter(age == age_in_2021) |>
    mutate(total_ex_2021 = age_in_2021 + ex) |>
    rename(ex_2021 = ex) |>
    select(-age),

  # Total life expectancy given their age in 2022
  dat_le |>
    filter(percentile == "median") |>
    select(yearofbirth, sex, age, ex) |>
    mutate(age_in_2022 = 2022L - yearofbirth) |>
    filter(age == age_in_2022) |>
    mutate(total_ex_2022 = age_in_2022 + ex) |>
    rename(ex_2022 = ex) |>
    select(-age),

  by = c("yearofbirth", "sex")
) |>
  filter(!is.na(age_in_2022)) |>
  mutate(total_ex_delta = total_ex_2022 - total_ex_2021) |>
  mutate(total_ex_delta_months = total_ex_delta * 12) |>
  mutate(total_ex_pct = total_ex_delta / ex_2021)

# Chart
chart <- dat_birthday_le |>
  ggplot(mapping = aes(
    x = age_in_2022,
    y = total_ex_delta_months,
    colour = sex
  )) +
  geom_smooth(se = FALSE) +
  scale_x_continuous(
    limits = c(0, 101),
    breaks = seq(0, 100, 5),
    expand = expansion(0, 0)
  ) +
  scale_y_continuous(
    breaks = seq(0, 12, 1),
    limits = c(0, 12),
    expand = expansion(0, 0)
  ) +
  scale_colour_qual(
    values = c("male", "female"),
    value_labels = c("Male", "Female"),
    guide = guide_legend(nrow = 1),
    name = NULL
  )

output_chart(
  chart = chart,
  path = here("outputs"),
  orientation = "wide",
  ggtitle = "Additional life expectancy from getting one year older",
  legend_position = "custom",
  custom_legend_position = c(0.15, 0.95),
  xlab = "Age in 2022 (years)",
  ylab = "Months",
  axis.title.y = element_text(angle = 0, hjust = 1),
  plot.title = element_text(size = rel(1.25), margin = margin(0, 0, 12, 0, "pt")),
  panel.grid.major.x = element_line(colour = "#c8c8c8", size = 0.15),
  plot.margin = margin(4, 8, 4, 4, "pt"),
  legend.background = element_blank()
)
