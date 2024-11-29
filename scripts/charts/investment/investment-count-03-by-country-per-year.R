library(tidyverse)


# ------------------------------------------------------------------------------
# Investment count in each country (per year)

pb_transactions |> 
  filter(type == "INVESTMENT") |> 
  mutate(
    year = year(date)
  ) |> 
  group_by(year, country) |> 
  summarize(
    count = n(),
    .groups = "drop"
  ) |> 
  ggplot(aes(x = country, y = count)) +
  geom_col() +
  geom_text(
    aes(label = count),
    vjust = -0.5,
    size = 3
  ) +
  facet_wrap(~ year, ncol = 1) +
  guides(x = guide_axis(angle = 60)) +
  labs(
    title = "Investment count by country (per year)",
    subtitle = str_c("today: ", pb_today),
    x = "Country",
    y = "Count"
  )

save_plot("investment/investment-count-03-by-country-per-year.png")
