library(tidyverse)


# ------------------------------------------------------------------------------
# Amount of money invested in each year (per country)

pb_transactions |> 
  filter(type == "INVESTMENT") |> 
  mutate(year = year(date)) |> 
  mutate(year = as.factor(year)) |> 
  group_by(country, year) |> 
  arrange(country) |> 
  summarize(
    total_amount = sum(amount),
    .groups = "drop_last"
  ) |> 
  ggplot(aes(x = year, y = total_amount)) +
  geom_col(width = .75) +
  geom_text(
    aes(label = total_amount),
    vjust = -0.5,
    size = 3,
    color = "#666"
  ) +
  facet_wrap(~ country) +
  scale_y_continuous(
    expand = expand_scale(mult = c(0.05, 0.15))
  ) +
  guides(x = guide_axis(angle = 60)) +
  labs(
    title = "Investment amount by year (per country)",
    x = "Purchease year",
    y = "Amount (€)"
  )

save_plot("investment-amount-by-year-per-country.png", width=30, height=20)
