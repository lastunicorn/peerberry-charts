library(tidyverse)


# ------------------------------------------------------------------------------
# Invested amount of money by month per country in current year.

pb_transactions |>  
  filter(type == "INVESTMENT") |> 
  mutate(
    year = year(date),
    month_as_date = floor_date(date, "month")
  ) |>
  filter(year == year(today())) |>
  group_by(country, month_as_date) |> 
  summarize(
    sum = sum(amount),
    .groups = "drop"
  ) |> 
  mutate(
    x = format(month_as_date, "%b")
  ) |> 
  ggplot(aes(x = reorder(x, month_as_date), y = sum)) +
  geom_col() +
  facet_wrap(~ country) +
  scale_x_discrete(
    limits = format(seq.Date(from = floor_date(today(), "year"), by = "month", length.out = 12), "%b")
  ) +
  guides(x = guide_axis(angle = 70)) +
  labs(
    title = paste("Investment amount by month in ", year(today()), " (per country)", sep = ""),
    x = "Purchase month",
    y = "Amount (€)"
  )

save_plot(paste("investment-amount-by-month-per-country-", year(today()), ".png", sep = ""))
