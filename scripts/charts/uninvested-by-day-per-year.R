library(dplyr)
library(purrr)


# ------------------------------------------------------------------------------
# For each year:
#   - Uninvested amount by date

pb_transactions |> 
  arrange(date) |> 
  mutate(
    year = year(date),
    date_without_year = as.Date(ISOdate(2000, month(date), day(date))),
    amount2 = ifelse(type == "INVESTMENT", -amount, amount),
    uninvested_amount = map_dbl(date, ~ sum(amount2[date < .x]))
  ) |>
  group_by(date) |> 
  summarize(
    year = first(year),
    date_without_year = first(date_without_year),
    uninvested_amount = first(uninvested_amount)
  ) |>
  ggplot(aes(x = date_without_year, y = uninvested_amount)) +
  geom_area(alpha = .5) +
  geom_step(direction = "hv", color = "black") +
  geom_smooth(method = 'loess', formula = 'y ~ x') +
  facet_wrap(~ year, ncol = 1) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b", minor_breaks = NULL) +
  labs(
    title = "Cash drag (uninvested amount) (per year)",
    x = "Date",
    y = "Amount (€)"
  )

# Save
ggsave("charts/uninvested-amount-by-date-per-year.png", width=30, height=20, units="cm", dpi=300)
