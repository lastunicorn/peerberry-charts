library(dplyr)
library(purrr)

# ------------------------------------------------------------------------------
# Calculate total amount for each transaction type

# Note: the value is stored in a temporary tibble because when it is displayed
# it looses number precision.

temp <- pb_transactions |> 
  group_by(type) |> 
  summarize(
    n = n(),
    total = sum(amount)
  )


# ------------------------------------------------------------------------------
# Calculate uninvested amount at a specific date.

# Note: the value is stored in a temporary tibble because when it is displayed
# it looses number precision.

temp <- pb_transactions |> 
  filter(date <= ymd("2024-09-29")) |> 
  mutate(
    amount2 = ifelse(type == "INVESTMENT", -amount, amount)
  ) |> 
  summarize(
    total = sum(amount2)
  )

# ------------------------------------------------------------------------------
# For each year:
#   - Uninvested amount by date

pb_transactions |> 
  arrange(date) |> 
  mutate(
    year = year(date),
    date_in_year = as.Date(ISOdate(2000, month(date), day(date))),
    amount2 = ifelse(type == "INVESTMENT", -amount, amount),
    uninvested_amount = map_dbl(date, ~ sum(amount2[date < .x]))
  ) |>
  ggplot(aes(x = date_in_year, y = uninvested_amount)) +
  geom_area(alpha = .5) +
  geom_step(direction = "hv", color = "black") +
  geom_smooth() +
  facet_wrap(~ year, ncol = 1) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  labs(
    title = "Cash drag (uninvested amount) (per year)",
    x = "Date",
    y = "Amount (€)"
  )

# Save
ggsave("charts/uninvested-amount-by-date-per-year.png", width=30, height=20, units="cm", dpi=300)
