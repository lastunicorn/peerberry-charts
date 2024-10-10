library(dplyr)
library(purrr)


# ------------------------------------------------------------------------------
# Uninvested amount by date

pb_transactions |> 
  arrange(date) |> 
  mutate(
    amount2 = ifelse(type == "INVESTMENT", -amount, amount),
    uninvested_amount = map_dbl(date, ~ sum(amount2[date <= .x]))
  ) |>
  group_by(date) |> 
  summarize(
    uninvested_amount = first(uninvested_amount)
  ) |>
  ggplot(aes(x = date, y = uninvested_amount)) +
  geom_area(fill = "gray60") +
  geom_step(direction = "hv", color = "gray20") +
  geom_smooth(method = 'loess', formula = 'y ~ x') +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y", minor_breaks = NULL) +
  guides(x = guide_axis(angle = 60)) +
  labs(
    title = "Cash drag (uninvested amount) (ever)",
    x = "Date",
    y = "Uninvested amount"
  )

save_plot("uninvested-amount-by-date.png")
