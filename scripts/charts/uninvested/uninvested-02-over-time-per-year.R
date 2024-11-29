library(dplyr)
library(purrr)


# ------------------------------------------------------------------------------
# Uninvested amount over time (per year)

pb_transactions |> 
  arrange(date) |> 
  mutate(
    year = year(date),
    date_without_year = as.Date(ISOdate(2000, month(date), day(date))),
    amount2 = ifelse(type == "INVESTMENT", -amount, amount),
    uninvested_amount = map_dbl(date, ~ sum(amount2[date <= .x]))
  ) |>
  group_by(date) |> 
  summarize(
    year = first(year),
    date_without_year = first(date_without_year),
    uninvested_amount = first(uninvested_amount)
  ) |>
  uncount(2) |> 
  mutate(
    uninvested_amount = lag(uninvested_amount)
  ) |> 
  filter(!is.na(uninvested_amount)) |> 
  ggplot(aes(x = date_without_year, y = uninvested_amount)) +
  geom_area(fill = "gray60") +
  geom_line(color = "gray20") +
  facet_wrap(~ year, ncol = 1) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b", minor_breaks = NULL) +
  scale_y_continuous(n.breaks = 20, minor_breaks = F) +
  labs(
    title = "Cash drag over time (per year)",
    subtitle = str_c("today: ", pb_today),
    x = "Date",
    y = "Cash drag (€)"
  )

save_plot("uninvested/uninvested-02-amount-over-time-per-year.png")
save_plot("uninvested-02-amount-over-time-per-year.png")
