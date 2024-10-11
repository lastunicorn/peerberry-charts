library(dplyr)
library(purrr)


# ------------------------------------------------------------------------------
# Total amount with uninvested amount by date

pb_transactions |> 
  arrange(date) |> 
  mutate(
    amount2 = ifelse(type == "INVESTMENT", -amount, amount),
    uninvested_amount = map_dbl(date, ~ sum(amount2[date <= .x])),
    amount3 = ifelse(is.element(type, c("DEPOSIT", "BUYBACK_INTEREST", "REPAYMENT_INTEREST")), amount, 0),
    total_amount = map_dbl(date, ~ sum(amount3[date <= .x])),
  ) |>
  group_by(date) |> 
  summarize(
    uninvested_amount = first(uninvested_amount),
    total_amount = first(total_amount)
  ) |>
  uncount(2) |> 
  mutate(
    uninvested_amount = lag(uninvested_amount),
    total_amount = lag(total_amount)
  ) |> 
  filter(!is.na(uninvested_amount) & !is.na(total_amount)) |> 
  ggplot(aes(x = date)) +
  geom_area(aes(y = total_amount), fill = "#D4E79E") +
  geom_step(aes(y = total_amount), direction = "hv", color = "#7E8C40") +
  geom_area(aes(y = uninvested_amount), fill = "gray60") +
  geom_step(aes(y = uninvested_amount), direction = "hv", color = "gray20") +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y", minor_breaks = NULL) +
  guides(x = guide_axis(angle = 60)) +
  labs(
    title = "Total amount with uninvested amount by date (ever)",
    x = "Date",
    y = "Amount (€)"
  )

save_plot("total-amount-with-uninvested-amount-by-date.png")
