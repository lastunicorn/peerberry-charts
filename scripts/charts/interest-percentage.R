# ------------------------------------------------------------------------------
# !!! Work in progress


pb_transactions |>
  #filter(is.element(type, c("BUYBACK_INTEREST", "REPAYMENT_INTEREST"))) |>
  arrange(date) |> 
  mutate(
    month_as_date = floor_date(date, "month"),
    amount_interest = ifelse(is.element(type, c("BUYBACK_INTEREST", "REPAYMENT_INTEREST")), amount, 0),
    total_interest = map_dbl(date, ~ sum(amount_interest[date <= .x])),
    amount_uninvested = ifelse(type == "INVESTMENT", -amount, amount),
    uninvested_amount = map_dbl(date, ~ sum(amount_uninvested[date < .x]))
  ) |>
  group_by(date) |> 
  summarize(
    month_as_date = first(month_as_date),
    total_interest = first(total_interest),
    uninvested_amount = first(uninvested_amount)
  ) |> 
  group_by(month_as_date) |>
  summarize(
    total_interest = first(total_interest),
    uninvested_amount = mean(uninvested_amount)
  )





  ggplot(aes(x = date_as_month, y = interest_amount)) +
  geom_col() +
  guides(x = guide_axis(angle = 60)) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y", minor_breaks = NULL) +
  labs(
    title = "Interest by month (ever)",
    x = "Month",
    y = "Amount (€)"
  )
