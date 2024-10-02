# ------------------------------------------------------------------------------
# !!! Work in progress


pb_transactions |>
  #filter(is.element(type, c("BUYBACK_INTEREST", "REPAYMENT_INTEREST"))) |>
  arrange(date) |> 
  mutate(
    date_as_month = as.Date(ISOdate(year(date), month(date), 1)),
    amount_interest = ifelse(is.element(type, c("BUYBACK_INTEREST", "REPAYMENT_INTEREST")), amount, 0),
    total_interest = map_dbl(date, ~ sum(amount_interest[date <= .x])),
    amount_uninvested = ifelse(type == "INVESTMENT", -amount, amount),
    uninvested_amount = map_dbl(date, ~ sum(amount_uninvested[date < .x]))
  ) |>
  group_by(date) |> 
  summarize(
    date_as_month = first(date_as_month),
    total_interest = first(total_interest),
    uninvested_amount = first(uninvested_amount)
  ) |> 
  group_by(date_as_month) |>
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
