# ------------------------------------------------------------------------------
# ROI - Net annualized return by month - ever

pb_transactions |> 
  mutate(
    incomming_amount = if_else(type %in% c("DEPOSIT", "BUYBACK_INTEREST", "REPAYMENT_INTEREST"), amount, 0),
    outgoing_amount = if_else(type %in% c("WITHDRAW"), amount, 0),
    funds_delta = incomming_amount - outgoing_amount
  ) |> 
  group_by(date) |> 
  summarize(
    interest = sum(if_else(type %in% c("BUYBACK_INTEREST", "REPAYMENT_INTEREST"), amount, 0)),
    funds_delta = sum(funds_delta)
  ) |> 
  mutate(
    funds = cumsum(funds_delta)
  ) |> 
  arrange(date) |> 
  mutate(
    start_date = min(date),
    end_date = max(date),
    month = month(date),
    month_as_date = floor_date(date, "month"),
    roi_percentage = if_else(is.na(lag(funds)), 0, interest * 100 / lag(funds)),
    investment_days = if_else(is.na(lag(date)), 0, as.numeric(date - lag(date))),
    roi_percentage_pa = if_else(investment_days == 0, 0, roi_percentage * 365 / investment_days)
  ) |> 
  group_by(month_as_date) |> 
  mutate(
    month_start_date = floor_date(date, "month"),
    month_start_date = if_else(month_start_date < start_date, start_date, month_start_date),
    month_end_date = ceiling_date(date, "month"),
    month_end_date = if_else(month_end_date > end_date, end_date, month_end_date),
    days_in_month = (month_start_date %--% month_end_date) / days(1)
  ) |> 
  summarize(
    roi = sum(roi_percentage_pa * investment_days / first(days_in_month))
  ) |> 
  ggplot(aes(x = month_as_date, y = roi)) +
  geom_col() +
  geom_text(aes(label = paste(format(round(roi, 2), nsmall = 2), "%")), vjust = -0.5, size = 3) +
  guides(x = guide_axis(angle = 70)) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y", minor_breaks = NULL) +
  labs(
    title = "Net annualized return (including cash drag) by month",
    x = "Month",
    y = "ROI (%)"
  )

save_plot("roi-by-month-ever.png")
