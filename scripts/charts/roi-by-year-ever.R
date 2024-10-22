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
    year = as.factor(year(date)),
    start_date = min(date),
    end_date = max(date) + days(1),
    roi_percentage = if_else(is.na(lag(funds)), 0, interest * 100 / lag(funds)),
    investment_days = if_else(is.na(lag(date)), 0, as.numeric(date - lag(date))),
    roi_percentage_pa = if_else(investment_days == 0, 0, roi_percentage * 365 / investment_days)
  ) |> 
  group_by(year) |> 
  mutate(
    year_start_date = floor_date(date, "year"),
    year_start_date = if_else(year_start_date < start_date, start_date, year_start_date),
    year_end_date = ceiling_date(date, "year"),
    year_end_date = if_else(year_end_date > end_date, end_date, year_end_date),
    days_in_year = (year_start_date %--% year_end_date) / days(1)
  ) |> 
  summarize(
    roi = sum(roi_percentage_pa * investment_days / first(days_in_year))
  ) |> 
  ggplot(aes(x = year, y = roi)) +
  geom_col(width = .5) +
  geom_text(aes(label = paste(format(round(roi, 2), nsmall = 2), "%")), vjust = -0.5, size = 3) +
  labs(
    title = "Net annualized return (including cash drag) by year",
    x = "Month",
    y = "ROI (% p.a.)"
  )

save_plot("roi-by-year-ever.png")
