# ------------------------------------------------------------------------------
# Interest cumulative amount by month (per year)

pb_transactions |>
  filter(is.element(type, c("BUYBACK_INTEREST", "REPAYMENT_INTEREST"))) |>
  add_row(
    date = generate_monthly_dates(pb_transactions.first_year_as_date, pb_transactions.last_year_as_date |> last_month_of_year()),
    amount = 0
  ) |> 
  mutate(
    month_as_date = floor_date(date, "month")
  ) |>
  group_by(month_as_date) |>
  arrange(date) |>
  summarize(interest_amount = sum(amount)) |>
  mutate(
    year = year(month_as_date),
    month = factor(format(month_as_date, "%b"), month.abb, ordered = T)
  ) |> 
  ggplot(aes(x = month, y = interest_amount)) +
  geom_col(width = .7) +
  geom_text(aes(label = if_else(interest_amount == 0, NA, interest_amount)), vjust = -0.5, size = 3) +
  facet_wrap(~ year, ncol = 1, scales = "free_x") +
  scale_y_continuous(
    n.breaks = 15,
    minor_breaks = F,
    expand = expand_scale(mult = c(0.05, 0.1))
  ) +
  labs(
    title = "Interest cumulative amount by month (per year)",
    subtitle = str_c("today: ", pb_today),
    x = "Month",
    y = "Amount (€)"
  )

save_plot("interest/interest-cumulative-amount-21-by-month-by-year.png")
save_plot("interest-cumulative-amount-21-by-month-by-year.png")
