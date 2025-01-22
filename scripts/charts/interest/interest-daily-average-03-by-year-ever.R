# ------------------------------------------------------------------------------
# Interest daily average by year (ever)

pb_transactions |>
  filter(is.element(type, c("BUYBACK_INTEREST", "REPAYMENT_INTEREST"))) |>
  mutate(
    year = year(date)
  ) |>
  group_by(year) |>
  summarize(
    amount = sum(amount)
  ) |>
  mutate(
    interest_amount = amount / pb_transactions.days_in_year(year)
  ) |> 
  arrange(year) |>
  ggplot(aes(x = as.factor(year), y = interest_amount)) +
  geom_col(width = .5) +
  geom_text(
    aes(label = format(round(interest_amount, 2), nsmall = 2)),
    vjust = -0.5,
    size = 3
  ) +
  scale_y_continuous(
    expand = expand_scale(mult = c(0.05, 0.075)),
    n.breaks = 20,
    minor_breaks = F
  ) +
  labs(
    title = "Interest daily average by year (ever)",
    subtitle = str_c("today: ", pb_today),
    x = "Year",
    y = "Amount (€)"
  )

save_plot("interest/interest-daily-average-03-by-year-ever.png")
save_plot("interest-daily-average-03-by-year-ever.png")
