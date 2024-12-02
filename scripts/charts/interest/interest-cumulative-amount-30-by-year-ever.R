# ------------------------------------------------------------------------------
# Interest cumulative amount by year (ever)

pb_transactions |> 
  filter(is.element(type, c("BUYBACK_INTEREST", "REPAYMENT_INTEREST"))) |>
  mutate(
    year = year(date),
    year = as.factor(year)
  ) |>
  group_by(year) |>
  summarize(interest_amount = sum(amount)) |>
  arrange(year) |> 
  ggplot(aes(x = year, y = interest_amount)) +
  geom_col(width = .5) +
  geom_text(
    aes(label = interest_amount),
    vjust = -0.5,
    size = 3
  ) +
  labs(
    title = "Interest cumulative amount by year (ever)",
    subtitle = str_c("today: ", pb_today),
    x = "Year",
    y = "Amount (€)"
  )

save_plot("interest/interest-cumulative-amount-30-by-year-ever.png")
save_plot("interest-cumulative-amount-30-by-year-ever.png")
