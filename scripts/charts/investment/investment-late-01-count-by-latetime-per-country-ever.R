# ------------------------------------------------------------------------------
# Investment late count by late-time per country (all years)

pb_loans |> 
  filter(status == "FINISHED") |> 
  mutate(
    year = year(date_of_purchase),
    year = as.factor(year),
    late_days = as.numeric(last_payment_date - estimated_final_payment_date)
  ) |> 
  group_by(year, late_days) |> 
  ggplot(aes(x = late_days, fill = year)) +
  geom_bar(position = "dodge", width = .5) +
  facet_wrap(~ country, ncol = 3) +
  scale_x_continuous(n.breaks = 20) +
  labs(
    title = "Investment late count by late-time per country (all years)",
    subtitle = str_c("today: ", pb_today),
    x = "Late days",
    y = "Count",
    fill = "Year"
  )

save_plot("investment-late-01-count-by-latetime-per-country.png")
