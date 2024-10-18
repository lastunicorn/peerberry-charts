# ------------------------------------------------------------------------------
# Investment late by late-time per country (all years)

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
  facet_wrap(~ country) +
  labs(
    title = "Investment late by late-time per country (all years)",
    x = "Late days",
    y = "Count",
    fill = "Year"
  )

save_plot("investment-late-by-latetime-per-country.png", height = 20, width = 60)
