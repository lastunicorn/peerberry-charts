# ------------------------------------------------------------------------------
# Invested cumulative amount by month in last year per country.

for (current_year in pb_transactions.first_year:pb_transactions.last_year) {
  pb_transactions |>  
    filter(type == "INVESTMENT") |> 
    mutate(
      year = year(date),
      month_as_date = floor_date(date, "month")
    ) |>
    filter(year == current_year) |>
    group_by(country, month_as_date) |> 
    summarize(
      sum = sum(amount),
      .groups = "drop"
    ) |> 
    mutate(
      x = format(month_as_date, "%b")
    ) |> 
    ggplot(aes(x = reorder(x, month_as_date), y = sum)) +
    geom_col() +
    facet_wrap(~ country) +
    scale_x_discrete(
      limits = format(seq.Date(from = pb_transactions.current_year_as_date, by = "month", length.out = 12), "%b")
    ) +
    guides(x = guide_axis(angle = 70)) +
    labs(
      title = str_c("Investment amount by month in ", current_year, " (per country)"),
      subtitle = str_c("today: ", pb_today),
      x = "Purchase month",
      y = "Amount (€)"
    )
  
  save_plot(str_c("history-investment-amount-by-month-per-country/investment-cumulative-amount-by-month-per-country-", current_year, ".png"))
  
  if(current_year == pb_transactions.current_year) {
    save_plot(str_c("investment/investment-cumulative-amount-03-by-month-per-country-", current_year, ".png"))
  }
}
