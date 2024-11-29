library(tidyverse)


# ------------------------------------------------------------------------------
# Investment count by length (days) in 2024 (per country).

for (current_year in pb_transactions.first_year:pb_transactions.last_year) {
  pb_loans |> 
    mutate(
      estimated_days = as.integer(estimated_final_payment_date - date_of_purchase),
      purchese_year = year(date_of_purchase)
    ) |> 
    filter(purchese_year == current_year) |> 
    ggplot(aes(x = estimated_days)) +
    geom_bar() +
    facet_wrap(~ country, ncol = 2) +
    scale_x_continuous(
      n.breaks = 30
    ) +
    expand_limits(x = 0) +
    labs(
      title = str_c("Investment count by length (days) in ", current_year, " (per country)"),
      subtitle = str_c("today: ", pb_today),
      x = "Days",
      y = "Count"
    )
  
  save_plot(str_c("investment/investment-count-05-by-length-per-country-", current_year, ".png"), width = 20, height = 30)
}
