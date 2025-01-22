library(tidyverse)


# ------------------------------------------------------------------------------
# Investments count by length in days (per year)

temp <- pb_loans |> 
  mutate(
    estimated_days = as.integer(estimated_final_payment_date - date_of_purchase),
    purchese_year = year(date_of_purchase)
  ) |> 
  filter(estimated_days <= 100)

temp |> 
  ggplot(aes(x = estimated_days)) +
  geom_bar() +
  facet_wrap(~ purchese_year, ncol = 1) +
  scale_x_continuous(
    breaks = round(seq(0, max(temp$estimated_days), by = 2), 1)
  ) +
  scale_y_continuous(
    n.breaks = 10,
    minor_breaks = F
  ) +
  expand_limits(x = 0) +
  labs(
    title = "Investment count by length in days (per year) - 100 days",
    subtitle = str_c("today: ", pb_today),
    x = "Days",
    y = "Count"
  )

remove(temp)

save_plot("investment/investment-count-04-by-length-per-year.png")
save_plot("investment-count-04-by-length-per-year.png")
