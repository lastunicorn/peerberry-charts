library(tidyverse)


# ------------------------------------------------------------------------------
# Individual investment amount over time (by year)

pb_transactions |> 
  filter(type == "INVESTMENT") |> 
  mutate(
    year = year(date),
    date_without_year = make_date(2000, month(date), day(date))
  ) |> 
  ggplot(aes(x = date_without_year, y = amount)) +
  geom_point(color = "#666", size = .75, position = "jitter") +
  facet_wrap(~ year, ncol = 1) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b", minor_breaks = NULL) +
  labs(
    title = "Individual investment amount over time (per year)",
    x = "Date",
    y = "Investment size (€)"
  )

# Save
ggsave("charts/investment-amount-over-time-by-year.png", width=30, height=20, units="cm", dpi=300)
