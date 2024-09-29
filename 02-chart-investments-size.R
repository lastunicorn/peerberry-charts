library(tidyverse)


# ------------------------------------------------------------------------------
# Individual investment size over time

pb_transactions |> 
  filter(type == "INVESTMENT") |> 
  mutate(year = year(date)) |> 
  mutate(year = as.factor(year)) |> 
  ggplot(aes(x = date, y = amount, color = year)) +
  geom_point()  +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
  guides(x = guide_axis(angle = 60)) +
  labs(
    title = "Individual investments size over time",
    x = "Date",
    y = "Investment size"
  )

# Save
ggsave("charts/investments-amount-over-time.png", width=30, height=20, units="cm", dpi=300)


# ------------------------------------------------------------------------------
# One chart per year
#   - Individual investment size over time

pb_transactions |> 
  filter(type == "INVESTMENT") |> 
  mutate(
    year = year(date),
    date_without_year = make_date(2000, month(date), day(date))
  ) |> 
  ggplot(aes(x = date_without_year, y = amount)) +
  geom_point(color = "#666") +
  facet_wrap(~ year, ncol = 1) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  labs(
    title = "Individual investments size over time (per year)",
    x = "Date",
    y = "Investment size"
  )

# Save
ggsave("charts/investments-amount-over-time-in-each-year.png", width=30, height=20, units="cm", dpi=300)