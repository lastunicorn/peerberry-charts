library(tidyverse)

# ------------------------------------------------------------------------------
# Individual investment size over time

pb |> 
  mutate(purchase_year = year(date_of_purchase)) |> 
  mutate(purchase_year = as.factor(purchase_year)) |> 
  arrange(purchase_year) |> 
  ggplot(aes(x = date_of_purchase, y = invested_amount)) +
  geom_point() +
  facet_wrap(~purchase_year, scales = "free_x") +
  guides(x = guide_axis(angle = 90)) +
  labs(
    title = "Individual investments size over time (per year)",
    x = "Date",
    y = "Investment size"
  )

# Save
ggsave("charts/investments-amount-over-time-in-each-year.png", width=30, height=20, units="cm", dpi=300)


# ------------------------------------------------------------------------------
# One chart per year
#   - Individual investment size over time

pb |> 
  mutate(purchase_year = year(date_of_purchase)) |> 
  mutate(purchase_year = as.factor(purchase_year)) |> 
  arrange(purchase_year) |> 
  ggplot(aes(x = date_of_purchase, y = invested_amount, color = purchase_year)) +
  geom_point() +
  guides(x = guide_axis(angle = 90)) +
  labs(
    title = "Individual investments size over time",
    x = "Date",
    y = "Investment size"
  )

# Save
ggsave("charts/investments-amount-over-time.png", width=30, height=20, units="cm", dpi=300)
