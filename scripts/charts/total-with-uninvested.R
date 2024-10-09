library(dplyr)
library(purrr)


# ------------------------------------------------------------------------------
# Total amount with uninvested amount by date

pb_transactions |> 
  arrange(date) |> 
  mutate(
    amount2 = ifelse(type == "INVESTMENT", -amount, amount),
    uninvested_amount = map_dbl(date, ~ sum(amount2[date <= .x])),
    amount3 = ifelse(is.element(type, c("DEPOSIT", "BUYBACK_INTEREST", "REPAYMENT_INTEREST")), amount, 0),
    total_amount = map_dbl(date, ~ sum(amount3[date <= .x])),
  ) |>
  group_by(date) |> 
  summarize(
    uninvested_amount = first(uninvested_amount),
    total_amount = first(total_amount)
  ) |>
  ggplot(aes(x = date)) +
  geom_area(aes(y = total_amount), alpha = .5, fill = "#bada55") +
  geom_step(aes(y = total_amount), direction = "hv", color = "darkgreen") +
  geom_area(aes(y = uninvested_amount), alpha = .5) +
  geom_step(aes(y = uninvested_amount), direction = "hv", alpha = .5, color = "black") +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y", minor_breaks = NULL) +
  guides(x = guide_axis(angle = 60)) +
  labs(
    title = "Total amount with uninvested amount by date (ever)",
    x = "Date",
    y = "Amount (€)"
  )

# Save
ggsave("charts/total-amount-with-uninvested-amount-by-date.png", width=30, height=20, units="cm", dpi=300)
