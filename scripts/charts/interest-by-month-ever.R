library(tidyverse)


# ------------------------------------------------------------------------------
# Interest by month (ever)

pb_transactions |>
  filter(is.element(type, c("BUYBACK_INTEREST", "REPAYMENT_INTEREST"))) |>
  mutate(
    month_as_date = floor_date(date, "month")
  ) |>
  group_by(month_as_date) |>
  arrange(date) |>
  summarize(interest_amount = sum(amount)) |>
  ggplot(aes(x = month_as_date, y = interest_amount)) +
  geom_col() +
  geom_text(aes(label = interest_amount), vjust = -0.5, size = 3) +
  guides(x = guide_axis(angle = 60)) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y", minor_breaks = NULL) +
  labs(
    title = "Interest by month (ever)",
    x = "Month",
    y = "Amount (€)"
  )


# Save
ggsave("charts/interest-by-month-ever.png", width=30, height=20, units="cm", dpi=300)
