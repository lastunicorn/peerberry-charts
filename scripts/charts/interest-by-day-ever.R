library(tidyverse)
library(lubridate)


# ------------------------------------------------------------------------------
# Interest by day (ever)

pb_transactions |> 
  filter(is.element(type, c("BUYBACK_INTEREST", "REPAYMENT_INTEREST"))) |>
  group_by(date) |>
  arrange(date) |> 
  summarize(interest_amount = sum(amount)) |>
  ggplot(aes(x = date)) +
  geom_col(aes(y = interest_amount)) +
  geom_smooth(aes(y = interest_amount), method = 'loess', formula = 'y ~ x') +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y", minor_breaks = NULL) +
  guides(x = guide_axis(angle = 60)) +
  labs(
    title = "Interest daily (ever)",
    x = "Date",
    y = "Amount (€)"
  )

# Save
ggsave("charts/interest-by-day-ever.png", width=30, height=20, units="cm", dpi=300)
