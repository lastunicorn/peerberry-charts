library(tidyverse)
library(lubridate)


# ------------------------------------------------------------------------------
# Total interest by date

pb_transactions |> 
  arrange(date) |> 
  mutate(
    amount3 = ifelse(is.element(type, c("BUYBACK_INTEREST", "REPAYMENT_INTEREST")), amount, 0),
    total_interest = map_dbl(date, ~ sum(amount3[date <= .x]))
  ) |>
  ggplot(aes(x = date)) +
  geom_area(aes(y = total_interest), alpha = .5) +
  geom_step(aes(y = total_interest), direction = "hv", alpha = .5, color = "black") +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y", minor_breaks = NULL) +
  guides(x = guide_axis(angle = 60)) +
  labs(
    title = "Total interest (ever)",
    x = "Date",
    y = "Amount (€)"
  )

# Save
ggsave("charts/interest-total-ever.png", width=30, height=20, units="cm", dpi=300)
