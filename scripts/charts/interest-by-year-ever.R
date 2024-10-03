library(tidyverse)


# ------------------------------------------------------------------------------
# Interest by year (ever)

pb_transactions |> 
  filter(is.element(type, c("BUYBACK_INTEREST", "REPAYMENT_INTEREST"))) |>
  mutate(
    year = year(date),
    year = as.factor(year)
  ) |>
  group_by(year) |>
  summarize(interest_amount = sum(amount)) |>
  arrange(year) |> 
  ggplot(aes(x = year)) +
  geom_col(aes(y = interest_amount), width = .5) +
  labs(
    title = "Interest by year (ever)",
    x = "Year",
    y = "Amount (€)"
  )

# Save
ggsave("charts/interest-by-year-ever.png", width=30, height=20, units="cm", dpi=300)
