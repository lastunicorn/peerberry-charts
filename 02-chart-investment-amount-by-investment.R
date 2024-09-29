library(tidyverse)


# ------------------------------------------------------------------------------
# Invested amount by investment (ever)

pb_transactions |> 
  filter(type == "INVESTMENT") |> 
  ggplot(aes(x = amount)) +
  geom_histogram(binwidth = 1) +
  labs(
    title = "Invested amount by investment (ever)",
    x = "Investment amount",
    y = "Investments (Count)"
  ) +
  scale_x_continuous(breaks = round(seq(min(pb_transactions$amount), max(pb_transactions$amount), by = 20), 1))

# Save
ggsave("charts/investment-amount-by-investment-ever.png", width=30, height=20, units="cm", dpi=300)


# ------------------------------------------------------------------------------
# Invested amount by investment (per year)

pb_transactions |> 
  filter(type == "INVESTMENT") |> 
  mutate(year = year(date)) |>
  ggplot(aes(x = amount)) +
  geom_histogram(binwidth = 1) +
  facet_wrap(~ year, ncol = 1) +
  labs(
    title = "Invested amount by investment (per year)",
    x = "Investment amount",
    y = "Investments (Count)"
  ) +
  scale_x_continuous(breaks = round(seq(min(pb_transactions$amount), max(pb_transactions$amount), by = 20), 1))

# Save
ggsave("charts/investment-amount-by-investment-per-year.png", width=30, height=20, units="cm", dpi=300)
