library(tidyverse)


# ------------------------------------------------------------------------------
# Invested count by amount (ever)

pb_transactions |> 
  filter(type == "INVESTMENT") |> 
  ggplot(aes(x = amount)) +
  geom_histogram(binwidth = 1) +
  labs(
    title = "Investments count by amount (ever)",
    x = "Investment size (€)",
    y = "Investments count"
  ) +
  scale_x_continuous(breaks = round(seq(min(pb_transactions$amount), max(pb_transactions$amount), by = 20), 1))

# Save
ggsave("charts/investment-count-by-amount-ever.png", width=30, height=20, units="cm", dpi=300)