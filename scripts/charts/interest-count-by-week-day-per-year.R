library(tidyverse)


# ------------------------------------------------------------------------------
# Interest count by week days (per year)

pb_transactions |> 
  filter(is.element(type, c("BUYBACK_INTEREST", "REPAYMENT_INTEREST"))) |>
  mutate(
    year = year(date),
    week_day = wday(date, label = TRUE)
  ) |> 
  ggplot(aes(x = week_day)) +
  geom_bar(width = .75) +
  facet_wrap(~ year, ncol = 1) +
  labs(
    title = "Interest count by week days (per year)",
    x = "Week day",
    y = "Interest count"
  )

# Save
ggsave("charts/interest-count-by-week-day-per-year.png", width=30, height=20, units="cm", dpi=300)
