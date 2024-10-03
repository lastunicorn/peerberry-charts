library(tidyverse)


# ------------------------------------------------------------------------------
# Interest count by week days (per month)

pb_transactions |> 
  filter(is.element(type, c("BUYBACK_INTEREST", "REPAYMENT_INTEREST"))) |>
  mutate(
    month_as_date = floor_date(date, "month"),
    week_day = wday(date, label = TRUE)
  ) |> 
  group_by(month_as_date, week_day) |> 
  summarize(count = n()) |> 
  ggplot(aes(x = week_day, y = count)) +
  geom_col(width = .75) +
  geom_text(aes(label = count), vjust = -0.5, size = 3, color = "#666") +
  facet_wrap(~ month_as_date, labeller = as_labeller(month_year_labeller)) +
  labs(
    title = "Interest count by week days (per month)",
    x = "Week day",
    y = "Interest count"
  )

# Save
ggsave("charts/interest-count-by-week-day-per-month.png", width=30, height=20, units="cm", dpi=300)
