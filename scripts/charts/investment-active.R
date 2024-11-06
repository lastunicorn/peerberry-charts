# ------------------------------------------------------------------------------
# Investment amount active by country

pb_loans |> 
  filter(status %in% c("CURRENT", "LATE")) |> 
  group_by(country, status) |> 
  summarize(
    total_amount = sum(invested_amount),
    .groups = "drop"
  ) |> 
  ggplot(aes(x = country, y = total_amount, fill = factor(desc(status)))) +
  geom_col() +
  scale_fill_manual(
    values = c("gray30", "gray50"),
    labels = c("Late", "Current")
  ) +
  geom_text(
    aes(label = total_amount),
    size = 3,
    position = position_stack(vjust = .5),
    color = "white"
  )+
  guides(x = guide_axis(angle = 60)) +
  labs(
    title = "Investment amount active by country",
    x = "Country",
    y = "Amount (€)",
    fill = "Status"
  )

save_plot("investment-amount-active.png")
