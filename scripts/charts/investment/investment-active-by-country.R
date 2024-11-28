# ------------------------------------------------------------------------------
# Investment amount active by country

pb_loans |> 
  filter(status %in% c("CURRENT", "LATE")) |> 
  group_by(country, status) |> 
  summarize(
    total_amount = sum(invested_amount),
    .groups = "drop"
  ) |> 
  mutate(
    country = fct(country, levels = pb_country_levels)
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
  scale_x_discrete(drop = FALSE) +
  guides(x = guide_axis(angle = 60)) +
  labs(
    title = "Investment amount active by country",
    subtitle = str_c("today: ", pb_today),
    x = "Country",
    y = "Amount (€)",
    fill = "Status"
  )

save_plot("investment-amount-active-by-country.png")
