# ------------------------------------------------------------------------------
# Calculate total amount for each transaction type

# Note: the value is stored in a temporary tibble because when it is displayed
# it looses number precision.

temp <- pb_transactions |> 
  group_by(type) |> 
  summarize(
    n = n(),
    total = sum(amount)
  )


# ------------------------------------------------------------------------------
# Calculate uninvested amount at a specific date.

# Note: the value is stored in a temporary tibble because when it is displayed
# it looses number precision.

temp <- pb_transactions |> 
  filter(date <= "2024-10-02") |> 
  mutate(
    amount2 = ifelse(type == "INVESTMENT", -amount, amount)
  ) |> 
  summarize(
    total = sum(amount2)
  ) |> 
  print()

# ------------------------------------------------------------------------------
# Count long investments (> 35 days)

pb |> 
  filter(status == "CURRENT" | status == "LATE") |>
  filter(estimated_final_payment_date - date_of_purchase > 35) |> 
  count()
