pb |>
  mutate(
    interest = received_payments - invested_amount,
    days = date_of_received_payment - date_of_purchase) |>
  ggplot(aes(x = days, y = interest)) +
  geom_point()

pb |>
  mutate(
    interest = received_payments - invested_amount,
    days = date_of_received_payment - date_of_purchase) |>
  ggplot(aes(x = days, y = interest)) +
  geom_point() +
  facet_wrap(~country)
