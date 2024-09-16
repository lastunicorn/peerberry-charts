library(tidyverse)

pb |> 
  mutate(purchase_year = year(date_of_purchase)) |> 
  ggplot(aes(x = country)) +
  geom_bar() +
  facet_wrap(~purchase_year) +
  guides(x = guide_axis(angle = 90)) +
  labs(title = "Investments (count) by Country")


pb |> 
  mutate(purchase_year = year(date_of_purchase)) |> 
  group_by(purchase_year, country) |> 
  summarize(
    sum = sum(invested_amount)
  ) |> 
  ggplot(aes(x = country, y = sum)) +
  geom_col() +
  facet_wrap(~purchase_year) +
  guides(x = guide_axis(angle = 90)) +
  labs(title = "Investments (amount) by Country")
