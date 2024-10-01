# ------------------------------------------------------------------------------
# Setup environment

source("scripts/misc/environment-setup.R")


# ------------------------------------------------------------------------------
# Generate charts - Interest

source("scripts/charts/interest-by-day-per-year.R")
source("scripts/charts/interest-by-day-ever.R")
source("scripts/charts/interest-by-month-ever.R")
source("scripts/charts/interest-by-year-ever.R")
source("scripts/charts/interest-total.R")

# ------------------------------------------------------------------------------
# Generate charts - Investment

source("scripts/charts/investment-amount-by-country.R")
source("scripts/charts/investment-amount-by-month-ever.R")
source("scripts/charts/investment-amount-by-month-2024.R")
source("scripts/charts/investment-amount-by-year.R")

source("scripts/charts/investment-amount-over-time.R")

source("scripts/charts/investment-count-by-amount.R")
#source("scripts/charts/investment-count-by-country.R")
source("scripts/charts/investment-count-by-length.R")

# ------------------------------------------------------------------------------
# Generate charts - Uninvested (cash drag)

source("scripts/charts/uninvested-by-day-ever.R")
source("scripts/charts/uninvested-by-day-per-year.R")
source("scripts/charts/uninvested-by-month-ever.R")
