# Build Project

To generate the charts you need:
- Install R  
- Install RStudio
- Export PeerBerry transactions
- Open project and run scripts

## Install R

Download and install R language from the official site:
- https://cran.r-project.org/

## Install RStudio

Download and install RStudio IDE from the official site:
- https://posit.co/downloads/

## Export PeerBerry transactions

**Step 1: Login into you PeerBerry account.**

**Step 2: Export current investments**
- Navigate to: `My investments -> Current investments -> Download selected investments`
- Save the file into: `/data-raw/investments - current.xlsx`

**Step 3: Export finished investments**
- Navigate to: `My investments -> Finished investments -> Download selected investments`
- Save the file into: `/data-raw/investments - finished.xlsx`

**Step 4: Export transactions**
- Navigate to: `Statement`
- Scroll to `Transactions` section
- Select the date range for the entire 2023 year
- Save the file into: `/data-raw/transactions - 2023.xlsx`
- Repeat for 2024

## Open project and run scripts

**Step 1: Open the project**
- Open the `PeerBerry Charts.Rproj` into RStudio.

**Step 2: Run scripts**
- `/scripts/00-set-environment.R`
- `/scripts/01-import-all.R`
- `/scripts/03-generate-all-charts.R`
