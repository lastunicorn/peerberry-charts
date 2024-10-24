# Build Charts

To generate the charts you need to:
1. Install R
2. Export PeerBerry's transactions (3 files)
3. Run script

## 1) Install R

Download and install R language from the official site:
- https://cran.r-project.org/

## 2) Export PeerBerry's transactions

The project needs three .xlsx files that must be exported from PeerBerry:

- current investments
- finished investments
- transactions (one file per year)

**Note**

- The downloaded files must be placed into the `/data-raw` directory in the project's root path.
- The file names are important. Make sure they are spelled correctly.

**Step 1: Login into your PeerBerry account.**

**Step 2: Export current investments**

- Navigate to: `My investments -> Current investments -> Download selected investments`
- Save the file into: `/data-raw/investments - current.xlsx`

**Step 3: Export finished investments**

- Navigate to: `My investments -> Finished investments -> Download selected investments`
- Save the file into: `/data-raw/investments - finished.xlsx`

**Step 4: Export transactions**

- Navigate to: `Statement`
- Scroll to `Transactions` section
- Select the date range for an entire year (for example 2023)
- Save the file into: `/data-raw/transactions - 2023.xlsx`
- Repeat for another year

## 3') Run script (Windows)

Go to the project's root path and, from command line, run the script:

```bat
run-all.cmd
```

The charts are generated under directory `/charts` from the root path of the project.

**Note**

- The path to the from R installation directory must be specified in PATH environment variable from Windows.
  - `RScript.exe` file is used to run the R scripts.

## 3'') Run script (Linux)

Go to the project's root path and, from command line, run the script:

```shell
run-all.sh
```

The charts are generated under directory `/charts` from the root path of the project.

# Edit Charts

To edit the charts you need RStudio which is free to install for non-commercial purposes.

## 1) Install RStudio

Download and install RStudio IDE from the official site:

- https://posit.co/downloads/

## 2) Open project

**Step 1: Open the project**

- Open the `PeerBerry Charts.Rproj` into RStudio.

**Step 2: Important scripts**

Start from here:

- `/run-all.R`

This script is calling a list of other scripts:

- `/scripts/00-set-environment.R`
- `/scripts/01-import-all.R`
- `/scripts/03-generate-all-charts.R`

The names should be self explanatory.
