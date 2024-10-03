library(tidyverse)

# Set locale to English
Sys.setlocale("LC_ALL", "English_United States.utf8")

# Set count of digits displayed in tibbles columns of type double
old_option_pillar.sigfig <- options(pillar.sigfig = 20)

# Load utility functions
source("scripts/utils/utils.R")
