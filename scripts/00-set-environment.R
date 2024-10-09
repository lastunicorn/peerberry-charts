library(tidyverse)


# ------------------------------------------------------------------------------
# Set locale to English
#Sys.setlocale("LC_ALL", "English_United States.utf8")
Sys.setlocale("LC_ALL", "en_US.utf8")


# ------------------------------------------------------------------------------
# Set count of digits displayed in tibbles columns of type double
old_option_pillar.sigfig <- options(pillar.sigfig = 20)

#options(old_option_pillar.sigfig)
#remove(old_option_pillar.sigfig)


# ------------------------------------------------------------------------------
# Load utility functions
source("scripts/utils/utils.R")


# ------------------------------------------------------------------------------
# Constants

pb.data_dir <- "data"
pb.charts_dir <- "charts"