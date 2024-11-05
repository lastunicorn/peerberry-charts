# ------------------------------------------------------------------------------
# Libraries

library(tidyverse)


# ------------------------------------------------------------------------------
# Locale

Sys.setlocale("LC_ALL", "en_US.utf8")


# ------------------------------------------------------------------------------
# Tibbles configuration

# Set count of digits displayed in tibbles columns of type double
old_option_pillar.sigfig <- options(pillar.sigfig = 20)

#options(old_option_pillar.sigfig)
#remove(old_option_pillar.sigfig)


# ------------------------------------------------------------------------------
# Utilities

source("scripts/environment/utils.R")
source("scripts/environment/config.R")
