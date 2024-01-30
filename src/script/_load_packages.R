# Meta Data ---------------------------------------------------------------
#
# Version:      1.0
# Author:       Oleksii Dovhaniuk
# Created on:   2024-01-16
# Updated on:   2024-01-16
#
# Description:  This script loads all required packages for
#               the TMM Analysis project. Can be called 
#               from any other job script.
#
# Location:     script/_load_packages.R
#


# Load Packages --------------------------------------------------------

library(devtools)
library(digest)
library(combinat)
library(ggplot2)
library(glue)
library(janitor)
library(lubridate)
library(plumber)
library(tidyverse)
