# Meta Data ---------------------------------------------------------------
#
# Version:      1.0
# Author:       Oleksii Dovhaniuk
# Created on:   2024-01-16
# Updated on:   2024-01-16
#
# Description:  This script installs all required packages for 
#               the TMM Analysis project if the R and Rstudio are
#               freshly installed.
#
# Location:     script/_install_packages.R


# Install Packages --------------------------------------------------------


install.packages(
    c(  
        'testthat',
        'combinat',
        'digest',
        'devtools',
        'ggplot2',
        'janitor',
        'glue',
        'lubridate',
        'plumber',
        'tidyverse'
    )
)
