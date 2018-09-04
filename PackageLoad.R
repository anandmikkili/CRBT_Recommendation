'
Script    : PackageLoad
Created   : August 31, 2018
'
# Load required libraries
pkgLoad <- function(package) {
  if (!require(package, character.only = TRUE)) {
    install.packages(package, dep = TRUE, repos = "http://cran.csie.ntu.edu.tw/")
    if (!require(package, character.only = TRUE))
      stop("Package not found")
  }
  suppressMessages(library(package, character.only = TRUE))
}
pkgLoad("dplyr")
pkgLoad("data.table")
pkgLoad("tidyr")
pkgLoad("RJDBC")
pkgLoad("properties")
pkgLoad("shiny")
pkgLoad("shinydashboard")
pkgLoad("sunburstR")
pkgLoad("DT")
pkgLoad("shinydashboardPlus")
pkgLoad("shinyjs")