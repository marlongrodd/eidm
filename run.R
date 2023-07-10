#-------------------------------------------------------------------------------
#
# Script for running the App via shiny-server
#
# Neseccary Packages
# install.packages("dplyr")
# install.packages("ggplot2")
# install.packages("gridExtra")
# install.packages("expm")
#
#-------------------------------------------------------------------------------

library(shiny)

#--------------------------------- if you run local ----------------------------

setwd(choose.dir()) # choose the folder of this script
runApp()

#--------------------------------- if you run local ----------------------------
# runApp(launch.browser = F, port = 443)

