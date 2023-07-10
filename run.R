#-------------------------------------------------------------------------------
#
# Script for running the App via shiny-server
#
#-------------------------------------------------------------------------------

library(shiny)

#--------------------------------- if you run local ----------------------------

setwd(choose.dir()) # choose the folder of this script
runApp()

#--------------------------------- if you run local ----------------------------
# runApp(launch.browser = F, port = 443)

