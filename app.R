
#-------------------------------------------------------------------------------
#
# script for starting the app
#
#-------------------------------------------------------------------------------

require(dplyr)
require(ggplot2)
require(gridExtra)
require(expm)

source("functions.R") 
source("interface.R")

shinyApp(ui, server)
