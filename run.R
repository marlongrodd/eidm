#-------------------------------------------------------------------------------
#
# Script for running the App via shiny-server
#
# Copyright (C) 2023 Marlon Grodd, Susanne Weber, Martin Wolkewitz
# 
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
# 
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#
# Neseccary Packages
# install.packages("dplyr")
# install.packages("ggplot2")
# install.packages("gridExtra")
# install.packages("expm")
# install.packages("shiny")
#
#-------------------------------------------------------------------------------

library(shiny)

#--------------------------------- if you run local ----------------------------

setwd(choose.dir()) # choose the folder of this script
# setwd("put/path/here") # choose the folder of this script
runApp()


