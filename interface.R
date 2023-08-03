#-------------------------------------------------------------------------------
#
#  Script for generating the Interface and overall appearance
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
#-------------------------------------------------------------------------------

# Define UI for application
ui <-  fluidPage(
  
  
  div(HTML('<img src="combacte_logo.png" width = 240 alt="COMBACTE logo" style="margin-top: 25px">
            <img src="ukl_logo.png" width = 240 alt="ukl logo" style="float:right;margin-top: 35px">
            <img src="fdm_logo.png" width = 240 alt="fdm logo" style="float:right">')),
      
  # div(img(src   = "combacte_logo.jpg",
  #         width = 240,
  #         alt   = "COMBACTE Logo")),
  
  # fluidRow(
  #   column(1, 
  #          tags$img(src   = "combacte_logo.jpg",
  #                                 width = 240,
  #                                 alt   = "COMBACTE Logo"),
  #          tags$img(src   = "fdm_logo.png",
  #                                 width = 240,
  #                                 alt   = "COMBACTE Logo"),
  #          tags$img(src   = "ukl_logo.png",
  #                                 width = 240,
  #                                 alt   = "COMBACTE Logo"))
  # ),
  
  # Banner
  # tags$figure(
  #   align = "left",
  #   tags$img(src   = "combacte_logo.jpg",
  #            width = 240,
  #            alt   = "COMBACTE Logo"),
  #   
  #   align = "right",
  #   tags$img(src   = "fdm_logo.png",
  #            width = 240,
  #            alt   = "COMBACTE Logo")),
  # 
  # tags$figure(
  #   align = "right",
  #   tags$img(src   = "ukl_logo.png",
  #            width = 240,
  #            alt   = "COMBACTE Logo")),
  
  # Application title
  titlePanel("Extended illness-death model with constant transition hazards"),
  div(HTML('This is a tool to visualize time-dependend effects in a multi-state-setting considering an extended illness-death model with constant hazards.<br>
          <br>
          <ul>
            <li>Two groups can be compared (Group A and Group B)</li>
            <li>Hazards can be provided in two ways:<br>
                <ul style="list-style-type:square;">
                  <li>Explicit for each group (Group A and B)</li>
                  <li>Baseline Hazards (Group A) and hazard ratios</li>
                </ul> 
            </li>
            <li>Description of items:<br>
                <ul style="list-style-type:square;">
                  <li>"limit of x-axis": set the time horizon</li>
                  <li>"order for stacked plot": order of the ploted areas in the stacked probability plots</li>
                  <li>"Show PAF plot": Render the plot the population attributable fraction</li>
                  <li>"Show PAF plot": Render the plot the attributable mortality</li>
                </ul>
            </li>
            <li>Formula for Hazards:</li>')),
  
  tags$figure(
    align = "center",
    tags$img(src   = "hazard.png",
             width = 300,
             alt   = "Formula for Hazards")),
  
  br(),
  
  # create one row with input and output:
  fluidRow(
    
    #allow for two input options
    navbarPage("Group Hazards or Hazard ratio?",
               
               #, create two separate tabPanels for each input option
               tabPanel("Group Hazards",
                        fluidRow(column(width=3,
                                        fluidRow(
                                          column(width = 6,
                                                 "Group A",      
                                                 numericInput("A01_a", "\u03BB\u2080\u2081", 0.05),
                                                 numericInput("A02_a", "\u03BB\u2080\u2082", 0.1),
                                                 numericInput("A03_a", "\u03BB\u2080\u2083", 0.05),
                                                 numericInput("A14_a", "\u03BB\u2081\u2084", 0.1),
                                                 numericInput("A15_a", "\u03BB\u2081\u2085", 0.05)
                                          ),
                                          column(width = 6,
                                                 "Group B",      
                                                 numericInput("A01_b", "\u03BB\u2080\u2081", 0.05),
                                                 numericInput("A02_b", "\u03BB\u2080\u2082", 0.1),
                                                 numericInput("A03_b", "\u03BB\u2080\u2083", 0.05),
                                                 numericInput("A14_b", "\u03BB\u2081\u2084", 0.1),
                                                 numericInput("A15_b", "\u03BB\u2081\u2085", 0.05)
                                          )),
                                        
                                        fluidRow(column(width = 12,
                                                        numericInput("x_lim", "limit of x-axis", 30),
                                                        textInput("order", 
                                                                  "order for stacked plot from top to bottom according to number of state in eidm
                                                         (enter a vector (comma delimited))", "2,3,0,5,4,1"),
                                                        checkboxInput("PAF", "Show PAF plot?", FALSE),
                                                        checkboxInput("AM", "Show AM plot?", FALSE),
                                        ))),
                                 column(9,
                                        tags$figure(
                                          align = "center",
                                          tags$figcaption("The extended illness death model with constant transition hazards"),
                                          tags$img(
                                            src   = "eidm.png",
                                            width = 600,
                                            alt   = "The extended illness death model with constant transition hazards")),
                                        br(),
                                        br(),
                                        plotOutput("stackedPlot2"),
                                        plotOutput("stackedPlot2a", height = "100px"),
                                        plotOutput("stackedPlot3"),
                                        plotOutput("stackedPlot4"))))
               
               ,
               tabPanel("Hazard ratio",
                        fluidRow(column(width = 3,
                                        fluidRow(
                                          column(width = 6,
                                                 "Baseline",      
                                                 numericInput("A01_c", "\u03BB\u2080\u2081", 0.05),
                                                 numericInput("A02_c", "\u03BB\u2080\u2082", 0.1),
                                                 numericInput("A03_c", "\u03BB\u2080\u2083", 0.05),
                                                 numericInput("A14_c", "\u03BB\u2081\u2084", 0.1),
                                                 numericInput("A15_c", "\u03BB\u2081\u2085", 0.05)
                                          ),
                                          column(width = 6,
                                                 "Hazard ratio",      
                                                 numericInput("A01_hr", "HR\u2080\u2081", 2),
                                                 numericInput("A02_hr", "HR\u2080\u2082", 1),
                                                 numericInput("A03_hr", "HR\u2080\u2083", 1),
                                                 numericInput("A14_hr", "HR\u2081\u2084", 1),
                                                 numericInput("A15_hr", "HR\u2081\u2085", 0.5)
                                          )),
                                        fluidRow(column(width = 12,
                                                        numericInput("x_lim_hr", "limit of x-axis", 30),
                                                        textInput("order_hr", "order for stacked plot from top to bottom according to number of state in eidm
                                                                   (enter a vector (comma delimited))", "2,3,0,5,4,1"),
                                                        checkboxInput("PAF_hr", "Show PAF plot?", FALSE),
                                                        checkboxInput("AM_hr", "Show AM plot?", FALSE),
                                        ))),
                                 column(9,
                                        tags$figure(
                                          align = "center",
                                          tags$figcaption("The extended illness death model with constant transition hazards"),
                                          tags$img(
                                            src   = "eidm.png",
                                            width = 600,
                                            alt   = "The extended illness death model with constant transition hazards")),
                                        br(),
                                        br(),
                                        plotOutput("stackedPlot2_hr"),
                                        plotOutput("stackedPlot2a_hr", height = "100px"),
                                        plotOutput("stackedPlot3_hr"),
                                        plotOutput("stackedPlot4_hr")))))))




# Define server in order to use input to create output
server <- function(input, output) {
  
  # for easy use a short version
  eidm_aggregated_hazards_short <- function(s                    = 0,
                                            t_max                = input$x_lim,
                                            x_lim                = input$x_lim,
                                            legend_position_eidm,
                                            plot_title_eidm      = "Stacked Probability Plot",
                                            order_eidm,
                                            Lambda_01,
                                            Lambda_02,
                                            Lambda_03,
                                            Lambda_14,
                                            Lambda_15,
                                            area_col_eidm        = c("khaki1",         # inpatient w/o intermediate
                                                                     "indianred1",     # intermediate inpatient
                                                                     "cornflowerblue", # discharge w/o intermediate
                                                                     "darkblue",       # death w/o intermediate
                                                                     "chocolate",      #discharge after intermediate
                                                                     "chocolate4"),    # death after intermediate
                                            legend_labels_eidm   = c("inpatient w/o intermediate",
                                                                     "intermediate",
                                                                     "discharge w/o intermediate",
                                                                     "death w/o intermediate",
                                                                     "discharge after intermediate",
                                                                     "death after intermediate"),
                                            paf_title = "PAF(t)",
                                            am_title = "AM(t)") {
    
    eidm_aggregated_hazards(s                    = s,
                            t_max                = t_max,
                            x_lim                = x_lim,
                            legend_position_eidm = legend_position_eidm,
                            plot_title_eidm      = plot_title_eidm,
                            order_eidm           = order_eidm,
                            Lambda_01            = Lambda_01,
                            Lambda_02            = Lambda_02,
                            Lambda_03            = Lambda_03,
                            Lambda_14            = Lambda_14,
                            Lambda_15            = Lambda_15,
                            area_col_eidm        = area_col_eidm,
                            legend_labels_eidm   = legend_labels_eidm,
                            paf_title            = paf_title,
                            am_title             = am_title)
    
  }
  
  # stacked plot group A and B
  output$stackedPlot2 <- renderPlot({
    
    order_vector <- as.numeric(unlist(strsplit(input$order, ",")))
    
    hui_a <- eidm_aggregated_hazards_short(legend_position_eidm = "bottom",
                                           order_eidm           = order_vector,
                                           Lambda_01            = input$A01_a,
                                           Lambda_02            = input$A02_a,
                                           Lambda_03            = input$A03_a,
                                           Lambda_14            = input$A14_a,
                                           Lambda_15            = input$A15_a)
    
    hui_b <- eidm_aggregated_hazards_short(legend_position_eidm = c(0.8, 0.85),
                                           order_eidm           = order_vector,
                                           Lambda_01            = input$A01_b,
                                           Lambda_02            = input$A02_b,
                                           Lambda_03            = input$A03_b,
                                           Lambda_14            = input$A14_b,
                                           Lambda_15            = input$A15_b)
    
    
    # draw plots without legend:
    grid.arrange(hui_a[[2]] + theme(legend.position = "none"),
                 hui_b[[2]] + theme(legend.position = "none"), nrow = 1)
    
    
  })
  
  # separate legend Plot for EIDM:
  output$stackedPlot2a <- renderPlot({
    
    order_vector <- as.numeric(unlist(strsplit(input$order, ",")))
    
    hui_a <- eidm_aggregated_hazards_short(legend_position_eidm = "top",
                                           order_eidm           = order_vector,
                                           Lambda_01            = input$A01_a,
                                           Lambda_02            = input$A02_a,
                                           Lambda_03            = input$A03_a,
                                           Lambda_14            = input$A14_a,
                                           Lambda_15            = input$A15_a)
    
    
    #get legend from one plot:
    mylegend <- g_legend(hui_a[[2]])
    
    # draw plots with one combined legend:
    grid.arrange(mylegend)
    
    
  })
  
  
  # PAF plot for group A and B
  output$stackedPlot3 <- renderPlot({
    
    order_vector <- as.numeric(unlist(strsplit(input$order, ",")))
    
    
    
    hui_a <- eidm_aggregated_hazards_short(legend_position_eidm = c(0.8, 0.85),
                                           order_eidm           = order_vector,
                                           Lambda_01            = input$A01_a,
                                           Lambda_02            = input$A02_a,
                                           Lambda_03            = input$A03_a,
                                           Lambda_14            = input$A14_a,
                                           Lambda_15            = input$A15_a)
    
    hui_b <- eidm_aggregated_hazards_short(legend_position_eidm = c(0.8, 0.85),
                                           order_eidm           = order_vector,
                                           Lambda_01            = input$A01_b,
                                           Lambda_02            = input$A02_b,
                                           Lambda_03            = input$A03_b,
                                           Lambda_14            = input$A14_b,
                                           Lambda_15            = input$A15_b)
    
    # plot only if input box is TRUE
    if(input$PAF == TRUE){
      grid.arrange(hui_a[[3]],
                   hui_b[[3]],
                   nrow = 1) } 
    
  })
  
  # AM plot for group A and B
  output$stackedPlot4 <- renderPlot({
    
    order_vector <- as.numeric(unlist(strsplit(input$order, ",")))
    
    
    
    hui_a <- eidm_aggregated_hazards_short(legend_position_eidm = c(0.8, 0.85),
                                           order_eidm           = order_vector,
                                           Lambda_01            = input$A01_a,
                                           Lambda_02            = input$A02_a,
                                           Lambda_03            = input$A03_a,
                                           Lambda_14            = input$A14_a,
                                           Lambda_15            = input$A15_a)
    
    hui_b <- eidm_aggregated_hazards_short(legend_position_eidm = c(0.8, 0.85),
                                           order_eidm           = order_vector,
                                           Lambda_01            = input$A01_b,
                                           Lambda_02            = input$A02_b,
                                           Lambda_03            = input$A03_b,
                                           Lambda_14            = input$A14_b,
                                           Lambda_15            = input$A15_b)
    
    # plot only if input box is TRUE
    if(input$AM == TRUE){
      grid.arrange(hui_a[[4]],
                   hui_b[[4]],
                   nrow = 1)}
    
  })
  
  
  #
  # Similar for HR input:
  #
  
  output$stackedPlot2_hr <- renderPlot({
    
    order_vector <- as.numeric(unlist(strsplit(input$order_hr, ",")))
    
    a01_b <- input$A01_c * input$A01_hr
    a02_b <- input$A02_c * input$A02_hr
    a03_b <- input$A03_c * input$A03_hr
    a14_b <- input$A14_c * input$A14_hr
    a15_b <- input$A15_c * input$A15_hr
    
    hui_a <- eidm_aggregated_hazards_short(t_max                = input$x_lim_hr,
                                           x_lim                = input$x_lim_hr,
                                           legend_position_eidm = "bottom",
                                           order_eidm           = order_vector,
                                           Lambda_01            = input$A01_c,
                                           Lambda_02            = input$A02_c,
                                           Lambda_03            = input$A03_c,
                                           Lambda_14            = input$A14_c,
                                           Lambda_15            = input$A15_c)
    
    hui_b <- eidm_aggregated_hazards_short(t_max                = input$x_lim_hr,
                                           x_lim                = input$x_lim_hr,
                                           legend_position_eidm = c(0.8, 0.85),
                                           order_eidm           = order_vector,
                                           Lambda_01            = a01_b,
                                           Lambda_02            = a02_b,
                                           Lambda_03            = a03_b,
                                           Lambda_14            = a14_b,
                                           Lambda_15            = a15_b)
    
    
    # draw plots without legend:
    grid.arrange(hui_a[[2]] + theme(legend.position = "none"),
                 hui_b[[2]] + theme(legend.position = "none"), nrow = 1)
    
    
  })
  
  
  output$stackedPlot2a_hr <- renderPlot({
    
    order_vector <- as.numeric(unlist(strsplit(input$order_hr, ",")))
    
    a01_b <- input$A01_c * input$A01_hr
    a02_b <- input$A02_c * input$A02_hr
    a03_b <- input$A03_c * input$A03_hr
    a14_b <- input$A14_c * input$A14_hr
    a15_b <- input$A15_c * input$A15_hr
    
    
    
    hui_a <- eidm_aggregated_hazards_short(t_max                = input$x_lim_hr,
                                           x_lim                = input$x_lim_hr,
                                           legend_position_eidm = "top",
                                           order_eidm           = order_vector,
                                           Lambda_01            = input$A01_c,
                                           Lambda_02            = input$A02_c,
                                           Lambda_03            = input$A03_c,
                                           Lambda_14            = input$A14_c,
                                           Lambda_15            = input$A15_c)
    
    
    #get legend from one plot:
    mylegend <- g_legend(hui_a[[2]])
    
    # draw plots with one combined legend:
    grid.arrange(mylegend)
    
    
  })
  
  
  
  output$stackedPlot3_hr <- renderPlot({
    
    order_vector <- as.numeric(unlist(strsplit(input$order_hr, ",")))
    
    a01_b <- input$A01_c * input$A01_hr
    a02_b <- input$A02_c * input$A02_hr
    a03_b <- input$A03_c * input$A03_hr
    a14_b <- input$A14_c * input$A14_hr
    a15_b <- input$A15_c * input$A15_hr
    
    
    hui_a <- eidm_aggregated_hazards_short(t_max                = input$x_lim_hr,
                                           x_lim                = input$x_lim_hr,
                                           legend_position_eidm = c(0.8, 0.85),
                                           order_eidm           = order_vector,
                                           Lambda_01            = input$A01_c,
                                           Lambda_02            = input$A02_c,
                                           Lambda_03            = input$A03_c,
                                           Lambda_14            = input$A14_c,
                                           Lambda_15            = input$A15_c)
    
    hui_b <- eidm_aggregated_hazards_short(t_max                = input$x_lim_hr,
                                           x_lim                = input$x_lim_hr,
                                           legend_position_eidm = c(0.8, 0.85),
                                           order_eidm           = order_vector,
                                           Lambda_01            = a01_b,
                                           Lambda_02            = a02_b,
                                           Lambda_03            = a03_b,
                                           Lambda_14            = a14_b,
                                           Lambda_15            = a15_b)
    
    # plot only if input box is TRUE
    if(input$PAF_hr == TRUE){
      grid.arrange(hui_a[[3]],
                   hui_b[[3]],
                   nrow = 1) } 
    
  })
  output$stackedPlot4_hr <- renderPlot({
    
    order_vector <- as.numeric(unlist(strsplit(input$order_hr, ",")))
    
    a01_b <- input$A01_c * input$A01_hr
    a02_b <- input$A02_c * input$A02_hr
    a03_b <- input$A03_c * input$A03_hr
    a14_b <- input$A14_c * input$A14_hr
    a15_b <- input$A15_c * input$A15_hr
    
    
    hui_a <- eidm_aggregated_hazards_short(t_max                = input$x_lim_hr,
                                           x_lim                = input$x_lim_hr,
                                           legend_position_eidm = c(0.8, 0.85),
                                           order_eidm           = order_vector,
                                           Lambda_01            = input$A01_c,
                                           Lambda_02            = input$A02_c,
                                           Lambda_03            = input$A03_c,
                                           Lambda_14            = input$A14_c,
                                           Lambda_15            = input$A15_c)
    
    hui_b <- eidm_aggregated_hazards_short(t_max                = input$x_lim_hr,
                                           x_lim                = input$x_lim_hr,
                                           legend_position_eidm = c(0.8, 0.85),
                                           order_eidm           = order_vector,
                                           Lambda_01            = a01_b,
                                           Lambda_02            = a02_b,
                                           Lambda_03            = a03_b,
                                           Lambda_14            = a14_b,
                                           Lambda_15            = a15_b)
    
    # plot only if input box is TRUE
    if(input$AM_hr == TRUE){
      grid.arrange(hui_a[[4]],
                   hui_b[[4]],
                   nrow = 1)}
    
  })
}

