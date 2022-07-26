library(tidyverse)
library(plotly)
library(highcharter)
library(lubridate)
library(xts)
library(DT)
library(anomalize)
library(tibbletime)
library(shiny)
library(bs4Dash)
library(shinycssloaders)
library(waiter)

dashboardPage(
 # preloader = list(html = tagList(spin_1(), "Loading ..."), color = "#18191A"),
  fullscreen = TRUE,
  dashboardHeader(title = dashboardBrand(
    title = "Bid Procurement",
    color = "danger",
    image = "logo.jpg"
  ),
  titleWidth = 500), # end of header
  dashboardSidebar(
    skin = "light",
    status = "danger",
    sidebarUserPanel(
      name = "Analysis",
      image = "data_analysis.jpeg"
    ),
    sidebarMenu(
      menuItem("Data Analysis", tabName = "da", icon = icon("chart-line"))),
    sidebarUserPanel(
      name = "Modelling",
      image = "anomaly_detection.png"
    ),
    sidebarMenu(id = "sidebar",
                menuItem("Anomaly Detection", tabName = "ad", icon = icon("database")))
  ), # end of Sidebar
  dashboardBody(
    tabItems(
      tabItem("da",
              fluidPage(
                fluidRow(
                  valueBoxOutput('min_bp', width = 4),
                  valueBoxOutput("max_bp", width = 4),
                  valueBoxOutput("mean_bp", width = 4)
                ),
                fluidRow(
                  column(5,
                         box(title = "Distribution of Bid Price", 
                             solidHeader = TRUE, width = 12,status = "gray-dark",
                             maximizable = TRUE, plotlyOutput("fig1", height = 300))),
                  column(7,
                         box(title = "Most Frequent Bids", solidHeader = TRUE, width = 12,status = "gray-dark",
                             collapsed = FALSE,
                             maximizable = TRUE, DTOutput("table1")))),
                fluidRow(
                  box(title = "Average Bid Price Overtime", width = 12, status = "gray-dark",
                      solidHeader = TRUE, maximizable = TRUE,
                      highchartOutput("fig2"))),
                fluidRow(
                  tabBox(width = 12, solidHeader = TRUE, maximizable = TRUE,
                         status = "white",
                         tabPanel("Top and Bottom Bid Price For Bid Indicators",
                                  uiOutput("bid_indicator_select"),
                                  fluidRow(column(6, 
                                                    box(highchartOutput("fig_top5_bp"), title = "Top 5",
                                                        collapsible = TRUE, status = "gray-dark",  
                                                        collapsed = TRUE, solidHeader = TRUE, width = 12)),
                                           column(6, 
                                                    box(highchartOutput("fig_bottom5_bp"), title = "Bottom 5" , 
                                                        collapsible = TRUE, status = "gray-dark",  
                                                        collapsed = TRUE, solidHeader = TRUE, width = 12)))),
                         tabPanel("Bid Data Table", icon = icon("table"),
                                  br(),
                                  DTOutput("table2")))
                ),
                fluidRow(
                  box(title = "Contact Name Average Bid Price Overtime", width = 12, status = "gray-dark",
                      solidHeader = TRUE, maximizable = TRUE,
                      uiOutput("cn_select"),
                      highchartOutput("fig3")))
              ) # end of fluid page 
              ) # end of data analysis tab
    ) # end of tab items
  ) # end of body
) # end of page