
library(shiny)
library(leaflet)
library(tidyverse)

# Define UI for application that draws a histogram
shinyUI(fluidPage(

    sidebarLayout(
                  sidebarPanel(
                      leafletOutput("map",height=300),
                      checkboxGroupInput("models","Models",
                                         choices=list("Rayleigh-Weibull","Weibull M1 & M2","Weibull M1 & M3","WAsP"),
                                         selected=list("Rayleigh-Weibull","Weibull M1 & M2","Weibull M1 & M3","WAsP")),
                      checkboxGroupInput("goodness.method","Goodness of fit",
                                         choices=list("R squared","Cramer-Von Mises","Anderson-Darling",
                                                      "RTAD","RTAD2"),
                                         selected = "Cramer-Von Mises"),
                      checkboxInput("logy","Log Y-Axis",value = F)
                  ),
                  mainPanel(
                      h2("Wind Profile Viewer"),
                      plotOutput("binsHist"),
                      h4("Fitting the wind distribution"),
                      h5("Click any green circle!"),
                      tags$div(id="cite",
                               'Data source ', tags$em('NOAA WMO Resolution 40')
                      ),
                      downloadButton("downloadData", "Download"),
                      helpText(   a("Click here for documentation",
                                    href="http://rpubs.com/bfayolle/WeibullDistAndRealWind")
                      )
                  )

    )
)
)
