#
# This is the user-interface definition of WindProfileViewer. You can
# run the application by clicking 'Run App' above.
#
# 

library(shiny)
library(leaflet)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
    
    tabPanel("Wind Profile Viewer",
             div(class="outer",
                 # 
                 # tags$head(
                 #     # Include our custom CSS
                 #     includeCSS("styles.css"),
                 #     includeScript("gomap.js")
                 # ),
                 
                 # If not using custom CSS, set height of leafletOutput to a number instead of percent
                 leafletOutput("map"),#, width="800", height="600"
                 
                 # Shiny versions prior to 0.11 should use class = "modal" instead.
                 absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                               draggable = TRUE, top = "auto", left = "auto", right = 20, bottom = 0,
                               width = 600, height = "auto",
                               
                               h4("Fitting the wind distribution"),
                               h5("Click any green circle!"),
                               plotOutput("binsHist", height = 250)
                               
                 )
                 
             ),
             # 
             tags$div(id="cite",
                      'Data source ', tags$em('NOAA WMO Resolution 40')
             ),
             downloadButton("downloadData", "Download"),
             helpText(   a("Click here for documentation",
                           href="http://rpubs.com/bfayolle/WeibullDistAndRealWind")
             )
             
    )
))
