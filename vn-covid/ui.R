#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(echarts4r)

# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Application title
    titlePanel("Old Faithful Geyser Data"),
# 
#     # Sidebar with a slider input for number of bins
#     sidebarLayout(position = 'left',
#                   sidebarPanel(echarts4rOutput("p1"), width = 6),
#                   
#                   # Show a plot of the generated distribution
#                   mainPanel(plotOutput("p2")))
    column(width = 6,echarts4rOutput("p1",height = '800%')) ,
    column(width = 6, plotlyOutput("p2"))
))
