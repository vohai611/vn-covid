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
library(plotly)


# Define UI for application that draws a histogram
shinyUI(fluidPage(
    # Application title
    titlePanel("Old Faithful Geyser Data"),
    column(width = 6, echarts4rOutput("p1", height = '800%')) ,
    column(
        width = 6,
        fluidRow(plotlyOutput("p2")),
        fluidRow(verbatimTextOutput('pro_infor'))
    ),
    fluidRow(' Addtional information', tabsetPanel(
        tabPanel(title = 't1'),
        tabPanel(title = 't2', plotlyOutput("p3"))
    ))
))
