# should change to shiny dashboard for better UI


library(shiny)
library(echarts4r)
library(plotly)
library(DT)


# Define UI for application that draws a histogram
shinyUI(fluidPage(
    # Application title
    titlePanel("Vietnam Covid Dashboard"),
    column(width = 6,
           fluidRow(htmlOutput("p1_title")),
           fluidRow(echarts4rOutput("p1", height = '1000%'))
           ) ,
    column(
        width = 6,
        htmlOutput("pro_infor_title"),
        tags$br(),
        fluidRow(verbatimTextOutput('pro_infor')),
        fluidRow(plotlyOutput("p2")),
        tags$br(),
        htmlOutput('dt_title'),
        tags$br(),
        tags$br(),
        fluidRow(DTOutput('medical_stat'))
    ),
    fluidRow(' Addtional information', tabsetPanel(
        tabPanel(title = 't1'),
        tabPanel(title = 't2', plotlyOutput("p3"))
    ))
))