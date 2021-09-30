## New UI


library(shiny)
library(echarts4r)
library(plotly)
library(DT)

ui <- fluidPage(title = titlePanel("Covid dashboard"),
                br(),
  column(5,
         fluidRow(htmlOutput("p1_title")),
         fluidRow(echarts4rOutput("p1", height = "1200%"))),
  column(7,
         uiOutput("h2"),
         fluidRow(tabsetPanel(
           tabPanel('Cases by day',
                    br(),
                    fluidRow(verbatimTextOutput('pro_infor')),
                    fluidRow(plotlyOutput('p2'))),
           tabPanel(title = "Medical infrastructure summary",
                    br(),
                    DTOutput("medical_stat"))
         )),
         h2("Country data"),
         fluidRow(tabsetPanel(
           tabPanel(title = 'Cases in all country', plotlyOutput("p4")),
           tabPanel(title = 'Cases in/out of blockade', plotlyOutput("p3"))
         ))))
