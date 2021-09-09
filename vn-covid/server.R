#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(plotly)
main_plot <- read_rds("../data/main-plot.rds")

shinyServer(function(input, output) {
    output$p1 <- renderEcharts4r({
        main_plot %>% 
        group_by(date) %>%
            e_chart(name, timeline = TRUE) %>%
            e_map_register('vn', small_vnjson) %>%
            e_map(moving_avg, map = 'vn', name = 'Average of 7 days before', ) %>%
            e_theme("infographic") %>%
            e_tooltip() %>% 
            e_visual_map(moving_avg, scale = function(x) x* 200)
   
    })
    # handle click map
    province_selected <- reactive({
        if (is.null(input$p1_clicked_data$name)) ("Ha Noi")
        else (input$p1_clicked_data$name)
        
    })
    # render side plot 
    output$p2 <- renderPlotly({
        
        p2 <- main_plot %>%
            filter(name == province_selected())  %>%
            ggplot(aes(date, value)) +
            geom_line() +
            geom_smooth(method = "loess",
                        se = FALSE) +
            labs(title = paste0("Number of case by day in ", province_selected()),
                 y = "Cases",
                 x = " ") +
            theme_light()
       ggplotly(p2) 
        
    })
    
})
