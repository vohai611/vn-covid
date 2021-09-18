library(shiny)
library(tidyverse)
library(plotly)
library(here)
library(rvest)
library(DT)
library(scales)
library(googlesheets4)
i_am("vn-covid/server.R")
# load vnjson map for echarts
small_vnjson <- read_rds(here('vn-covid/small-vnjson.rds'))
# id to googlesheet
ss <- "1NL6ikAYrvB2law5ZqdF3gTURMfONIgxdDbu88bvr36k"
# Load data ---------------------------------------------------------------------------------------------
#gs4_auth(cache = here("vn-covid/.secret"), email= TRUE)

main_plot <- read_sheet(ss, sheet = "main-plot")
case_in_com <- read_sheet(ss, "case_in_community")

# Load total cases/ death
df <- read_html("https://static.pipezero.com/covid/data.json") |> 
  html_text() |> 
  jsonlite::fromJSON()

df <- df$locations |> 
  as_tibble() |> 
  mutate(name = stringi::stri_trans_general(name, id = "Latin - ASCII"),
         name = if_else(name == "TP. Ho Chi Minh", "Ho Chi Minh", name)) |> 
  select(name, death, cases)

# Load population
population <- read_rds(here('vn-covid/danso.rds'))
population <- population %>% 
  mutate(tinh_thanh = if_else(tinh_thanh == 'TP HCM', 'Ho Chi Minh', tinh_thanh))
# load medical_stats
medical_stat <- read_rds(here("vn-covid/medical-stat.rds"))

# Server ------------------------------------------------------------------------------------------------

shinyServer(function(input, output) {
    # render main plot title
  output$p1_title <- renderUI({
    HTML(paste0(province_selected(), " is now selected <br/>
                Please click on the map to select province"))
  })
    
    # render main plot
  output$p1 <- renderEcharts4r({
    main_plot %>%
      group_by(date) %>%
      e_chart(name, timeline = TRUE) %>%
      e_map_register('vn', small_vnjson) %>%
      e_map(moving_avg, map = 'vn', name = 'Average of 7 days before') %>%
      e_theme("infographic") %>%
      e_tooltip() %>%
      e_visual_map(
        moving_avg,
        scale = function(x)
          x * 200
      ) 
   
    })
    # handle click map, # might use reactiveVal() to get stable result
    province_selected <- reactive({
        if (is.null(input$p1_clicked_data) || is.numeric(input$p1_clicked_data)) ("Ha Noi")
        else (input$p1_clicked_data$name)
        
    })
    
    # render side plot 
    output$p2 <- renderPlotly({
      p2 <- main_plot %>%
        mutate(date = lubridate::date(date)) %>%
        filter(name == province_selected()) %>%
        highlight_key(~date) %>% 
        ggplot(aes(date, value)) +
        geom_smooth(
          se = FALSE,
          method = 'loess',
          color = 'firebrick',
          alpha = .7,
          size = .6
        ) +
        geom_col(aes(text = paste0(date, "\nCases: ", value))) +
        labs(title = "Number of cases by day",
             y = NULL,
             x = " ") +
        theme_light()+
        theme(plot.title.position = "plot")
      
      
      ggplotly(p2,tooltip = "text") %>% 
        highlight(on = "plotly_hover", color = "grey30", off = 'plotly_doubleclick') %>% 
        config(displayModeBar = FALSE)
      
    })
    observeEvent(input$p1_clicked_data, print(input$p1_clicked_data))
    
    output$p3 <- renderPlotly({
        p3 <- case_in_com |>
            ggplot(aes(
                date,
                value,
                color = name,
                group = name,
                text = glue::glue("Cases in {name}: {value} " )
            )) +
            geom_line() +
            scale_color_manual(values = c("#6D4444", "#38639C", "#7D7834FC"))+
            scale_y_continuous(labels = scales::comma)+
            labs(x="", y = '', color = "")+
            theme_light()
        
        ggplotly(p3,tooltip = c('text', 'x')) |> 
            layout(hovermode = 'x') |> 
            config(displayModeBar = FALSE)
        
        
    })
    
    # province covid info
    output$pro_infor_title <- renderUI({
      HTML(paste0('<font size="5"><strong>General covid statistics of ', province_selected()))
    })
    output$pro_infor <- renderPrint({
        df <- df |>
            filter(name == province_selected())
        population <- population |> 
            filter(tinh_thanh == province_selected())
        
        glue::glue("Total cases😖: {comma(df$cases)}
                   Total death💀: {comma(df$death)}
                   Population: {comma(population$dan_so_nguoi)}
                   ")
    })
    
    # render medical statistic DT
    output$dt_title <- renderUI({
      HTML(paste0('<font size ="5"><strong> Medical statistics of ', province_selected(), "</strong>"))
    })
    
    output$medical_stat <- renderDT({
      medical_stat %>% 
        filter(Province == province_selected()) %>% 
        datatable()
      
    })
    
    
})
