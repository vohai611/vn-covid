library(shiny)
library(tidyverse)
library(plotly)
library(here)
library(rvest)
library(DT)
library(scales)
library(googlesheets4)
i_am("vn-covid/server.R")
# # load vnjson map for echarts
# small_vnjson <- read_rds(here('vn-covid/small-vnjson.rds'))
# # id to googlesheet
# ss <- "1NL6ikAYrvB2law5ZqdF3gTURMfONIgxdDbu88bvr36k"
# # Load data ---------------------------------------------------------------------------------------------
# #gs4_auth(cache = here("vn-covid/.secret"), email= TRUE)
# 
# main_plot <- read_sheet(ss, sheet = "main-plot")
# case_in_com <- read_sheet(ss, "case_in_community") %>% 
#   filter(name %in% c('blockade', 'community')) %>% 
#   mutate(date = lubridate::date(date))
# 
# # Load total cases/ death
# df <- read_html("https://static.pipezero.com/covid/data.json") |> 
#   html_text() |> 
#   jsonlite::fromJSON()
# 
# df <- df$locations |> 
#   as_tibble() |> 
#   mutate(name = stringi::stri_trans_general(name, id = "Latin - ASCII"),
#          name = if_else(name == "TP. Ho Chi Minh", "Ho Chi Minh", name)) |> 
#   select(name, death, cases)
# 
# # Load population
# population <- read_rds(here('vn-covid/danso.rds'))
# population <- population %>% 
#   mutate(tinh_thanh = if_else(tinh_thanh == 'TP HCM', 'Ho Chi Minh', tinh_thanh))
# # load medical_stats
# medical_stat <- read_rds(here("vn-covid/medical-stat.rds"))
source(here("vn-covid/load-and-cache.R"))
# Server ------------------------------------------------------------------------------------------------

server <- function(input, output, session) {
    # render main plot title ----
  output$p1_title <- renderUI({
    HTML(paste0("<h1 >", province_selected(), " is now selected <br/></h1>
                Please click on the map to select province"))
  })
    
    # render main plot ----
  output$p1 <- renderEcharts4r({
    main_plot %>%
      group_by(date) %>%
      e_chart(name, timeline = TRUE) %>%
      e_map_register('vn', small_vnjson) %>%
      e_map(moving_avg, map = 'vn', name = 'Average cases of 7 days before') %>%
      e_tooltip() %>%
      e_timeline_opts(
        playInterval = 150,
        currentIndex = 150, 
        symbol = "pin",
        top = 5,
        right = 50,
        left = 200,
      ) %>% 
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
    # render h2 title
    output$h2 <- renderUI(HTML(paste0("<h2>", province_selected(), " data</h2>")))
    
    # province covid info -----
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
    
    # render side plot ----
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
    
    # render country aggregate data
    output$p4 <- renderPlotly({
    
        p4 <- main_plot %>% 
        group_by(date) %>% 
        summarise(total = sum(value)) %>% 
          ggplot(aes(date, total))+
          geom_line()+
          geom_point()+
          theme_light()+
          labs(x =NULL, y = "Total cases by day")
        ggplotly(p4) %>% 
          config(displayModeBar =FALSE)
        
    })
    
    # render plot 'case in/out blockade' ----
    output$p3 <- renderPlotly({
        p3 <- case_in_com |>
            ggplot(aes(
                date,
                value,
                fill = name,
                group = name,
                text = glue::glue("Cases in {name}: {value} " )
            )) +
            geom_area(alpha = .8) +
            scale_fill_manual(values = c("#CDAA7D", "#8B2323"))+
            scale_y_continuous(labels = scales::comma)+
            labs(x="", y = '', fill = "")+
            theme_light()
        
        ggplotly(p3,tooltip = c('text', 'x')) |> 
            layout(hovermode = 'x') |> 
            config(displayModeBar = FALSE)
        
        
    })
    
    # render medical statistic DT ----
    output$dt_title <- renderUI({
      HTML(paste0('<font size ="5"><strong> Medical statistics of ', province_selected(), "</strong>"))
    })
    
    output$medical_stat <- renderDT({
      medical_stat %>% 
        filter(Province == province_selected()) %>% 
        select(Category = category,`Medical unit` = name, Quantity = value) %>% 
        datatable(rownames = FALSE)
      
    })
    
    
}