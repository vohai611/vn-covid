# Load and cache necessary data

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

# set auth option
options(gargle_oauth_cache = here("vn-covid/.secrets"),
         gargle_oath_email  = "hai835559@gmail.com")
# gs4_auth() 
gs4_auth(email = TRUE)
# Load data ---------------------------------------------------------------------------------------------
# read and cache

data <- read_rds(here("vn-covid/cache_data.rds"))

if (data$date != Sys.Date()) {
  main_plot <- read_sheet(ss, sheet = "main-plot")
  case_in_com <- read_sheet(ss, "case_in_community") %>%
    filter(name %in% c('blockade', 'community')) %>%
    mutate(date = lubridate::date(date))
  
  # Load total cases/ death
  df <-
    read_html("https://static.pipezero.com/covid/data.json") |>
    html_text() |>
    jsonlite::fromJSON()
  
  df <- df$locations |>
    as_tibble() |>
    mutate(
      name = stringi::stri_trans_general(name, id = "Latin - ASCII"),
      name = if_else(name == "TP. Ho Chi Minh", "Ho Chi Minh", name)
    ) |>
    select(name, death, cases)
  
  date <- Sys.Date()
  
  cache_data <- lst(main_plot,case_in_com, df, date)
  write_rds(cache_data, here("vn-covid/cache_data.rds"))
} else {
  main_plot <- data$main_plot
  case_in_com <- data$case_in_com
  df <- data$df
}

# Load population
population <- read_rds(here('vn-covid/danso.rds'))
population <- population %>% 
  mutate(tinh_thanh = if_else(tinh_thanh == 'TP HCM', 'Ho Chi Minh', tinh_thanh))
# load medical_stats
medical_stat <- read_rds(here("vn-covid/medical-stat.rds"))