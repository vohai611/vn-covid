#! /bin/Rscript

library(tidyverse)
library(rvest)
library(httr)
link1 <- "https://vnexpress.net/microservice/sheet/type/covid19_2021_by_map"
link2 <- "https://vnexpress.net/microservice/sheet/type/covid19_2021_by_location"
link3 <- "https://vnexpress.net/microservice/sheet/type/covid19_2021_by_total"
link4 <- "https://vnexpress.net/microservice/sheet/type/covid19_2021_by_day"

link <- tibble(url = c(link1, link2, link3, link4))
get_data <- function(link) {
  GET(link) %>%
    read_html() %>%
    html_text() %>%
    read_csv()
  
}

result <- link %>% 
  mutate(data= map(url, get_data))

df2 <- result[1, ]$data[[1]] %>% 
  janitor::clean_names()

df2 <- df2 %>% 
  select(name = english)

# main-plot (map) data ----------------------------------------------------------------------------------
# load data to google sheet
library(googlesheets4)
gs4_auth(email = 'hai835559@gmail.com')
ss <- "1NL6ikAYrvB2law5ZqdF3gTURMfONIgxdDbu88bvr36k"

df1 <- result[2,]$data[[1]]


df1 <- df1 %>% 
  select(1:63) %>% 
  rename(date =1) %>% 
  filter(!is.na(date)) %>% 
  pivot_longer(-date)  %>% 
  mutate(name = haitools::str_remove_accent(name)) %>% 
  replace_na(list(value =0))

df1 <- df1 %>% 
  mutate(date = paste0(date, "/2021"),
         date = lubridate::dmy(date)) %>% 
  group_by(name) %>% 
  arrange(date) %>% 
  mutate(total = cumsum(value)) %>% 
  ungroup()

df1 %>%  
  mutate(name = case_when(name == "TP HCM" ~ "Ho Chi Minh",
                          str_detect(name, "Ba Ria")~ "Ba Ria - Vung Tau",
                          TRUE ~ name)) %>% 
  group_by(name) %>% 
  slice(1: (n()-1)) %>% 
  mutate(moving_avg = slider::slide_dbl(value ,~ mean(.x),.before =7)) %>% 
  ungroup() %>% 
  write_sheet(ss, sheet = "main-plot")


  

# case-in-community and population data -----------------------------------------------------------------

  
result$data[[4]] |> 
  janitor::clean_names() |> 
  select(date = ngay, outside = cong_dong, blockade, community) |> 
  filter(!is.na(outside)) |>
  replace_na(list(blockade = 1, community = 1)) %>%
  slice_head(n = nrow(.)-1) |> 
  mutate(date = paste0(date,'/2021'),
         date =lubridate::dmy(date)) |> 
  pivot_longer(-date) |> 
  write_sheet(ss, "case_in_community")


result$data[[1]] |> 
  janitor::clean_names() |> 
  select(tinh_thanh) |> 
  mutate(tinh_thanh = haitools::str_remove_accent(tinh_thanh),
         tinh_thanh = if_else(tinh_thanh == "TP. Ho Chi Minh", "Ho Chi Minh", tinh_thanh)) |> 
  write_sheet(ss, "danso")


# read medical statistic --------------------------------------------------------------------------------
# library(tidyverse)
# df_bs <- read_csv("~/Downloads/vn-medical-stat/so_bs.csv",skip = 1) |> 
#   janitor::clean_names()
# 
# df_bv <- read_csv("~/Downloads/vn-medical-stat/so_cs_yte.csv") |> 
#   janitor::clean_names()
# 
# df_gbenh <- read_csv("~/Downloads/vn-medical-stat/so_giuong_benh.csv",skip = 1) |> 
#   janitor::clean_names()
# remove_province <- c(
#   "WHOLE COUNTRY",
#   "Mekong River Delta"  ,
#   "South East",
#   "Red River Delta",
#   "Northern midlands and mountain areas",
#   "Northern Central area and Central coastal area",
#   "Central Highlands",
#   "Ha Tay"
# )
# 
# cleaner <- . %>% 
#   pivot_longer(cols = -cities_provincies) %>% 
#   filter(! cities_provincies %in% remove_province ) %>% 
#   mutate(name = str_remove(name, "x2017_")) %>%  
#   filter( name != 'total')
#   
#   
# 
# df_bv %>% 
#   cleaner() %>% 
#   mutate(category = "# Hospitals") %>% 
#   bind_rows(
#     df_bs %>% 
#       cleaner() %>% 
#       mutate(category = 'medical staff')
#   ) %>% 
#   bind_rows(
#     df_gbenh %>% 
#       cleaner() %>% 
#       mutate(category = "# bed")
#   ) %>%
#   rename(Province = cities_provincies) %>% 
#   mutate(Province = case_when(str_detect(Province,"Ho Chi Minh") ~ "Ho Chi Minh",
#                    Province == "Thua Thien-Hue"~ 'Thua Thien Hue',
#                    TRUE ~ Province),
#          Province  = str_replace_all(Province, "\\s+", " ")) %>% 
#   write_sheet(ss, "medical-stat")
#   













