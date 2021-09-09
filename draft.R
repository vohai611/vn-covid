library(tidyverse)
library(rvest)
library(httr)
link1 <- "https://vnexpress.net/microservice/sheet/type/covid19_2021_by_map"
link2 <- "https://vnexpress.net/microservice/sheet/type/covid19_2021_by_location"
link3 <- "https://vnexpress.net/microservice/sheet/type/covid19_2021_by_total"
link4 <- "https://vnexpress.net/microservice/sheet/type/covid19_2021_by_day"

link <- tibble(url= c(link1, link2, link3, link4))
get_data <- function(link){
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
  select(name = english, dan_so_nguoi)

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

df_end <- df1 %>% 
  left_join(df2) %>% 
  mutate(case_pop10000 = total / dan_so_nguoi* 10000) %>% 
  mutate(name = case_when(name == "TP HCM" ~ "Ho Chi Minh",
                          str_detect(name, "Ba Ria")~ "Ba Ria - Vung Tau",
                          TRUE ~ name))

library(echarts4r)
library(haitools)

df_end %>% 
  mutate(case_pop10000 = round(case_pop10000,3)) %>% 
  arrange(desc(date)) %>%
  group_by(date) %>%
  e_chart(name, timeline = TRUE) %>%
  e_map_register('vn', small_vnjson) %>%
  e_map(case_pop10000, map = 'vn', name = 'Case percentage', ) %>%
  e_theme("infographic") %>%
  e_tooltip() %>%  
  e_visual_map(case_pop10000,scale = function(x) x *10000)
## should be using moving average

cars |> 
  dplyr::mutate(
    dist = dist / 120
  ) |> 
  e_charts(speed) |> 
  e_scatter(dist, symbol_size = 10) |> 
  e_tooltip(
    formatter = e_tooltip_item_formatter("percent")
  )


df1 %>% 
  mutate(name = case_when(name == "TP HCM" ~ "Ho Chi Minh",
                          str_detect(name, "Ba Ria")~ "Ba Ria - Vung Tau",
                          TRUE ~ name)) %>% 
  group_by(name) %>% 
  mutate(moving_avg = slider::slide_dbl(value ,~ mean(.x),.before =7)) %>% 
  ungroup() %>% 
  write_rds("data/main-plot.rds",compress = "gz")


# Case by map
# Death/ case/ pop... by province
# Description


  


