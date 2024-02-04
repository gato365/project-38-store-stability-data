


library(lubridate)
library(tidyverse)

source("main_functions.R")
excel_df <- readxl::read_xlsx("sample_data.xlsx") %>% 
  rename(Food_Quality = `Food Quality`)


# Get today's date
today <- Sys.Date()
# If today is a Sunday, go back to the previous week
if (wday(today, week_start = 1) == 1) {
  today <- today - 7
}
# Find the most recent Sunday
first_day <- today - wday(today, week_start = 1) 
last_day <- first_day + 6

mod_1_excel_df <- excel_df %>% 
  separate(Goals,paste0('Goal_',seq(1:7)),sep = ';') %>%
  separate(Mood,paste0('Mood_',seq(1:7)),sep = ';') %>% 
  separate(Food_Quality,paste0('FQ_',seq(1:7)),sep = ';') %>% 
  filter(Date>= first_day & Date<=last_day) 

m_df = mod_1_excel_df  %>% 
  select(contains('Mood_'))%>% 
  mutate_if(is.character,str_trim)

g_df = mod_1_excel_df  %>% 
  select(contains('Goal_')) %>% 
  mutate_if(is.character,str_trim)
fq_df = mod_1_excel_df  %>% 
  select(contains('FQ_')) %>% 
  mutate_if(is.character,str_trim)