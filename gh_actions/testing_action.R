

library(tidyverse)
library(googledrive)
library(googlesheets4)

SHEET_NAME = "https://docs.google.com/spreadsheets/d/1yDE1m9J04ljDzzvKLqZIxL7xwMoIB1W3j5kpeOjUvoo/edit?gid=0#gid=0"

x = read_sheet(SHEET_NAME)
x %>% write.csv(SHEETNAME)