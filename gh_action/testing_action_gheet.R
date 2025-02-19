
sample_key = googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1ThSoogME7LvBl5U5FSi7sm601WvlV2fKrBsi43g7kvk/")

sample_key_clean = 
  sample_key %>% 
  dplyr::select(sample_label, region, site)
