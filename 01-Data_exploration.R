# load packages
library(tidyverse)
library(readxl)

# load data
sheets <- excel_sheets("Input/8635_opn_2020_covid_all_waves_variable_catalogue.xlsx")

bind_sheets <- function(sheetname) {
read_xlsx("Input/8635_opn_2020_covid_all_waves_variable_catalogue.xlsx",
          sheet = sheetname) %>% 
    mutate(sheet = sheetname)
}

opn_var <- purrr::map_dfr(sheets, ~bind_sheets(.))
opn_var <- opn_var %>% 
  mutate(sheetID = str_extract(sheet, pattern = "^(.=?_)|^(..=?_)") %>% str_remove("_"),
         year = str_extract(sheet, pattern = "202."), # extract year
         month = str_extract(sheet, pattern = "(?<=2020)..")) %>% # extract month
  group_by(month) %>% 
  mutate(rep = as.numeric(as.factor(sheetID))) %>% # create variable for repetition within month
  ungroup() %>% 
  mutate(wave = as.numeric(as.factor(sheetID)))

          

length(sheets)
