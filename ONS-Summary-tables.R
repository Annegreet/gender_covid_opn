library(readxl)
library(httr)
library(xlsx)
library(tidyverse)
library(tidyxl)
library(unpivotr)
library(janitor)

# df with urls to the ONS Opinions and lifestyle public data 
urls <- read_xlsx("URL_list_opn.xlsx")
urls <- urls %>% 
  mutate(Date = as.character(Date)) # convert date type to character

# create lookup table of table headers from the content sheets, to see what data is available
get_tables_opn <- function(url, date) {
  GET(url, write_disk(tf <- tempfile(fileext = ".xlsx")))
  tbl <- read_excel(tf, sheet = "Contents") %>% # extract content page 
    select(tables = 3) %>% # table column with table titles
    filter(str_detect(tables, pattern = "Table") == TRUE) %>% # filter for table title only
    na.omit() 
}

tbls <- map2(urls$url, urls$Date, ~get_tables_opn(.x, .y) %>%
               mutate(date = .y,
                      sheet = str_split_fixed(tables, pattern = ":", 2)[,1] %>% 
                        str_remove(., "[:punct:]"))) %>% # sheet name containing the table
   bind_rows()

# function to extract data from the excel sheet (uncleaned)
get_data_opn <- function(date, sheet) {
  GET(urls$url[urls$Date == date], write_disk(tf <- tempfile(fileext = ".xlsx")))
  tbl <- read_excel(tf, sheet =  sheet, col_names =  FALSE, skip = 3)
}

# functions to extract data from excel and get in to tidy format
get_cells_opn <- function(date, sheet) {
  GET(urls$url[urls$Date == date], write_disk(tf <- tempfile(fileext = ".xlsx")))
  tbl <- xlsx_cells(tf, sheets =  sheet) #  imports data from spreadsheets without coercing it into a rectangle
}

get_format_opn <- function(date, sheet) {
  GET(urls$url[urls$Date == date], write_disk(tf <- tempfile(fileext = ".xlsx")))
  tbl <- xlsx_formats(tf) # gets formatting of excel
}

clean_opn <- function(date, sheet, nrow = 3) {
cells <- get_cells_opn(date, sheet)
formats <- get_format_opn(date, sheet)

bold <- formats$local$font$bold

tidied <-  cells %>%
  filter(!is_blank) %>% 
  filter(!row <= nrow) %>% 
  behead("up-left", "Category") %>% # Sex, age, location etc.
  behead("up", "Confidence") %>% # mean, upper confidence, lower confidence, sample size
  behead_if(bold[local_format_id],
            direction = "left-up",
            name = "Question") %>% 
  behead("left", "Option") %>% 
  filter(!Option %in% c("Weighted count", "Sample size")) %>% 
  select(Question, Option, Category, Confidence, percentage = numeric) 
}

# get date and sheet name of tables of interest
wrk_hom_sheets <- tbls %>% 
  filter(str_detect(tables, "Working at home")) %>% 
  mutate(date = as.character(date))

wrk_hom <- map2(wrk_hom_sheets$date, wrk_hom_sheets$sheet,
          ~clean_opn(date = .x, sheet = .y) %>% 
           mutate(date = .x)) %>% # add date
  # rearrange list to merge by date and group by question
  bind_rows() %>% 
  mutate(Question = ifelse(str_detect(Question, pattern = "why have you worked from home"),
                           "In the past seven days, why have you worked from home?",
                           ifelse(str_detect(Question, pattern = "have you worked from home"),
                                  "In the past seven days, have you worked from home because of the Coronavirus pandemic?", 
                                  "Among those who said they were working:"))) %>% 
  mutate(Category = str_replace(Category, "Male", "Men"),
         Category = str_replace(Category, "Female", "Women"),
         Category = str_replace(Category, "Working population1", "Working population"),
         Category = str_replace(Category, "16 to 69", "16-69"),
         Category = str_replace(Category, "16 to 29", "16-29"),
         Category = str_replace(Category, "30 to 49", "30-49"),
         Category = str_replace(Category, "50 to 69", "50-69")) %>% 
  mutate(Characteristic = ifelse(str_detect(Category, pattern = "Working population|All persons total"),
                                 "Total",
                                 ifelse(str_detect(Category, "Men|Women"), "Sex",
                                        ifelse(str_detect(Category, "[:digit:][:digit:]"), "Age", Category)))) %>% 
  group_split(Question)

saveRDS(wrk_hom, "Gender-covid-2/Home-working.rds")

care_sheets <- tbls %>% 
  filter(str_detect(tables, "caring")) %>% 
  mutate(date = as.character(date))

care <- map2(care_sheets$date[-6], care_sheets$sheet[-6],
                ~clean_opn(date = .x, sheet = .y) %>% 
                  mutate(date = .x))  %>%  # add date
  # rearrange list to merge by date and group by question
  bind_rows()  %>% 
  filter(Question == "In the past seven days, how have your caring responsibilities been affected? 1")

care$Option <- 
  str_remove(care$Option, "\\([:digit:][:digit:]\\)|\\([:digit:]\\)") %>% # remove numbers
  str_remove("\\((.*?)\\)") %>% # remove anything between brackets
  str_trim()
                            
saveRDS(care, "Gender-covid-2/Caring.rds") # limited data available, only of the total population

finance_sheets <- tbls %>% 
  filter(str_detect(tables, "finance")) %>% 
  mutate(date = as.character(date))

finance <- map2(finance_sheets$date, finance_sheets$sheet,
             ~clean_opn(date = .x, sheet = .y) %>% 
               mutate(date = .x))  %>%  # add date
  # rearrange list to merge by date and group by question
  bind_rows()  %>% 
  select(date,  Question = Category, Category = Question, Option, Confidence,
         percentage) %>% 
  group_split(Question)

saveRDS(finance, "Gender-covid-2/Finance.rds")

wellbeing_sheets <- tbls %>% 
  filter(str_detect(tables, "Impact on well-being|Impacts on well-being")) %>% 
  mutate(date = as.character(date))

wellbeing <- map2(wellbeing_sheets$date, wellbeing_sheets$sheet,
                ~clean_opn(date = .x, sheet = .y) %>% 
                  mutate(date = .x)) %>%  # add date
  bind_rows() %>% 
  filter(!Option == "Weighted Count" & !Option == "Sample Size") 

# Cleaning up strings
wellbeing$Option <- str_remove(wellbeing$Option, 
                               pattern = "\\([:digit:]\\)[:space:]|\\([:digit:][:digit:]\\)[:space:]") %>% 
  str_trim()
wellbeing$Category <- str_replace(wellbeing$Category, pattern = "Key worker2|Key worker1",
                                   replacement = "Key worker")
wellbeing$Question <- str_remove(wellbeing$Question, pattern = "[:space:][:digit:]|[:digit:]")

# Add characteristic
wellbeing <- wellbeing %>% mutate(Characteristic = ifelse(str_detect(Category, pattern = "All person total"),
                               "Total", 
                               ifelse(str_detect(Category, pattern = "Key worker"), "Key worker",
                                      ifelse(str_detect(Category, "Men|Women"), "Sex",
                                             ifelse(str_detect(Category, "[:digit:]"), "Age", Category)))))

saveRDS(wellbeing, "Gender-covid-2/Wellbeing.rds")

homeschool_sheets <- tbls %>% 
  filter(str_detect(tables, "school") & str_detect(tables, "home")) %>% 
  mutate(date = as.character(date))

homeschool <- map2(homeschool_sheets$date, homeschool_sheets$sheet,
       ~clean_opn(date = .x, sheet = .y) %>% 
         mutate(date = .x))  %>%  # add date
  bind_rows() %>% 
  filter(!Option == "Weighted Count" & !Option == "Sample Size") %>% 
  group_split(Question)

saveRDS(homeschool, "Gender-covid-2/Homeschool.rds")

contact_work_sheets <- tbls %>% 
  filter(str_detect(tables, "contact|Contact") & str_detect(tables, "work")) %>% 
  mutate(date = as.character(date))

contact_work <- map2(contact_work_sheets$date, contact_work_sheets$sheet,
                   ~clean_opn(date = .x, sheet = .y) %>% 
                     mutate(date = .x))  %>%  # add date
  bind_rows() %>% 
  filter(!Option == "Weighted Count" & !Option == "Sample Size") %>% 
  mutate(Characteristic = ifelse(str_detect(Category, pattern = "All persons total"),
                                 "Total",
                                 ifelse(str_detect(Category, "Men|Women"), "Sex", Category))) 

contact_work$Question <- str_replace(contact_work$Question, 
"In the past seven days, have you done any paid work requiring direct physical contact with other people?",
"Do you do paid work requiring direct physical contact with others?")
contact_work$Question <- 
  str_replace(contact_work$Question, "In the past seven days, how often have you used PPE while at work?",
              "How often do you use personal protective equipment at work?")
contact_work$Question <- str_replace(contact_work$Question, "In the past seven days, how often have you stayed at least two metres away from other people while at work?",
              "How often have you maintainted at least 2 meters distance from others while at work?")
  
saveRDS(contact_work, "Gender-covid-2/ContactWork.rds")
