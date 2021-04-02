# load packages
library(tidyverse)
library(readxl)
library(xlsx)

# prepare data
sheets <- excel_sheets("Input/8635_opn_2020_covid_all_waves_variable_catalogue.xlsx")

bind_sheets <- function(sheetname) {
read_xlsx("Input/8635_opn_2020_covid_all_waves_variable_catalogue.xlsx",
          sheet = sheetname) %>% 
    rename(var.name = 1, var.label = 2) %>% 
    mutate(sheet = sheetname)
}

opn_var_raw <- purrr::map_dfr(sheets, ~bind_sheets(.))

# create wave, month and year variable
opn_var <- opn_var_raw %>% 
  mutate(sheetID = str_extract(sheet, pattern = "^(.=?_)|^(..=?_)") %>% str_remove("_"),
         year = str_extract(sheet, pattern = "202."), # extract year
         month = str_extract(sheet, pattern = "(?<=2020)..")) %>% # extract month
  group_by(month) %>% 
  mutate(rep = as.numeric(as.factor(sheetID))) %>% # create variable for repetition of survey within month
  ungroup() %>% 
  mutate(wave = as.numeric(as.factor(sheetID))) # numerica ID for wave

# subset derived variables
opn_dv <- opn_var %>% 
  filter(str_detect(var.label, "DV"))  
opn_var <- opn_var %>%
  filter(!str_detect(var.label, "DV"))

# unique_q <- opn_var %>% 
#   mutate(id = 1:nrow(.)) %>% 
#   spread(wave, var.name) %>% 
#   select(var.label, 8:ncol(.)) %>% 
#   group_by(var.label) %>% 
#   summarise_all(~first(na.omit(.)))
# 
# names(unique_q) <- c("Question", sheets)
# 
# unique_q_col <- unique_q %>% 
#   unite("Var.name", A_OPN202003:AM_202012, na.rm = TRUE, sep =", ")
#write.xlsx(unique_q_col, "Unique_questions.xlsx")


# Selecting variable
# possible response variables
covidvar <- 
  ## COVID related  
  # work
  c("COV_8", #	Coronavirus (COVID-19) will cause problems for my work
    "COV_34", #	In the past seven days, have you worked from home because of the Coronavirus (COVID-19) outbreak?
  # finance
  "COV_42",	# In the past seven days, have your household finances improved, remained the same or worsened because of the Coronavirus (COVID-19) outbreak?
  "COV_7", #	Coronavirus (COVID-19) will cause problems for my household finances  
      )

# survey characteristics
surveychar <- 
  c("Month", "Year",
    "WkDayInd","WkDyMkr") # Interview day, Whether survey completed on weekday or weekend

# characteristics of respondent
responchar <- c(# Respondent
                "RAge", #Respondent's age
                "RSex", #Respondent's sex
                "Defacto", #Defacto marital status (9 levels)
                "Defact1", #Defacto marital status (7 levels)
                "Disability", #Disability status
                "CL_EthnGrp2", #Ethnicity grouped (2 categories) (white/other)
                "CL_EthnGrp5", #Ethnicity grouped (5 categories)
                "NatID1","NatID2","NatID3","NatID4","NatID5","NatID6", #National identity
                "CitCheck_b_1","CitCheck_b1", #Which of the following passports are you entitled to?
                
                #Education
                "HighEd4", #Highest education level (4 groupings)
                "HighEd1", #Highest qualification level
                
                # Parent
                "ParTod", #Is a parent of child aged 0-4
                "Parent", #Parent
                "COV_Change_Family", #Starting a family or expanding my family
                
                # Marital status
                "RespMar", #Legal marital status OPN person
                "LivWth12", #Whether living as a couple with another household member
                
                # Income
                "TelBand", #We put answers into income bands. Which band represents your total      personal
                
                "CL_Country", #Country
                "GORA", #Government Office Region
                "sector_numeric") #Postcode sector (anonymised) - numeric
              
workvar <- c(# Employement
             "InEcAc",  #Employment status
             "EverWk",  #Have you ever had paid work, apart from casual or holiday work (or the job you are waiting to begin)?
             "FtPtWk",  #And is that job full-time or part-time?
             "CasWrk",  #Did you do any casual work for payment, even for an hour, in the week Monday ^Ref
             "Stat",  #In your main job, are you an employee or self-employed?
             "OccD", #What do you mainly do in your job?
             "OccT", #What is your main job title? [LookUp]
             "Sectro03", #Which of the following best describes the firm or organisation?
             
             # Key worker 
             "COV_KeyWrk",  #Due to the coronavirus (COVID-19) outbreak, have you been given ?key worker? status?
             "COV_D9","COV_E9",  #Due to the Coronavirus (COVID-19) outbreak, have you been given ?key worker? status?
             "COV_KeyWrk",  #Due to the Coronavirus (COVID-19) outbreak, have you been given ?key worker? status?

             # Work from home
             "COV_WrkReaB_govadvice","COV_WrkReaC_govadvice","COV_WrkRea_govadvice",  #I am following government advice to work from home
             "COV_WrkReaB_normhome","COV_WrkReaC_normhome", "COV_WrkRea_normhome",  #I normally work from home some or all of the time
             "COV_WrkReaB_prefwrkhom", "COV_WrkReaC_prefwrkhom","COV_WrkRea_prefwrkhom",  #I prefer to work from home
             "COV_WrkPlan","COV_WhyWrk01","COV_WhyWrkB01",  #In the next seven days, why do you intend to work from home?
             "COV_WrkHom", "COV_34","COV_B69","COV_C83","COV_D77","COV_E77","COV_WrkHom","COV_WrkHom",  #In the past seven days, have you worked from home because of the coronavirus (COVID-19) pandemic?
             "COV_WrkReaB01","COV_WrkReaC01","COV_WrkRea01",  #In the past seven days, why have you worked from home?
             "COV_WrkReaB_emplwrkhom","COV_WrkReaC_emplwrkhom", "COV_WrkRea_emplwrkhom",  #My employer asked me to work from home
             "COV_WrkReaB_wrkclose","COV_WrkReaC_wrkclose", "COV_WrkRea_wrkclose",  #My workplace is closed
             "COV_WhyWrkSp", #Please specify the reasons you intend to work from home.
             "COV_WrkReaSp", #Please specify the reasons you have worked from home in the past seven days.
             
             # COVID safety at work
             "COV_SocDis",  #In the past seven days, how often have you stayed at least two metres away from other people while a work?
             "COV_WrkCon",  #In the past seven days, have you done any paid work requiring direct physical contact with othe people?
             "COV_WrkPhyCon","COV_D54","COV_E54","COV_WrkCon",  #Over the last 24 hours, how many people at work have you had direct physical contact with?
             "COV_WrkClProx",  #Over the last 24 hours, how many people have you been in close proximity with at work?
             "COV_WrkPPE",  #Personal protective equipment, or PPE, may include gloves, face masks or face shields. In the pas seven days, how often have you used PPE while at work?
             "COV_E13aMSp", "COV_HealSafSp",  #Please specify the other concerns you have about your health and safety at work due to coronaviru (COVID-19)
             "COV_D13MSp","COV_E13MSp","COV_WrkSp",  #Please specify the other ways in which Coronavirus (COVID-19) has affected your work
             "COV_SkillSp",  #Please specify the other ways your work has changed since the coronavirus (COVID-19) outbreak.
             "COV_E13aM01", "COV_Healsaf01", "COV_HealSafA01", #What concerns do you have about your health and safety at work due to coronavirus (COVID-19)?
             
             # other
             "COV_WkSitJan1",  #During the month of January, which of the following situations do you expect to apply to your wok?
             "COV_WkSitOct1",  #During the month of October, which of the following situations do you expect to apply to your wok?
             "COV_WkSitOctNov1",  #During the months of October and November, which of the following situations do you expect t apply to your work?
             "COV_WkSitDecSp",  #Please specify the other situations that you expect to apply to your work in December.
             "COV_WkSitOctNovSp",  #Please specify the other situations that you expect to apply to your work in October an November.
             "COV_WkSitOctSp",  #Please specify the other situations that you expect to apply to your work in October.
             "COV_WrkC01","COV_C12M01","COV_D13M01", "COV_E13M01","COV_Wrk01","COV_WrkA01","COV_WrkB01", #In the past seven days, how has your work been affected?
             "COV_Skill1",  #In which, if any, of the following ways has your work changed since the coronavirus (COVID-19 pandemic?
             "COV_WkSitInfo1",  #Is this expectation based on information from your employer or from other sources?
             "Cov_RetWk", #Thinking of the main job you were doing before lockdown, how likely or unlikely is it that you wil return to that job?
             "COV_Skill1") #Which, if any, of the following ways has your work changed since the coronavirus outbreak?


#"COV_32bM1", "COV_B84M01", "COV_32aM1","COV_B83M01", #Was this travel within the UK for personal or work purposes?
   