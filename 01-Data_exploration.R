###############################################################################
####            Opinions and lifestyle survey                             #####
####                  Data exploration                                    #####    
###############################################################################


# 1) Set-up ----
# Required packages
if(!require(tidyverse)) install.packages("tidyverse")
if(!require(readxl)) install.packages("readxl")
if(!require(magrittr)) install.packages("magrittr")
if(!require(ggthemes)) install.packages("ggthemes")

# load packages
library(tidyverse) # Data wrangling
library(readxl) # read in xlsx files
library(magrittr) # set colnames function
library(ggthemes)

# Look-up table for variable names and questions
# names of excel sheets
sheets <- excel_sheets("Input/8635_opn_2020_covid_all_waves_variable_catalogue.xlsx")

# load and bind excel sheets to 1 df 
bind_sheets <- function(sheetname) {
read_xlsx("Input/8635_opn_2020_covid_all_waves_variable_catalogue.xlsx",
          sheet = sheetname) %>% 
    rename(var.name = 1, var.label = 2) %>% # give the same name 
    mutate(sheet = sheetname) # add sheet name column
}

opn_lookup <- purrr::map_dfr(sheets, ~bind_sheets(.))

# Look-up table for variable levels
# NB excel file only has variable levels of the last waves, first waves in pdf
sheetslev <- excel_sheets("Input/8635_waves_l_am_variable_values.xlsx")

# load and bind excel sheets to 1 df 
bind_sheets_lev <- function(sheetname) {
  read_xlsx("Input/8635_waves_l_am_variable_values.xlsx",
            sheet = sheetname) %>% 
    select(position = 1, var.name = 2, var.name2 = 3, val.label = 4, value = 5) %>% # give the same name 
    mutate(sheet = sheetname) # add sheet name column
}
lev_lookup <- map_dfr(sheetslev, ~bind_sheets_lev(.))

lev_lookup <- lev_lookup %>% 
  select(position, var.name, val.label, value) %>% 
  fill(var.name) %>% 
  distinct()

# 2) Selecting variables -----
# Survey characteristics
surveychar <- 
  c("Month", "Year")

# General COVID situation
covidgen <- c(
  "COV_LocLD", #Do you live in an area that is currently under local lockdown measures?"
  "COV_WrkReaB_LocalLD", #I live in a local lockdown area and have been advised to work from home"
  "COV_WrkReaC_LockDown") #I live in a lockdown area and have been advised to work from home"

# Characteristics of respondent
# these variables should be in all waves
responchar <- c(# Respondent
                "RAge", #Respondent's age
                "RSex", #Respondent's sex
                "Disability", #Disability status
                "CL_EthnGrp5", #Ethnicity grouped (5 categories)
                
                # Education
                "HighEd4", #Highest education level (4 groupings)

                # Parent
                "ParTod", #Is a parent of child aged 0-4
                "Parent", #Parent

                # Income
                "TelBand", #We put answers into income bands. Which band represents your total personal
                
                # Place
                "CL_Country", #Country
                "GORA") #Government Office Region

# Characteristics of the household
housevar <- c("DVHSize", #Household size", 
              "HHTypA", #Household type"
              "NumAdult", #Number of adults in household aged 16 and over"
              "N1to4", #Number of children in household aged 0-4"
              "N5to10", #Number of children in household aged 5-10"
              "N11to15", #Number of children in household aged 11-15"
              "NumChild", #Number of children in household aged under 16"
              "NumDepCh") #Number of dependent children in household"

# questions related to work              
workvar <- c(# Employement
             "InEcAc",  #Employment status
             
             "CasWrk", # Whether did any casual work for payment, even for an hour, in the reference week
             "FtPtWk",  #And is that job full-time or part-time?
             "Stat",  #In your main job, are you an employee or self-employed?
             "OccT", #What is your main job title? [LookUp]
             "Sectro03", #Which of the following best describes the firm or organisation?
             
             # Key worker 
             "COV_KeyWrk","COV_D9","COV_E9",  #Due to the Coronavirus (COVID-19) outbreak, have you been given ?key worker? status?
             
             # Work from home
             "COV_WrkReaB_govadvice","COV_WrkReaC_govadvice","COV_WrkRea_govadvice",  #I am following government advice to work from home
             "COV_WrkReaB_normhome","COV_WrkReaC_normhome", "COV_WrkRea_normhome",  #I normally work from home some or all of the time
             "COV_WrkHom","COV_34","COV_B69","COV_C83","COV_D77","COV_E77", #In the past seven days, have you worked from home because of the coronavirus (COVID-19) pandemic?
             "COV_WrkReaB01","COV_WrkReaC01","COV_WrkRea01",  #In the past seven days, why have you worked from home?
             "COV_WrkReaB_emplwrkhom","COV_WrkReaC_emplwrkhom", "COV_WrkRea_emplwrkhom",  #My employer asked me to work from home
             "COV_WrkReaB_wrkclose","COV_WrkReaC_wrkclose", "COV_WrkRea_wrkclose",  #My workplace is closed
             "COV_WrkReaSp", #Please specify the reasons you have worked from home in the past seven days.
             "COV_WrkReaB_govadvice","COV_WrkReaC_govadvice", "COV_WrkRea_govadvice", # I am following government advice to work from home
             "COV_WrkReaB_nochildcare", "COV_WrkReaC_nochildcare", "COV_WrkRea_nochildcare", #I don't have childcare available
             "COV_WrkReaB_normhome", "COV_WrkReaC_normhome", "COV_WrkRea_normhome",  # I normally work from home some or all of the time

             # COVID safety at work
             "COV_SocDis",  #In the past seven days, how often have you stayed at least two meters away from other people while a work?
             "COV_WrkCon",  #In the past seven days, have you done any paid work requiring direct physical contact with other people?
             "COV_WrkPhyCon","COV_D54","COV_E54","COV_WrkCon",  #Over the last 24 hours, how many people at work have you had direct physical contact with?
             "COV_WrkClProx",  #Over the last 24 hours, how many people have you been in close proximity with at work?
             "COV_WrkPPE",  #Personal protective equipment, or PPE, may include gloves, face masks or face shields. In the pas seven days, how often have you used PPE while at work?
             "COV_E13aMSp", "COV_HealSafSp",  #Please specify the other concerns you have about your health and safety at work due to coronaviru (COVID-19)
             "COV_E13aM01", "COV_Healsaf01", "COV_HealSafA01", #What concerns do you have about your health and safety at work due to coronavirus (COVID-19)?
             # filtered
             "COV_D13MSp","COV_E13MSp","COV_WrkSp",  #Please specify the other ways in which Coronavirus (COVID-19) has affected your work
             "COV_SkillSp",  #Please specify the other ways your work has changed since the coronavirus (COVID-19) outbreak.
            
             # How work has changed
             "COV_WrkC01","COV_C12M01","COV_D13M01", "COV_E13M01","COV_Wrk01","COV_WrkA01","COV_WrkB01", #In the past seven days, how has your work been affected?
             "COV_Skill1",  #In which, if any, of the following ways has your work changed since the coronavirus (COVID-19 pandemic?
             "COV_WkSitInfo1",  #Is this expectation based on information from your employer or from other sources?
             "Cov_RetWk") #Thinking of the main job you were doing before lockdown, how likely or unlikely is it that you wil return to that job?
       

# questions related to childcare
childvar <- c("COV_E17M1", "COV_NotSen1") #For what reasons did the children within your household not attend nursery or school?
              
              
# Questions related to home-schooling
homevar <- c("COV_D22", "COV_E22", "COV_JobHom", #Homeschooling is negatively affecting my job
             "COV_D23", "COV_E23", "COV_WelHom", #Homeschooling is negatively affecting my well-being
             "COV_WelCh", "COV_D21", "COV_E21", "COV_RelHom", #Homeschooling is putting a strain on my relationships with others in the household
             "COV_E19", "COV_AbHom", #I am confident in my abilities to home school the children within my household
             "COV_HomSch", "COV_E18","COV_HomSch", #In the past seven days, have you home schooled your children due to the coronavirus (COVID-19) outbreak?
             
             #resources
             "COV_ResCh" , #I have access to the resources I need to help me homeschool my children well
             "COV_ResHSc1", #Which, if any, of the following resources has the child in your home used for their homeschooling?
             "COV_ResOld1", #Which, if any, of the following resources has the oldest child in your home used for their homeschooling?
             "COV_LeHom", #The children within my household are continuing to learn whilst being homeschooled
             
             # Filtered
             "COV_MyResSp", #Please specify the other resources that you have used in your homeschooling
             "COV_ResHScSp", #Please specify the other resources the child in your home has used for their homeschooling
             "COV_ResOldSp", #Please specify the other resources the oldest child in your home has used for their homeschooling
             "COV_ResOldSp", #Please specify the other resources the oldest child in your home who is still being home schooled has used for their homeschooling.
             "COV_ChildStSp" #Please specify the other things that are affecting your ability to continue your childrens' studies from home
             
)

# Questions related to mental health
mentalvar <- c("HealIll", #Do you have any physical or mental health conditions or illnesses?"
               "COV_Medic", #Before the coronavirus (COVID-19) outbreak, were you receiving medical care for any long-term mental or physical health condition, problem or illness?"
              
               "COV_Lon", "COV_1", #And how often do you feel lonely?"
               "COV_D32M01","COV_E32M01","COV_Wellb01","COV_WellC01","COV_WellD01", #In the past seven days, how has well-being been affected?"
               "COV_Down", #Over the last two weeks, how often have you been bothered by feeling down, depressed or hopeless?"
               "COV_Neg", #Over the last two weeks, how often have you been bothered by feeling negative about yourself or that you are a failure or have let yourself or your family down?"
               "COV_GAD1", #Over the last two weeks, how often have you been bothered by feeling nervous, anxious or on edge?"
               "COV_Energy", #Over the last two weeks, how often have you been bothered by feeling tired or having little energy?"
               "COV_Appet", #Over the last two weeks, how often have you been bothered by having a poor appetite or overeating?"
               "COV_Inter", #Over the last two weeks, how often have you been bothered by having little interest or pleasure in doing things?"
               "COV_Conc", #Over the last two weeks, how often have you been bothered by having trouble concentrating on things, such as reading the newspaper or watching television?"
               "COV_Sleep", #Over the last two weeks, how often have you been bothered by having trouble falling or staying asleep, or sleeping too much?"
               "COV_Rest", #Over the last two weeks, how often have you been bothered by moving or speaking so slowly that other people could have noticed; or being so fidgety or restless that you have been moving around a lot more than usual?"
               "COV_GAD2", #Over the last two weeks, how often have you been bothered by not being able to stop or control worrying?"
               "MCZ_4", #Overall, how anxious did you feel yesterday?"
               "MCZ_3", #Overall, how happy did you feel yesterday, where 0 is 'not at all happy' and 10 is 'completely happy'?"
               "MCZ_1", #Overall, how satisfied are you with your life nowadays, where 0 is 'not at all satisfied' and 10 is 'completely satisfied?"
               "MCZ_2", #Overall, to what extent do you feel that the things you do in your life are worthwhile, where 0 is 'not at all worthwhile' and 10 is 'completely worthwhile'?"
                # filtered

               "COV_WellbSp","COV_WellCSp","COV_D32MSp","COV_E32MSp","COV_WellCSp") #Please specify the other ways in which the coronavirus (COVID-19) pandemic has affected your wellbeing."

# questions related to finance
financevar <- c("GRSBand", #Banded gross income <- check
                "COV_PayEx","COV_D46","COV_E46", #Could your household afford to pay an unexpected, but necessary, expense of ?850?
                "COV_ChFin", "COV_B22", "COV_C39", "COV_D41", "COV_E41", #How do you expect the financial position of your household to change over the next 12 months?
                # filtered
                "COV_FinSp") #Please specify the other ways in which the coronavirus (COVID-19) outbreak has affected your household finances.


# 3) Harmonization variable names ----
# Create dummy data set for now

# this needs to replaced by loading the actual data (
dummy <- function(sheet) {
w <- read_xlsx("Input/8635_opn_2020_covid_all_waves_variable_catalogue.xlsx", 
                sheet = sheet) %>% 
  rename(var.name = 1, var.label = 2) 
df <- data.frame(matrix(rep(1, ncol(w)),ncol = nrow(w), nrow = 1))
colnames(df) <- w$var.name
df <- df %>% mutate(sheet = sheet, .before = 1) 
}

opn_dummy <- purrr::map(sheets, ~dummy(.))
# )

# create wave, month and year variable
opn_var <- opn_dummy %>%
  purrr::map(., . %>% 
  mutate(sheetID = str_extract(sheet, pattern = "^(.=?_)|^(..=?_)") %>% str_remove("_"),
         year = str_extract(sheet, pattern = "202."), # extract year
         month = as.numeric(str_extract(sheet, pattern = "(?<=2020)..")), .before = 1) %>% # extract month
  group_by(month) %>% 
  mutate(rep = as.numeric(as.factor(sheetID)), .before = 1) %>% # create variable for repetition of survey within month
  ungroup() %>% 
  mutate(wave = as.numeric(as.factor(sheetID)), .before = 1) %>%  # numerical ID for wave
# select variables
  select_if(names(.) %in% c("sheetID", "year", "month", "rep", "wave", responchar, 
                            workvar, childvar, homevar, financevar, mentalvar)))

# Load harmonization table
harm <- read_xlsx("Harmonization_table_varnames.xlsx")

# Harmonization function
harmonize.var <- function(df){
  var.matches <- match(colnames(df), 
                         harm$var.name)
  used.var <- harm[var.matches, ]
  df <- df %>% set_colnames(used.var$accvar.name)
}

# harmonized data 
opn <- purrr::map(opn_var,  ~harmonize.var(.x)) %>% 
  # bind rows of waves
  bind_rows() %>% 
  # sort alphabetically
  select(wave, rep, sheetID, year, month, sort(colnames(.)))


# 4) Cleaning the data set ----
# set refusal/prefer not to say NA
opn <- opn %>% 
  mutate(MCZ_1 = na_if(MCZ_1, "98a"), 
         MCZ_2 = na_if(MCZ_2, "98a"),
         MCZ_3 = na_if(MCZ_3, "98a"),
         MCZ_4 = na_if(MCZ_4, "98a"), 
         Sectro03 = na_if(Sectro03,"98a"), 
         TelBand = na_if(TelBand,"98a"), 
         COV_Wrk01 = na_if(COV_Wrk01,"98a"), 
         COV_Wellb01 = na_if(COV_Wellb01,"98a"),
         CasWrk = na_if(CasWrk, 8),
         HighEd4 = na_if(HighEd4, "8a"),
         Parent = na_if(Parent, "8a"),
         COV_ChFin = na_if(COV_ChFin, "8a"),
         COV_HomSch = na_if(COV_HomSch, "8a"),
         COV_WrkHom = na_if(COV_WrkHom, "8a"),
         GRSBand = na_if(GRSBand, "99999998a"),
         CL_EthnGrp5 = na_if(CL_EthnGrp5, "-8a"),
         COV_Lon = na_if(COV_Lon, 7),
         HealIll = na_if(HealIll, 5),
         COV_KeyWrk = na_if(COV_KeyWrk, 4),
         COV_WrkCon = na_if(COV_WrkCon, "8a"),
         COV_WrkPPE = na_if(COV_WrkPPE, 7),
         COV_SocDis = na_if(COV_SocDis, 7),
         COV_WrkRea01 = na_if(COV_WrkRea01, "98a"),
         COV_WrkCon = na_if(COV_WrkCon, "8a"),
         COV_ResHSc1 = na_if(COV_ResHSc1, "98a"),
         COV_ResOld1 = na_if(COV_ResOld1, "98a"),
         COV_Wellb01 = na_if(COV_Wellb01,"98a"),
         COV_ChFin = na_if(COV_ChFin, "8a"),
         Cov_RetWk = na_if(Cov_RetWk, 8),
         COV_Skill1 = na_if(COV_Skill1, 98),
         COV_Skill1 = na_if(COV_Skill1, "98a"),
         COV_Medic = na_if(COV_Medic, 4),
         COV_WrkRea01 = na_if(COV_WrkRea01, "98a"),
         COV_Inter = na_if(COV_Inter, "8a"),
         COV_Down = na_if(COV_Down, "8a"),
         COV_Sleep = na_if(COV_Energy, "8a"),
         COV_Appet = na_if(COV_Appet, "8a"),
         COV_Neg = na_if(COV_Neg, "8a"),
         COV_Conc = na_if(COV_Conc, "8a"),
         COV_Rest = na_if(COV_Rest, "8a"),
         COV_GAD1 = na_if(COV_GAD1, "8a"),
         COV_GAD2 = na_if(COV_GAD2, "8a"),
         COV_WrkPhyCon = na_if(COV_WrkPhyCon, 9998),
         COV_WrkClProx = na_if(COV_WrkClProx, 9998),
         HealIll = na_if(HealIll, 5)
         )
        
# set "don't know" to NA
opn <- opn %>% 
  mutate(MCZ_1 = na_if(MCZ_1, "99a"), 
         MCZ_2 = na_if(MCZ_2, "99a"),
         MCZ_3 = na_if(MCZ_3, "99a"),
         MCZ_4 = na_if(MCZ_4, "99a"),
         COV_Lon = na_if(COV_Lon, 6),
         HealIll = na_if(HealIll, 4),
         CasWrk = na_if(CasWrk, 9),
         Sectro03 = na_if(Sectro03, "99a"),
         Stat = na_if(Stat, "9a"),
         FtPtWk = na_if(FtPtWk,  "9a"),
         TelBand = na_if(TelBand,"99a"),
         COV_KeyWrk = na_if(COV_KeyWrk, 6),
         COV_WrkHom = na_if(COV_WrkHom, "9a"),
         COV_Wrk01 = na_if(COV_Wrk01,"99a"), 
         COV_WrkCon = na_if(COV_WrkCon, "9a"),
         COV_WrkPPE = na_if(COV_WrkPPE, 6),
         COV_SocDis = na_if(COV_SocDis, 6),
         COV_HomSch = na_if(COV_HomSch, "9a"),
         COV_Wellb01 = na_if(COV_Wellb01,"99a"),
         COV_ChFin = na_if(COV_ChFin, "9a"),
         COV_PayEx = na_if(COV_PayEx, 3),
         COV_ResHSc1 = na_if(COV_ResHSc1,"99a"),
         COV_ResOld1 = na_if(COV_ResOld1,"99a"),
         HighEd4 = na_if(HighEd4, "9a"),
         Parent = na_if(Parent, "9a"),
         InEcAc = na_if(InEcAc, "9a"),
         GRSBand = na_if(GRSBand, "99999999a"),
         CL_EthnGrp5 = na_if(CL_EthnGrp5, "-9a"),
         COV_WrkRea01 = na_if(COV_WrkRea01, "99a"),
         Cov_RetWk = na_if(Cov_RetWk, 7),
         COV_Skill1 = na_if(COV_Skill1, 7),
         COV_WrkRea01 = na_if(COV_WrkRea01, "99a"),
         COV_WrkPhyCon = na_if(COV_WrkPhyCon, "9991a"),
         COV_WrkClProx = na_if(COV_WrkClProx, "9991a"),
         COV_WrkPhyCon = na_if(COV_WrkPhyCon, 9999),
         COV_WrkClProx = na_if(COV_WrkClProx, 9999),
         COV_Inter = na_if(COV_Inter, "9a"),
         COV_Down = na_if(COV_Down, "9a"),
         COV_Sleep = na_if(COV_Energy, "9a"),
         COV_Appet = na_if(COV_Appet, "9a"),
         COV_Neg = na_if(COV_Neg, "9a"),
         COV_Conc = na_if(COV_Conc, "9a"),
         COV_Rest = na_if(COV_Rest, "9a"),
         COV_GAD1 = na_if(COV_GAD1, "9a")
         )

# 5) Data exploration ----
# sample size per wave and sex
# 1 = male, 2 = female
sample_size <- opn %>%
  group_by(wave, RSex) %>%
  summarise(SEXSAMPLE = n())  %>% 
  group_by(wave) %>%
  mutate(SAMPLE = sum(SEXSAMPLE)) # adding the total sample size
sample_size

sex.label <- c("Men", "Women")
names(sex.label) <- c(1, 2)

# check variable types
str(opn)

opn <- opn %>% 
  mutate(across(CasWrk:TelBand, as.factor))

saveRDS(opn, "OPN_cleaned.rds")


## Differences between men and women in the amount of home-working
# available from end of April
COV_WrkHom # 1 = Yes, 2 = not able to work from home, 3 No

# Line graph showing the proportion men/women working from home
opn %>% 
  select(month, RSex, COV_WrkHom) %>% 
  # calculate percentage men/women working from home
  group_by(month, RSex, COV_WrkHom) %>% 
  summarise(count = n()) %>% 
  group_by(month, RSex) %>% 
  mutate(percentage = count/sum(count)*100) %>%
  filter(COV_WrkHom == 1) %>% # yes i'm working from home
  print() %>% 
  # line graph
  ggplot(aes(x = month, y = percentage, colour = RSex)) +
  geom_line(size = 1, show.legend = FALSE) +
  # geom_text(data = data.frame(RSex = c(1, 2),
  #                             label = sex.label,
  #                             x = c(12, 12), # label coordinates 
  #                             y = c()), # fill y coordinate so label is positioned at the end of the line
  #                             aes(x = x, y = y, label = label)) + 
  scale_y_continuous("%") +
  scale_x_continuous(labels = month.abb[3:12],
                     breaks = 3:12) + # add month labels
  labs(x = NULL, y = NULL,
       title ="Some kind of statement about the trend shown in the graph",
       subtitle  = "Percentage of men and women working from home",
       caption = c("Source: Office for National Statistics - Opinions and Lifestyle survey"))
 #theme?

# Why are you working from home
# 2 bargraphs (1 men 1 women) with reasons for working at home
opn %>% 
  select(RSex,
         COV_WrkRea_emplwrkhom, # My employer asked me to work from home
         COV_WrkRea_wrkclose,  # My workplace is closed
         COV_WrkRea_govadvice, # I am following government advice to work from home
         COV_WrkRea_nochildcare, # I don't have childcare available
         COV_WrkRea_normhome) %>%   # I normally work from home some or all of the time
  # create one variable for reasons of working from home
  mutate(HomWrkRea = ifelse(COV_WrkRea_emplwrkhom == 1, "My employer asked me to work from home",
                            ifelse(COV_WrkRea_wrkclose == 1, "My workplace is closed",
                                   ifelse(COV_WrkRea_govadvice == 1, "I am following government advice to work from home", 
                                          ifelse(COV_WrkRea_nochildcare == 1, "I don't have childcare available",
                                                 ifelse(COV_WrkRea_normhome == 1, "I normally work from home some or all of the time", NA)))))) %>% 
  filter(!is.na(HomWrkRea)) %>% 
  # calculate percentage
  group_by(RSex, HomWrkRea) %>% 
  summarise(count = n()) %>%
  group_by(RSex) %>% 
  mutate(percentage = count/sum(count)*100) %>%
  print() %>% 
  # barplot
  ggplot(aes(x = HomWrkRea, y = percentage, group = RSex)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  # facet by sex
  facet_wrap(~RSex, labeller = labeller(RSex = sex.label)) +
  labs(x = NULL, y = NULL,
       title = "Some kind of statement about the trend shown in the graph",
       subtitle  = "Reasons for working from home",
       caption = c("Source: Office for National Statistics - Opinions and Lifestyle survey"))


# more reasons available, include?
#  #In the past seven days, why have you worked from home?
# 1. I normally work from home some or all of the time
# 2. My workplace is closed 
# 3. My employer asked me to work from home
# 4. I don't have childcare available
# 5. I don’t have a safe way to travel to work
# 6. I am following government advice to work from home
# 7. I prefer to work from home / I live in a local lockdown area and have been advised to work from home
# 8. I am self-isolating due to having symptoms of the coronavirus / I prefer to work from home
# 9. I am self-isolating due to someone else in my household having symptoms of the coronavirus (COVID-19)/ I am self-isolating due to having symptoms of the coronavirus (COVID-19)
# 10. I am shielding at home due to being clinically vulnerable / I am self-isolating due to someone else in my household having symptoms of the coronavirus (COVID-19)
# 11. Other (please specify) at COV_WrkSp /I am shielding at home due to being clinically vulnerable
# 12 Other (please specify)

### Q2 Are there differences between (working class) men and women thinking about the burden caused by home-schooling?
# Whether home-schooled children 1 = yes 2 = no. 
# available apr-jun
opn %>% 
  select(RSex, COV_HomSch) %>% 
  # calculate percentage
  group_by(COV_HomSch, RSex) %>% 
  summarise(count = n()) %>% 
  group_by(COV_HomSch) %>% 
  mutate(percentage = count/sum(count)*100) %>% 
  filter(!is.na(COV_HomSch) & !COV_HomSch == 2) %>% 
  print() %>% 
  # pie chart
  ggplot(aes(x = "", y = percentage, fill = RSex)) +
  # geom_bar(stat="identity", width=1, color="white") +
  # geom_text(data.frame(RSex = c(1,2),
  #                      label = sex.label,
  #                      x = c(50,50), # change position of labels
  #                      y = c(25,75)), 
  #           aes(x = x, y = y, label = label)) +
  coord_polar("y", start = 0) +
  #theme_void() +
  theme(legend.position = "none") +
  labs(x = NULL, y = NULL,
       title ="Some kind of statement about what is shown in the graph",
       subtitle  = "Percentage of men and women that had close physical contact at work in the last 7 days",
       caption = c("Source: Office for National Statistics - Opinions and Lifestyle survey"))

# Homeschooling is negatively affecting my job 1 strongly agree - 5 strongly disagree. 
# available apr-may 
opn %>% 
  select(RSex, COV_WelHom) %>% 
  # calculate percentage
  group_by(RSex, COV_WelHom) %>% 
  summarise(count = n()) %>% 
  group_by(RSex) %>% 
  mutate(percentage = count/sum(count) * 100) %>% 
  filter(!is.na(COV_WelHom)) %>% 
  print() %>% 
  # stacked 100 % bar plot
  ggplot(aes(x = COV_WelHom, y = percentage, group = RSex)) +
  geom_bar(position = "fill", stat = "identity", colour = "black") +
  coord_flip() +
  scale_color_manual(labels = c("Strongly agree", "Somewhat agree", "Neutral", "Somewhat disagree",
                                "Strongly disagree")) +
  facet_wrap(~RSex, labeller = labeller(RSex = sex.label)) +
  labs(x = NULL, y = NULL,
       title = "Some kind of statement about what is shown in the graph",
       caption = c("Source: Office for National Statistics - Opinions and Lifestyle survey")) +
  theme_void() +
  theme(legend.position = "top")
  
#  Homeschooling is negatively affecting my well-being. 1 strongly agree - 5 strongly disagree 
# available apr-may 
opn %>% 
  select(RSex, COV_JobHom) %>% 
  # calculate percentage
  group_by(RSex, COV_JobHom) %>% 
  summarise(count = n()) %>% 
  group_by(RSex) %>% 
  mutate(percentage = count/sum(count) * 100) %>% 
  filter(!is.na(COV_JobHom)) %>% 
  print() %>% 
  # stacked 100 % bar plot
  ggplot(aes(x = COV_JobHom, y = percentage, group = RSex)) +
  geom_bar(position = "fill", stat = "identity", colour = "black") +
  coord_flip() +
  scale_color_manual(labels = c("Strongly agree", "Somewhat agree", "Neutral", "Somewhat disagree",
                                "Strongly disagree")) +
  facet_wrap(~RSex, labeller = labeller(RSex = sex.label)) +
  labs(x = NULL, y = NULL,
       title = "Some kind of statement about what is shown in the graph",
       caption = c("Source: Office for National Statistics - Opinions and Lifestyle survey")) +
  theme_void() +
  theme(legend.position = "top")

# I am confident in my abilities to home school the children within my household. 1 strongly agree - 5 strongly disagree
# available apr-may 
opn %>% 
  select(RSex, COV_AbHom) %>% 
  # calculate percentage
  group_by(RSex, COV_AbHom) %>% 
  summarise(count = n()) %>% 
  group_by(RSex) %>% 
  mutate(percentage = count/sum(count) * 100) %>% 
  filter(!is.na(COV_AbHom)) %>% 
  print() %>% 
  # stacked 100 % bar plot
  ggplot(aes(x = COV_AbHom, y = percentage, group = RSex)) +
  geom_bar(position = "fill", stat = "identity", colour = "black") +
  coord_flip() +
  scale_color_manual(labels = c("Strongly agree", "Somewhat agree", "Neutral", "Somewhat disagree",
                                "Strongly disagree")) +
  facet_wrap(~RSex, labeller = labeller(RSex = sex.label)) +
  labs(x = NULL, y = NULL,
       title = "Some kind of statement about what is shown in the graph",
       caption = c("Source: Office for National Statistics - Opinions and Lifestyle survey")) +
  theme_void() +
  theme(legend.position = "top")




## Availability of home-schooling resources?

## Are there difference between men and women regarding the impact of covid on mental health?
# work?

## Impact of COVID on household finances? 
# 




  

