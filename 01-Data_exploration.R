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


# Selecting variables -----

# survey characteristics
surveychar <- 
  c("Month", "Year",
    "WkDayInd","WkDyMkr") # Interview day, Whether survey completed on weekday or weekend

# General COVID situation
covidgen <- c(
  "COV_LocLD", #Do you live in an area that is currently under local lockdown measures?"
  "COV_WrkReaB_LocalLD", #I live in a local lockdown area and have been advised to work from home"
  "COV_WrkReaC_LockDown") #I live in a lockdown area and have been advised to work from home"

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

                # Education
                "HighEd4", #Highest education level (4 groupings)
                "HighEd1", #Highest qualification level
     
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
                "TelBand", #We put answers into income bands. Which band represents your total personal
                # Place
                "CL_Country", #Country
                "GORA", #Government Office Region
                "sector_numeric") #Postcode sector (anonymised) - numeric

# questions related to work              
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

             # filtered
             "COV_WhyWrkSp", #Please specify the reasons you intend to work from home.
             "COV_WrkReaSp", #Please specify the reasons you have worked from home in the past seven days.
             
             # COVID safety at work
             "COV_SocDis",  #In the past seven days, how often have you stayed at least two metres away from other people while a work?
             "COV_WrkCon",  #In the past seven days, have you done any paid work requiring direct physical contact with othe people?
             "COV_WrkPhyCon","COV_D54","COV_E54","COV_WrkCon",  #Over the last 24 hours, how many people at work have you had direct physical contact with?
             "COV_WrkClProx",  #Over the last 24 hours, how many people have you been in close proximity with at work?
             "COV_WrkPPE",  #Personal protective equipment, or PPE, may include gloves, face masks or face shields. In the pas seven days, how often have you used PPE while at work?
             "COV_E13aMSp", "COV_HealSafSp",  #Please specify the other concerns you have about your health and safety at work due to coronaviru (COVID-19)
             "COV_E13aM01", "COV_Healsaf01", "COV_HealSafA01", #What concerns do you have about your health and safety at work due to coronavirus (COVID-19)?
             # filtered
             "COV_D13MSp","COV_E13MSp","COV_WrkSp",  #Please specify the other ways in which Coronavirus (COVID-19) has affected your work
             "COV_SkillSp",  #Please specify the other ways your work has changed since the coronavirus (COVID-19) outbreak.

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
             "COV_Skill1", #Which, if any, of the following ways has your work changed since the coronavirus outbreak?
             # filtered 
             "COV_WkSitDecSp",  #Please specify the other situations that you expect to apply to your work in December.
             "COV_WkSitOctNovSp",  #Please specify the other situations that you expect to apply to your work in October an November.
             "COV_WkSitOctSp",  #Please specify the other situations that you expect to apply to your work in October.
             "COV_WrkB_DV_wrkocar", "COV_Wrk_DV_wrkocar") #Whether specified 'I have to work around other caring responsibilties' at COV_WrkC/B
             
# more filtered
c("COV_Keep_WrkHme",  #I spend more time working from home' at COV_Keep"
"COV_Life_DV2",  #Whether 'work' is being affected"
"COV_Life_DV_work",  #Whether 'work' is being affected"
"COV_Life_DV15",  #Whether 'work travel plans' are being affected"
"COV_Life_wrktrav",  #Whether 'work travel plans' are being affected"
"CasWrk",  #Whether did any casual work for payment, even for an hour, in the reference week"
"OwnBus",  #Whether did any unpaid or voluntary work in the reference week"
"WrkRef",  #Whether did any work in paid job in reference week"
"Wrking",  #Whether had paid job in reference week"
"COV_HealSaf_DV_wrkhom",  #Whether specified 'asked to go into work when I could work from home instead' at COV_HealSaf"
"COV_HealSaf_DV_wrkhom",  #Whether specified 'asked to go into work when I could work from home instead' at COV_HealSafA"
"COV_WrkB_DV_workhm",  #Whether specified 'asked to work from home' at COV_WrkB"
"COV_Wrk_DV_workhm",  #Whether specified 'asked to work from home' at COV_WrkB"
"COV_RulesB_AvoidTrav",  #Whether specified 'Avoid travelling to other parts of the UK, including for overnight stays except where necessary, such as for work or education' at COV_RulesB"
"COV_HealSaf_DV_coldiag",  #Whether specified 'colleagues have been diagnosed with coronavirus (COVID-19)' at COV_HealSaf"
"COV_HealSaf_DV_colsym",  #Whether specified 'colleagues have shown symptoms of coronavirus (COVID-19) whilst remaining at work' at COV_HealSaf"
"COV_WrkB_DV_dechrs",  #Whether specified 'decrease in hours worked' at COV_WrkB"
"COV_Wrk_DV_dechrs",  #Whether specified 'decrease in hours worked' at COV_WrkB"
"COV_HealSaf_DV3",  #Whether specified 'employer does not provide staff with antibacterial products to use whilst working' at COV_HealSaf"
"COV_HealSaf_DV_noantbac",  #Whether specified 'employer does not provide staff with antibacterial products to use whilst working' at COV_HealSaf"
"COV_BritTod_Emp",  #Whether specified 'Employment' at COV_BritTod"
"COV_WellD_DV_FurlghDiff",  #Whether specified 'Finding being on furlough difficult' at COV_WellD"
"COV_WellC_DV_wrkhom",  #Whether specified 'finding working from home difficult' at COV_WellC"
"COV_WrkB_DV_homdif",  #Whether specified 'finding working from home difficult' at COV_WrkB"
"COV_Wrk_DV_homdif",  #Whether specified 'finding working from home difficult' at COV_WrkB"
"COV_Wrk_DV_homdif",  #Whether specified 'finding working from home difficult' at COV_WrkC"
"COV_HealSaf_DV2",  #Whether specified 'following hygiene advice is difficult due to my role' at COV_HealSaf"
"COV_HealSaf_DV_hygdif",  #Whether specified 'following hygiene advice is difficult due to my role' at COV_HealSaf"
"COV_HealSaf_DV_hygdif",  #Whether specified 'following hygiene advice is difficult due to my role' at COV_HealSafA"
"COV_HealSaf_DV1",  #Whether specified 'following social distancing advice is difficult due to my role' at COV_HealSaf"
"COV_HealSaf_DV_socdis",  #Whether specified 'following social distancing advice is difficult due to my role' at COV_HealSaf"
"COV_HealSaf_DV_socdis",  #Whether specified 'following social distancing advice is difficult due to my role' at COV_HealSafA"
"COV_WhyWrk_GovAd",  #Whether specified 'I am following government advice to work from home' at COV_WhyWrk"
"COV_WhyWrkB_GovAd",  #Whether specified 'I am following government advice to work from home' at COV_WhyWrkB"
"COV_WkSitDec_Rednt",  #Whether specified 'I expect to be made redundant' at COV_WkSitDec"
"COV_WkSitJan_Rednt",  #Whether specified 'I expect to be made redundant' at COV_WkSitJan"
"COV_WkSitOct_Rednt",  #Whether specified 'I expect to be made redundant' at COV_WkSitOct"
"COV_WkSitOctNov_Rednt",  #Whether specified 'I expect to be made redundant' at COV_WkSitOctNov"
"COV_WkSitDec_IncHrs",  #Whether specified 'I expect to increase my working hours' at COV_WkSitDec"
"COV_WkSitJan_IncHrs",  #Whether specified 'I expect to increase my working hours' at COV_WkSitJan"
"COV_WkSitOct_IncHrs",  #Whether specified 'I expect to increase my working hours' at COV_WkSitOct"
"COV_WkSitOctNov_IncHrs",  #Whether specified 'I expect to increase my working hours' at COV_WkSitOctNov"
"COV_WkSitDec_RedHrs",  #Whether specified 'I expect to reduce my working hours' at COV_WkSitDec"
"COV_WkSitJan_RedHrs",  #Whether specified 'I expect to reduce my working hours' at COV_WkSitJan"
"COV_WkSitOct_RedHrs",  #Whether specified 'I expect to reduce my working hours' at COV_WkSitOct"
"COV_WkSitOctNov_RedHrs",  #Whether specified 'I expect to reduce my working hours' at COV_WkSitOctNov"
"COV_WkSitDec_Furl",  #Whether specified 'I expect to remain on furlough' at COV_WkSitDec"
"COV_WkSitJan_Furl",  #Whether specified 'I expect to remain on furlough' at COV_WkSitJan"
"COV_WkSitOct_Furl",  #Whether specified 'I expect to remain on furlough' at COV_WkSitOct"
"COV_WkSitOctNov_Furl",  #Whether specified 'I expect to remain on furlough' at COV_WkSitOctNov"
"COV_WkSitDec_RetFT",  #Whether specified 'I expect to return to work full time' at COV_WkSitDec"
"COV_WkSitJan_RetFT",  #Whether specified 'I expect to return to work full time' at COV_WkSitJan"
"COV_WkSitOct_RetFT",  #Whether specified 'I expect to return to work full time' at COV_WkSitOct"
"COV_WkSitOctNov_RetFT",  #Whether specified 'I expect to return to work full time' at COV_WkSitOctNov"
"COV_WkSitDec_RetPT",  #Whether specified 'I expect to return to work part time' at COV_WkSitDec"
"COV_WkSitJan_RetPT",  #Whether specified 'I expect to return to work part time' at COV_WkSitJan"
"COV_WkSitOct_RetPT",  #Whether specified 'I expect to return to work part time' at COV_WkSitOct"
"COV_WkSitOctNov_RetPT",  #Whether specified 'I expect to return to work part time' at COV_WkSitOctNov"
"COV_Wrk_DV_Retfurlgh",  #Whether specified 'I have been asked to return from furlough' at COV_WrkC"
"COV_WrkB_DV_furlough",  #Whether specified 'I have been furloughed' at COV_WrkB"
"COV_Wrk_DV_furlough",  #Whether specified 'I have been furloughed' at COV_WrkB"
"COV_Wrk_DV_furlough",  #Whether specified 'I have been furloughed' at COV_WrkC"
"COV_Skill_WkNewWays",  #Whether specified 'I have had to work in new ways' at COV_Skill"
"COV_WhyWrk_WkHme",  #Whether specified 'I normally work from home some or all of the time' at COV_WhyWrk"
"COV_WhyWrkB_WkHme",  #Whether specified 'I normally work from home some or all of the time' at COV_WhyWrkB"
"COV_WhyWrk_Prefer",  #Whether specified 'I prefer to work from home' at COV_WhyWrk"
"COV_WhyWrkB_Prefer",  #Whether specified 'I prefer to work from home' at COV_WhyWrkB"
"COV_LifeBet_DV_WrkHme",  #Whether specified 'I spend more time working from home' at COV_LifeBet"
"COV_VaccineNot_Wait",  #Whether specified 'I would wait to see how well the vaccine works' at COV_VaccineNot"
"COV_WrkB_DV_inchrs",  #Whether specified 'increase in hours worked' at COV_WrkB"
"COV_Wrk_DV_inchrs",  #Whether specified 'increase in hours worked' at COV_WrkB"
"COV_Wrk_DV_inchrs",  #Whether specified 'increase in hours worked' at COV_WrkC"
"COV_ConInf_DV_plnsch",  #Whether specified 'Information to help me plan work around school arrangements' at COV_ConInf"
"COV_LeftH_DV1",  #Whether specified 'key worker, travelling to and from work' at COV_LeftH"
"COV_LeftH_DV_keywktrav",  #Whether specified 'key worker, travelling to and from work' at COV_LeftH"
"COV_HealSaf_DV5",  #Whether specified 'limited or no protective clothing available' at COV_HealSaf"
"COV_HealSaf_DV_nocloth",  #Whether specified 'limited or no protective clothing available' at COV_HealSaf"
"COV_HealSaf_DV_noequip",  #Whether specified 'limited or no protective equipment available' at COV_HealSaf"
"COV_WhyWrk_Empr",  #Whether specified 'My employer asked me to work from home' at COV_WhyWrk"
"COV_WhyWrkB_Empr",  #Whether specified 'My employer asked me to work from home' at COV_WhyWrkB"
"COV_Skill_NoChange",  #Whether specified 'My work has not changed' at COV_Skill"
"COV_Life_DV_work",  #Whether specified 'My work is being affected' at COV_LifeC01 to COV_LifeC18"
"COV_WhyWrk_WkClsd",  #Whether specified 'My workplace is closed' at COV_WhyWrk"
"COV_WhyWrkB_WkClsd",  #Whether specified 'My workplace is closed' at COV_WhyWrkB"
"COV_LeftH_DV2",  #Whether specified 'non-key worker, travelling to and from work' at COV_LeftH"
"COV_LeftH_DV_non_keywktrav",  #Whether specified 'non-key worker, travelling to and from work' at COV_LeftH"
"COV_HealSaf_DV_sdtrav",  #Whether specified 'social distancing is difficult when travelling to work' at COV_HealSafA"
"COV_WellC_DV_strwrk",  #Whether specified 'strain on my work relationships' at COV_WellC"
"COV_WellD_DV_strwrk",  #Whether specified 'strain on my work relationships' at COV_WellD"
"COV_LeftH_DV14",  #Whether specified 'travel outside of UK for work' at COV_LeftH"
"COV_LeftH_DV_wrkabr",  #Whether specified 'travel outside of UK for work' at COV_LeftH"
"COV_LeftHB_DV_wrkabr",  #Whether specified 'travel outside of UK for work' at COV_LeftHB"
"COV_LeftHC_DV_wrkabr",  #Whether specified 'travel outside of UK for work' at COV_LeftHC"
"COV_LeftHD_DV_wrkabr",  #Whether specified 'travel outside of UK for work' at COV_LeftHD"
"COV_LeftHB_DV_wktrav",  #Whether specified 'travelling to and from work' at COV_LeftHB"
"COV_LeftHC_DV_wktrav",  #Whether specified 'travelling to and from work' at COV_LeftHC"
"COV_LeftHD_DV_wktrav",  #Whether specified 'travelling to and from work' at COV_LeftHD"
"COV_LeftHG_DV_wktrav",  #Whether specified 'travelling to and from work' at COV_LeftHG"
"COV_WrkB_DV_uwrkccar",  #Whether specified 'unable to work at all due to childcare responsibilties' at COV_WrkB"
"COV_Wrk_DV_uwrkccar",  #Whether specified 'unable to work at all due to childcare responsibilties' at COV_WrkB"
"COV_WrkB_DV_uwrkhmsch",  #Whether specified 'unable to work at all due to home schooling responsibilities' at COV_WrkB"
"COV_Wrk_DV_uwrkhmsch",  #Whether specified 'unable to work at all due to home schooling responsibilities' at COV_WrkB"
"COV_WrkB_DV_unptran",  #Whether specified 'unable to work due to lack of public transport' at COV_WrkB"
"COV_Wrk_DV_unptran",  #Whether specified 'unable to work due to lack of public transport' at COV_WrkC"
"COV_WrkB_DV_seliso",  #Whether specified 'unable to work due to self-isolation' at COV_WrkB"
"COV_Wrk_DV_seliso",  #Whether specified 'unable to work due to self-isolation' at COV_WrkB"
"COV_Wrk_DV_SelisoShield",  #Whether specified 'unable to work due to self-isolation or shielding' at COV_WrkB"
"COV_FacSitu_DV_work",  #Whether specified 'while at work' at COV_FacSitu"
"COV_WrkB_DV_wrkocar",  #Whether specified 'I have to work around other caring responsibilties' at COV_WrkB"
"COV_Wrk_DV_wrkocar",  #Whether specified 'I have to work around other caring responsibilties' at COV_WrkC"
"COV_WkChild_ProbNotStop",  #Whether specified 'I have had problems with childcare providers but it has not stopped me returning to work' at COV_WkChild"
"COV_WrkB_DV_wrkccar",  #Whether specified 'I have to work around childcare' at COV_WrkB"
"COV_Wrk_DV_wrkccar",  #Whether specified 'I have to work around childcare' at COV_WrkC"
"COV_WrkB_DV_wrkhmsch",  #Whether specified 'I have to work around home schooling responsibilities' at COV_WrkB"
"COV_WellD_DV_JbLoss",  #Whether specified 'Feeling worried about possible job loss' at COV_WellD"
"COV_WellD_DV_WorRetWrk",  #Whether specified 'Feeling worried about returning to work' at COV_WellD"
"COV_WellD_DV_WorTravWrk")  #Whether specified 'Feeling worried about travel to work' at COV_WellD"


# questions related to childcare
childvar <- c("COV_E17M1", "COV_NotSen1", #For what reasons did the children within your household not attend nursery or school?
              "COV_WrkReaB_nochildcare", "COV_WrkReaC_nochildcare", "COV_WrkRea_nochildcare", #I don't have childcare available
              "COV_WkChild1", #In the past seven days, have you had problems with childcare providers that have stopped you from returning to work or being able to work the number of hours that you want to?
              "COV_WkChildSp")  #Please specify the other problems with childcare providers that have stopped you from returning to work or being able to work the number of hours that you want to.

# Questions related to home-schooling
homevar <- c("COV_D22", "COV_E22", "COV_JobHom", #Homeschooling is negatively affecting my job
             "COV_D23", "COV_E23", "COV_WelHom", #Homeschooling is negatively affecting my well-being
             "COV_WelCh", "COV_D21", "COV_E21", "COV_RelHom", #Homeschooling is putting a strain on my relationships with others in the household
             "COV_E19", "COV_AbHom", #I am confident in my abilities to home school the children within my household
             "COV_HomSch", "COV_E18","COV_HomSch", #In the past seven days, have you home schooled your children due to the coronavirus (COVID-19) outbreak?
             
             # time spent
             "Cov_LeaCh", #In the past seven days, how many hours of learning has the child in your home done using online lessons, worksheets, or other materials provided by their teachers?
             "Cov_OldCh", #In the past seven days, how many hours of learning has the oldest child in your home done using online lessons, worksheets, or other materials provided by their teacher
             
             #resources
             "COV_ResCh" , #I have access to the resources I need to help me homeschool my children well
             "COV_ResHSc1", #Which, if any, of the following resources has the child in your home used for their homeschooling?
             "COV_ResOld1", #Which, if any, of the following resources has the oldest child in your home used for their homeschooling?
             "COV_LeHom", #The children within my household are continuing to learn whilst being homeschooled
             
             # Filtered
             "COV_MyResSp", #Please specify the other resources that you have used in your homeschooling
             "COV_ResHScSp",#Please specify the other resources the child in your home has used for their homeschooling
             "COV_ResOldSp", #Please specify the other resources the oldest child in your home has used for their homeschooling
             "COV_ResOldSp", #Please specify the other resources the oldest child in your home who is still being home schooled has used for their homeschooling.
             "COV_ChildStSp", #Please specify the other things that are affecting your ability to continue your childrens' studies from home
             
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
               "COV_4Sp", #Please specify the other condition(s), problem(s) or illness(s) that you currently have"
               "COV_CHealSp", #Please specify the other condition(s), problem(s) or illness(s) that you currently have"
               "COV_WellbSp","COV_WellCSp","COV_D32MSp","COV_E32MSp","COV_WellCSp") #Please specify the other ways in which the coronavirus (COVID-19) pandemic has affected your wellbeing."

# questions related to finance
financevar <- c("GRSBand", #Banded gross income
                "COV_PayEx","COV_D46","COV_E46", #Could your household afford to pay an unexpected, but necessary, expense of ?850?
                "COV_AffHol", #Could your household afford to take a week's holiday away from home this year?
                "COV_ChFin", #How do you expect the financial position of your household to change over the next 12 months?
                "COV_B22", "COV_C39", "COV_D41", "COV_E41", #How do you expect the financial position of your household to change over the next 12 months?
                # filtered
                "COV_FinSp") #Please specify the other ways in which the coronavirus (COVID-19) outbreak has affected your household finances.

