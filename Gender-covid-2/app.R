# libraries
library(shiny)
library(tidyverse)
library(ggplot2)
library(ggthemes)
library(janitor)
library(sjlabelled)
library(shinythemes)

# House keeping
sexlabel <- c("Men", "Women")
sexcolour <- c("Men" = "#DF9216", "Women" = "#791F83")
agecolor <-  c("16-29" = "#FC8DB7", "30-49"= "#006295", "50-69" = "#A3BE41", "70+" = "#791F83")


#----load & prepare data sets----
## Homeworking
homwrk <- readRDS("Home-working.rds") 
homwrk <- homwrk %>% 
    pluck(2) %>% 
    filter(!Confidence == "Sample", !Confidence == "Weighted count") %>% 
    filter(Option == "Yes") %>% 
    filter(Characteristic %in% c("Sex", "Age")) %>% 
    filter(!Category == "16-69",
           !Category == "70+") %>% 
    # confidence intervals and mean in separate columns
    pivot_wider(names_from = Confidence, values_from = percentage) %>% 
    rename(percentage = "%") %>% 
    mutate(date = as.Date(date)) %>% 
    # add NAs for dates this question wasn't asked
    complete(date = seq.Date(as.Date("2020-07-10"), as.Date("2020-11-27"), by = "week"),
             nesting(Characteristic, Category), Question, Option) 

## Why home-working
why_homwrk <- readRDS("Home-working.rds") %>% 
    pluck(3) %>% 
    mutate(Option = 
               ifelse(str_detect(Option, "self-isolating|self isolating"), 
                      "I am self-isolating",
                      ifelse(str_detect(Option,"childcare"), 
                             "I don't have childcare available",
                             ifelse(str_detect(Option, "travel"),
                                    "I don't have a safe way to travel to work",
                                    ifelse(str_detect(Option, "local lockdown"),
                                           "I live in a local lockdown area", Option))))) %>% 
    filter(!is.na(Option)) %>% 
    filter(!Category == "16-69",
           !Category == "70+") %>%
    filter(!Option == "Other (please specify)") %>% 
    filter(Characteristic %in% c("Age", "Sex")) %>% 
    filter(Confidence == "%") %>% # add confidence intervals?
    mutate(Period = ifelse(str_detect(date, "2020-06"), "June 2020",
                           ifelse(str_detect(date, "2020-07"), "July 2020",
                                  ifelse(str_detect(date, "2020-11"), "November 2020",
                                         ifelse(str_detect(date, "2020-12"), "December 2020",
                                                ifelse(str_detect(date, "2021-01"), "January 2021",
                                                       ifelse(str_detect(date, "2021-02"), "February 2021",
                                                              ifelse(str_detect(date, "2021-03"), "March 2021",
                                                                     ifelse(str_detect(date, "2021-04"), "April 2021",
                                                                            ifelse(str_detect(date, "2021-05"), "May 2021",
                                                                                   ifelse(str_detect(date, "2021-06"), "June 2021", NA)))))))))))

## Mental health
wellbeing <- readRDS("Wellbeing.rds") %>% 
    filter(Confidence == "%") %>%  
    filter(!Category == "16 to 69" & !Category == "70+") %>%
    filter(!Option %in% c("Other", "Unable to exercise as normal", "Feeling like a burden to others",
                          "Spending too much time alone", "Feeling bored", 
                          "Strain on my personal relationships")
           ) %>% 
    mutate(Period = ifelse(date <= "2020-07-10", "Spring 2020", "Winter 2020/21"),
           Option = str_replace(Option, "Making my mental health worse", "My mental health is worse")) %>% 
    mutate(Characteristic = ifelse(str_detect(Category, pattern = "Working population|All persons total"),
                                   "Total",
                                   ifelse(str_detect(Category, "Men|Women"), "Sex",
                                          ifelse(str_detect(Category, "[:digit:][:digit:]"), "Age", Category)))) 
## Contact at work
contact_work <- readRDS("ContactWork.rds") %>% 
    filter(Confidence == "%") %>% 
    filter(Category == "Men" | Category == "Women" | Category == "All persons total")

## Employment data
lfs <- readRDS("Employement.rds") 
## non-standard work
d_precariety <- readRDS("Precarious.rds") %>% 
    mutate(NSECMJ10_2 = factor(NSECMJ10_2, levels = c("Lower supervisory & technical", "Intermediate", "Routine & semi-routine",
                          "Management & professional" , "Small employers & own account",
                          "Never worked, unemployed, and nec", NA)))

# Define UI ----
ui <-
navbarPage("",
           theme = shinytheme("yeti"),
           tabPanel("Introduction",
                    includeMarkdown("About.md")
                    ),
           tabPanel("Employment",
                    selectInput(inputId = "characteristic.em", label = "Characteristic",
                                choices = list("None", "Age", "Ethnicity")),
                    sliderInput(inputId = "quarter", "Quarter",
                                min = 1, max = 18, 
                                value = c(1,18)),
                    mainPanel(
                        plotOutput("linegraph.em")
                        )
                    ),
           navbarMenu("Non-standard work",
                    tabPanel("Part-time work",
                    selectInput(inputId = "characteristic.nsw", label = "Characteristic",
                                    choices = list("None", "Ethnicity", "Occupation", "Ethnicity & Occupation")),
                    sliderInput(inputId = "quarter.nsw", label = "Quarter",
                                min = 1, max = 18, 
                                value = c(1, 18)),
                    mainPanel(
                        plotOutput("parttime"))
                    ),
                    tabPanel("Temporary work",
                             sliderInput(inputId = "quarter.tw", label = "Quarter",
                                         min = 1, max = 18, 
                                         value = c(1,18)),
                    mainPanel(
                        plotOutput("why_permanent"))
                    ),
                    tabPanel("Atypical work",
                    selectInput(inputId = "characteristic.flex", label = "Characteristic",
                                choices = list("Sex", "Ethnicity")),
                    sliderInput(inputId = "quarter.aw", label = "Quarter",
                                min = 1, max = 18, 
                                value = c(1, 18)),
                    mainPanel(
                        plotOutput("flex"))
                    )),
           tabPanel("Homeworking",
                    selectInput(inputId = "characteristic", label = "Characteristic",
                                choices = list("Sex", "Age")),
                    checkboxInput(inputId = "confidence",
                                  label = "Confidence inteval",
                                  value = FALSE),
                    dateRangeInput("daterange", label = "Time period",
                                   start = min(homwrk$date), end = max(homwrk$date)),
                    mainPanel(
                        plotOutput("linegraph"),
                        plotOutput("barplot")
                        )
           ),
           tabPanel("Mental health",
                    selectInput(inputId = "characteristic.mh", label = "Characteristic",
                                choices = list("Sex", "Age")),
                    mainPanel(
                        plotOutput("dotplot")
                    )
                    
           ),
           tabPanel("Social-distancing at work",
                    mainPanel(
                        plotOutput("dotplot2"),
                        plotOutput("barplot2")
                        
                    )
           )
           
)

# Define server ----
server <- function(input, output) {

        # Employment
        output$linegraph.em <- renderPlot({
           p <-  lfs %>%
                filter(AGE >= 18 & AGE <= 64 & ILODEFR == "In employment"  |
                           ILODEFR == "Not employed"  & QUARTER <= 16) %>%
                filter(!AGEEUL_2 == "Above 64" |!AGEEUL_2 == "Below 18") %>%
                group_by(QUARTER, ILODEFR, SEX) %>%
                summarise(Active = sum(PWT18))  %>%
                pivot_wider(names_from = 2, values_from = Active) %>%
                adorn_totals("col") %>%
                mutate(Rate = `Not employed` / Total) %>%
                ggplot(aes(x = QUARTER, y = Rate, group = SEX, color = SEX)) +
                geom_vline(xintercept = 4, color = "#c9c9c9") + # grid line between 2019/2020
                geom_vline(xintercept = 15, color = "#c9c9c9") + # grid line between 2020/2021
                geom_line(size = 1, show.legend = FALSE) +
                geom_point() +
                # scales
                scale_colour_manual("",values = c("#DF9216", "#791F83")) +
                scale_y_continuous( breaks = seq(0,0.2, 0.02), limits = c(0, 0.18), labels =
                                       scales::percent_format(accuracy = 1L)) +
                scale_x_continuous(breaks = c(1:18),
                                   limits = c(input$quarter[1], input$quarter[2]),
                                   labels = c("Jan-\nMar","Apr-\nJun","Jul-\nSep","Oct-\nDec",
                                              "Jan-\nMar", "Feb-\nApr", "Mar-\nMay", "Apr-\nJun", "May-\nJul",
                                              "Jun-\nAug", "Jul-\nSep","Aug-\nOct", "Sep-\nNov", "Oct-\nDec",
                                              "Nov-\nJan", "Dec-\nFeb", "Jan-\nMar", "Feb-\nApr")) +
                # # theme
                theme_economist_white(gray_bg = FALSE) +
                theme(plot.margin = unit(c(1,3,1,1), units = "lines"),
                      plot.title = element_text(vjust = 3),
                      text = element_text(size = 12),
                      plot.caption = element_text(size = 10, hjust = 1))  +
                # annotation
                labs(x = NULL, y = NULL,
                     title = "Unemployment during the COVID-19 pandemic",
                     subtitle = "Unemployment rate UK population",
                     caption = "Source: UK Labour Force Survey (Person)") +
                annotate(geom = "text", x = 2, y = 0.17, label = "2019", size = 4) +
                annotate(geom = "text", x = 9.5, y = 0.17, label = "2020", size = 4) +
                annotate(geom = "text", x = 16, y = 0.17, label = "2021", size = 4) 
           p 
           if(input$characteristic.em == "Age")
            p <-
               lfs %>%
                filter(AGE >= 18 & AGE <= 64 & ILODEFR == "In employment"  |
                           ILODEFR == "Not employed"  & QUARTER <= 16) %>%
                group_by(QUARTER, ILODEFR, AGEEUL_2, SEX) %>%
                summarise(Active = sum(PWT18))  %>%
                pivot_wider(names_from = 2, values_from = Active) %>%
                adorn_totals("col") %>%
                mutate(Rate = `Not employed` / Total) %>%
               subset(., !AGEEUL_2 %in% c("Above 64", "Below 18")) %>% 
               ggplot(aes(x = QUARTER, y = Rate, group = SEX, color = SEX)) +
                geom_vline(xintercept = 4, color = "#c9c9c9") + # grid line between 2019/2020
                geom_vline(xintercept = 15, color = "#c9c9c9") + # grid line between 2020/2021
                geom_line(size = 1, show.legend = FALSE) +
                geom_point() +
               # faceting 
               facet_wrap(~AGEEUL_2, ncol = 2) +
                #scales
                scale_colour_manual("",values = c("#DF9216", "#791F83")) +
                scale_y_continuous(breaks = seq(0,0.2, 0.02), limits = c(0,0.18), labels =
                                       scales::percent_format(accuracy = 1L)) +
                scale_x_continuous(breaks = c(1:16),
                                   limits = c(input$quarter[1], input$quarter[2]),
                                   labels = c("Jan-\nMar","","Jul-\nSep","",
                                              "Jan-\nMar", "", "Mar-\nMay", "", "May-\nJul",
                                              "", "Jul-\nSep","", "Sep-\nNov", "",
                                              "Nov-\nJan", "")) +
                
                # theme
                theme_economist_white(gray_bg = FALSE) +
                theme(plot.margin = unit(c(1,3,1,1), units = "lines"),
                      plot.title = element_text(vjust = 3),
                      text = element_text(size = 12),
                      plot.caption = element_text(size = 10, hjust = 1))  +
                # annotation
                labs(x = NULL, y = NULL,
                     title = "Unemployment during the COVID-19 pandemic",
                     subtitle = "Unemployment rate UK population",
                     caption = "Source: UK Labour Force Survey (Person)") +
                annotate(geom = "text", x = 2, y = 0.17, label = "2019", size = 3) +
                annotate(geom = "text", x = 9.5, y = 0.17, label = "2020", size = 3) +
                annotate(geom = "text", x = 16, y = 0.17, label = "2021", size = 3) 
                
           p
        if(input$characteristic.em == "Ethnicity")
           p <- lfs %>%
                filter(AGE >= 18 & AGE <= 64 & ILODEFR == "In employment"  |
                           ILODEFR == "Not employed"  & QUARTER <= 16) %>%
                group_by(QUARTER, ILODEFR, ETHUKEUL, SEX) %>%
                summarise(Active = sum(PWT18))  %>%
                pivot_wider(names_from = 2, values_from = Active) %>%
                adorn_totals("col") %>%
                mutate(Rate = `Not employed` / Total) %>%
                filter(!is.na(ETHUKEUL)) %>%
                droplevels() %>%
                ggplot(aes(x = QUARTER, y = Rate, group = SEX, color = SEX)) +
                geom_vline(xintercept = 4, color = "#c9c9c9") + # grid line between 2019/2020
                geom_vline(xintercept = 15, color = "#c9c9c9") + # grid line between 2020/2021
                geom_line(size = 1, show.legend = FALSE) +
                geom_point() +
                #scales
                scale_colour_manual("", values = c("#DF9216", "#791F83")) +
                scale_y_continuous(breaks = seq(0,0.2, 0.02), limits = c(0,0.18), labels =
                                       scales::percent_format(accuracy = 1L)) +
                scale_x_continuous(breaks = c(1:16),
                                   limits = c(input$quarter[1], input$quarter[2]),
                                   labels = c("Jan-\nMar","","Jul-\nSep","",
                                              "Jan-\nMar", "", "Mar-\nMay", "", "May-\nJul",
                                              "", "Jul-\nSep","", "Sep-\nNov", "",
                                              "Nov-\nJan", "")) +
                # theme
                theme_economist_white(gray_bg = FALSE) +
                theme(plot.margin = unit(c(1,3,1,1), units = "lines"),
                      plot.title = element_text(vjust = 3),
                      text = element_text(size = 12),
                      plot.caption = element_text(size = 10, hjust = 1),
                      axis.text.x = element_text(size = 9))  +
                # annotation
                labs(x = NULL, y = NULL,
                     title = "Unemployment during the COVID-19 pandemic",
                     subtitle = "Unemployment rate UK population",
                     caption = "Source: UK Labour Force Survey (Person)") +
                annotate(geom = "text", x = 2, y = 0.17, label = "2019", size = 3) +
                annotate(geom = "text", x = 9.5, y = 0.17, label = "2020", size = 3) +
                annotate(geom = "text", x = 16, y = 0.17, label = "2021", size = 3) +
                # faceting
                facet_wrap(~ETHUKEUL, ncol = 3)
           p
           
            },
        height = 600,
        width = 900)
        # Non-standard work
        ## Part-time  
        output$parttime <- renderPlot({
        g <- d_precariety %>% # table summary
                filter(!is.na(FTPTWK)) %>% # # omit no answers and no apply
                group_by(QUARTER, SEX, FTPTWK) %>%
                summarise(count = sum(PWT18)) %>%
                group_by(QUARTER, SEX) %>%
                # percentage part-time per quarter
                mutate(percentage = count/sum(count)) %>%
                filter(FTPTWK == 2) %>%
            ggplot(aes(x = QUARTER, y = percentage, colour = SEX)) + # line graph
            geom_vline(xintercept = 4, color = "#c9c9c9") + # gridline between 2019/2020
            geom_vline(xintercept = 15, color = "#c9c9c9") + # gridline between 2020/2021
            geom_line(size = 1, show.legend = FALSE) +
            geom_point() +
            geom_vline(xintercept = 7, linetype="dotted") +

            # scale and colour
            scale_colour_manual("", values = c("#DF9216", "#791F83")) +
            scale_y_continuous(breaks = seq(0,0.7, 0.1), limits = c(0, 0.75), labels =
                                   scales::percent_format(accuracy = 1)) +
            scale_x_continuous(breaks = c(1:18),
                               limits = c(input$quarter.nsw[1], input$quarter.nsw[2]),
                               labels = c("Jan-\nMar","Apr-\nJun","Jul-\nSep","Oct-\nDec",
                                        "Jan-\nMar", "Feb-\nApr", "Mar-\nMay", "Apr-\nJun", "May-\nJul",
                                        "Jun-\nAug", "Jul-\nSep","Aug-\nOct", "Sep-\nNov", "Oct-\nDec",
                                        "Nov-\nJan", "Dec-\nFeb", "Jan-\nMar", "Feb-\nApr")) +
            # annotation
            labs(x = "",
                 y = "",
                 title ="Part-time work during the COVID-19 pandemic",
                 subtitle  = "Percentage of workers in a part-time job",
                 caption = c("Source: UK Labour Force Survey (Person)")) +
            annotate(geom = "text", x = 2, y = 0.74, label = "2019", size = 3.5) +
            annotate(geom = "text", x = 9.5, y = 0.74, label = "2020", size = 3.5) +
            annotate(geom = "text", x = 17, y = 0.74, label = "2021", size = 3.5) +

            # theme
            theme_economist_white(gray_bg = FALSE) +
            theme(plot.margin = unit(c(1,3,1,1), units = "lines"),
                  plot.title = element_text(vjust = 3),
                  text = element_text(size = 12),
                  plot.caption = element_text(size = 10, hjust = 1))
        g
        if(input$characteristic.nsw == "Ethnicity")
        g <- d_precariety %>% # table summary
            filter(!is.na(FTPTWK)) %>% 
            group_by(QUARTER, SEX, ETHUKEUL_2, FTPTWK) %>%
            summarise(count = sum(PWT18)) %>%
            group_by(QUARTER, SEX, ETHUKEUL_2) %>%
            # percentage part-time per quarter
            mutate(percentage = count/sum(count)) %>%
            filter(FTPTWK == 2 & !is.na(ETHUKEUL_2)) %>%
            ggplot(aes(x = QUARTER, y = percentage, colour = SEX)) + # line graph
            geom_vline(xintercept = 4, color = "#c9c9c9") + # gridline between 2019/2020
            geom_vline(xintercept = 15, color = "#c9c9c9") + # gridline between 2020/2021
            geom_line(size = 1, show.legend = FALSE) +
            geom_point(shape = 19) +
            geom_vline(xintercept = 7, linetype="dotted") +
            facet_wrap(~ETHUKEUL_2, ncol =2) +

            # scale and colour
            scale_colour_manual("", values = c("#DF9216", "#791F83")) +
            scale_y_continuous(breaks = seq(0,0.7, 0.1), limits = c(0, 0.75), labels =
                                   scales::percent_format(accuracy = 1)) +
            scale_x_continuous(breaks=c(1:18),
                               limits = c(input$quarter.nsw[1], input$quarter.nsw[2]),
                               labels=c("Jan-\nMar","","Jul-\nSep","",
                                        "Jan-\nMar", "", "Mar-\nMay", "", "May-\nJul",
                                        "", "Jul-\nSep","", "Sep-\nNov", "",
                                        "Nov-\nJan", "", "Jan-\nMar", "")) +
            # annotation
            labs(x = "",
                 y = "",
                 title ="Part-time work during the COVID-19 pandemic",
                 subtitle  = "Percentage of workers in a part-time job",
                 caption = c("Source: UK Labour Force Survey (Person)")) +
            annotate(geom = "text", x = 2, y = 0.74, label = "2019", size = 3.5) +
            annotate(geom = "text", x = 9.5, y = 0.74, label = "2020", size = 3.5) +
            annotate(geom = "text", x = 17, y = 0.74, label = "2021", size = 3.5) +

            # theme
            theme_economist_white(gray_bg = FALSE) +
            theme(plot.margin = unit(c(1,3,1,1), units = "lines"),
                  plot.title = element_text(vjust = 3),
                  text = element_text(size = 12),
                  strip.text = element_text(size = 9),
                  plot.caption = element_text(size = 10, hjust = 1))
        g
        if(input$characteristic.nsw == "Occupation")
        g <- d_precariety %>% # table summary
            filter(!is.na(FTPTWK) & !NSECMJ10_2 == "Never worked, unemployed, and nec") %>% # # omit no answers and no apply
            group_by(QUARTER, SEX, NSECMJ10_2, FTPTWK) %>%
            summarise(count = sum(PWT18)) %>%
            group_by(QUARTER, SEX, NSECMJ10_2) %>%
            # percentage part-time per quarter
            mutate(percentage = count/sum(count)) %>%
            filter(FTPTWK == 2 & !is.na(NSECMJ10_2)) %>%
            ggplot(aes(x = QUARTER, y = percentage, colour = SEX)) + # line graph
            geom_vline(xintercept = 4, color = "#c9c9c9") + # gridline between 2019/2020
            geom_vline(xintercept = 15, color = "#c9c9c9") + # gridline between 2020/2021
            geom_line(size = 1, show.legend = FALSE) +
            geom_point(shape = 19) +
            geom_vline(xintercept = 7, linetype="dotted") +
            facet_wrap(~NSECMJ10_2, ncol = 3) +
            
            # scale and colour
            scale_colour_manual("", values = c("#DF9216", "#791F83")) +
            scale_y_continuous(breaks = seq(0,0.7, 0.1), limits = c(0, 0.75), labels =
                                   scales::percent_format(accuracy = 1)) +
            scale_x_continuous(breaks=c(1:18),
                               limits = c(input$quarter.nsw[1], input$quarter.nsw[2]),
                               labels=c("Jan-\nMar","","Jul-\nSep","",
                                        "Jan-\nMar", "", "Mar-\nMay", "", "May-\nJul",
                                        "", "Jul-\nSep","", "Sep-\nNov", "",
                                        "Nov-\nJan", "", "Jan-\nMar", "")) +
            # annotation
            labs(x = "",
                 y = "",
                 title ="Part-time work during the COVID-19 pandemic",
                 subtitle  = "Percentage of workers in a part-time job",
                 caption = c("Source: UK Labour Force Survey (Person)")) +
            annotate(geom = "text", x = 2, y = 0.74, label = "2019", size = 3.5) +
            annotate(geom = "text", x = 9.5, y = 0.74, label = "2020", size = 3.5) +
            annotate(geom = "text", x = 17, y = 0.74, label = "2021", size = 3.5) +
            
            # theme
            theme_economist_white(gray_bg = FALSE) +
            theme(plot.margin = unit(c(1,3,1,1), units = "lines"),
                  plot.title = element_text(vjust = 3),
                  axis.text.x =  element_text(size = 9),
                  text = element_text(size = 12),
                  plot.caption = element_text(size = 10, hjust = 1))
        g
        if(input$characteristic.nsw == "Ethnicity & Occupation")
        g <- d_precariety %>% # table summary
            filter(!is.na(FTPTWK) & !NSECMJ10_2 == "Never worked, unemployed, and nec") %>% # # omit no answers and no apply
            group_by(QUARTER, SEX, ETHUKEUL_2, NSECMJ10_2, FTPTWK) %>%
            summarise(count = sum(PWT18)) %>%
            group_by(QUARTER, SEX, ETHUKEUL_2, NSECMJ10_2) %>%
            # percentage part-time per quarter
            mutate(percentage = count/sum(count)) %>%
            filter(FTPTWK == 2 & !is.na(ETHUKEUL_2)) %>%
            ggplot(aes(x = QUARTER, y = percentage, colour = SEX)) + # line graph
            geom_vline(xintercept = 4, color = "#c9c9c9") + # gridline between 2019/2020
            geom_vline(xintercept = 15, color = "#c9c9c9") + # gridline between 2020/2021
            geom_line(size = 1, show.legend = FALSE) +
            geom_point(shape = 19) +
            geom_vline(xintercept = 7, linetype="dotted") +
            facet_wrap(ETHUKEUL_2~NSECMJ10_2, ncol = 3 ) +
            
            # scale and colour
            scale_colour_manual("", values = c("#DF9216", "#791F83")) +
            scale_y_continuous(breaks = seq(0,0.7, 0.1), limits = c(0, 0.75), labels =
                                   scales::percent_format(accuracy = 1)) +
            scale_x_continuous(breaks=c(1:18),
                               limits = c(input$quarter.nsw[1], input$quarter.nsw[2]),
                                   labels=c("Jan-\nMar","","Jul-\nSep","",
                                            "Jan-\nMar", "", "Mar-\nMay", "", "May-\nJul",
                                            "", "Jul-\nSep","", "Sep-\nNov", "",
                                            "Nov-\nJan", "", "Jan-\nMar", "")) +
            # annotation
            labs(x = "",
                 y = "",
                 title ="Part-time work during the COVID-19 pandemic",
                 subtitle  = "Percentage of workers in a part-time job",
                 caption = c("Source: UK Labour Force Survey (Person)")) +
            annotate(geom = "text", x = 2, y = 0.74, label = "2019", size = 3.5) +
            annotate(geom = "text", x = 9.5, y = 0.74, label = "2020", size = 3.5) +
            annotate(geom = "text", x = 17, y = 0.74, label = "2021", size = 3.5) +
            
            # theme
            theme_economist_white(gray_bg = FALSE) +
            theme(plot.margin = unit(c(1,3,1,1), units = "lines"),
                  plot.title = element_text(vjust = 3),
                  axis.text.x =  element_text(size = 9),
                  text = element_text(size = 12),
                  plot.caption = element_text(size = 10, hjust = 1))
        g
        
        },
        height = 800,
        width = 900)
        ## Temporary work
        output$why_permanent <- renderPlot({
        k <- d_precariety %>% # table summary
            # only temp
            filter(JOBTYP == 2) %>% # only those in par-time work
            group_by(QUARTER, SEX, WHYTMP6) %>% 
            summarise(count = sum(PWT18)) %>% 
            group_by(QUARTER, SEX) %>% 
            # percentage reason
            mutate(percentage = count/sum(count)) %>% 
            filter(WHYTMP6 %in% c("3", "4")) %>% 
            ggplot(aes(x = QUARTER, y = percentage, colour = as_label(SEX))) + # line graph
            geom_vline(xintercept = 4, color = "#c9c9c9") + # gridline between 2019/2020
            geom_vline(xintercept = 15, color = "#c9c9c9") + # gridline between 2020/2021
            geom_line(size = 1, show.legend = FALSE) +
            geom_point(shape = 19) +
            geom_vline(xintercept = 7, linetype="dotted") +
            facet_wrap(.~as_label(WHYTMP6)) + 
            
            # scale and colour
            scale_colour_manual("", values = c("#DF9216", "#791F83")) +
            scale_y_continuous(breaks = seq(0,0.4, 0.05), limits = c(0, 0.45), labels =
                                   scales::percent_format(accuracy = 1)) +
            scale_x_continuous(breaks=c(1:18),
                               limits = c(input$quarter.tw[1], input$quarter.tw[2]),
                               labels=c("Jan-\nMar","","Jul-\nSep","",
                                        "Jan-\nMar", "", "Mar-\nMay", "", "May-\nJul",
                                        "", "Jul-\nSep","", "Sep-\nNov", "",
                                        "Nov-\nJan", "", "Jan-\nMar", ""))  +
            # annotation
            labs(x = "", 
                 y = "",
                 title ="Reasons for having temporary work",
                 subtitle  = "Percentage of the respondents with temporary work",
                 caption = c("Source: UK Labour Force Survey (Person)")) +
            annotate(geom = "text", x = 2, y = 0.42, label = "2019", size = 3.5) +
            annotate(geom = "text", x = 9.5, y = 0.42, label = "2020", size = 3.5) +
            annotate(geom = "text", x = 17, y = 0.42, label = "2021", size = 3.5) +
            
            # theme
            theme_economist_white(gray_bg = FALSE) +
            theme(plot.margin = unit(c(1,3,1,1), units = "lines"),
                  plot.title = element_text(vjust = 3),
                  text = element_text(size = 12),
                  plot.caption = element_text(size = 10, hjust = 1))
        k},
        width = 900)
        ## flextime
        output$flex <- renderPlot({

        l <- d_precariety %>% # table summary 
                filter(QUARTER %in% c(2, 4, 6, 7, 8, 9, 10, 12, 13, 14, 15, 16, 17, 18)) %>% 
                group_by(QUARTER, FLED10, SEX) %>% 
                summarise(count = sum(PWT18)) %>% 
                group_by(QUARTER, SEX) %>% 
                # percentage reason
                mutate(percentage = count/sum(count)) %>% 
                filter(!is.na(FLED10) & FLED10 %in% c("Flexitime", "Annualised hours", 
                                                      "Zero hours contract", "On-Call Working")) %>%  # omit NAs and include only certain categories
            ggplot(aes(x = QUARTER, y = percentage, colour = FLED10)) + # line graph
                geom_vline(xintercept = 4, color = "#c9c9c9") + # gridline between 2019/2020
                geom_vline(xintercept = 15, color = "#c9c9c9") + # gridline between 2020/2021
                geom_line(size = 1, show.legend = FALSE) +
                geom_point(shape = 19) +
                geom_vline(xintercept = 7, linetype = "dotted") +
                facet_wrap(.~SEX) +
                
                # scale and colour
                scale_color_manual("", values = c("#FC8DB7","#006295","#A3BE41","#791F83"))+
                scale_y_continuous(breaks = seq(0,0.2, 0.02), limits = c(0, 0.22), labels =
                                       scales::percent_format(accuracy = 1)) +
                scale_x_continuous(breaks=c(1:18),
                                   limits = c(input$quarter.aw[1], input$quarter.aw[2]),
                               labels=c("Jan-\nMar","","Jul-\nSep","",
                                        "Jan-\nMar", "", "Mar-\nMay", "", "May-\nJul",
                                        "", "Jul-\nSep","", "Sep-\nNov", "",
                                        "Nov-\nJan", "", "Jan-\nMar", "")) +
                # annotation
                labs(x = "", 
                     y = "",
                     title = "Atypical work arrangements during the COVID-19 pandemic",
                     subtitle  = "Percentage of workers in an atypical work arrangement ",
                     caption = c("Source: UK Labour Force Survey (Person)")) +
                annotate(geom = "text", x = 2, y = 0.21, label = "2019", size = 3.5) +
                annotate(geom = "text", x = 9.5, y = 0.21, label = "2020", size = 3.5) +
                annotate(geom = "text", x = 17, y = 0.21, label = "2021", size = 3.5) +
                
                # theme
                theme_economist_white(gray_bg = FALSE) +
                theme(plot.margin = unit(c(1,3,1,1), units = "lines"),
                      plot.title = element_text(vjust = 3),
                      text = element_text(size = 12),
                      plot.caption = element_text(size = 10, hjust = 1))   
        
        if(input$characteristic.flex == "Ethnicity")
        l <- d_precariety %>% # table summary 
            filter(QUARTER %in% c(2, 4, 6, 7, 8, 9, 10, 12, 13, 14, 15, 16, 17, 18)) %>% 
            group_by(QUARTER, FLED10, ETHUKEUL_2) %>% 
            summarise(count = sum(PWT18)) %>% 
            group_by(QUARTER, ETHUKEUL_2) %>% 
            # percentage reason
            mutate(percentage = count/sum(count)) %>% 
            filter(!is.na(FLED10) & FLED10 %in% c("Flexitime", "Annualised hours", "Zero hours contract", "On-Call Working")) %>%  # omit NAs and include only certain categories
            filter(!is.na(ETHUKEUL_2)) %>% 
            ggplot(aes(x = QUARTER, y = percentage, colour = FLED10)) + # line graph
            geom_vline(xintercept = 4, color = "#c9c9c9") + # gridline between 2019/2020
            geom_vline(xintercept = 15, color = "#c9c9c9") + # gridline between 2020/2021
            geom_line(size = 1, show.legend = FALSE) +
            geom_point(shape = 19) +
            geom_vline(xintercept = 7, linetype = "dotted") +
            facet_wrap(.~ETHUKEUL_2) +
            
            # scale and colour
            scale_color_manual("", values = c("#FC8DB7","#006295","#A3BE41","#791F83")) +
            scale_y_continuous(breaks = seq(0,0.2, 0.02), limits = c(0, 0.22), labels =
                                   scales::percent_format(accuracy = 1)) +
            scale_x_continuous(breaks=c(1:18),
                               limits = c(input$quarter.aw[1], input$quarter.aw[2]),
                               labels=c("Jan-\nMar","","Jul-\nSep","",
                                        "Jan-\nMar", "", "Mar-\nMay", "", "May-\nJul",
                                        "", "Jul-\nSep","", "Sep-\nNov", "",
                                        "Nov-\nJan", "", "Jan-\nMar", "")) +
            # annotation
            labs(x = "", 
                 y = "",
                 title = "Atypical work arrangements during the COVID-19 pandemic",
                 subtitle  = "Percentage of workers in an atypical work arrangement",
                 caption = c("Source: UK Labour Force Survey (Person)")) +
            annotate(geom = "text", x = 2, y = 0.21, label = "2019", size = 3.5) +
            annotate(geom = "text", x = 9.5, y = 0.21, label = "2020", size = 3.5) +
            annotate(geom = "text", x = 17, y = 0.21, label = "2021", size = 3.5) +
            
            # theme
            theme_economist_white(gray_bg = FALSE) +
            theme(plot.margin = unit(c(1,3,1,1), units = "lines"),
                 plot.title = element_text(vjust = 3),
                 text = element_text(size = 12),
                  plot.caption = element_text(size = 10, hjust = 1))
        l
        },
        width = 900)
        # home-working
        output$linegraph <- renderPlot({homwrk %>% 
                filter(Characteristic == input$characteristic) %>% 
                ggplot(aes(x = date, y = percentage, group = Category, 
                           ymin = LCL, ymax = UCL, fill = Category, color = Category)) +
                geom_line(size = 1, show.legend = FALSE) +
                geom_point() +
                scale_color_manual("", values = c(sexcolour, agecolor)) + 
                scale_fill_manual("", values = c(sexcolour, agecolor)) +
                # scale and colour
                scale_y_continuous("%") +
                scale_x_date("", limits = c(input$daterange[1], input$daterange[2])) +
                # theme
                theme_economist_white(gray_bg = FALSE) +
                theme(plot.margin = unit(c(1,3,1,1), units = "lines"),
                      plot.title = element_text(vjust = 3),
                      plot.caption = element_text(size = 10, hjust = 1)) +
                # annotation 
                labs(x = NULL, y = NULL,
                     title = "Homeworking during the COVID-19 pandemic",
                     subtitle = "Percentage of UK working population working from home*",
                     caption = "Source: Office for National Statistics - Opinions and Lifestyle survey/n
                            *A person is said to be working if last week: they had a paid job, either as an employee or self-employed;\nor they did any casual work for payment; or they did any unpaid or voluntary work.") +
                if (input$confidence) 
                    geom_ribbon(alpha = 0.5) 
            
            
        },
        width = 900)
        output$barplot <- renderPlot({why_homwrk %>% 
                filter(Characteristic == input$characteristic) %>%  
                filter(date >= input$daterange[1] & date <= input$daterange[2])  %>% 
                group_by(Characteristic, Category, Option) %>% 
                summarise(mean = mean(percentage, na.rm = TRUE)) %>% 
                drop_na() %>% 
                # reorder bar plot
                mutate(Option = reorder(Option, mean))  %>% 
                ggplot(aes(x = Option, y = mean, group = Category, fill = Category)) +
                geom_bar(stat = "identity") +
                scale_color_manual("", values = c(sexcolour, agecolor)) + 
                scale_fill_manual("", values = c(sexcolour, agecolor)) +
                # scale and color
                coord_flip() +
                scale_y_continuous("%") +
                scale_x_discrete("") + 
                #theme
                theme_economist_white(gray_bg = FALSE) + 
                theme(legend.position = "none",
                      plot.title = element_text(vjust = 3),
                      axis.text.y = element_text(hjust = 1),
                      panel.grid.major.y = element_blank(),
                      panel.grid.major.x =  element_line(colour = "grey40"),
                      axis.line.x = element_blank(),
                      axis.ticks.x = element_blank(),
                      text = element_text(size = 12),
                      plot.caption = element_text(size = 10, hjust = 1)) +
                # annotation 
                labs(x = NULL, y = NULL,
                     title = "Reasons for working from home",
                     subtitle = "Percentage of UK working population working from home*",
                     caption = "Source: Office for National Statistics - Opinions and Lifestyle survey/n
                     *Respondents were able to choose more than one option") +
                facet_wrap(~Category, ncol = 3)
        },
        height = 500,
        width = 900)
        # Mental health
        output$dotplot <-  renderPlot({wellbeing %>% 
                filter(Characteristic == input$characteristic.mh) %>% 
                group_by(Option, Period, Category) %>% 
                summarise(percentage = mean(percentage, na.rm = TRUE))%>% 
                drop_na() %>% 
                ggplot(., aes(x = percentage, y = Period, color = Category)) +
                geom_line(size = 1.5, colour = "#999999") +
                geom_point(size = 2.5) +
                scale_colour_manual(name = "",
                                    values = c(sexcolour, agecolor, "Total" = "Black")) +
                scale_y_discrete("") +
                facet_wrap(~Option, ncol = 1) +
                # annotation
                labs(x = NULL, y = NULL,
                     title ="Mental health during the pandemic",
                     subtitle = "Percentage of the respondents*",
                     caption = "Source: Office for National Statistics - Opinions and Lifestyle survey\n
                     *Respondents were able to choose more than one option") +
                #theme
                theme_economist_white(gray_bg = FALSE) +
                theme(legend.position = c("top"),
                      plot.margin = unit(c(1,3,1,1), units = "lines"),
                      plot.title = element_text(vjust = 3),
                      text = element_text(size = 12),
                      plot.caption = element_text(size = 10, hjust = 1))
        },
        height = 900,
        width = 900)
        # Social-distancing at work
        output$barplot2 <- renderPlot({
            contact_work  %>% 
                filter(Question == "Do you do paid work requiring direct physical contact with others?") %>% 
                filter(!Characteristic == "Total") %>% 
                filter(!Option == "Don't know") %>% 
                filter(Category %in% c("Men", "Women")) %>% 
                filter(Option == "Yes") %>% 
                droplevels() %>% 
                group_by(Question, Option, Category) %>% 
                summarise(percentage = mean(percentage, na.rm = TRUE)) %>% 
                ggplot(aes(x = Category, y = percentage, fill = Category, colour = Category)) +
                geom_bar(stat = "identity") +
                # scale and color
                scale_color_manual("", values = c(sexcolour)) + 
                scale_fill_manual("", values = c(sexcolour)) +
                coord_flip() +
                scale_y_continuous("%") +
                scale_x_discrete("") + 
                #theme
                theme_economist_white(gray_bg = FALSE) + 
                theme(legend.position = "none",
                      plot.title = element_text(vjust = 3),
                      axis.text.y = element_text(hjust = 1),
                      panel.grid.major.y = element_blank(),
                      panel.grid.major.x =  element_line(colour = "grey40"),
                      axis.line.x = element_blank(),
                      axis.ticks.x = element_blank(),
                      text = element_text(size = 12),
                      plot.caption = element_text(size = 10, hjust = 1)) +
                # annotation 
                labs(x = NULL, y = NULL,
                     title = "Paid work requiring direct physical contact with others",
                     subtitle = "Percentage of UK working population",
                     caption = "Source: Office for National Statistics - Opinions and Lifestyle survey")
        },
        height = 200,
        width = 900)
        output$dotplot2 <- renderPlot({
            contact_work  %>% 
                filter(!Question == "Do you do paid work requiring direct physical contact with others?") %>% 
                filter(!Characteristic == "Total") %>% 
                filter(!Option == "Don't know") %>% 
                filter(Category %in% c("Men", "Women")) %>% 
                droplevels() %>% 
                group_by(Question, Option, Category) %>% 
                summarise(percentage = mean(percentage, na.rm = TRUE)) %>% 
                ggplot(., aes(x = percentage, y = Option, color = Category)) +
                geom_line(size = 1.5, colour = "#999999") +
                geom_point(size = 2.5) +
                scale_colour_manual(name = "",
                                    values = c(sexcolour, "All persons total" = "Black")) +
                scale_y_discrete("") +
                facet_wrap(~Question,ncol = 1, scales = "free_y",
                           strip.position = "top") +
                # annotation
                labs(x = NULL, y = NULL,
                     title ="Social-distancing at work",
                     subtitle = "Percentage of people who had to travel and from work",
                     caption = c("Source: Office for National Statistics - Opinions and Lifestyle survey")) +
                #theme
                theme_economist_white(gray_bg = FALSE) +
                theme(legend.position = c("top"),
                      plot.margin = unit(c(1,3,1,1),units = "lines"),
                      plot.title = element_text(vjust = 3),
                      plot.caption = element_text(size = 10, hjust = 1),
                      strip.placement = "outside") 
        },
        height = 400,
        width = 900
        )
}

# Run the application 
shinyApp(ui = ui, server = server)
