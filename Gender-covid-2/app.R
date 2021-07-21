# libraries
library(shiny)
library(tidyverse)
library(ggplot2)
library(ggthemes)

# House keeping
sexlabel <- c("Men", "Women")
sexcolour <- c("Men" = "#DF9216", "Women" = "#791F83")
agecolor <-  c("16-29" = "#FC8DB7", "30-49"= "#006295", "50-69" = "#A3BE41", "70+" = "#791F83")
likertcolor <-  c("(1) Strongly agree"  = "#FC8DB7", "(2) Somewhat agree" = "#791F83", 
                  "(3) Neither agree nor disagree" = "#006295",
                  "(4) Somewhat disagree" = "#DF9216", "(5) Strongly disagree" = "#A3BE41")

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
    filter(!Option == "Other" &
               !Option == "Unable to exercise as normal" ) %>% 
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

# Define UI ----
ui <-
navbarPage("ONS-covid",
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
                    ),
           ),
           tabPanel("Mental health",
                    selectInput(inputId = "characteristic.mh", label = "Characteristic",
                                choices = list("Sex", "Age")),
                    mainPanel(
                        plotOutput("dotplot")
                    )
                    
           ),
           tabPanel("Social-distancing at work",
                    # selectInput(inputID = "characteristic.sd", label = "Characteristic",
                    #              choices = list("Sex", "Total")),
                    mainPanel(
                        plotOutput("dotplot2")
                    )
           ),
           
           navbarMenu("More",
                      tabPanel("About the project",
                               verbatimTextOutput(outputId = "about", placeholder = TRUE)
                      ),
                      tabPanel("Contact",
                               verbatimTextOutput(outputId = "contact", placeholder = TRUE)
                      )
           )
)

# Define server ----
server <- function(input, output) {
        # home-working
        output$linegraph <- renderPlot({homwrk %>% 
                filter(Characteristic == input$characteristic) %>% 
                ggplot(aes(x = date, y = percentage, group = Category, 
                           ymin = LCL, ymax = UCL, fill = Category, color = Category)) +
                geom_line(size = 1, show.legend = FALSE) +
                geom_point() +
                scale_color_manual("", values = c(sexcolour,agecolor)) + 
                scale_fill_manual("", values = c(sexcolour, agecolor)) +
                # scale and colour
                scale_y_continuous("%") +
                scale_x_date("", limits = c(input$daterange[1], input$daterange[2])) +
                # theme
                theme_economist_white(gray_bg = FALSE) +
                theme(plot.margin = unit(c(1,3,1,1), units = "lines"),
                      plot.title = element_text(vjust = 3)) +
                # annotation 
                labs(x = NULL, y = NULL,
                     title = "Homeworking during the COVID-19 pandemic",
                     subtitle = "Percentage of people working from home",
                     caption = "Source: Office for National Statistics - Opinions and Lifestyle survey") +
                if (input$confidence) 
                    geom_ribbon(alpha = 0.5) 
            
            
        })
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
                      axis.ticks.x = element_blank()) +
                # annotation 
                labs(x = NULL, y = NULL,
                     title = "Reasons for working from home",
                     caption = "Source: Office for National Statistics - Opinions and Lifestyle survey") +
                facet_wrap(~Category, ncol = 3)
        })
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
                     caption = c("Source: Office for National Statistics - Opinions and Lifestyle survey")) +
                #theme
                theme_economist_white(gray_bg = FALSE) +
                theme(legend.position = c("top"),
                      plot.margin = unit(c(1,3,1,1), units = "lines"),
                      plot.title = element_text(vjust = 3))
        },
        height = 800)
        
        # Social-distancing at work
        output$dotplot2 <- renderPlot({contact_work  %>% 
                filter(!Characteristic == "Total") %>% 
                # filter(Characteristic == input$characteristic.sd) %>% 
                group_by(Question, Option, Category) %>% 
                summarise(percentage = mean(percentage, na.rm = TRUE)) %>% 
                ggplot(., aes(x = percentage, y = Option, color = Category)) +
                geom_line(size = 1.5, colour = "#999999") +
                geom_point() +
                scale_colour_manual(name = "",
                                    values = c(sexcolour, "All persons total" = "Black")) +
                scale_y_discrete("") +
                facet_wrap(~Question, ncol = 1, scales = "free_y") +
                # annotation
                labs(x = NULL, y = NULL,
                     title ="Social-distancing at work",
                     caption = c("Source: Office for National Statistics - Opinions and Lifestyle survey")) +
                #theme
                theme_economist_white(gray_bg = FALSE) +
                theme(legend.position = c("top"),
                      plot.margin = unit(c(1,3,1,1),units = "lines"),
                      plot.title = element_text(vjust = 3)) 
        })
        # More tab
        output$about <- renderText({ input$txt })
        output$contact <- renderText({ input$txt })
}

# Run the application 
shinyApp(ui = ui, server = server)
