library(tidyverse)
library(readr)
library(stringr)

setwd("~/Documents/UCSD/FALL17/MGTA452 Collecting:Analyzing Large Data/datasets/h1b")
h1b_kaggle <- read_csv("data/h1b_kaggle.csv")

# Data pre-processing ----------------------------------------------------

# split worksite column to city/state
h1b <- separate(h1b_kaggle, WORKSITE, into = c("CITY","STATE"), sep = ",")
# h1b$CITY <- tolower(h1b$CITY) %>% str_trim()
#h1b$STATE <- toupper(h1b$STATE) %>% str_trim()
glimpse(h1b)

# Remove 4 small states beyond main US continent
h1b <- subset(h1b, ! STATE %in% c(" HAWAII", " ALASKA", " NA", " PUERTO RICO")) 
#table(h1b$STATE) # 50 states left


# Recode job positions
h1b$JOB_TITLE <- ifelse(grepl("SOFTWARE", h1b$JOB_TITLE), "SOFTWARE DEVELOPMENT",
                        ifelse(h1b$JOB_TITLE == "BUSINESS ANALYST", "BUSINESS ANALYST", 
                               ifelse(grepl("CONSULTANT", h1b$JOB_TITLE), "CONSULTANT",
                                      ifelse(h1b$JOB_TITLE == "TECHNOLOGY ANALYST - US" | h1b$JOB_TITLE == "PROGRAMMER ANALYST"|
                                               h1b$JOB_TITLE == "SYSTEMS ANALYST" | h1b$JOB_TITLE == "COMPUTER SYSTEMS ANALYST", 
                                             "DATABASE/DATA SCIENTIST/ANALYST",
                                             ifelse(grepl("PROFESSOR", h1b$JOB_TITLE) | h1b$JOB_TITLE == "RESEARCH ASSOCIATE", 
                                                    "PROFESSOR/RESEARCHER",
                                                    ifelse(h1b$JOB_TITLE == "ACCOUNTANT", "ACCOUNTANT", "OTHERS"))))))
# group by job position
#h1b %>% 
#  group_by(JOB_TITLE) %>% 
#  summarize(count = n()) %>% 
#  arrange(desc(count)) 

# Condense dataset by removing all NAs
h1b <- h1b  %>%
  select(EMPLOYER_NAME, lon, lat, CITY, STATE, JOB_TITLE, YEAR, PREVAILING_WAGE) %>% 
  group_by(EMPLOYER_NAME) %>% 
  na.omit(lon) %>% 
  na.omit(lat) %>% 
  na.omit(JOB_TITLE)

# check NAs
sum(is.na(h1b)) # 0 NA
sum(is.null(h1b))


# Shiny
library(shiny)
library(maps)
library(ggplot2)

# Name all state and job name for UI
states = c("ALABAMA", "ARIZONA", "ARKANSAS", "CALIFORNIA", "COLORADO", "CONNECTICUT" ,"DELAWARE", 
           "DISTRICT OF COLUMBIA", "FLORIDA", "GEORGIA",  "IDAHO", "ILLINOIS","INDIANA","IOWA","KANSAS", "KENTUCKY", 
           "LOUISIANA", "MAINE","MARYLAND", "MASSACHUSETTS","MICHIGAN","MINNESOTA","MISSISSIPPI", "MISSOURI", "MONTANA", 
           "NEBRASKA", "NEVADA", "NEW HAMPSHIRE", "NEW JERSEY", "NEW MEXICO", "NEW YORK", "NORTH CAROLINA", "NORTH DAKOTA",
           "OHIO", "OKLAHOMA", "OREGON", "PENNSYLVANIA", "RHODE ISLAND", "SOUTH CAROLINA", "SOUTH DAKOTA" , "TENNESSEE", 
           "TEXAS", "UTAH", "VERMONT", "VIRGINIA", "WASHINGTON", "WEST VIRGINIA", "WISCONSIN","WYOMING")

#tolower(states)
# states = as.character(unique(h1b_count$STATE)[order(unique(h1b_count$STATE))])
# remove <- c ("HAWAII", "ALASKA")
# states = states[! states %in% remove]
# jobs = as.character(unique(h1b_count$JOB_TITLE))

jobs = c("BUSINESS ANALYST", "CONSULTANT", "DATABASE/DATA SCIENTIST/ANALYST", "OTHERS", "PROFESSOR/RESEARCHER",
         "SOFTWARE DEVELOPMENT", "ACCOUNTANT" )

# UI
ui <- fluidPage(
  titlePanel("H1B Statistics"),
  fluidRow(
    column(4, "",  # position
           sliderInput(inputId = "year",
                       label = "Year Range", sep = '',
                       min = 2011, round = TRUE, ticks = FALSE,
                       max = 2016, dragRange = TRUE,
                       value = c(2011, 2016)
           ),
           selectInput(inputId = "state",
                       label = "State",
                       choices = c('ALL', states)
           ),
           selectInput(inputId = "job",
                       label = "Job Title",
                       choices = c('ALL', jobs)
           )
    ),
    column(6, "",
           plotOutput("map")
    )
  ),
  fluidRow(
    column(6, tableOutput("AppNum")),
    column(6, plotOutput("Trend"))
  ),
  fluidRow(
    "Acknowledgement: the dataset is based on Kaggle H-1B Visa Petitions from 2011 to 2016. The data contains New H1B petitions(before the lottery) + Extension Petitions + Positions exempt from H-1B visa cap ( PHD, Researchers)."
  )
)


# Server
server <- shinyServer(function(input, output){
  data_count<-reactive({
    if (input$state!='ALL' & input$job!='ALL'){
      d = subset(h1b, YEAR>=input$year[1] & YEAR<=input$year[2] & STATE == input$state & JOB_TITLE == input$job)
      d
    }
    else if(input$state=='ALL' & input$job!='ALL'){
      d = subset(h1b, YEAR>=input$year[1] & YEAR<=input$year[2] & JOB_TITLE == input$job)
      d
    }
    else if (input$state!='ALL' & input$job=='ALL'){
      d = subset(h1b, YEAR>=input$year[1] & YEAR<=input$year[2] & STATE == input$state)
      d
    }
    else{
      d = subset(h1b, YEAR>=input$year[1] & YEAR<=input$year[2])
      d
    }
  })
  data_map<-reactive({
    if (input$state!='ALL' & input$job!='ALL'){
      d = subset(h1b, YEAR >= input$year[1] & YEAR <= input$year[2] & STATE == input$state & JOB_TITLE == input$job)
      d
    }
    else if(input$state == 'ALL' & input$job != 'ALL'){
      d = subset(h1b, YEAR >= input$year[1] & YEAR <= input$year[2] & JOB_TITLE == input$job)
      d
    }
    else if(input$state!='ALL' & input$job=='ALL'){
      d = subset(h1b, YEAR >= input$year[1] & YEAR <= input$year[2] & STATE == input$state)
      d
    }
    else{
      d = subset(h1b, YEAR >= input$year[1] & YEAR <= input$year[2])
      d
    }
  })
  output$AppNum <- renderTable({
    s = data_count()%>%
      group_by(EMPLOYER_NAME) %>%
      dplyr::summarise(Apps = n())
    s = s[order(s$Apps, decreasing = TRUE), ]
    colnames(s) = c("Company", "No. of Applications")
    head(subset(s), 10)
  })
  
  output$Trend <- renderPlot({
    s = data_count()%>%
      group_by(YEAR) %>%
      dplyr::summarise(apps = n())
    barplot(height = s$apps, las = 1, main = 'No. of Applications by Year', col = "#FFCCFF", names.arg = s$YEAR)
  })
  
  output$map <- renderPlot({
    all_states <- map_data('state') # import US state map (49 states)
    toupper(all_states$region) # added
    #all_states <- subset(all_states, region %in% tolower(unique(data_map()$STATE))) ???
    locations = data_map() %>%
      group_by(lon, lat) %>%
      dplyr::summarise(TotalNumber = n(),
                       MedianSalary = mean(PREVAILING_WAGE))
    locations = subset(locations, lon > -124.4 & lon < -60 & lat > 25 & MedianSalary < 120000) # lon/lat to be removed
  

    p <- ggplot() + 
          labs(x = "", y = "") + 
          geom_polygon(data = all_states, aes(x = long, y = lat, group = group), col = "white", fill = "grey") +
          geom_point(aes(x = lon, y = lat, size = TotalNumber, col = MedianSalary), data = locations, show.legend = TRUE) +
          scale_colour_gradient(low = 'blue', high = 'yellow') 
    p
  })
})

shinyApp(ui = ui, server = server)
