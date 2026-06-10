# load the necessary libraries
library(tidyverse)
library(DT)
library(markdown)
library(shinydashboard)
linkedIn <- read_csv("LinkedIn_RDB_three.csv")

# tidy the dataset by extracting the state abbreviation from job_location
# and make compensation a numerical data type
tidy_location <- linkedIn |> 
  mutate(job_location = str_extract(linkedIn$job_location, "[A-Z]{2}")) |>
  mutate(compensation = str_remove_all(linkedIn$compensation, "[$,]")) |>
  mutate(compensation = as.numeric(compensation))|>
  filter(!is.na(job_location)) |> # filter out rows that have missing entries for job_location
  group_by(job_location) 



ui <- dashboardPage(
  dashboardHeader(title = "Job Market Summary"),
  dashboardSidebar(sidebarMenu(
    # create 2 tabs: Geographic Information and Job Domain
    menuItem("Geographic Information", tabName = "geo_info"),
    menuItem("Job Domain", tabName = "job_domain") #,
  
  )),
  
  dashboardBody(
    tabItems(
      tabItem(tabName = "geo_info",
              fluidRow(
                box(
                  # have a dropdown menu for the user to choose what state to look at
                  selectInput("location","Which location are you looking at?", tidy_location$job_location)
                ),
                box(plotOutput("plot1", height = 250))
              )),
      tabItem(tabName = "job_domain",
              fluidRow(
                box(
                  # allow the user to choose x-variable: either job_domain or level
                  varSelectInput(inputId = "variable1",
                                 label = "Select x-axis variable",
                                 data = linkedIn |> select(job_domain, level)),
                  selectInput("location","Which location are you looking at?", tidy_location$job_location)
                  
                  
                )
                
              ), 
              fluidRow(
                box(
                  plotOutput("plot2", height = 600)
                )
                
              )
      )
      
    )
    
  )
  
  
)

server <- function(input,output) {
  set.seed(122)
  hisdata <- rnorm(500)
  
  output$plot1 <- renderPlot({
    tidy_location |> 
      # show the plot of number of openings for each level in the state the user specifies
      filter(job_location == input$location) |> 
      filter(!is.na(level)) |>
      group_by(level, job_location) |>
      count() |> # keep track of the number of openings for each level in each state
      ggplot()+
      geom_col(aes(x= level, y = n), fill = "blue")+
      labs(x = "Level of Role", y = "Number of Openings", title = "Number of Openings for each level of entry")
    
    
    
  })
  
  output$plot2 <- renderPlot({
    tidy_location |> 
      # allow the user to choose the state 
      filter(job_location == input$location) |>
      group_by(!!input$variable1) |> # group by the user's chosen x-variable
      # if the user chooses job_domain as x-variable, show the average compensation for each domain
      # if the user chooses level as x-variable, show the average compensation for each level
      summarise(compensation = mean(compensation), .groups = "drop") |>
      ggplot()+
      # show a column plot of the average compensation across job domains or levels in the specified state
      geom_col(aes(x= !!input$variable1, y = compensation), fill = "red") + 
      # scale the y values to display thousands of dollars instead of dollars
      scale_y_continuous(labels = scales::label_dollar(scale = 1e-3, suffix = "K")) + 
      labs(x = "Job Domain/ Job Level", y = "Average Compensation in Thousand Dollars")+
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 11), # make text more readable
        plot.title = element_text(face = "bold", size = 14)
      )
    
  })
}

shinyApp(ui = ui, server = server)