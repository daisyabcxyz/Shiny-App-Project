
library(tidyverse)
library(DT)
library(markdown)
library(shinydashboard)
linkedIn <- read_csv("LinkedIn_RDB_three.csv")

tidy_location <- linkedIn |> 
  mutate(job_location = str_extract(linkedIn$job_location, "[A-Z]{2}")) |>
  mutate(compensation = str_remove_all(linkedIn$compensation, "[$,]")) |>
  mutate(compensation = as.numeric(compensation))|>
  filter(!is.na(job_location)) |>
  group_by(job_location)
#|>
#summarize(n = n())



ui <- dashboardPage(
  dashboardHeader(title = "Job Market Summary"),
  dashboardSidebar(sidebarMenu(
    menuItem("Geographic Information", tabName = "geo_info"),
    menuItem("Job Domain", tabName = "job_domain") #,
    #  menuItem("More Information", startExpanded = FALSE,
    #           menuSubItem("Work Type", tabName ="work_type"),
    #           menuSubItem("Remote Allowed", tabName = "remote_allowed"))
  )),
  
  dashboardBody(
    tabItems(
      tabItem(tabName = "geo_info",
              fluidRow(
                box(
                  #selectInput("location","Which location are you looking at?", linkedIn$job_location)
                  selectInput("location","Which location are you looking at?", tidy_location$job_location)
                ),
                box(plotOutput("plot1", height = 250))
              )),
      tabItem(tabName = "job_domain",
              fluidRow(
                box(
                  varSelectInput(inputId = "variable1",
                                 label = "Select x-axis variable",
                                 data = linkedIn |> select(job_domain, level)),
                  #   varSelectInput(inputId = "variable2",
                  #                  label = "Select y-axis variable",
                  #                  data = linkedIn |> select(compensation, work_type)),
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
      filter(job_location == input$location) |>
      filter(!is.na(level)) |>
      #  filter(!is.na(compensation)) |>
      group_by(level, job_location) |>
      count() |>
      ggplot()+
      geom_col(aes(x= level, y = n), fill = "blue")+
      labs(x = "Level of Role", y = "Number of Openings", title = "Number of Openings for each level of entry")
    
    
    
  })
  
  output$plot2 <- renderPlot({
    tidy_location |> 
      filter(job_location == input$location) |>
      group_by(!!input$variable1) |> 
      summarise(compensation = mean(compensation), .groups = "drop") |>
      ggplot()+
      geom_col(aes(x= !!input$variable1, y = compensation), fill = "red") + 
      scale_y_continuous(labels = scales::label_dollar(scale = 1e-3, suffix = "K")) +
      labs(x = "Job Domain/ Job Level", y = "Average Compensation in Thousand Dollars")+
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 11),
        plot.title = element_text(face = "bold", size = 14)
      )
    
  })
}

shinyApp(ui = ui, server = server)