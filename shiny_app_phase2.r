library(tidyverse)
library(DT)
library(markdown)
library(shinydashboard)
library(scales)
library(leaflet)
library(plotly) 

# Read data safely
linkedIn <- read_csv("LinkedIn_RDB_three.csv")

# Clean data and keep it reactive-ready
tidy_location <- linkedIn |> 
  mutate(job_location = str_extract(job_location, "[A-Z]{2}")) |>
  mutate(compensation = str_remove_all(compensation, "[$,]")) |>
  mutate(compensation = as.numeric(compensation)) |>
  filter(!is.na(job_location), !is.na(compensation))

# Aggregated map layer counts
state_counts <- tidy_location |> 
  group_by(job_location) |> 
  summarise(openings = n(), .groups = "drop")

states_map_sf <- sf::st_as_sf(maps::map("state", plot = FALSE, fill = TRUE)) |> 
  mutate(ID = str_to_title(ID)) |> 
  mutate(job_location = state.abb[match(ID, state.name)]) |> 
  filter(!is.na(job_location)) |> 
  left_join(state_counts, by = "job_location") |> 
  mutate(openings = replace_na(openings, 0))


# ----------------- UI DESIGN -----------------
ui <- dashboardPage(
  dashboardHeader(title = "Job Market Summary"),
  dashboardSidebar(sidebarMenu(
    menuItem("Geographic Information", tabName = "geo_info"),
    menuItem("Job Domain Sandbox", tabName = "job_domain")
  )),
  
  dashboardBody(
    tabItems(
      # Tab 1: Geographic Map Filter Space
      tabItem(tabName = "geo_info",
              fluidRow(
                box(
                  title = "Click a State on the Map to Filter This Tab",
                  leafletOutput("map_plot", height = 400), 
                  width = 7
                ),
                box(
                  title = textOutput("dynamic_plot1_title"),
                  plotOutput("plot1", height = 400),
                  width = 5
                )
              )),
      
      # Tab 2: Job Domain Comparison Sandbox
      tabItem(tabName = "job_domain",
              fluidRow(
                # Left Column Controls Panel
                box(
                  title = "Salary Filter Controls", status = "primary", solidHeader = TRUE,
                  varSelectInput(inputId = "variable1",
                                 label = "1. Break Down By Variable:",
                                 data = linkedIn |> select(job_domain, level)),
                  
                  # Slider to control minimum salary requirements
                  sliderInput("salary_range", "2. Filter by Compensation Range:",
                              min = 20000, max = 300000, value = c(40000, 200000), step = 10000, pre = "$"),
                  
                  hr(),
                  h4(strong("Compare Two States Side-by-Side:")),
                  selectInput("comp_state1", "State A:", choices = sort(unique(tidy_location$job_location)), selected = "CA"),
                  selectInput("comp_state2", "State B:", choices = sort(unique(tidy_location$job_location)), selected = "NY"),
                  width = 4
                ),
                
                # Right Column Interactive Output Chart
                box(
                  title = "Comparative Compensation Benchmark", status = "success", solidHeader = TRUE,
                  plotlyOutput("plot2_interactive", height = 500), 
                  width = 8
                )
              ))
    )
  )
)


# ----------------- SERVER LOGIC -----------------
server <- function(input, output, session) {
  set.seed(122)
  
  # --- TAB 1 CONTROL: MAP REACTIVE ---
  current_state <- reactive({
    click <- input$map_plot_shape_click
    if (is.null(click)) return("CA") else return(click$id)
  })
  
  output$dynamic_plot1_title <- renderText({
    paste("Job Breakdowns for:", current_state())
  })
  
  output$map_plot <- renderLeaflet({
    pal <- colorNumeric(palette = "Blues", domain = states_map_sf$openings)
    leaflet(states_map_sf) |> 
      addProviderTiles(providers$CartoDB.Positron) |> 
      setView(lng = -96, lat = 37.8, zoom = 4) |> 
      addPolygons(
        layerId = ~job_location, 
        fillColor = ~pal(openings), weight = 1, opacity = 1, color = "white", fillOpacity = 0.8,
        highlightOptions = highlightOptions(weight = 3, color = "#666", fillOpacity = 0.9, bringToFront = TRUE),
        label = ~paste0(ID, ": ", openings, " openings")
      ) |> 
      addLegend(pal = pal, values = ~openings, opacity = 0.7, title = "Openings", position = "bottomright")
  })
  
  output$plot1 <- renderPlot({
    tidy_location |> 
      filter(job_location == current_state()) |>
      filter(!is.na(level)) |>
      group_by(level) |>
      count() |>
      ggplot() +
      geom_col(aes(x = level, y = n), fill = "blue") +
      labs(x = "Level of Role", y = "Number of Openings") +
      theme_minimal() #+
    # theme(axis.text.x = element_text(angle = 30, hjust = 1))
  })
  
  
  # --- TAB 2 CONTROL: INDEPENDENT SANDBOX ---
  output$plot2_interactive <- renderPlotly({
    
    # 1. Filter raw data based on the two dropdown states AND slider settings
    sandbox_data <- tidy_location |> 
      filter(
        job_location %in% c(input$comp_state1, input$comp_state2),
        compensation >= input$salary_range[1],
        compensation <= input$salary_range[2],
        !is.na(!!input$variable1)
      ) |> 
      group_by(!!input$variable1, job_location) |> 
      summarise(
        avg_comp = mean(compensation), 
        job_count = n(),
        .groups = "drop"
      )
    
    # 2. Build the comparative double-bar plot
    p <- ggplot(sandbox_data, aes(x = !!input$variable1, y = avg_comp, fill = job_location,
                                  text = paste0("State: ", job_location, 
                                                "<br>Avg Pay: $", round(avg_comp/1000, 1), "K",
                                                "<br>Sample Count: ", job_count))) +
      geom_col(position = "dodge") + 
      scale_y_continuous(labels = scales::label_dollar(scale = 1e-3, suffix = "K")) +
      # FIXED: Swapped out the syntax error line for standard, safe brewer scaling
      scale_fill_brewer(palette = "Set2") + 
      labs(x = "Categorical Axis", y = "Average Compensation", fill = "State") +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 9),
        legend.position = "top"
      )
    
    # 3. Wrap standard ggplot inside plotly rendering engine to add hover-popups
    ggplotly(p, tooltip = "text")
  })
}

shinyApp(ui = ui, server = server)