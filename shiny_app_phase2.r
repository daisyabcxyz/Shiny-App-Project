# load the necessary libraries
library(tidyverse)
library(DT) # Interface for rendering datatables
library(markdown)
library(shinydashboard) # Framework for building structural admin-style dashboard dashboards
library(scales) # Formatting tools to scale axes labels
library(leaflet) # Engine used to generate the interactive geospatial US maps
library(plotly) # Transforms static plots into dynamic charts featuring interactive hover popups

# Read data safely
linkedIn <- read_csv("LinkedIn_RDB_three.csv")

# Clean data and keep it reactive-ready
# tidy the dataset by extracting the state abbreviation from job_location
# and make compensation a numerical data type
tidy_location <- linkedIn |> 
  mutate(job_location = str_extract(job_location, "[A-Z]{2}")) |>
  mutate(compensation = str_remove_all(compensation, "[$,]")) |>
  mutate(compensation = as.numeric(compensation)) |>
  filter(!is.na(job_location), !is.na(compensation)) # filter out rows that have missing entries for job_location and compensation

# Aggregated map layer counts
state_counts <- tidy_location |> 
  group_by(job_location) |> 
  summarise(openings = n(), .groups = "drop") # count total openings per state

# Fetch the structural US boundaries natively from the maps package and convert to a Spatial Features (SF) dataframe
states_map_sf <- sf::st_as_sf(maps::map("state", plot = FALSE, fill = TRUE)) |> 
  # Format internal region names to capital Title Case (e.g., "california" -> "California")
  mutate(ID = str_to_title(ID)) |> 
  # Cross-reference the full textual state names with R's built-in vector mappings to derive abbreviations
  mutate(job_location = state.abb[match(ID, state.name)]) |> 
  # Keep only matching records within the official index
  filter(!is.na(job_location)) |> 
  # Append our job metric aggregations calculated above directly into the geospatial shapes
  left_join(state_counts, by = "job_location") |> 
  # Switch states missing structural records from numerical NA into an integer zero
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
                  # the interactive map where users can hover over to see opening counts and click to choose state
                  title = "Click a State on the Map to Filter This Tab",
                  leafletOutput("map_plot", height = 400), 
                  width = 7
                ),
                box(
                  # plot of number of openings for each level in the state the user specifies
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
                  # allow the user to choose x-variable: either job_domain or level
                  varSelectInput(inputId = "variable1",
                                 label = "1. Break Down By Variable:",
                                 data = linkedIn |> select(job_domain, level)),
                  
                  # Slider to control minimum salary requirements
                  sliderInput("salary_range", "2. Filter by Compensation Range:",
                              min = 20000, max = 300000, value = c(40000, 200000), step = 10000, pre = "$"),
                  
                  hr(),
                  h4(strong("Compare Two States Side-by-Side:")),
                  # allow user to choose 2 states they want to compare
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
    # the input is the user's mouse click, no click is default to California
    click <- input$map_plot_shape_click
    if (is.null(click)) return("CA") else return(click$id)
  })
  
  # show dynamic plot's title: update to user's mouse click
  output$dynamic_plot1_title <- renderText({
    paste("Job Breakdowns for:", current_state())
  })
  
  output$map_plot <- renderLeaflet({
    pal <- colorNumeric(palette = "Blues", domain = states_map_sf$openings)
    leaflet(states_map_sf) |> 
      # Apply a modern canvas background styling base (Light gray style)
      addProviderTiles(providers$CartoDB.Positron) |> 
      # Center geographical coordinates roughly over the geographic center of mainland USA
      setView(lng = -96, lat = 37.8, zoom = 4) |> 
      addPolygons(
        layerId = ~job_location, # Assigns the 2-letter state identifier abbreviation to map shape clicks
        fillColor = ~pal(openings), weight = 1, opacity = 1, color = "white", fillOpacity = 1,
        # Interactive glow effect when the user hovers over a target polygon border
        highlightOptions = highlightOptions(weight = 3, color = "#666", fillOpacity = 1, bringToFront = TRUE),
        label = ~paste0(ID, ": ", openings, " openings")
      ) |> 
      addLegend(pal = pal, values = ~openings, opacity = 0.7, title = "Openings", position = "bottomright")
  })
  
  output$plot1 <- renderPlot({
    tidy_location |> 
      # the filter is based on user's mouse click on the map
      filter(job_location == current_state()) |>
      filter(!is.na(level)) |>
      group_by(level) |>
      count() |>  # keep track of the number of openings for each level in each state
      # show the plot of number of openings for each level in the state the user clicks on
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
        # limit the location to the 2 states that the user chooses in the dropdown
        job_location %in% c(input$comp_state1, input$comp_state2),
        # include only rows where compensations are within the selected range
        compensation >= input$salary_range[1],
        compensation <= input$salary_range[2],
        !is.na(!!input$variable1)
      ) |> 
      group_by(!!input$variable1, job_location) |> 
      summarise(
        avg_comp = mean(compensation), # show the average compensation across job domains or levels in the 2 specified state
        job_count = n(), # keep track of the job count for each column
        .groups = "drop"
      )
    
    # 2. Build the comparative double-bar plot
    p <- ggplot(sandbox_data, aes(x = !!input$variable1, y = avg_comp, fill = job_location,
                                  # When the user hovers over any column, show info on state, average compensation, and number of openings
                                  text = paste0("State: ", job_location, 
                                                "<br>Avg Pay: $", round(avg_comp/1000, 1), "K",
                                                "<br>Sample Count: ", job_count))) +
      geom_col(position = "dodge") + # show columns side by side 
      # scale the y values to display thousands of dollars instead of dollars
      scale_y_continuous(labels = scales::label_dollar(scale = 1e-3, suffix = "K")) +
      # FIXED: Swapped out the syntax error line for standard, safe brewer scaling
      scale_fill_brewer(palette = "Set2") + 
      labs(x = "Categorical Axis", y = "Average Compensation", fill = "State") +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 9), # make text more readable
        legend.position = "top"
      )
    
    # 3. Wrap standard ggplot inside plotly rendering engine to add hover-popups
    ggplotly(p, tooltip = "text")
  })
}

shinyApp(ui = ui, server = server)