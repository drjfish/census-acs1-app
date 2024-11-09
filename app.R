# Load required libraries
library(shiny)
library(ggplot2)
library(dplyr)
library(plotly)
library(webshot2)
library(htmlwidgets)

# Load the dataset
acs_data <- read.csv("acs_data.csv")

# Determine the latest year (e.g., 2023) as the default value
default_year <- max(acs_data$year, na.rm = TRUE)

# Define the UI for the app
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      .sidebar-panel .shiny-input-container {
        width: 100% !important;
      }
      .sidebar-panel .btn {
        width: 100% !important;
        font-size: 0.85em;
      }
    "))
  ),
  titlePanel("State Population Estimates"),
  
  # Define the tabset for National and State views
  tabsetPanel(
    tabPanel("National",
      sidebarLayout(
        sidebarPanel(
          class = "sidebar-panel",
          selectInput("selected_year", 
                      "Select Year:", 
                      choices = unique(acs_data$year),
                      selected = default_year),
          br(), br(),
          downloadButton("download_national", "Grab Data"),
          br(), # Line break for spacing
          actionButton("screenshot_national", "Screenshot Plot"),
          width = 2
        ),
        mainPanel(
          plotlyOutput("populationPlot")
        )
      )
    ),
    tabPanel("State",
      sidebarLayout(
        sidebarPanel(
          class = "sidebar-panel",
          selectInput("selected_state", 
                      "Select State:", 
                      choices = unique(acs_data$state)),
          selectInput("selected_year_state",
                      "Select Year:", 
                      choices = unique(acs_data$year),
                      selected = default_year),
          br(), br(),
          downloadButton("download_state", "Grab Data"),
          br(), # Line break for spacing
          actionButton("screenshot_state", "Screenshot Plot"),
          width = 2
        ),
        mainPanel(
          plotlyOutput("statePlot")
        )
      )
    )
  )
)

# Define server logic for the app
server <- function(input, output, session) {
  
  # Generate Plotly object for the National tab
  national_plot <- reactive({
    filtered_data <- acs_data %>%
      filter(year == input$selected_year, age == "total") %>%
      arrange(desc(estimate))
    
    ggplot(filtered_data, aes(x = reorder(state, -estimate), y = estimate, 
                              text = paste("State:", state, "<br>Estimate:", scales::comma(estimate)))) +
      geom_bar(stat = "identity", fill = "steelblue", color = "black") +
      scale_y_continuous(labels = scales::label_number(scale = 1e-6, suffix = "M")) + # Format y-axis in millions
      labs(x = "State", y = "Population Estimate", 
           title = paste("Population Estimates for", input$selected_year)) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  # Render the National Plotly plot
  output$populationPlot <- renderPlotly({
    ggplotly(national_plot(), tooltip = "text") %>%
      layout(hoverlabel = list(bgcolor = "white", bordercolor = "black"))
  })
  
  # Generate Plotly object for the State tab
  state_plot <- reactive({
    filtered_data <- acs_data %>%
      filter(state == input$selected_state, year == input$selected_year_state, age != "total") %>%
      arrange(age)
    
    ggplot(filtered_data, aes(x = age, y = estimate, fill = sex, 
                              text = paste("Age Range:", age, "<br>Sex:", sex, "<br>Estimate:", scales::comma(estimate)))) +
      geom_bar(stat = "identity", position = "stack", color = "black") + # Use stacked bars
      scale_fill_manual(values = c("M" = "blue", "F" = "orange")) + # Set colors for male and female
      scale_y_continuous(labels = scales::label_number(scale = 1e-6, suffix = "M")) + # Format y-axis in millions
      labs(x = "Age Range", y = "Population Estimate", 
           title = paste("Population Estimates by Age and Sex for", input$selected_state, "in", input$selected_year_state)) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  # Render the State Plotly plot
  output$statePlot <- renderPlotly({
    ggplotly(state_plot(), tooltip = "text") %>%
      layout(hoverlabel = list(bgcolor = "white", bordercolor = "black"))
  })
  
  # Download handler for National data
  output$download_national <- downloadHandler(
    filename = function() {
      paste("National_Population_Data_", input$selected_year, ".csv", sep = "")
    },
    content = function(file) {
      filtered_data <- acs_data %>%
        filter(year == input$selected_year, age == "total") %>%
        arrange(desc(estimate))
      write.csv(filtered_data, file, row.names = FALSE)
    }
  )
  
  # Download handler for State data
  output$download_state <- downloadHandler(
    filename = function() {
      paste("State_Population_Data_", input$selected_state, "_", input$selected_year_state, ".csv", sep = "")
    },
    content = function(file) {
      filtered_data <- acs_data %>%
        filter(state == input$selected_state, year == input$selected_year_state, age != "total") %>%
        arrange(age)
      write.csv(filtered_data, file, row.names = FALSE)
    }
  )
  
  # Screenshot handler for National tab using webshot2
  observeEvent(input$screenshot_national, {
    # Save the national plot as an HTML widget
    widget <- saveWidget(as_widget(ggplotly(national_plot())), "national_plot.html", selfcontained = TRUE)
    webshot2::webshot("national_plot.html", file = paste("National_Population_Plot_", input$selected_year, ".png", sep = ""))
  })
  
  # Screenshot handler for State tab using webshot2
  observeEvent(input$screenshot_state, {
    # Save the state plot as an HTML widget
    widget <- saveWidget(as_widget(ggplotly(state_plot())), "state_plot.html", selfcontained = TRUE)
    webshot2::webshot("state_plot.html", file = paste("State_Population_Plot_", input$selected_state, "_", input$selected_year_state, ".png", sep = ""))
  })
}

# Run the app
shinyApp(ui = ui, server = server)