#' Run Shiny App for CDR Job Results
#' @export
run_shiny_app <- function(output_path) {
  library(shiny)
  library(ggplot2)
  library(dplyr)

  # Dynamically fetch CSV datasets in the output directory, excluding global datasets
  all_datasets <- list.files(output_path, pattern = "\\.csv$", full.names = FALSE)
  regional_datasets <- all_datasets[!grepl("_global", all_datasets)] # Exclude global datasets explicitly

  # Check if there are any regional datasets available
  if (length(regional_datasets) == 0) {
    stop("No regional datasets found in the specified output path.")
  }

  # UI
  ui <- fluidPage(
    titlePanel("CDR Job Estimation Results Viewer"),
    sidebarLayout(
      sidebarPanel(
        selectInput("dataset", "Choose Dataset:", choices = regional_datasets),
        selectInput("metric", "Choose Metric:", choices = c("mean_Jobs", "min_Jobs", "max_Jobs")),
        selectInput("type", "Visualization Type:",
                    choices = c("total_year", "cum_tech", "tech_year", "cum_total")),
        uiOutput("scenarioFilterUI"), # Dynamic scenarios
        uiOutput("regionFilterUI"),   # Dynamic regions
        sliderInput("yearRange", "Select Year Range:", min = 2025, max = 2100, value = c(2025, 2050), step = 5),
        actionButton("update", "Update Plot"),
        numericInput("ncol", "Number of Columns for Facets:", value = 2, min = 1),
        numericInput("nrow", "Number of Rows for Facets:", value = 2, min = 1)
      ),
      mainPanel(
        plotOutput("plot"),
        textOutput("summary")
      )
    )
  )

  # Server
  server <- function(input, output, session) {
    # Reactive function to load the selected dataset
    selected_data <- reactive({
      req(input$dataset) # Ensure dataset is selected
      file_path <- file.path(output_path, input$dataset)
      if (!file.exists(file_path)) {
        stop("File does not exist: ", file_path)
      }
      data <- tryCatch({
        read.csv(file_path)
      }, error = function(e) {
        stop("Failed to load dataset: ", e$message)
      })

      # If the selected dataset is "cum_total.csv", add the total_cdr column
      if (grepl("cum_total", input$dataset) && !"total_cdr" %in% colnames(data)) {
        data <- data %>%
          mutate(total_cdr = "total_cdr")
      }

      return(data)
    })

    # Dynamically update scenario and region options
    observe({
      data <- selected_data()

      # Update scenario options
      if ("scenario" %in% colnames(data)) {
        scenarios <- unique(data$scenario)
        updateSelectInput(session, "scenarioFilter", choices = scenarios, selected = scenarios)
      }

      # Update region options
      if ("region" %in% colnames(data)) {
        regions <- unique(data$region)
        updateSelectInput(session, "regionFilter", choices = regions, selected = regions)
      }
    })

    # Dynamic UI for scenarios and regions
    output$scenarioFilterUI <- renderUI({
      data <- selected_data()
      if ("scenario" %in% colnames(data)) {
        selectInput("scenarioFilter", "Scenarios:", choices = unique(data$scenario), multiple = TRUE)
      } else {
        NULL
      }
    })

    output$regionFilterUI <- renderUI({
      data <- selected_data()
      if ("region" %in% colnames(data)) {
        selectInput("regionFilter", "Regions:", choices = unique(data$region), multiple = TRUE)
      } else {
        NULL
      }
    })

    # Reactive function to filter the dataset
    filtered_data <- reactive({
      data <- selected_data()

      # Filter by year range if applicable
      if ("year" %in% colnames(data)) {
        data <- data[data$year >= input$yearRange[1] & data$year <= input$yearRange[2], ]
      }

      # Filter by selected scenarios
      if (!is.null(input$scenarioFilter) && "scenario" %in% colnames(data)) {
        data <- data[data$scenario %in% input$scenarioFilter, ]
      }

      # Filter by selected regions
      if (!is.null(input$regionFilter) && "region" %in% colnames(data)) {
        data <- data[data$region %in% input$regionFilter, ]
      }

      return(data)
    })

    # Generate the plot
    output$plot <- renderPlot({
      req(filtered_data())
      data <- filtered_data()

      # Ensure the selected metric exists in the data
      if (!input$metric %in% colnames(data)) {
        stop("The selected metric is not available in the dataset.")
      }

      # Scale the selected metric to millions for readability
      data[[input$metric]] <- data[[input$metric]] / 1e6

      # Adjust plot based on type
      if (input$type == "cum_tech" && "main_technology" %in% colnames(data)) {
        ggplot(data, aes(x = main_technology, y = .data[[input$metric]], fill = main_technology)) +
          geom_bar(stat = "identity", position = position_dodge(preserve = "single"), width = 0.9) +
          facet_grid(scenario ~ region, scales = "free") +
          labs(
            title = paste("Cumulative Jobs -", input$type),
            x = "Technology",
            y = "Million Jobs"
          ) +
          theme_minimal()+
          theme(
            legend.position = "none", # Remove legend
            axis.text.x = element_text(angle = 90)) # Remove legend
      } else if (input$type == "total_year" && "year" %in% colnames(data)) {
        ggplot(data, aes(x = year, y = .data[[input$metric]], fill = region)) +
          geom_bar(stat = "identity", position = position_dodge(preserve = "single"), width = 5) +
          facet_grid(scenario ~ region, scales = "free") +
          labs(
            title = paste("Annual Jobs -", input$type),
            x = "Year",
            y = "Million Jobs"
          ) +
          theme_minimal()+
          theme(
            legend.position = "none", # Remove legend
            axis.text.x = element_text(angle = 90)) # Remove legend
      } else if (input$type == "tech_year" && "year" %in% colnames(data)) {
        ggplot(data, aes(x = year, y = .data[[input$metric]], fill = main_technology)) +
          geom_bar(stat = "identity", position = position_dodge(preserve = "single"), width = 5) +
          facet_grid(scenario ~ region, scales = "free") +
          labs(
            title = paste("Annual Jobs -", input$type),
            x = "Year",
            y = "Million Jobs"
          ) +
          theme_minimal()+
          theme(
            axis.text.x = element_text(angle = 90)) # Remove legend
      } else if (input$type == "cum_total" && "scenario" %in% colnames(data)) {
        ggplot(data, aes(x = total_cdr, y = .data[[input$metric]])) +
          geom_bar(stat = "identity", fill = "lightblue", position = position_dodge(preserve = "single"), width = 0.9) +
          facet_grid(scenario ~ region, scales = "free") +
          labs(
            title = paste("Cumulative Total -", input$type),
            x = "Total CDR",
            y = "Million Jobs"
          ) +
          theme_minimal() +
          theme(
            legend.position = "none", # Remove legend
            axis.text.x = element_text(angle = 90)) # Remove legend
      } else {
        ggplot() + labs(title = "Unsupported dataset or visualization type")
      }
    })

    # Summary information about the selected dataset
    output$summary <- renderText({
      data <- selected_data()
      paste("Loaded dataset with", nrow(data), "rows and", ncol(data), "columns.")
    })
  }

  # Run the app
  shinyApp(ui = ui, server = server)
}
