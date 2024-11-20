#' Visualize Results
#'
#' Creates faceted plots for CDR job estimation results.
#'
#' @param data The dataset to visualize.
#' @param type The type of visualization. One of "total_year" or "cum_tech".
#' @param job_metric The metric to visualize. One of "mean_Jobs", "min_Jobs", "max_Jobs".
#' @param selected_scenarios A vector of scenario names to include, or NULL for all.
#' @param selected_regions A vector of region names to include, or NULL for all.
#' @param selected_years A vector of years to include (for "total_year"), or NULL for all.
#' @param ncol Number of columns for the facets.
#' @param nrow Number of rows for the facets. If NULL, rows are determined dynamically by ggplot2.
#' @param output_path The directory where plots will be saved as PNG files.
#' @examples
#' visualize_results(data = results$Job_total_year, type = "total_year", job_metric = "mean_Jobs")
#' @export
visualize_results <- function(data,
                              type = c("total_year", "cum_tech", "tech_year", "cum_total"),
                              job_metric = c("mean_Jobs", "min_Jobs", "max_Jobs"),
                              selected_scenarios = NULL,
                              selected_regions = NULL,
                              selected_years = NULL,
                              ncol = 2,
                              nrow = NULL,
                              output_path = getwd(),
                              dataset_name = NULL) {  # Added dataset_name for validation
  # Debugging the `type` value
  print(paste("Debug: type received =", type))

  # Validate `type` argument
  type <- match.arg(type)
  print(paste("Debug: validated type =", type))

  # Validate `job_metric` argument
  job_metric <- match.arg(job_metric)

  # Validate output path
  if (!dir.exists(output_path)) {
    stop("The specified output_path does not exist.")
  }

  # Ensure the data is a data frame
  if (!is.data.frame(data)) {
    stop("The provided data is not a valid data frame.")
  }

  # Add `total_cdr` column for "cum_total.csv" dataset
  if (type == "cum_total" && !"total_cdr" %in% colnames(data)) {
    data <- data %>%
      mutate(total_cdr = "total_cdr")  # Add `total_cdr` column
    print("Debug: Added 'total_cdr' column for cum_total dataset")
  }

  # Filter data based on user selections
  if (!is.null(selected_scenarios)) {
    data <- data[data$scenario %in% selected_scenarios, ]
  }
  if (!is.null(selected_regions)) {
    data <- data[data$region %in% selected_regions, ]
  }
  if (!is.null(selected_years) && "year" %in% colnames(data)) {
    data <- data[data$year %in% selected_years, ]
  }

  # Check if data is empty
  if (nrow(data) == 0) {
    stop("No data available after filtering. Please check your selected scenarios, regions, or years.")
  }

  # Ensure faceting variables exist
  if (!"region" %in% colnames(data)) {
    data$region <- "No Region"
  }
  if (!"scenario" %in% colnames(data)) {
    data$scenario <- "No Scenario"
  }

  # Scale the job metrics to millions
  data[[job_metric]] <- data[[job_metric]] / 1e6

  # Ensure data is ordered by year for proper line plotting
  if (type == "total_year") {
    data <- dplyr::arrange(data, scenario, region, year)
  }

  # Convert year to numeric to ensure proper ordering and continuity in the line plot
  if ("year" %in% colnames(data)) {
    data$year <- as.numeric(data$year)
  }

  # Define theme for all plots
  custom_theme <- theme(
    legend.position = "none",
    legend.title = element_blank(),
    axis.text.x = element_text(size = 8, color = "black", face = "bold", angle = 0, hjust = 0.4),
    axis.text.y = element_text(size = 8, color = "black", face = "bold"),
    axis.title.y = element_text(size = 8, color = "black", face = "bold"),
    plot.title = element_text(size = 10, color = "darkred", face = "bold"),
    plot.subtitle = element_text(size = 10, color = "darkgreen", face = "italic"),
    plot.caption = element_text(size = 10, color = "purple", face = "italic"),
    strip.text = element_text(size = 10, color = "black", face = "bold"),
    legend.text = element_text(size = 12, face = "bold"),
    panel.border = element_rect(colour = "black", fill = NA, size = 0.2),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.spacing = unit(1.5, "lines")
  )

  # Plot based on type
  if (type == "cum_tech") {
    p <- ggplot(data, aes(x = main_technology, y = .data[[job_metric]])) +
      geom_bar(stat = "identity", fill = "lightblue") +
      facet_grid(region ~ scenario, scales = "free_y") +
      labs(title = "Cumulative Jobs by Technology",
           x = "Technology",
           y = paste(job_metric, "(Million)")) +
      custom_theme
  } else if (type == "total_year") {
    p <- ggplot(data, aes(x = year, y = .data[[job_metric]])) +
      geom_bar(stat = "identity", fill = "lightblue") +
      facet_grid(region ~ scenario, scales = "free_y") +
      labs(title = "Total Jobs by Year",
           x = "Year",
           y = paste(job_metric, "(Million)")) +
      custom_theme
  } else if (type == "tech_year") {
    p <- ggplot(data, aes(x = year, y = .data[[job_metric]])) +
      geom_bar(stat = "identity", fill = "lightblue") +
      facet_grid(region ~ scenario, scales = "free_y") +
      labs(title = "Total Jobs by tech by Year",
           x = "Year",
           y = paste(job_metric, "(Million)")) +
      custom_theme
  } else if (type == "cum_total") {
    p <- ggplot(data, aes(x = total_cdr, y = .data[[job_metric]])) +
      geom_bar(stat = "identity", fill = "lightblue") +
      facet_grid(region ~ scenario, scales = "free_y") +
      labs(title = "Total Cumulative Jobs",
           x = "Total CDR",
           y = paste(job_metric, "(Million)")) +
      custom_theme
  }

  # Save plot
  file_name <- paste0(type, "_facet_plot.png")
  ggsave(filename = file.path(output_path, file_name),
         plot = p, width = 10, height = 8)

  # Return plot for optional interactive use
  return(p)
}
