#' Calculate CDR Jobs
#' @export
calculate_cdr_jobs <- function(db_path, db_name, dat_file = NULL, scenario_list, region_list, output_path,
                               output_type = "csv", create_plots = TRUE, job_metric = "max_Jobs",
                               ncol = 2, nrow = NULL, selected_years = NULL) {

  # Validate output_type
  output_type <- match.arg(output_type, several.ok = TRUE)

  # Validate output path
  if (!dir.exists(output_path)) {
    stop("The specified output_path does not exist.")
  }

  # Load required libraries
  library(dplyr)
  library(tidyr)
  library(rgcam)

  # Define job intensities
  CDR_Job_Inten <- data.frame(
    subtechnology = c(
      # BECCS
      "biomass liquids", "biomass (IGCC CCS)", "biomass (conv CCS)", "biomass",
      # DAC
      "dac",
      # ERW
      "rock weathering",
      # Biochar
      "Biochar"
    ),
    main_technology = c(
      rep("BECCS", 4),  # BECCS technologies
      "DAC",            # DAC technology
      "ERW",            # ERW technology
      "Biochar"         # Biochar technology
    )
  )

  # Define job intensities for each main technology
  job_intensities <- list(
    BECCS = c(0.001, 0.003, 0.0001),
    DAC = c(0.003),
    ERW = c(0.002),
    Biochar = c(0.00003, 0.02)
  )

  # Expand the job intensities for each subtechnology
  expanded_job_intensities <- CDR_Job_Inten %>%
    rowwise() %>%
    mutate(intensities = list(job_intensities[[main_technology]])) %>%
    unnest(intensities) %>%
    group_by(subtechnology) %>%
    summarize(
      min_int = min(intensities),
      mean_int = mean(intensities),
      max_int = max(intensities),
      .groups = "drop"
    ) %>%
    left_join(CDR_Job_Inten, by = "subtechnology")

  # Create the XML query dynamically
  CDR_query <- '<?xml version="1.0"?>
<queries>
  <aQuery>
    <all-regions/>
    <emissionsQueryBuilder title="CO2 sequestration by tech">
      <axis1 name="subsector">subsector</axis1>
      <axis2 name="Year">emissions-sequestered</axis2>
      <xPath buildList="true" dataName="emissions" group="false" sumAll="false">
        *[@type = "sector"]/*[@type="subsector"]/*[@type="technology"]//CO2/emissions-sequestered/node()
      </xPath>
    </emissionsQueryBuilder>
  </aQuery>
</queries>'

  # Save the query to a temporary file
  query_file <- tempfile(fileext = ".xml")
  writeLines(CDR_query, query_file)

  # Connect to GCAM database and run the query
  conn <- localDBConn(db_path, db_name)
  CDR_tech <- addScenario(conn, paste0(dat_file, ".dat"), scenario_list, query_file)
  CDR_Output <- getQuery(CDR_tech, "CO2 sequestration by tech")

  # Filter data by scenarios and regions
  CDR_Output <- CDR_Output %>%
    filter(scenario %in% scenario_list, region %in% region_list)

  # Filter expanded job intensities to include only subtechnologies present in the model's output
  valid_subtechnologies <- unique(CDR_Output$subsector)
  expanded_job_intensities <- expanded_job_intensities %>%
    filter(subtechnology %in% valid_subtechnologies)

  # Join with job intensities using subsector from CDR_Output and subtechnology from expanded_job_intensities
  Job_data <- left_join(CDR_Output, expanded_job_intensities, by = c("subsector" = "subtechnology")) %>%
    filter(!is.na(main_technology)) # Exclude rows with NA main_technology after filtering

  # Debugging: Ensure Job_data has valid rows
  if (nrow(Job_data) == 0) stop("No matching data after joining subsector and subtechnology.")

  # Function to calculate aggregated job metrics
  calculate_metrics <- function(data, grouping_vars) {
    data %>%
      group_by(across(all_of(grouping_vars))) %>%
      summarize(
        mean_Jobs = sum(mean_int * 3.667 * 10^6 * value, na.rm = TRUE),
        min_Jobs = sum(min_int * 3.667 * 10^6 * value, na.rm = TRUE),
        max_Jobs = sum(max_int * 3.667 * 10^6 * value, na.rm = TRUE),
        .groups = "drop"
      )
  }

  # Calculate different job metrics
  Job_by_tech_year <- calculate_metrics(Job_data, c("scenario", "region", "main_technology", "year"))
  Job_by_tech_year_global <- calculate_metrics(Job_data, c("scenario", "main_technology", "year"))
  Job_total_year <- calculate_metrics(Job_data, c("scenario", "region", "year"))
  Job_total_year_global <- calculate_metrics(Job_data, c("scenario", "year"))
  Job_cum_tech <- calculate_metrics(Job_data, c("scenario", "region", "main_technology"))
  Job_cum_tech_global <- calculate_metrics(Job_data, c("scenario", "main_technology"))
  Job_cum_total <- calculate_metrics(Job_data, c("scenario", "region"))
  Job_cum_total_global <- calculate_metrics(Job_data, c("scenario"))

  # Save results as CSV if specified
  if ("csv" %in% output_type) {
    write.csv(Job_by_tech_year, file = file.path(output_path, "Job_by_tech_year.csv"), row.names = FALSE)
    write.csv(Job_by_tech_year_global, file = file.path(output_path, "Job_by_tech_year_global.csv"), row.names = FALSE)
    write.csv(Job_total_year, file = file.path(output_path, "Job_total_year.csv"), row.names = FALSE)
    write.csv(Job_total_year_global, file = file.path(output_path, "Job_total_year_global.csv"), row.names = FALSE)
    write.csv(Job_cum_tech, file = file.path(output_path, "Job_cum_tech.csv"), row.names = FALSE)
    write.csv(Job_cum_tech_global, file = file.path(output_path, "Job_cum_tech_global.csv"), row.names = FALSE)
    write.csv(Job_cum_total, file = file.path(output_path, "Job_cum_total.csv"), row.names = FALSE)
    write.csv(Job_cum_total_global, file = file.path(output_path, "Job_cum_total_global.csv"), row.names = FALSE)
  }

  # Create visualizations for all result types if requested
  if (create_plots) {
    message("Generating and saving visualizations...")

    visualize_results(
      data = Job_cum_tech,
      type = "cum_tech",
      job_metric = job_metric,
      selected_scenarios = scenario_list,
      selected_regions = region_list,
      ncol = ncol,
      nrow = nrow,
      output_path = output_path
    )


    visualize_results(
      data = Job_total_year,
      type = "total_year",
      job_metric = job_metric,
      selected_scenarios = scenario_list,
      selected_regions = region_list,
      ncol = ncol,
      nrow = nrow,
      output_path = output_path
    )


    visualize_results(
      data = Job_by_tech_year,
      type = "tech_year",
      job_metric = job_metric,
      selected_scenarios = scenario_list,
      selected_regions = region_list,
      ncol = ncol,
      nrow = nrow,
      output_path = output_path
    )


    visualize_results(
      data = Job_cum_total,
      type = "cum_total",
      job_metric = job_metric,
      selected_scenarios = scenario_list,
      selected_regions = region_list,
      ncol = ncol,
      nrow = nrow,
      output_path = output_path
    )

  }

  # Return results as a list
  return(list(
    Job_total_year = Job_total_year,
    Job_by_tech_year = Job_by_tech_year,
    Job_cum_tech = Job_cum_tech,
    Job_cum_total = Job_cum_total,
    Job_total_year_global = Job_total_year_global,
    Job_by_tech_year_global = Job_by_tech_year_global,
    Job_cum_tech_global = Job_cum_tech_global,
    Job_cum_total_global = Job_cum_total_global
  ))
}
