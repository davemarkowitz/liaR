#' Calculate Comprehensive Deception Detection and SDT Metrics
#'
#' This function calculates both basic detection accuracy metrics and comprehensive
#' Signal Detection Theory (SDT) metrics for deception detection studies, optionally 
#' grouped by study and/or participant. The function automatically detects unique values in your data 
#' and provides guidance if the standard "honest"/"deceptive" coding is not used.
#'
#' @param ground_truth A vector of ground truth labels. Should contain exactly 2 unique values
#'   (excluding NA). Standard coding uses "honest" and "deceptive", but other binary
#'   coding schemes are supported (e.g., "truth"/"lie", "0"/"1", etc.)
#' @param response A vector of participant responses. Should use the same coding
#'   scheme as ground_truth
#' @param study An optional vector of study identifiers for grouping (default: NULL)
#' @param participant_id An optional vector of participant identifiers for grouping (default: NULL)
#' @param honest_value Optional: specify which value represents "honest" responses
#'   (default: auto-detected or "honest")
#' @param deceptive_value Optional: specify which value represents "deceptive" responses
#'   (default: auto-detected or "deceptive")
#' @param export_csv Logical. If TRUE, exports results to a CSV file with current date (default: FALSE)
#' @param corr_table Logical. If TRUE, creates APA-style correlation table of all metrics (default: FALSE)
#'
#' @return A data frame containing comprehensive deception detection metrics:
#' \describe{
#'   \item{study}{Study identifier (if provided)}
#'   \item{participant_id}{Participant identifier (if provided)}
#'   \item{n_total}{Total number of observations}
#'   \item{n_valid}{Number of valid (non-NA) responses}
#'   \item{accuracy}{Proportion of correct responses}
#'   \item{truth_bias}{Proportion of responses coded as "honest"}
#'   \item{truth_accuracy}{Accuracy on trials where ground truth is "honest"}
#'   \item{lie_accuracy}{Accuracy on trials where ground truth is "deceptive"}
#'   \item{hits}{Number of correctly identified deceptive statements}
#'   \item{misses}{Number of deceptive statements incorrectly identified as honest}
#'   \item{false_alarms}{Number of honest statements incorrectly identified as deceptive}
#'   \item{correct_rejections}{Number of correctly identified honest statements}
#'   \item{hit_rate}{Proportion of deceptive statements correctly identified}
#'   \item{false_alarm_rate}{Proportion of honest statements incorrectly identified as deceptive}
#'   \item{d_prime}{Sensitivity measure (parametric)}
#'   \item{a_prime}{Sensitivity measure (non-parametric)}
#'   \item{beta}{Response bias measure (beta)}
#'   \item{bppd}{Response bias measure (b-double prime)}
#'   \item{criterion}{Response bias measure (criterion c)}
#'   \item{honest_value}{Value used for "honest" coding}
#'   \item{deceptive_value}{Value used for "deceptive" coding}
#' }
#'
#' @examples
#' # Standard coding
#' ground_truth <- c("honest", "deceptive", "honest", "deceptive")
#' response <- c("honest", "honest", "honest", "deceptive")
#' deception_metrics(ground_truth, response)
#'
#' # Alternative coding schemes
#' ground_truth <- c("truth", "lie", "truth", "lie")
#' response <- c("truth", "truth", "truth", "lie")
#' deception_metrics(ground_truth, response, honest_value = "truth", deceptive_value = "lie")
#'
#' # Multiple studies with CSV export and correlation table
#' study <- c(rep("study1", 2), rep("study2", 2))
#' ground_truth <- c("honest", "deceptive", "honest", "deceptive")
#' response <- c("honest", "honest", "honest", "deceptive")
#' deception_metrics(ground_truth, response, study, export_csv = TRUE, corr_table = TRUE)
#'
#' # With participant grouping
#' participant_id <- c("P1", "P1", "P2", "P2")
#' ground_truth <- c("honest", "deceptive", "honest", "deceptive")
#' response <- c("honest", "honest", "honest", "deceptive")
#' deception_metrics(ground_truth, response, participant_id = participant_id)
#'
#' # With both study and participant grouping
#' study <- c(rep("study1", 4), rep("study2", 4))
#' participant_id <- c("P1", "P1", "P2", "P2", "P3", "P3", "P4", "P4")
#' ground_truth <- rep(c("honest", "deceptive"), 4)
#' response <- c("honest", "honest", "honest", "deceptive", "honest", "deceptive", "deceptive", "deceptive")
#' deception_metrics(ground_truth, response, study = study, participant_id = participant_id)
#'
#' @export
#' @importFrom dplyr group_by group_modify ungroup
#' @importFrom magrittr %>%
deception_metrics <- function(ground_truth, response, study = NULL, participant_id = NULL,
                             honest_value = NULL, deceptive_value = NULL,
                             export_csv = FALSE, corr_table = FALSE) {
  
  # Automatically load required packages
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("dplyr package is required. Install with: install.packages('dplyr')")
  }
  if (!requireNamespace("magrittr", quietly = TRUE)) {
    stop("magrittr package is required. Install with: install.packages('magrittr')")
  }
  
  # Load the packages
  library(dplyr, quietly = TRUE)
  library(magrittr, quietly = TRUE)
  
  # Input validation
  if (length(ground_truth) != length(response)) {
    stop("ground_truth and response must have the same length")
  }
  
  if (!is.null(study) && length(study) != length(ground_truth)) {
    stop("study must have the same length as ground_truth and response")
  }
  
  if (!is.null(participant_id) && length(participant_id) != length(ground_truth)) {
    stop("participant_id must have the same length as ground_truth and response")
  }
  
  # Convert empty strings to NA and filter out NA values before detecting unique values
  gt_clean <- ground_truth[!is.na(ground_truth) & ground_truth != ""]
  resp_clean <- response[!is.na(response) & response != ""]
  all_unique <- unique(c(gt_clean, resp_clean))
  
  # Check for binary coding
  if (length(all_unique) != 2) {
    stop(paste0("Data must contain exactly 2 unique values (excluding NA and empty strings). ",
                "Found: ", length(all_unique), " unique values: ", paste(all_unique, collapse = ", "), ". ",
                "Please ensure binary coding is consistent across ground truth and response variables."))
  }
  
  # Auto-detect or use specified values
  if (is.null(honest_value) && is.null(deceptive_value)) {
    # Try to auto-detect standard values
    if ("honest" %in% all_unique && "deceptive" %in% all_unique) {
      honest_value <- "honest"
      deceptive_value <- "deceptive"
    } else if ("truth" %in% all_unique && "lie" %in% all_unique) {
      honest_value <- "truth"
      deceptive_value <- "lie"
    } else if ("0" %in% all_unique && "1" %in% all_unique) {
      honest_value <- "0"
      deceptive_value <- "1"
    } else {
      # If can't auto-detect, provide guidance
      stop(paste0("Could not auto-detect coding scheme. Found values: ", paste(all_unique, collapse = ", "),
                  "\nPlease specify honest_value and deceptive_value parameters.",
                  "\nExample: deception_metrics(gt, resp, honest_value = '", all_unique[1], 
                  "', deceptive_value = '", all_unique[2], "')"))
    }
  } else if (is.null(honest_value) || is.null(deceptive_value)) {
    stop("If specifying coding values, both honest_value and deceptive_value must be provided")
  }
  
  # Convert specified values to character for consistent comparison
  honest_value <- as.character(honest_value)
  deceptive_value <- as.character(deceptive_value)
  
  # Validate specified values exist in data
  if (!honest_value %in% all_unique || !deceptive_value %in% all_unique) {
    stop(paste0("Specified values not found in data. Available values: ", paste(all_unique, collapse = ", ")))
  }
  
  message(paste0("Using coding scheme: honest = '", honest_value, "', deceptive = '", deceptive_value, "'"))
  
  # Create data frame and convert empty strings to NA
  data <- data.frame(
    ground_truth = ifelse(ground_truth == "", NA, ground_truth),
    response = ifelse(response == "", NA, response),
    study = if (is.null(study)) rep("overall", length(ground_truth)) else study,
    stringsAsFactors = FALSE
  )
  
  # Add participant_id if provided
  if (!is.null(participant_id)) {
    data$participant_id <- participant_id
  }
  
  # Calculate basic metrics using detected values
  data$accuracy <- ifelse(is.na(data$response), NA,
                         ifelse(as.character(data$response) == as.character(data$ground_truth), 1, 0))
  data$truth_bias <- ifelse(as.character(data$response) == honest_value, 1, 0)
  
  # Determine grouping strategy
  if (!is.null(participant_id)) {
    # Calculate participant-level metrics first
    participant_results <- data %>%
      dplyr::group_by(study, participant_id) %>%
      dplyr::group_modify(~ calculate_sdt_metrics(.x, honest_value, deceptive_value)) %>%
      dplyr::ungroup()
    
    # Calculate study-level metrics by averaging participant results within studies
    study_results <- participant_results %>%
      dplyr::group_by(study) %>%
      dplyr::summarise(
        n_total = sum(n_total, na.rm = TRUE),
        n_valid = sum(n_valid, na.rm = TRUE),
        accuracy = mean(accuracy, na.rm = TRUE),
        truth_bias = mean(truth_bias, na.rm = TRUE),
        truth_accuracy = mean(truth_accuracy, na.rm = TRUE),
        lie_accuracy = mean(lie_accuracy, na.rm = TRUE),
        hits = sum(hits, na.rm = TRUE),
        misses = sum(misses, na.rm = TRUE),
        false_alarms = sum(false_alarms, na.rm = TRUE),
        correct_rejections = sum(correct_rejections, na.rm = TRUE),
        hit_rate = mean(hit_rate, na.rm = TRUE),
        false_alarm_rate = mean(false_alarm_rate, na.rm = TRUE),
        d_prime = mean(d_prime, na.rm = TRUE),
        a_prime = mean(a_prime, na.rm = TRUE),
        beta = mean(beta, na.rm = TRUE),
        bppd = mean(bppd, na.rm = TRUE),
        criterion = mean(criterion, na.rm = TRUE),
        honest_value = first(honest_value),
        deceptive_value = first(deceptive_value),
        .groups = 'drop'
      ) %>%
      # Round the averaged values
      dplyr::mutate(
        accuracy = round(accuracy, 4),
        truth_bias = round(truth_bias, 4),
        truth_accuracy = round(truth_accuracy, 4),
        lie_accuracy = round(lie_accuracy, 4),
        hit_rate = round(hit_rate, 4),
        false_alarm_rate = round(false_alarm_rate, 4),
        d_prime = round(d_prime, 4),
        a_prime = round(a_prime, 4),
        beta = round(beta, 4),
        bppd = round(bppd, 4),
        criterion = round(criterion, 4)
      )
    
    result <- participant_results
    study_level_result <- study_results
    
  } else {
    # Just aggregate by study
    result <- data %>%
      dplyr::group_by(study) %>%
      dplyr::group_modify(~ calculate_sdt_metrics(.x, honest_value, deceptive_value)) %>%
      dplyr::ungroup()
    
    study_level_result <- NULL
  }
  
  # If no study grouping was provided, remove the study column
  if (is.null(study)) {
    result$study <- NULL
  }
  
  # Export to CSV if requested
  if (export_csv) {
    current_date <- format(Sys.Date(), "%Y-%m-%d")
    
    if (!is.null(participant_id)) {
      # Export participant-level results
      participant_filename <- paste0("deception_analysis_participant_level_", current_date, ".csv")
      write.csv(result, participant_filename, row.names = FALSE)
      message(paste("Participant-level results exported to:", participant_filename))
      
      # Export study-level results (averaged from participants)
      study_filename <- paste0("deception_analysis_study_level_", current_date, ".csv")
      write.csv(study_level_result, study_filename, row.names = FALSE)
      message(paste("Study-level results (averaged from participants) exported to:", study_filename))
      
    } else {
      # Export study-level results only
      filename <- paste0("deception_analysis_study_level_", current_date, ".csv")
      write.csv(result, filename, row.names = FALSE)
      message(paste("Study-level results exported to:", filename))
    }
  }
  
  # Create correlation table if requested
  if (corr_table) {
    if (!is.null(participant_id)) {
      # Create both participant-level and study-level correlation tables
      create_correlation_table(result, "participant")
      create_correlation_table(study_level_result, "study")
    } else {
      # Create study-level correlation table only
      create_correlation_table(result, "study")
    }
  }
  
  return(result)
}
