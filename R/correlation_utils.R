#' Create correlation table for deception and SDT metrics
#'
#' Internal function to create correlation table using apaTables
#'
#' @param result_data A data frame with calculated metrics
#' @param exclude_study Character vector of study names to exclude from correlation analysis
#' @keywords internal
create_correlation_table <- function(result_data, exclude_study = NULL) {
  
  # Check if apaTables is available
  if (!requireNamespace("apaTables", quietly = TRUE)) {
    warning("apaTables package is required for correlation tables. Install with: install.packages('apaTables')")
    return(invisible(NULL))
  }
  
  # Store original studies for documentation
  original_studies <- if ("study" %in% names(result_data)) {
    unique(result_data$study)
  } else {
    NULL
  }
  
  # Filter out excluded studies if specified
  if (!is.null(exclude_study) && "study" %in% names(result_data)) {
    original_n <- nrow(result_data)
    result_data <- result_data[!result_data$study %in% exclude_study, ]
    excluded_n <- original_n - nrow(result_data)
    
    if (excluded_n > 0) {
      message(paste("Excluded", excluded_n, "rows from studies:", paste(exclude_study, collapse = ", ")))
    }
  }
  
  # Get list of included studies
  included_studies <- if ("study" %in% names(result_data)) {
    unique(result_data$study)
  } else {
    NULL
  }
  
  # Check if we have multiple studies for correlation
  if (nrow(result_data) < 2) {
    warning("Correlation table requires at least 2 observations. Only ", nrow(result_data), " found after exclusions.")
    return(invisible(NULL))
  }
  
  # Extract the nine measures for correlation analysis
  # Order: Accuracy, Truth_Bias, Truth_Accuracy, Lie_Accuracy, A_Prime, D_Prime, Criterion, Beta, B_double_prime
  correlation_data <- result_data[, c("accuracy", "truth_bias", "truth_accuracy", "lie_accuracy", 
                                      "a_prime", "d_prime", "criterion", "beta", "bppd")]
  
  # Rename columns for better display
  names(correlation_data) <- c("Accuracy", "Truth_Bias", "Truth_Accuracy", "Lie_Accuracy",
                               "A_Prime", "D_Prime", "Criterion", "Beta", "B_double_prime")
  
  # Remove rows with all NA values
  correlation_data <- correlation_data[rowSums(is.na(correlation_data)) < ncol(correlation_data), ]
  
  # Check if we still have enough data
  if (nrow(correlation_data) < 2) {
    warning("Insufficient data for correlation analysis after removing NA values.")
    return(invisible(NULL))
  }
  
  # Create filename with current date
  current_date <- format(Sys.Date(), "%Y-%m-%d")
  csv_filename <- paste0("deception_sdt_correlation_table_", current_date, ".csv")
  
  tryCatch({
    # Create correlation results without saving .doc file
    correlation_results <- apaTables::apa.cor.table(correlation_data,
                                                   filename = NA,  # Don't save doc file
                                                   show.conf.interval = TRUE)
    
    # Get the correlation table
    corr_table <- correlation_results$table.body
    
    # Add blank rows for separation
    blank_row1 <- matrix("", nrow = 1, ncol = ncol(corr_table))
    colnames(blank_row1) <- colnames(corr_table)
    blank_row2 <- matrix("", nrow = 1, ncol = ncol(corr_table))
    colnames(blank_row2) <- colnames(corr_table)
    
    # Create studies information row
    if (!is.null(included_studies)) {
      # Sort studies alphabetically for consistency
      included_studies <- sort(included_studies)
      
      # Create a single string with all study names, properly formatted
      studies_info <- paste("Studies included:", paste(included_studies, collapse = "; "))
      n_studies <- paste("Number of studies:", length(included_studies))
      n_obs <- paste("Number of observations:", nrow(result_data))
      
      # Create info rows - put all text in first column to avoid CSV splitting issues
      info_row1 <- matrix("", nrow = 1, ncol = ncol(corr_table))
      info_row1[1, 1] <- studies_info
      colnames(info_row1) <- colnames(corr_table)
      
      info_row2 <- matrix("", nrow = 1, ncol = ncol(corr_table))
      info_row2[1, 1] <- n_studies
      colnames(info_row2) <- colnames(corr_table)
      
      info_row3 <- matrix("", nrow = 1, ncol = ncol(corr_table))
      info_row3[1, 1] <- n_obs
      colnames(info_row3) <- colnames(corr_table)
      
      # Combine correlation table with info
      combined_table <- rbind(corr_table, blank_row1, blank_row2, info_row1, info_row2, info_row3)
    } else {
      combined_table <- corr_table
    }
    
    # Save the combined table as CSV
    write.csv(combined_table, csv_filename, row.names = TRUE)
    
    message(paste("Correlation table saved as:", csv_filename))
    if (!is.null(included_studies)) {
      message(paste("Studies included in correlation:", paste(included_studies, collapse = ", ")))
    }
    
  }, error = function(e) {
    warning("Error creating correlation table: ", e$message)
  })
  
  invisible(NULL)
}
