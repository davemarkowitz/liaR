#' Create correlation table for deception and SDT metrics
#'
#' Internal function to create correlation table using apaTables
#'
#' @param result_data A data frame with calculated metrics
#' @keywords internal
create_correlation_table <- function(result_data) {
  
  # Check if apaTables is available
  if (!requireNamespace("apaTables", quietly = TRUE)) {
    warning("apaTables package is required for correlation tables. Install with: install.packages('apaTables')")
    return(invisible(NULL))
  }
  
  # Check if we have multiple studies for correlation
  if (nrow(result_data) < 2) {
    warning("Correlation table requires at least 2 studies. Only ", nrow(result_data), " study found.")
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
    
    # Save the correlation matrix as CSV
    write.csv(correlation_results$table.body, csv_filename, row.names = TRUE)
    
    message(paste("Correlation table saved as:", csv_filename))
    
  }, error = function(e) {
    warning("Error creating correlation table: ", e$message)
  })
  
  invisible(NULL)
}