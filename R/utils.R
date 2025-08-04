#' Calculate comprehensive deception detection metrics for a subset of data
#'
#' Internal function to calculate both basic and Signal Detection Theory metrics
#'
#' @param data A data frame with ground_truth and response columns
#' @param honest_value The value representing honest responses
#' @param deceptive_value The value representing deceptive responses
#' @return A data frame with calculated metrics
#' @keywords internal
calculate_sdt_metrics <- function(data, honest_value, deceptive_value) {
  # Remove NA responses
  clean_data <- data[!is.na(data$response), ]
  
  if(nrow(clean_data) == 0) {
    return(data.frame(
      n_total = nrow(data), n_valid = 0, accuracy = NA, truth_bias = NA,
      truth_accuracy = NA, lie_accuracy = NA,
      hits = NA, misses = NA, false_alarms = NA, correct_rejections = NA,
      hit_rate = NA, false_alarm_rate = NA, d_prime = NA, a_prime = NA,
      beta = NA, bppd = NA, criterion = NA, honest_value = honest_value, deceptive_value = deceptive_value
    ))
  }
  
  # Create contingency table - R automatically detects all values
  table_data <- table(clean_data$ground_truth, clean_data$response)
  
  # Calculate basic metrics
  n_total <- nrow(data)
  n_valid <- nrow(clean_data)
  accuracy <- mean(clean_data$accuracy, na.rm = TRUE)
  truth_bias <- sum(as.character(clean_data$response) == honest_value, na.rm = TRUE) / n_valid
  
  # Calculate truth accuracy and lie accuracy
  # Truth accuracy: accuracy on trials where ground truth is "honest"
  honest_trials <- clean_data[as.character(clean_data$ground_truth) == honest_value, ]
  truth_accuracy <- ifelse(nrow(honest_trials) > 0,
                          sum(as.character(honest_trials$response) == honest_value) / nrow(honest_trials),
                          NA)
  
  # Lie accuracy: accuracy on trials where ground truth is "deceptive"  
  deceptive_trials <- clean_data[as.character(clean_data$ground_truth) == deceptive_value, ]
  lie_accuracy <- ifelse(nrow(deceptive_trials) > 0,
                        sum(as.character(deceptive_trials$response) == deceptive_value) / nrow(deceptive_trials),
                        NA)
  
  # SDT calculations (using deceptive as positive case)
  # Extract values directly from the table using character indexing
  hits <- ifelse(deceptive_value %in% rownames(table_data) & deceptive_value %in% colnames(table_data),
                 table_data[deceptive_value, deceptive_value], 0)
  misses <- ifelse(deceptive_value %in% rownames(table_data) & honest_value %in% colnames(table_data),
                   table_data[deceptive_value, honest_value], 0)
  false_alarms <- ifelse(honest_value %in% rownames(table_data) & deceptive_value %in% colnames(table_data),
                         table_data[honest_value, deceptive_value], 0)
  correct_rejections <- ifelse(honest_value %in% rownames(table_data) & honest_value %in% colnames(table_data),
                               table_data[honest_value, honest_value], 0)
  
  # Calculate rates
  hit_rate <- ifelse((hits + misses) > 0, hits / (hits + misses), NA)
  false_alarm_rate <- ifelse((false_alarms + correct_rejections) > 0,
                             false_alarms / (false_alarms + correct_rejections), NA)
  
  # Calculate d-prime
  d_prime <- ifelse(!is.na(hit_rate) & !is.na(false_alarm_rate) &
                    hit_rate > 0 & hit_rate < 1 & false_alarm_rate > 0 & false_alarm_rate < 1,
                    qnorm(hit_rate) - qnorm(false_alarm_rate), NA)
  
  # Calculate a-prime (non-parametric sensitivity measure)
  a_prime <- ifelse(!is.na(hit_rate) & !is.na(false_alarm_rate),
                    ifelse(hit_rate >= false_alarm_rate,
                           0.5 + ((hit_rate - false_alarm_rate) * (1 + hit_rate - false_alarm_rate)) /
                             (4 * hit_rate * (1 - false_alarm_rate)),
                           0.5 - ((false_alarm_rate - hit_rate) * (1 + false_alarm_rate - hit_rate)) /
                             (4 * false_alarm_rate * (1 - hit_rate))), NA)
  
  # Calculate beta (response bias - likelihood ratio at criterion)
  beta <- ifelse(!is.na(hit_rate) & !is.na(false_alarm_rate) &
                 hit_rate > 0 & hit_rate < 1 & false_alarm_rate > 0 & false_alarm_rate < 1,
                 exp((qnorm(false_alarm_rate)^2 - qnorm(hit_rate)^2) / 2), NA)
  
  # Calculate b-double prime (bppd) - non-parametric response bias measure
  bppd <- ifelse(!is.na(hit_rate) & !is.na(false_alarm_rate),
                 (hit_rate * (1 - hit_rate) - false_alarm_rate * (1 - false_alarm_rate)) /
                 (hit_rate * (1 - hit_rate) + false_alarm_rate * (1 - false_alarm_rate)), NA)
  
  # Calculate criterion (c - response bias measure)
  criterion <- ifelse(!is.na(hit_rate) & !is.na(false_alarm_rate) &
                      hit_rate > 0 & hit_rate < 1 & false_alarm_rate > 0 & false_alarm_rate < 1,
                      -0.5 * (qnorm(hit_rate) + qnorm(false_alarm_rate)), NA)
  
  return(data.frame(
    n_total = n_total,
    n_valid = n_valid,
    accuracy = round(accuracy, 4),
    truth_bias = round(truth_bias, 4),
    truth_accuracy = round(truth_accuracy, 4),
    lie_accuracy = round(lie_accuracy, 4),
    hits = hits,
    misses = misses,
    false_alarms = false_alarms,
    correct_rejections = correct_rejections,
    hit_rate = round(hit_rate, 4),
    false_alarm_rate = round(false_alarm_rate, 4),
    d_prime = round(d_prime, 4),
    a_prime = round(a_prime, 4),
    beta = round(beta, 4),
    bppd = round(bppd, 4),
    criterion = round(criterion, 4),
    honest_value = honest_value,
    deceptive_value = deceptive_value
  ))
}