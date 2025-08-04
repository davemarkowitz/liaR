# liaR

An R package for calculating deception detection accuracy and Signal Detection Theory (SDT) metrics for deception detection research. The package calculates four raw deception detection metrics (i.e., overall accuracy, truth-bias, truth accuracy, lie accuracy) and five signal detection metrics (i.e., d-prime, a-prime, beta, b-double-prime d, criterion). Raw hits, misses, false alarms, and correct rejections are calculated as well.

## Installation

You can install the development version of liaR from GitHub:

```r
# install.packages("devtools")
devtools::install_github("davemarkowitz/liaR")

```
```r
# Load the package
library(liaR)

# Example with sample data
ground_truth <- c("honest", "deceptive", "honest", "deceptive", "honest", "deceptive")
response <- c("honest", "honest", "honest", "deceptive", "deceptive", "deceptive")

# Run analysis
result <- deception_metrics(
ground_truth = ground_truth,
response = response)
print(result)
```
```r
# Load your CSV file
data <- read.csv("your_data.csv")

# Run comprehensive analysis
result <- deception_metrics(
  ground_truth = data$actual,  # column containing ground truth labels
  response = data$predicted,   # column containing participant responses  
  study = data$study_id,       # column containing study labels (optional)
  export_csv = TRUE,           # export results to CSV
  corr_table = TRUE            # create correlation matrix from raw deception detection and SDT metrics
)
```
```r
# View results
print(result)

# Works with different coding schemes automatically
ground_truth1 <- c("truth", "lie", "truth", "lie")
response1 <- c("truth", "truth", "lie", "lie")
result1 <- deception_metrics(
  ground_truth = ground_truth1,
  response = response1
)

# Or specify custom coding
ground_truth2 <- c("A", "B", "A", "B")  
response2 <- c("A", "A", "B", "B")
result2 <- deception_metrics(
  ground_truth = ground_truth2,
  response = response2, 
  honest_value = "A", 
  deceptive_value = "B"
)
```
# liaR Author Notes

This R package was created by David M. Markowitz (dmm@msu.edu). All underlying technical aspects of the code were created by him and prepared for GitHub submission using Claude (e.g., commenting for clarity, code clarity and efficiency, document preparation). Please contact the author with any questions or suggestions for improvement.
