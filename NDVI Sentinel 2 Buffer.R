# Install and load necessary packages
if (!requireNamespace("terra", quietly = TRUE)) {
  install.packages("terra", dependencies = TRUE)
}
if (!requireNamespace("Kendall", quietly = TRUE)) {
  install.packages("Kendall", dependencies = TRUE)
}
if (!requireNamespace("ggplot2", quietly = TRUE)) {
  install.packages("ggplot2", dependencies = TRUE)
}
library(terra)
library(Kendall)
library(ggplot2)

# Set the working directory
setwd("here")

# List NDVI GeoTIFF files
ndvi_files <- list.files(pattern = "\\.tif$", full.names = TRUE)
if (length(ndvi_files) == 0) {
  stop("No TIFF files found in the directory.")
}

# Load and stack NDVI rasters using terra
ndvi_stack <- rast(ndvi_files)

# Function to apply the Mann-Kendall test and return tau and p-value
apply_mk_test <- function(data) {
  data <- na.omit(data)
  if (length(data) < 3) {
    return(c(tau=NA, p.value=NA))
  } else {
    mk_result <- Kendall::MannKendall(data)
    return(c(tau=mk_result$tau, p.value=mk_result$sl))
  }
}

# Calculate trends and p-values for each pixel
mk_results <- app(ndvi_stack, apply_mk_test)

# Extract tau and p-value maps
tau_map <- mk_results[[1]]
p_value_map <- mk_results[[2]]

# Define a continuous color palette for the trend map
colors_trend <- colorRampPalette(c("red", "white", "blue"))(100)

# Plot NDVI Trend Map using Kendall's Tau
png("ndvi_trend_map.png", width=900, height=600)
plot(tau_map, main="NDVI Trend Map (Kendall's Tau)", col=colors_trend, legend=TRUE)
dev.off()


# Define colors and breaks for significant p-value visualization
colors_pvalue <- c("red", "orange", "yellow", "white")  # Map colors to intervals
breaks_pvalue <- c(0, 0.01, 0.05, 0.1, 1)  # Adjusted breaks for the specified intervals

# Create conditional maps for significant p-values
significant_positive_p_values <- ifel(tau_map > 0 & p_value_map < 1, p_value_map, NA)
significant_negative_p_values <- ifel(tau_map < 0 & p_value_map < 1, p_value_map, NA)

# Plotting Significant P-Value Maps for Positive Tau Values
png("significant_positive_p_values.png", width=900, height=600)
plot(significant_positive_p_values, col=colors_pvalue, breaks=breaks_pvalue, legend=TRUE, main="Significant Positive Tau P-values")
dev.off()

# Plotting Significant P-Value Maps for Negative Tau Values
png("significant_negative_p_values.png", width=900, height=600)
plot(significant_negative_p_values, col=colors_pvalue, breaks=breaks_pvalue, legend=TRUE, main="Significant Negative Tau P-values")
dev.off()



# Table

# Extract all p-value data as a numeric vector
p_values_numeric <- values(p_value_map)
tau_values_numeric <- values(tau_map)

# Define positive and negative p-values based on tau values
positive_p_values <- p_values_numeric[tau_values_numeric > 0]
negative_p_values <- p_values_numeric[tau_values_numeric < 0]
# Calculate counts for positive p-values
count_positive_lt_001 <- sum(positive_p_values < 0.01, na.rm = TRUE)
count_positive_001_to_005 <- sum(positive_p_values >= 0.01 & positive_p_values < 0.05, na.rm = TRUE)
count_positive_005_to_01 <- sum(positive_p_values >= 0.05 & positive_p_values < 0.1, na.rm = TRUE)
count_positive_gt_01 <- sum(positive_p_values >= 0.1, na.rm = TRUE)  # p-values > 0.1

# Calculate counts for negative p-values
count_negative_lt_001 <- sum(negative_p_values < 0.01, na.rm = TRUE)
count_negative_001_to_005 <- sum(negative_p_values >= 0.01 & negative_p_values < 0.05, na.rm = TRUE)
count_negative_005_to_01 <- sum(negative_p_values >= 0.05 & negative_p_values < 0.1, na.rm = TRUE)
count_negative_gt_01 <- sum(negative_p_values >= 0.1, na.rm = TRUE)  # p-values > 0.1

# Calculate total number of significant pixels for both conditions
total_positive_pixels <- length(na.omit(positive_p_values))
total_negative_pixels <- length(na.omit(negative_p_values))

# Create tables of percentages
percentage_table_positive <- data.frame(
  P_Value_Range = c("< 0.01", "0.01 - 0.05", "0.05 - 0.1", "> 0.1"),
  Count = c(count_positive_lt_001, count_positive_001_to_005, count_positive_005_to_01, count_positive_gt_01),
  Percentage = c(count_positive_lt_001 / total_positive_pixels * 100,
                 count_positive_001_to_005 / total_positive_pixels * 100,
                 count_positive_005_to_01 / total_positive_pixels * 100,
                 count_positive_gt_01 / total_positive_pixels * 100)
)

percentage_table_negative <- data.frame(
  P_Value_Range = c("< 0.01", "0.01 - 0.05", "0.05 - 0.1", "> 0.1"),
  Count = c(count_negative_lt_001, count_negative_001_to_005, count_negative_005_to_01, count_negative_gt_01),
  Percentage = c(count_negative_lt_001 / total_negative_pixels * 100,
                 count_negative_001_to_005 / total_negative_pixels * 100,
                 count_negative_005_to_01 / total_negative_pixels * 100,
                 count_negative_gt_01 / total_negative_pixels * 100)
)

# Print the tables
print("Positive Tau P-Values:")
print(percentage_table_positive)
print("Negative Tau P-Values:")
print(percentage_table_negative)

