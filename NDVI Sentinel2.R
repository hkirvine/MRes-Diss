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

# Check for NA values in the loaded data
#na_check <- sum(is.na(values(ndvi_stack)))
#print(paste("Number of NA values in the stack: ", na_check))


#Frag stats

#library(terra)  # Ensure the terra library is loaded
# Reproject the tau_map to UTM zone 38N
#tau_map_metric <- project(tau_map, "+proj=utm +zone=38 +datum=WGS84 +units=m +no_defs")

# Confirm the new CRS
#print(crs(tau_map_metric))

# Proceed with further analysis
# Categorize tau values for landscape metrics
#tau_classes <- cut(values(tau_map_metric), breaks = c(-Inf, -0.5, -0.1, 0.1, 0.5, Inf),
#                   labels = c("Strong decrease", "Moderate decrease", "Stable", "Moderate increase", "Strong increase"))
#tau_categorical <- rast(tau_map_metric)
#values(tau_categorical) <- factor(tau_classes, levels = c("Strong decrease", "Moderate decrease", "Stable", "Moderate increase", "Strong increase"))

# Check landscape suitability for metrics computation using landscapemetrics package

# Load dplyr for data manipulation
#library(dplyr)

# Assuming 'metrics' is your dataframe from landscapemetrics
# Filter for relevant metrics
#selected_metrics <- metrics %>%
#filter(metric %in% c("ai", "area_cv", "area_mn", "area_sd", "circle_mn", "circle_sd"))

# Summarize each metric separately
#summary_metrics <- metrics %>%
#  group_by(metric) %>%
#  summarize(mean_value = mean(value, na.rm = TRUE), .groups = 'drop')

# Print the summarized metrics
#print(summary_metrics)



#library(ggplot2)
#library(dplyr)

# Assuming 'metrics' is your complete dataset
#metrics_to_plot <- c("ai", "area_mn", "cai_mn", "circle_mn")
#filtered_metrics <- metrics %>%
#  filter(metric %in% metrics_to_plot)

# Plot histograms with adjusted bin widths and limits
#ggplot(filtered_metrics, aes(x = value)) +
#  geom_histogram(bins = 30, fill = "skyblue", color = "black") +
#  facet_wrap(~ metric, scales = "free_x") +
#  labs(title = "Distribution of Landscape Metrics", x = "Value", y = "Frequency") +
#  theme_minimal()

# Applying a transformation if the data is highly skewed
#ggplot(filtered_metrics, aes(x = value)) +
#  geom_histogram(bins = 30, fill = "skyblue", color = "black") +
#  scale_x_log10() +  # Applying log transformation to the x-axis
#  facet_wrap(~ metric, scales = "free") +
#  labs(title = "Log-transformed Distribution of Landscape Metrics", x = "Log(Value)", y = "Frequency") +
#  theme_minimal()

# Summary of values for each metric
#filtered_metrics %>%
#  group_by(metric) %>%
#  summarise(min_value = min(value, na.rm = TRUE),
#            max_value = max(value, na.rm = TRUE),
#            mean_value = mean(value, na.rm = TRUE),
#            sd_value = sd(value, na.rm = TRUE))

# Check the unique values in the original tau_map to understand variability
#unique_values <- unique(values(tau_map))
#print(unique_values)

# List all available metrics from your full metrics data
#unique(metrics$metric)

# Assuming you have a spatial plot or way to visualize these metrics on a map
#library(raster)
#plot(tau_map, main="Spatial Distribution of Metric")


# Example of using a basic statistical test to compare areas of high and low NDVI trends
# Hypothetical grouping based on tau value signs
#high_trend_area <- metrics[metrics$value > 0, ]
#low_trend_area <- metrics[metrics$value < 0, ]

# Perform a t-test to see if differences in a metric like 'ai' are statistically significant
#t_test_result <- t.test(high_trend_area$value, low_trend_area$value)

# Print results
#print(t_test_result)
