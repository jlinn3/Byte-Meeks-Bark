rabies_positive <- filter(rabies, TestResult == "Positive")

# We need to convert Month to a factor with ordered levels to ensure correct ordering in the time series plot, since our excel has data all over
rabies_positive$Month <- factor(rabies_positive$Month, levels = c("January", "February", "March"), ordered = TRUE)

# Let's create a time series plot for positive rabies cases
time_series_plot <- ggplot(rabies_positive, aes(x = Month, group = Year, color = as.factor(Year))) +
  geom_line(stat = "count") +
  labs(title = "Temporal Trends in Positive Rabies Cases",
       x = "Month",
       y = "Count",
       color = "Year") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability

# Show plot
time_series_plot
