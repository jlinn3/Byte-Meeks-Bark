summary2022 <- summary(subset(rabies, Year == 2022))
summary2022
rabies22 <- subset(rabies, Year == 2022)
rabies22pos <- subset(rabies, Year == 2022 & TestResult == "Positive")

# Let's create a bar plot for only the year 2022
ggplot(rabies22pos, aes(x = Month)) +
  geom_bar(fill = "red", stat = "count") +
  labs(title = "Distribution of Rabies Positive Cases in 2022 by Month",
       x = "Month",
       y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels 
rabies22neg <- subset(rabies, Year == 2022 & TestResult == "Negative")

# Let's create a bar plot for only the year 2022
ggplot(rabies22neg, aes(x = Month)) +
  geom_bar(fill = "green", stat = "count") +
  labs(title = "Distribution of Rabies Negative Cases in 2022 by Month",
       x = "Month",
       y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels 
# Filter the dataset for 2022 and positive test results.
rabies22_positive <- subset(rabies, Year == 2022 & TestResult == "Positive")

# Now we count the number of positive cases for each county
county_rabies_cases <- table(rabies22_positive$County)

# Find the county with the most positive cases
highest_county <- names(which.max(county_rabies_cases))

# Find the county with the least positive cases
lowest_county <- names(which.min(county_rabies_cases))

# Display the result
cat("County with the most positive cases in 2022:", highest_county, "\n")
cat("County with the least positive cases in 2022:", lowest_county, "\n")

ggplot(rabies22, aes(x = Animal)) +
  geom_bar(fill = "skyblue", stat = "count") +
  labs(title = "Distribution of Rabies Cases by Animal Type in 2022",
       x = "Animal Type",
       y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels 

ggplot(rabies22, aes(x = Animal)) +
  geom_bar(fill = "skyblue", stat = "count") +
  labs(title = "Distribution of Rabies Cases by Animal Type by Months of 2022",
       x = "Animal Type",
       y = "Count") +
  facet_wrap(~ Month) +  # Facet by month
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability

# Create the bar plot for the distribution of positive rabies cases by animal type
ggplot(rabies22_positive, aes(x = Animal)) +
  geom_bar(fill = "skyblue", stat = "count") +
  labs(title = "Distribution of Positive Rabies Cases by Animal Type by Months of 2022",
       x = "Animal Type",
       y = "Count") +
  facet_wrap(~ Month) +  # Facet by month
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels
ggplot(rabies22_positive, aes(x = factor(PublicHealthRegion))) +
  geom_bar(fill = "skyblue", stat = "count") +
  labs(title = "Distribution of Positive Rabies Cases by Public Health Region",
       x = "Public Health Region",
       y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability

phr_positive_cases <- table(rabies22_positive$PublicHealthRegion)

# Find the PublicHealthRegion with the most positive cases
most_positive_phr <- names(which.max(phr_positive_cases))

most_positive_phr

# Find the PublicHealthRegion with the least positive cases
least_positive_phr <- names(which.min(phr_positive_cases))

least_positive_phr

phr_cases <- table(rabies$PublicHealthRegion)

# Find the PublicHealthRegion with the most cases
most_cases_phr <- names(which.max(phr_cases))

most_cases_phr

# Find the PublicHealthRegion with the least cases
least_cases_phr <- names(which.min(phr_cases))

least_cases_phr

summary2023 <- summary(subset(rabies, Year == 2023))
summary2023

rabies23pos <- subset(rabies, Year == 2023 & TestResult == "Positive")

# Let's create a bar plot for only the year 2023
ggplot(rabies23pos, aes(x = Month)) +
  geom_bar(fill = "red", stat = "count") +
  labs(title = "Distribution of Rabies Positive Cases in 2023 by Month",
       x = "Month",
       y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels 
rabies23neg <- subset(rabies, Year == 2023 & TestResult == "Negative")

# Let's create a bar plot for only the year 2023
ggplot(rabies23neg, aes(x = Month)) +
  geom_bar(fill = "green", stat = "count") +
  labs(title = "Distribution of Rabies Negative Cases in 2023 by Month",
       x = "Month",
       y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels 
county_rabies_cases23 <- table(rabies23pos$County)

# Find the county with the most positive cases
highest_county23 <- names(which.max(county_rabies_cases23))

# Find the county with the least positive cases
lowest_county23 <- names(which.min(county_rabies_cases23))

# Display the result
cat("County with the most positive cases in 2023:", highest_county23, "\n")

cat("County with the least positive cases in 2023:", lowest_county23, "\n")

rabies23 <- subset(rabies, Year == 2023)

ggplot(rabies23, aes(x = Animal)) +
  geom_bar(fill = "skyblue", stat = "count") +
  labs(title = "Distribution of Rabies Cases by Animal Type in 2023",
       x = "Animal Type",
       y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels 

ggplot(rabies23, aes(x = Animal)) +
  geom_bar(fill = "skyblue", stat = "count") +
  labs(title = "Distribution of Rabies Cases by Animal Type by Months of 2023",
       x = "Animal Type",
       y = "Count") +
  facet_wrap(~ Month) +  # Facet by month
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability
# Create the bar plot for the distribution of positive rabies cases by animal type
ggplot(rabies23pos, aes(x = Animal)) +
  geom_bar(fill = "skyblue", stat = "count") +
  labs(title = "Distribution of Positive Rabies Cases by Animal Type by Months of 2023",
       x = "Animal Type",
       y = "Count") +
  facet_wrap(~ Month) +  # Facet by month
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels

ggplot(rabies23pos, aes(x = factor(PublicHealthRegion))) +
  geom_bar(fill = "skyblue", stat = "count") +
  labs(title = "Distribution of Positive Rabies Cases by Public Health Region",
       x = "Public Health Region",
       y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability

phr_positive_cases23 <- table(rabies23pos$PublicHealthRegion)

# Find the PublicHealthRegion with the most positive cases
most_positive_phr23 <- names(which.max(phr_positive_cases23))

most_positive_phr23

# Find the PublicHealthRegion with the most positive cases
least_positive_phr23 <- names(which.min(phr_positive_cases23))

least_positive_phr23

rabies_2023 <- subset(rabies, Year == 2023)

# Let's count the number of cases for each PublicHealthRegion in 2023
phr_cases_2023 <- table(rabies_2023$PublicHealthRegion)

print(phr_cases_2023)

# Find the PublicHealthRegion with the least cases
least_cases_phr23 <- names(which.min(phr_cases_2023))

least_cases_phr23

# Find the PublicHealthRegion with the least cases
most_cases_phr23 <- names(which.max(phr_cases_2023))

most_cases_phr23

analysis_positive_negative <- function(data, year) {
  analysis <- data %>%
    group_by(Year, TestResult) %>%
    summarize(Count = n())
  analysis$Year <- as.factor(year)
  return(analysis)
}

# Make a bar graph for each year
positive_negative_plot_2022 <- ggplot(analysis_positive_negative(rabies22, 2022), aes(x = TestResult, y = Count, fill = TestResult)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Distribution of Positive and Negative Rabies Cases in 2022",
       x = "Test Result",
       y = "Count") +
  scale_fill_manual(values = c("Positive" = "red", "Negative" = "green")) +  # Set colors for positive and negative cases
  theme_minimal()

positive_negative_plot_2023 <- ggplot(analysis_positive_negative(rabies23, 2023), aes(x = TestResult, y = Count, fill = TestResult)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Distribution of Positive and Negative Rabies Cases in 2023",
       x = "Test Result",
       y = "Count") +
  scale_fill_manual(values = c("Positive" = "red", "Negative" = "green")) +  # Set colors for positive and negative cases
  theme_minimal()

# Display the plots
positive_negative_plot_2022
positive_negative_plot_2023

count_cases <- function(data) {
  cases <- nrow(data)
  return(cases)
}
cases_2022 <- count_cases(rabies22)
cases_2023 <- count_cases(rabies23)

# Print results
print("Year 2022:")
print(cases_2022)
print("Year 2023:")
print(cases_2023)
