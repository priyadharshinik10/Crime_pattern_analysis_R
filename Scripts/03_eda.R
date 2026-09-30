# ==============================================================================
# Script 03: Exploratory Data Analysis (EDA) and Visualization
# Project: Crime Pattern Analysis and Crime Risk Prediction in India
# ==============================================================================

cat("----------------------------------------------------------------------\n")
cat("STEP 3: EXPLORATORY DATA ANALYSIS (EDA)\n")
cat("----------------------------------------------------------------------\n")

# Load required packages
suppressPackageStartupMessages({
  library(ggplot2)
  library(dplyr)
})

# Create Graphs directory if not present
if (!dir.exists("Graphs")) {
  dir.create("Graphs")
}

# Import dataset
data_path <- "Output/cleaned_crime_data.csv"
if (!file.exists(data_path)) {
  data_path <- "Data/india_district_crime_2014_2023_30k.csv"
}

crime <- read.csv(data_path, stringsAsFactors = FALSE)

# Ensure engineered features exist if running EDA independently
if (!"Conviction_Rate" %in% colnames(crime)) {
  crime$Conviction_Rate <- ifelse(crime$Cases_Reported > 0, crime$Convictions / crime$Cases_Reported, 0)
}
if (!"Chargesheet_Rate" %in% colnames(crime)) {
  crime$Chargesheet_Rate <- ifelse(crime$Cases_Reported > 0, crime$Chargesheeted / crime$Cases_Reported, 0)
}
if (!"Crime_Risk" %in% colnames(crime)) {
  q <- quantile(crime$Crime_Rate_per_100k, probs = c(0.3333, 0.6667), na.rm = TRUE)
  crime$Crime_Risk <- cut(crime$Crime_Rate_per_100k, breaks = c(-Inf, q[1], q[2], Inf), labels = c("Low", "Medium", "High"))
}

# Common custom theme settings for clean academic output
academic_theme <- theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5, margin = margin(b = 10)),
    plot.subtitle = element_text(size = 10, hjust = 0.5, color = "gray30", margin = margin(b = 10)),
    axis.title = element_text(face = "bold", size = 11),
    axis.text = element_text(size = 10),
    panel.grid.minor = element_blank(),
    legend.position = "bottom"
  )

# ------------------------------------------------------------------------------
# 1. Total Crime Cases Reported by Year
# ------------------------------------------------------------------------------
cat("Generating Graph 1: Crime cases by year...\n")
g1_data <- crime %>%
  group_by(Year) %>%
  summarise(Total_Cases = sum(Cases_Reported, na.rm = TRUE))

p1 <- ggplot(g1_data, aes(x = factor(Year), y = Total_Cases / 100000)) +
  geom_col(fill = "#2c3e50", width = 0.6) +
  geom_text(aes(label = sprintf("%.2fL", Total_Cases / 100000)), vjust = -0.5, size = 3.5) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(
    title = "1. Total Crime Cases Reported by Year in India (2014-2023)",
    subtitle = "Aggregated national cases reported across 300 districts (in Lakhs)",
    x = "Year",
    y = "Cases Reported (Lakhs)"
  ) +
  academic_theme

ggsave("Graphs/01_crime_cases_by_year.png", p1, width = 8, height = 5, dpi = 300)

# ------------------------------------------------------------------------------
# 2. Average Crime Rate per 100k Population by Year
# ------------------------------------------------------------------------------
cat("Generating Graph 2: Crime rate by year...\n")
g2_data <- crime %>%
  group_by(Year) %>%
  summarise(Avg_Crime_Rate = mean(Crime_Rate_per_100k, na.rm = TRUE))

p2 <- ggplot(g2_data, aes(x = Year, y = Avg_Crime_Rate)) +
  geom_line(color = "#e74c3c", linewidth = 1.2) +
  geom_point(color = "#c0392b", size = 3) +
  geom_text(aes(label = sprintf("%.2f", Avg_Crime_Rate)), vjust = -1, size = 3.5) +
  scale_x_continuous(breaks = 2014:2023) +
  scale_y_continuous(limits = c(min(g2_data$Avg_Crime_Rate) - 2, max(g2_data$Avg_Crime_Rate) + 2)) +
  labs(
    title = "2. Average Crime Rate per 100k Population by Year",
    subtitle = "Annual trend of mean district-level crime rate",
    x = "Year",
    y = "Average Crime Rate per 100k"
  ) +
  academic_theme

ggsave("Graphs/02_crime_rate_by_year.png", p2, width = 8, height = 5, dpi = 300)

# ------------------------------------------------------------------------------
# 3. Top 10 States by Average Crime Rate
# ------------------------------------------------------------------------------
cat("Generating Graph 3: Top 10 states by crime rate...\n")
g3_data <- crime %>%
  group_by(State) %>%
  summarise(Avg_Crime_Rate = mean(Crime_Rate_per_100k, na.rm = TRUE)) %>%
  arrange(desc(Avg_Crime_Rate)) %>%
  head(10)

p3 <- ggplot(g3_data, aes(x = reorder(State, Avg_Crime_Rate), y = Avg_Crime_Rate)) +
  geom_col(fill = "#3498db", width = 0.7) +
  coord_flip() +
  geom_text(aes(label = sprintf("%.2f", Avg_Crime_Rate)), hjust = -0.1, size = 3.5) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(
    title = "3. Top States by Average Crime Rate per 100k Population",
    subtitle = "State-level comparison of mean crime rate per 100k (2014-2023)",
    x = "State",
    y = "Average Crime Rate per 100k"
  ) +
  academic_theme

ggsave("Graphs/03_top10_states_crime_rate.png", p3, width = 8, height = 5, dpi = 300)

# ------------------------------------------------------------------------------
# 4. Total Crime Cases Reported by State
# ------------------------------------------------------------------------------
cat("Generating Graph 4: Crime cases by state...\n")
g4_data <- crime %>%
  group_by(State) %>%
  summarise(Total_Cases = sum(Cases_Reported, na.rm = TRUE)) %>%
  arrange(desc(Total_Cases))

p4 <- ggplot(g4_data, aes(x = reorder(State, Total_Cases), y = Total_Cases / 100000)) +
  geom_col(fill = "#16a085", width = 0.7) +
  coord_flip() +
  geom_text(aes(label = sprintf("%.2fL", Total_Cases / 100000)), hjust = -0.1, size = 3.5) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(
    title = "4. Total Crime Cases Reported by State",
    subtitle = "Cumulative reported cases across 10 years (in Lakhs)",
    x = "State",
    y = "Total Cases Reported (Lakhs)"
  ) +
  academic_theme

ggsave("Graphs/04_crime_cases_by_state.png", p4, width = 8, height = 5, dpi = 300)

# ------------------------------------------------------------------------------
# 5. Crime Type Distribution (Total Cases by Category)
# ------------------------------------------------------------------------------
cat("Generating Graph 5: Crime type distribution...\n")
g5_data <- crime %>%
  group_by(Crime_Type) %>%
  summarise(Total_Cases = sum(Cases_Reported, na.rm = TRUE)) %>%
  arrange(desc(Total_Cases))

p5 <- ggplot(g5_data, aes(x = reorder(Crime_Type, Total_Cases), y = Total_Cases / 100000)) +
  geom_col(fill = "#8e44ad", width = 0.7) +
  coord_flip() +
  geom_text(aes(label = sprintf("%.2fL", Total_Cases / 100000)), hjust = -0.1, size = 3.5) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(
    title = "5. Crime Type Distribution in India (2014-2023)",
    subtitle = "Total cases reported by crime category (in Lakhs)",
    x = "Crime Category",
    y = "Total Cases Reported (Lakhs)"
  ) +
  academic_theme

ggsave("Graphs/05_crime_type_distribution.png", p5, width = 8, height = 5, dpi = 300)

# ------------------------------------------------------------------------------
# 6. Crime Type Trend Over Years
# ------------------------------------------------------------------------------
cat("Generating Graph 6: Crime type trend over years...\n")
g6_data <- crime %>%
  group_by(Year, Crime_Type) %>%
  summarise(Total_Cases = sum(Cases_Reported, na.rm = TRUE), .groups = "drop")

p6 <- ggplot(g6_data, aes(x = Year, y = Total_Cases / 1000, color = Crime_Type)) +
  geom_line(linewidth = 1) +
  scale_x_continuous(breaks = 2014:2023) +
  labs(
    title = "6. Annual Trend of Reported Cases by Crime Type",
    subtitle = "Comparison of major crime categories over time (in Thousands)",
    x = "Year",
    y = "Total Cases Reported (Thousands)",
    color = "Crime Type"
  ) +
  academic_theme

ggsave("Graphs/06_crime_type_trend_over_years.png", p6, width = 10, height = 6, dpi = 300)

# ------------------------------------------------------------------------------
# 7. Top 10 Districts by Crime Rate
# ------------------------------------------------------------------------------
cat("Generating Graph 7: Top 10 districts by crime rate...\n")
g7_data <- crime %>%
  group_by(District, State) %>%
  summarise(Avg_Crime_Rate = mean(Crime_Rate_per_100k, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(Avg_Crime_Rate)) %>%
  head(10) %>%
  mutate(District_Label = paste0(District, " (", State, ")"))

p7 <- ggplot(g7_data, aes(x = reorder(District_Label, Avg_Crime_Rate), y = Avg_Crime_Rate)) +
  geom_col(fill = "#d35400", width = 0.7) +
  coord_flip() +
  geom_text(aes(label = sprintf("%.2f", Avg_Crime_Rate)), hjust = -0.1, size = 3.5) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(
    title = "7. Top 10 Highest Crime Rate Districts in India",
    subtitle = "Mean district crime rate per 100k population (2014-2023)",
    x = "District (State)",
    y = "Average Crime Rate per 100k"
  ) +
  academic_theme

ggsave("Graphs/07_top10_districts_crime_rate.png", p7, width = 8, height = 5, dpi = 300)

# ------------------------------------------------------------------------------
# 8. Population vs Crime Rate Scatter Plot
# ------------------------------------------------------------------------------
cat("Generating Graph 8: Population vs crime rate scatter plot...\n")
p8 <- ggplot(crime, aes(x = Population / 1000000, y = Crime_Rate_per_100k)) +
  geom_point(alpha = 0.2, color = "#2980b9", size = 1.5) +
  geom_smooth(method = "lm", color = "#e74c3c", se = TRUE) +
  labs(
    title = "8. District Population vs Crime Rate per 100k",
    subtitle = "Scatter plot with linear regression trend line",
    x = "District Population (in Millions)",
    y = "Crime Rate per 100k"
  ) +
  academic_theme

ggsave("Graphs/08_population_vs_crime_rate.png", p8, width = 8, height = 5, dpi = 300)

# ------------------------------------------------------------------------------
# 9. Conviction Rate by Year
# ------------------------------------------------------------------------------
cat("Generating Graph 9: Conviction rate by year...\n")
g9_data <- crime %>%
  group_by(Year) %>%
  summarise(
    Total_Cases = sum(Cases_Reported, na.rm = TRUE),
    Total_Convictions = sum(Convictions, na.rm = TRUE),
    Overall_Conviction_Rate = (Total_Convictions / Total_Cases) * 100
  )

p9 <- ggplot(g9_data, aes(x = Year, y = Overall_Conviction_Rate)) +
  geom_line(color = "#27ae60", linewidth = 1.2) +
  geom_point(color = "#2e7d32", size = 3) +
  geom_text(aes(label = sprintf("%.2f%%", Overall_Conviction_Rate)), vjust = -1, size = 3.5) +
  scale_x_continuous(breaks = 2014:2023) +
  scale_y_continuous(limits = c(min(g9_data$Overall_Conviction_Rate) - 2, max(g9_data$Overall_Conviction_Rate) + 2)) +
  labs(
    title = "9. National Conviction Rate Trend by Year",
    subtitle = "Percentage of reported cases resulting in conviction",
    x = "Year",
    y = "Conviction Rate (%)"
  ) +
  academic_theme

ggsave("Graphs/09_conviction_rate_by_year.png", p9, width = 8, height = 5, dpi = 300)

# ------------------------------------------------------------------------------
# 10. Chargesheeting Rate by Year
# ------------------------------------------------------------------------------
cat("Generating Graph 10: Chargesheeting rate by year...\n")
g10_data <- crime %>%
  group_by(Year) %>%
  summarise(
    Total_Cases = sum(Cases_Reported, na.rm = TRUE),
    Total_Chargesheeted = sum(Chargesheeted, na.rm = TRUE),
    Overall_Chargesheet_Rate = (Total_Chargesheeted / Total_Cases) * 100
  )

p10 <- ggplot(g10_data, aes(x = Year, y = Overall_Chargesheet_Rate)) +
  geom_line(color = "#f39c12", linewidth = 1.2) +
  geom_point(color = "#d35400", size = 3) +
  geom_text(aes(label = sprintf("%.2f%%", Overall_Chargesheet_Rate)), vjust = -1, size = 3.5) +
  scale_x_continuous(breaks = 2014:2023) +
  scale_y_continuous(limits = c(min(g10_data$Overall_Chargesheet_Rate) - 2, max(g10_data$Overall_Chargesheet_Rate) + 2)) +
  labs(
    title = "10. National Chargesheeting Rate Trend by Year",
    subtitle = "Percentage of reported cases chargesheeted by law enforcement",
    x = "Year",
    y = "Chargesheeting Rate (%)"
  ) +
  academic_theme

ggsave("Graphs/10_chargesheeting_rate_by_year.png", p10, width = 8, height = 5, dpi = 300)

# ------------------------------------------------------------------------------
# 11. State-wise Crime Trend Over Years
# ------------------------------------------------------------------------------
cat("Generating Graph 11: State-wise crime trend...\n")
g11_data <- crime %>%
  group_by(Year, State) %>%
  summarise(Avg_Crime_Rate = mean(Crime_Rate_per_100k, na.rm = TRUE), .groups = "drop")

p11 <- ggplot(g11_data, aes(x = Year, y = Avg_Crime_Rate, color = State)) +
  geom_line(linewidth = 0.9) +
  scale_x_continuous(breaks = c(2014, 2017, 2020, 2023)) +
  facet_wrap(~ State, ncol = 4) +
  labs(
    title = "11. State-wise Average Crime Rate Trend (2014-2023)",
    subtitle = "Annual crime rate trajectory faceted by state",
    x = "Year",
    y = "Average Crime Rate per 100k"
  ) +
  academic_theme +
  theme(legend.position = "none")

ggsave("Graphs/11_statewise_crime_trend.png", p11, width = 10, height = 7, dpi = 300)

# ------------------------------------------------------------------------------
# 12. Crime Type vs Cases Reported (Boxplot Distribution)
# ------------------------------------------------------------------------------
cat("Generating Graph 12: Crime type vs cases reported...\n")
p12 <- ggplot(crime, aes(x = reorder(Crime_Type, Cases_Reported, FUN = median), y = Cases_Reported, fill = Crime_Type)) +
  geom_boxplot(outlier.alpha = 0.2, show.legend = FALSE) +
  coord_flip() +
  labs(
    title = "12. Distribution of Cases Reported by Crime Type",
    subtitle = "Boxplot showing median, IQR, and outliers across district observations",
    x = "Crime Type",
    y = "Cases Reported per Observation"
  ) +
  academic_theme

ggsave("Graphs/12_crime_type_vs_cases.png", p12, width = 8, height = 5, dpi = 300)

# ------------------------------------------------------------------------------
# 13. Crime Rate Distribution (Histogram + Density)
# ------------------------------------------------------------------------------
cat("Generating Graph 13: Crime rate distribution...\n")
p13 <- ggplot(crime, aes(x = Crime_Rate_per_100k)) +
  geom_histogram(aes(y = after_stat(density)), binwidth = 3, fill = "#34495e", color = "white", alpha = 0.7) +
  geom_density(color = "#e74c3c", linewidth = 1.2) +
  labs(
    title = "13. Distribution of Crime Rate per 100k Population",
    subtitle = "Histogram with kernel density estimation overlay",
    x = "Crime Rate per 100k",
    y = "Density"
  ) +
  academic_theme

ggsave("Graphs/13_crime_rate_distribution.png", p13, width = 8, height = 5, dpi = 300)

# ------------------------------------------------------------------------------
# 14. Crime Risk Category Distribution
# ------------------------------------------------------------------------------
cat("Generating Graph 14: Crime risk category distribution...\n")
g14_data <- crime %>%
  group_by(Crime_Risk) %>%
  summarise(Count = n())

p14 <- ggplot(g14_data, aes(x = Crime_Risk, y = Count, fill = Crime_Risk)) +
  geom_col(width = 0.6, show.legend = FALSE) +
  scale_fill_manual(values = c("Low" = "#2ecc71", "Medium" = "#f39c12", "High" = "#e74c3c")) +
  geom_text(aes(label = sprintf("%d\n(%.1f%%)", Count, Count/sum(Count)*100)), vjust = -0.3, size = 4) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(
    title = "14. Distribution of Crime Risk Categories",
    subtitle = "Data-driven classification based on 33rd and 66th percentile quantiles",
    x = "Crime Risk Tier",
    y = "Record Count"
  ) +
  academic_theme

ggsave("Graphs/14_crime_risk_category_distribution.png", p14, width = 8, height = 5, dpi = 300)

cat("All 14 EDA graphs generated and saved successfully to Graphs/\n")
cat("----------------------------------------------------------------------\n")
cat("STEP 3 COMPLETED SUCCESSFULLY!\n")
cat("----------------------------------------------------------------------\n\n")
