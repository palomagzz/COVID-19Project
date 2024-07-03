#PopHealth3
#Author Paloma gonzalez 


library(tidyverse)
library(knitr)
library(kableExtra)
library(plotly)
library(ggplot2)
library(dplyr)

df_health_mortality <- read.csv('main_oecd_file.csv')
df_stats <- read.csv("summary_stats.csv")
df_merged <- read.csv("merged_data.csv")
df_death <- read.csv("death_rates.csv")
df_percentage <- read.csv("percentage_death.csv")

df_health_mortality$DATE <- as.Date(paste0(df_health_mortality$YEAR, "-", df_health_mortality$WEEK, "-1"), format="%Y-%U-%w")

countries <- c('Australia', 'Austria', 'Belgium', 'Canada', 'Czechia', 'Denmark', 'Finland',
               'France', 'Germany', 'Greece', 'Hungary', 'Iceland', 'Italy', 'Luxembourg',
               'Netherlands', 'New Zealand', 'Norway', 'Poland', 'Portugal',
               'Slovak Republic', 'Spain', 'Sweden', 'Switzerland', 'United Kingdom',
               'United States', 'Chile', 'Estonia', 'Israel', 'Latvia', 'Lithuania',
               'Slovenia', 'Mexico', 'Colombia', 'Ireland', 'Costa Rica')

# death rate in every country from 2015 to 2022 and plots
for (country in countries) {
  country_data <- df_health_mortality %>%
    filter(Country == country, Variable == 'All-cause deaths (number)', YEAR >= 2015, YEAR <= 2022)
  
  total_deaths_by_week <- country_data %>%
    group_by(DATE) %>%
    summarize(total_deaths = sum(Value, na.rm = TRUE))
  
  ggplot(total_deaths_by_week, aes(x = DATE, y = total_deaths)) +
    geom_line(color = "blue", linetype = "solid") +
    labs(title = paste("Mortality Rates in", country, "(2015-2022)"),
         x = "Year",
         y = "Number of Deaths") +
    theme_minimal()
  
  #ggsave(paste("Mortality_Rates_", gsub(" ", "_", country), ".jpg", sep = ""), height = 6, width = 10)
}



#statistics from every country by year (2015-2022)
mortality_data <- df_health_mortality %>%
  filter(Variable == 'All-cause deaths (number)', YEAR >= 2015, YEAR <= 2022)

summary_stats <- mortality_data %>%
  group_by(Country, YEAR) %>%
  summarize(
    mean_mortality = mean(Value, na.rm = TRUE),
    median_mortality = median(Value, na.rm = TRUE),
    sd_mortality = sd(Value, na.rm = TRUE),
    .groups = 'drop'
  )

print(summary_stats)



#all cause deaths by year and country
total_deaths_all_countries <- data.frame(Country = character(), YEAR = integer(), total_deaths = integer())

for (country in countries) {
  country_data <- df_health_mortality %>%
    filter(Country == country, Variable == 'All-cause deaths (number)', YEAR >= 2015, YEAR <= 2022)
  
  total_deaths_by_year <- country_data %>%
    group_by(YEAR) %>%
    summarize(total_deaths = sum(Value, na.rm = TRUE))
  
  total_deaths_all_countries <- bind_rows(total_deaths_all_countries, mutate(total_deaths_by_year, Country = country))
}

write.table(total_deaths_all_countries, file = "total_deaths_by_year_all_countries.txt", sep = "\t", row.names = FALSE)




#NZ death rates
nz_all_cause_deaths <- df_health_mortality %>%
  filter(Country == "New Zealand",
         Variable == "All-cause deaths (number)",
         YEAR >= 2019,
         YEAR <= 2022)

# Convert DATE to date format
nz_all_cause_deaths$DATE <- as.Date(paste0(nz_all_cause_deaths$YEAR, "-", nz_all_cause_deaths$WEEK, "-1"), format = "%Y-%U-%w")

# Group by week and summarize total deaths
nz_weekly_deaths <- nz_all_cause_deaths %>%
  group_by(WEEK, YEAR) %>%
  summarize(total_deaths = sum(Value, na.rm = TRUE))

# View the resulting dataframe
print(nz_weekly_deaths)
write.table(nz_weekly_deaths, file = "nz_weekly.txt", sep = "\t", row.names = FALSE)











##############################################################
#new
deaths <- df_death %>%
  group_by(Country) %>%
  arrange(Country, Year) %>%
  mutate(PercentageChange = (all_cause_deaths - lag(all_cause_deaths)) / lag(all_cause_deaths) * 100) %>%
  filter(!is.na(PercentageChange))

# Print the data with percentage change
print(deaths)
write.csv(deaths, "percentage_death.csv", row.names = FALSE)
write.table(deaths, "percentage.txt", sep = "\t", quote = FALSE, row.names = FALSE)

ggplot(deaths, aes(x = Country, y = PercentageChange, fill = as.factor(Year))) +
  geom_col(position = "dodge", width = 0.8, color = "black") +
  labs(title = "Percentage Change in Mortality Rates",
       x = "Country",
       y = "Percentage Change",
       fill = "Year") +  # Set legend title to "Year"
  scale_y_continuous(breaks = seq(-100, 100, by = 10)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

filtered_data <- deaths %>% 
  filter(Year %in% c(2020, 2021, 2022))

ggplot(filtered_data, aes(x = Country, y = PercentageChange, fill = as.factor(Year))) +
  geom_col(position = "dodge", width = 0.8, color = "black") +
  labs(title = "Percentage Change in Mortality Rates",
       x = "Country",
       y = "Percentage Change",
       fill = "Year") +  # Set legend title to "Year"
  scale_y_continuous(breaks = seq(-100, 100, by = 10)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


filtered_data_p <- df_percentage %>%
  filter(Year == 2022)

top_countries_2020 <- filtered_data_p %>%
  group_by(Country) %>%
  summarise(TotalPercentageChange = sum(PercentageChange, na.rm = TRUE)) %>%
  arrange(desc(TotalPercentageChange)) %>%
  top_n(38)

print(top_countries_2020)

write.table(top_countries_2020, "highest21.txt", sep = "\t", quote = FALSE, row.names = FALSE)



filtered_data_2020 <- df_percentage %>%
  filter(Year == 2020)

top_countries_decrease_2020 <- filtered_data_2020 %>%
  arrange(PercentageChange) %>%
  top_n(6)

print(top_countries_decrease_2020)

write.table(top_countries_decrease_2020, "low22.txt", sep = "\t", quote = FALSE, row.names = FALSE)


mortality_data <- df_death %>%
  filter(all_cause_deaths %>% is.na() %>% `!`, Year >= 2015, Year <= 2022) %>%
  mutate(COVID_period = ifelse(Year >= 2020, "During COVID", "Before COVID"))

summary_stats <- mortality_data %>%
  group_by(Country, COVID_period) %>%
  summarize(
    mean_mortality = mean(all_cause_deaths, na.rm = TRUE),
    median_mortality = median(all_cause_deaths, na.rm = TRUE),
    sd_mortality = sd(all_cause_deaths, na.rm = TRUE),
    .groups = 'drop'
  )

print(summary_stats)
write.table(summary_stats, "summary2groups.txt", sep = "\t", quote = FALSE, row.names = FALSE)
write.csv(summary_stats, "summary2groups.csv", row.names = FALSE)

############################################################################################
#scatter plots for every year all countries 
plots_list <- list()

for (year in unique(df_merged$YEAR)) {
  year_data <- df_merged[df_merged$YEAR == year, ]
  
  plot <- plot_ly(year_data, x = ~Population, y = ~mean_mortality, color = ~Country, text = ~Country, mode = "markers") %>%
    layout(title = paste("All-cause Deaths in", year),
           xaxis = list(title = 'Population'),
           yaxis = list(title = 'Deaths'))
  
  plots_list[[as.character(year)]] <- plot
}

print(plots_list)


#t-test comparing years in death rates for every country
t_test_results_list <- list()

years <- unique(df_health_mortality$YEAR)

for (i in 2:length(years)) {
  year1 <- years[i-1]
  year2 <- years[i]
  
  data_year1 <- df_health_mortality %>%
    filter(Variable == 'All-cause deaths (number)', YEAR == year1)
  
  data_year2 <- df_health_mortality %>%
    filter(Variable == 'All-cause deaths (number)', YEAR == year2)
  
  t_test_result <- t.test(data_year1$Value, data_year2$Value)
  
  t_test_results_list[[paste(year1, "vs.", year2)]] <- t_test_result
  
  cat(paste("T-test for", year1, "vs.", year2, ":\n"))
  print(t_test_result)
  cat("\n")
}

t_test_results_df <- data.frame(
  Comparison = names(t_test_results_list),
  t_value = sapply(t_test_results_list, function(x) x$statistic),
  df = sapply(t_test_results_list, function(x) x$parameter),
  p_value = sapply(t_test_results_list, function(x) x$p.value)
)

#write.table(t_test_results_df, "t_test_results.txt", sep = "\t", quote = FALSE, row.names = FALSE)




#t-test using week by week each country
t_test_results_list <- list()

for (country in countries) {
  
  data_country <- df_health_mortality %>% filter(Country == country, Variable == "All-cause deaths (number)")
  
  years <- unique(data_country$YEAR)
  
  for (i in 2:length(years)) {
    year1 <- years[i-1]
    year2 <- years[i]
    
    data_year1 <- data_country %>% filter(YEAR == year1)
    data_year2 <- data_country %>% filter(YEAR == year2)
    
    t_test_result <- t.test(data_year1$Value, data_year2$Value)
    
    t_test_results_list[[paste(country, year1, "vs.", year2)]] <- t_test_result
    
    cat(paste("T-test for", country, year1, "vs.", year2, ":\n"))
    print(t_test_result)
    cat("\n")
  }
}

t_test_results_df <- data.frame(
  Comparison = names(t_test_results_list),
  t_value = sapply(t_test_results_list, function(x) x$statistic),
  df = sapply(t_test_results_list, function(x) x$parameter),
  p_value = sapply(t_test_results_list, function(x) x$p.value)
)

print(t_test_results_df)
#write.table(t_test_results_df, "t_test_results_df.txt", sep = "\t", quote = FALSE, row.names = FALSE)



##############################################################################################
t_test_results_list <- list()

for (country in countries) {
  
  data_country <- df_health_mortality %>% 
    filter(Country == country, Variable == "All-cause deaths (number)")
  
  # Group 1: 2015-2019
  data_group1 <- data_country %>% filter(YEAR %in% 2015:2019)
  
  # Group 2: 2020-2022
  data_group2 <- data_country %>% filter(YEAR %in% 2020:2022)
  
  # Perform the t-test
  t_test_result <- t.test(data_group1$Value, data_group2$Value)
  
  t_test_results_list[[paste(country, "before COVID-19 vs. during COVID-19")]] <- t_test_result
  
  cat(paste("T-test for", country, "2015-2019 vs. 2020-2022:\n"))
  print(t_test_result)
  cat("\n")
}

# Create the results dataframe
t_test_results_df <- data.frame(
  Comparison = names(t_test_results_list),
  t_value = sapply(t_test_results_list, function(x) x$statistic),
  df = sapply(t_test_results_list, function(x) x$parameter),
  p_value = sapply(t_test_results_list, function(x) x$p.value)
)

print(t_test_results_df)
write.table(t_test_results_df, "t_test_results2periods.txt", sep = "\t", quote = FALSE, row.names = FALSE)


#############################################################################################


#percentage of change(from previous year)
percentage_changes_data <- df_stats %>%
  group_by(Country) %>%
  arrange(Country, YEAR) %>%
  mutate(PercentageChange = (mean_mortality - lag(mean_mortality)) / lag(mean_mortality) * 100) %>%
  filter(!is.na(PercentageChange))


print(percentage_changes_data)
write.table(percentage_changes_data, "percentage_changes_data2.txt", sep = "\t", quote = FALSE, row.names = FALSE)
#write.csv(percentage_changes_data, "percentage_changes.csv", row.names = FALSE)




#plots
ggplot(percentage_changes_data, aes(x = Country, y = PercentageChange, fill = as.factor(YEAR))) +
  geom_col(position = "dodge", width = 0.8, color = "black") +
  labs(title = "Percentage Change in Mortality Rates",
       x = "Country",
       y = "Percentage Change",
       fill = "Year") +  # Set legend title to "Year"
  scale_y_continuous(breaks = seq(-100, 100, by = 10)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))




#percentage for only 2020 to 2022
filtered_data <- percentage_changes_data %>% 
  filter(YEAR %in% c(2020, 2021, 2022))

ggplot(filtered_data, aes(x = Country, y = PercentageChange, fill = as.factor(YEAR))) +
  geom_col(position = "dodge", width = 0.8, color = "black") +
  labs(title = "Percentage Change in Mortality Rates",
       x = "Country",
       y = "Percentage Change",
       fill = "Year") +  # Set legend title to "Year"
  scale_y_continuous(breaks = seq(-100, 100, by = 10)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 9))







#plot of all cause deaths and covid deaths (2020-2022)
filtered_data <- df_health_mortality %>%
  filter(Variable %in% c('All-cause deaths (number)', 'COVID-19 deaths (number)'),
         YEAR %in% c(2020, 2021, 2022))

for (country in countries) {
  country_data <- filtered_data %>% filter(Country == country)
  
  plot <- ggplot(country_data, aes(x = DATE, y = Value, color = Variable)) +
    geom_line() +
    labs(title = paste("Death Rates and COVID Deaths in", country, "(2020-2022)"),
         x = "Date",
         y = "Number of Deaths",
         color = "Variable") +
    theme_minimal()
  
  #ggsave(paste("Death_Rates_and_COVID_Deaths_", gsub(" ", "_", country), ".jpg", sep = ""), plot, height = 6, width = 10)
}



#plots for total deaths and covid deaths in all countries
filtered_data <- df_health_mortality %>%
  filter(Variable %in% c('All-cause deaths (number)', 'COVID-19 deaths (number)'),
         YEAR %in% c(2020, 2021, 2022))

for (country in countries) {
  country_data <- filtered_data %>% filter(Country == country)
  
  plot <- ggplot(country_data, aes(x = DATE, y = Value, fill = Variable)) +
    geom_bar(stat = 'identity') +
    labs(title = paste("Total Deaths and COVID Deaths in", country, "(2020-2022)"),
         x = "Date",
         y = "Number of Deaths",
         fill = "Variable") +
    theme_minimal()
  
  ggsave(paste("Total_and_COVID_Deaths_", gsub(" ", "_", country), ".jpg", sep = ""), plot, height = 6, width = 10)
}





#plot for the 3 countries
countries <- c("New Zealand", "Australia", "Iceland")

data_filtered <- subset(df_death, Year %in% c(2020, 2021, 2022) & Country %in% countries)

nz_data <- subset(data_filtered, Country == "New Zealand")
aus_data <- subset(data_filtered, Country == "Australia")
ice_data <- subset(data_filtered, Country == "Iceland")

for (country in countries) {
  country_data <- switch(
    country,
    "New Zealand" = nz_data,
    "Australia" = aus_data,
    "Iceland" = ice_data
  )
  
  
  ggplot(country_data, aes(x = c("All_0_44", "All_45_64", "All_65_over"), y = all_cause_deaths, fill = Year)) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(title = paste0("All-Cause Deaths by Age Group (", country, ")"),
         x = "Age Group", y = "Deaths") +
    theme_bw()
  
  
  ggsave(paste("age_distribution_", gsub(" ", "_", country), ".jpg", sep = ""), width = 8, height = 6)
  
  
  
  
  #group by age  
  wide1 <- df1 %>% 
    pivot_wider(
      id_cols = c("COUNTRY", "Country", "WEEK", "Year"),
      names_from = c("Variable", "Age"),
      values_from = "Value"
    )
  
  print(wide1)
  write.csv(wide1, "ages.csv", row.names = FALSE)
  
  
  wide1_sum <- wide1 %>%
    group_by(COUNTRY, Country, Year) %>%
    summarise(
      All_cause_deaths_0_to_44 = sum(`All-cause deaths (number)_0 to 44`, na.rm = TRUE),
      All_cause_deaths_45_to_64 = sum(`All-cause deaths (number)_45 to 64`, na.rm = TRUE),
      All_cause_deaths_65_and_over = sum(`All-cause deaths (number)_65 and over`, na.rm = TRUE),
      All_cause_deaths_Total = sum(`All-cause deaths (number)_Total`, na.rm = TRUE),
      COVID_deaths_Total = sum(`COVID-19 deaths (number)_Total`, na.rm = TRUE),
      Excess_deaths_65_and_over = sum(`Excess deaths (number)_65 and over`, na.rm = TRUE),
      Excess_deaths_45_to_64 = sum(`Excess deaths (number)_45 to 64`, na.rm = TRUE),
      Excess_deaths_0_to_44 = sum(`Excess deaths (number)_0 to 44`, na.rm = TRUE),
      Excess_deaths_Total = sum(`Excess deaths (number)_Total`, na.rm = TRUE),
      COVID_deaths_65_and_over = sum(`COVID-19 deaths (number)_65 and over`, na.rm = TRUE),
      COVID_deaths_45_to_64 = sum(`COVID-19 deaths (number)_45 to 64`, na.rm = TRUE),
      COVID_deaths_0_to_44 = sum(`COVID-19 deaths (number)_0 to 44`, na.rm = TRUE)
    )
  
  print(wide1_sum)
  
  write.csv(wide1_sum, "summarized_ages2.csv", row.names = FALSE)
  
  
  
  
  #summary stats
  summary_stats <- death_rates %>%
    group_by(Country, Year) %>%
    summarize(
      mean_all_cause_deaths = mean(All_cause_deaths_Total, na.rm = TRUE),
      median_all_cause_deaths = median(All_cause_deaths_Total, na.rm = TRUE),
      sd_all_cause_deaths = ifelse(n() > 1, sd(All_cause_deaths_Total, na.rm = TRUE), NA),
      .groups = 'drop'
    )
  
  print(summary_stats)
  
}



#percentage of change age 65 and over
deaths <- df_rates %>%
  group_by(Country) %>%
  arrange(Country, Year) %>%
  mutate(PercentageChange = (pop_65_over - lag(pop_65_over)) / lag(pop_65_over) * 100) %>%
  filter(!is.na(PercentageChange))

print(deaths)
write.csv(deaths, "percentage_death.csv", row.names = FALSE)
write.table(deaths, "percentage65.txt", sep = "\t", quote = FALSE, row.names = FALSE)




#Percentage change covid deaths
covid <- df_rates %>%
  group_by(Country) %>%
  arrange(Country, Year) %>%
  mutate(PercentageChange = (covid_deaths - lag(covid_deaths)) / lag(covid_deaths) * 100) %>%
  filter(!is.na(PercentageChange))

print(covid)
write.table(deaths, "percentagecovid.txt", sep = "\t", quote = FALSE, row.names = FALSE)


df_rates$percentage_pop_65_over <- (df_rates$pop_65_over / df_rates$Population) * 100

print(df_rates)
write.table(df_rates, "percentagepop65.txt", sep = "\t", quote = FALSE, row.names = FALSE)







