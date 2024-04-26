# Load necessary libraries
library(dplyr)
library(tidyverse)
library(stats)
library(ggplot2)
library(cluster)
library(factoextra)

#load datasets into your Environment in R

#Summary Statistics
#Create one Excel per cell, one table per variation

# Function to calculate the total, percentage of total, and percentage of variation users
calculate_values <- function(filter_condition, variation, total_variation_users) {
  total <- sum(data[filter_condition, variation])
  percentage_of_total <- (total / sum(data[[variation]])) * 100
  user_percentage <- (nrow(data[filter_condition & data[[variation]] > 0, ]) / total_variation_users) * 100
  return(c(total, round(percentage_of_total, 2), round(user_percentage, 2)))
}


# Create a summary table for a given language variation
create_summary_table <- function(variation) {
  
  # Calculate the total number of unique users for this variation
  total_variation_users <- nrow(data[data[[variation]] > 0, ])
  
  # Gender and age
  women_under_75 <- calculate_values((data$Gender.age == 'female under 75'), variation, total_variation_users)
  women_75_plus <- calculate_values((data$Gender.age == 'female 75 plus'), variation, total_variation_users)
  men <- calculate_values((data$Gender.age == 'male'), variation, total_variation_users)
  
  # Area
  east <- calculate_values((data$Area == 'East'), variation, total_variation_users)
  west <- calculate_values((data$Area == 'West'), variation, total_variation_users)
  poland <- calculate_values((data$Area == 'Poland'), variation, total_variation_users)
  
  # Interview Length
  length_1 <- calculate_values((data$Length == 1), variation, total_variation_users)
  length_2 <- calculate_values((data$Length == 2), variation, total_variation_users)
  length_3 <- calculate_values((data$Length == 3), variation, total_variation_users)
  length_4 <- calculate_values((data$Length == 4), variation, total_variation_users)
  
  # Creating the table
  table <- data.frame(
    Variable = c('Women < 75', 'Women 75+', 'Men', 'East', 'West', 'Poland', 'Length 1', 'Length 2', 'Length 3', 'Length 4'),
    Total = c(women_under_75[1], women_75_plus[1], men[1], east[1], west[1], poland[1], length_1[1], length_2[1], length_3[1], length_4[1]),
    Percentage_of_Total = c(women_under_75[2], women_75_plus[2], men[2], east[2], west[2], poland[2], length_1[2], length_2[2], length_3[2], length_4[2]),
    Percentage_of_Variation_Users = c(women_under_75[3], women_75_plus[3], men[3], east[3], west[3], poland[3], length_1[3], length_2[3], length_3[3], length_4[3])
  )
  
  return(table)
}

#1.Person Accusative Plural:

data= PerAccPlu.wide

# Create a summary table for each language variation
summary_tables <- lapply(c("B5", "C5", "D5", "E5"), create_summary_table)

# Print the summary tables
summary_tables

# Create a list of tables with named elements (the names will be used as sheet names)
tables_list <- list(
  B5 = summary_tables[[1]],
  C5 = summary_tables[[2]],
  D5 = summary_tables[[3]],
  E5 = summary_tables[[4]]
)

# Write the tables to an Excel file
write_xlsx(tables_list, "C:/1.summary_PerAccPlu.xlsx")

#2.Person ADMN:

data= PerADMN.wide

# Create a summary table for each language variation
summary_tables <- lapply(c("B8", "C8", "D8"), create_summary_table)

# Print the summary tables
summary_tables

# Create a list of tables with named elements (the names will be used as sheet names)
tables_list <- list(
  B8 = summary_tables[[1]],
  C8 = summary_tables[[2]],
  D8 = summary_tables[[3]])

# Write the tables to an Excel file
write_xlsx(tables_list, "C:/2.summary_PerADMN.xlsx")

#3.Person dative plural:

data= PerDatPlu.wide

# Create a summary table for each language variation
summary_tables <- lapply(c("B11", "C11", "D11"), create_summary_table)

# Print the summary tables
summary_tables

# Create a list of tables with named elements (the names will be used as sheet names)
tables_list <- list(
  B11 = summary_tables[[1]],
  C11 = summary_tables[[2]],
  D11 = summary_tables[[3]])

# Write the tables to an Excel file
write_xlsx(tables_list, "C:/3.summary_PerDatPlu.xlsx") 

#4.Person dative Singular:

data= PerDatSin.wide

# Create a summary table for each language variation
summary_tables <- lapply(c("B10", "C10"), create_summary_table)

# Print the summary tables
summary_tables

# Create a list of tables with named elements (the names will be used as sheet names)
tables_list <- list(
  B10 = summary_tables[[1]],
  C10 = summary_tables[[2]])

# Write the tables to an Excel file
write_xlsx(tables_list, "C:/4.summary_PerDatSin.xlsx") 

#5.Person Genitive plural:

data= PerGenPlu.wide

# Create a summary table for each language variation
summary_tables <- lapply(c("B7", "C7", "D7","E7","F7","G7","H7","I7"), create_summary_table)

# Print the summary tables
summary_tables

# Create a list of tables with named elements (the names will be used as sheet names)
tables_list <- list(
  B7 = summary_tables[[1]],
  C7 = summary_tables[[2]],
  D7 = summary_tables[[3]],
  E7 = summary_tables[[4]],
  F7 = summary_tables[[5]],
  G7 = summary_tables[[6]],
  H7 = summary_tables[[7]],
  I7 = summary_tables[[8]])

# Write the tables to an Excel file
write_xlsx(tables_list, "C:/5.summary_PerGenPlu.xlsx") 

#6.Person Nominative Plural:

data= PerNomPlu.wide

# Create a summary table for each language variation
summary_tables <- lapply(c("B3", "C3", "D3", "E3"), create_summary_table)

# Print the summary tables
summary_tables

# Create a list of tables with named elements (the names will be used as sheet names)
tables_list <- list(
  B3 = summary_tables[[1]],
  C3 = summary_tables[[2]],
  D3 = summary_tables[[3]],
  E3 = summary_tables[[4]]
)

# Write the tables to an Excel file
write_xlsx(tables_list, "C:/6.summary_PerNomPlu.xlsx")

#7.Person Nominative Singular:

data= PerNomSin.wide

# Create a summary table for each language variation
summary_tables <- lapply(c("B2", "C2", "D2"), create_summary_table)

# Print the summary tables
summary_tables


# Create a list of tables with named elements (the names will be used as sheet names)
tables_list <- list(
  B2 = summary_tables[[1]],
  C2 = summary_tables[[2]],
  D2 = summary_tables[[3]]
)

# Write the tables to an Excel file
write_xlsx(tables_list, "C:/7.summary_PerNomSin.xlsx") 

#8.Year Accusative Plural:
  
data= YrAccPlu.wide

# Create a summary table for each language variation
summary_tables <- lapply(c("B5", "C5"), create_summary_table)

# Print the summary tables
summary_tables

# Create a list of tables with named elements (the names will be used as sheet names)
tables_list <- list(
  B5 = summary_tables[[1]],
  C5 = summary_tables[[2]]
)

# Write the tables to an Excel file
write_xlsx(tables_list, "C:/8.summary_YrAccPlu.xlsx") 

#9.Year Accusative Singular:
  
data= YrAccSin.wide

# Create a summary table for each language variation
summary_tables <- lapply(c("B4", "C4","D4","E4"), create_summary_table)

# Print the summary tables
summary_tables

# Create a list of tables with named elements (the names will be used as sheet names)
tables_list <- list(
  B4 = summary_tables[[1]],
  C4 = summary_tables[[2]],
  D4 = summary_tables[[3]],
  E4 = summary_tables[[4]]
)

# Write the tables to an Excel file
write_xlsx(tables_list, "C:/9.summary_YrAccSin.xlsx") 

#10.Year ADMN:

data= YrADMN.wide

# Create a summary table for each language variation
summary_tables <- lapply(c("B8", "C8","D8"), create_summary_table)

# Print the summary tables
summary_tables

# Create a list of tables with named elements (the names will be used as sheet names)
tables_list <- list(
  B8 = summary_tables[[1]],
  C8 = summary_tables[[2]],
  D8 = summary_tables[[3]]
)

# Write the tables to an Excel file
write_xlsx(tables_list, "C:/10.summary_YrADMN.xlsx")

#11.Year Genetive Plural:
  
data= YrGenPlu.wide

# Create a summary table for each language variation
summary_tables <- lapply(c("B7", "C7","D7","E7","F7","G7"), create_summary_table)

# Print the summary tables
summary_tables

# Create a list of tables with named elements (the names will be used as sheet names)
tables_list <- list(
  B7 = summary_tables[[1]],
  C7 = summary_tables[[2]],
  D7 = summary_tables[[3]],
  E7 = summary_tables[[4]],
  F7 = summary_tables[[5]],
  G7 = summary_tables[[6]])

# Write the tables to an Excel file
write_xlsx(tables_list, "C:/11.summary_YrGenPlu.xlsx") 

#12 Year Genitive Singular

data= YrGenSin.wide

# Create a summary table for each language variation
summary_tables <- lapply(c("B6", "C6","D6","E6","F6","G6"), create_summary_table)

# Print the summary tables
summary_tables

# Create a list of tables with named elements (the names will be used as sheet names)
tables_list <- list(
  B6 = summary_tables[[1]],
  C6 = summary_tables[[2]],
  D6 = summary_tables[[3]],
  E6 = summary_tables[[4]],
  F6 = summary_tables[[5]],
  G6 = summary_tables[[6]])

# Write the tables to an Excel file
write_xlsx(tables_list, "C:/12.summary_YrGenSin.xlsx") 


#13.Year Ins Plural:
  
data= YrINSPlu.wide

# Create a summary table for each language variation
summary_tables <- lapply(c("B10", "C10","D10"), create_summary_table)

# Print the summary tables
summary_tables

# Create a list of tables with named elements (the names will be used as sheet names)
tables_list <- list(
  B10 = summary_tables[[1]],
  C10 = summary_tables[[2]],
  D10 = summary_tables[[3]]
)

# Write the tables to an Excel file
write_xlsx(tables_list, "C:/13.summary_YrInsPlu.xlsx") 

#14.Year Locative Plural:
  
#data= YrLocPlu.wide

# Create a summary table for each language variation
summary_tables <- lapply(c("B12", "C12","D12","E12"), create_summary_table)

# Print the summary tables
summary_tables

# Create a list of tables with named elements (the names will be used as sheet names)
tables_list <- list(
  B12 = summary_tables[[1]],
  C12 = summary_tables[[2]],
  D12 = summary_tables[[3]],
  E12 = summary_tables[[4]]
)

# Write the tables to an Excel file
write_xlsx(tables_list, "C:/14.summary_YrLocPlu.xlsx") 

#15.Year Locative Singular:

data= YrLocSin.wide

# Create a summary table for each language variation
summary_tables <- lapply(c("B11", "C11","D11","E11","F11", "G11","H11","I11"), create_summary_table)

# Print the summary tables
summary_tables

# Create a list of tables with named elements (the names will be used as sheet names)
tables_list <- list(
  B11 = summary_tables[[1]],
  C11 = summary_tables[[2]],
  D11 = summary_tables[[3]],
  E11 = summary_tables[[4]],
  F11 = summary_tables[[5]],
  G11 = summary_tables[[6]],
  H11 = summary_tables[[7]],
  I11 = summary_tables[[8]]
)


# Write the tables to an Excel file
write_xlsx(tables_list, "C:/15.summary_YrLocSin.xlsx") 

#16.Year Nominative Plural:
  
data= YrNomPlu.wide

# Create a summary table for each language variation
summary_tables <- lapply(c("B3", "C3","D3"), create_summary_table)

# Print the summary tables
summary_tables

# Create a list of tables with named elements (the names will be used as sheet names)
tables_list <- list(
  B3 = summary_tables[[1]],
  C3 = summary_tables[[2]],
  D3 = summary_tables[[3]]
)

# Write the tables to an Excel file
write_xlsx(tables_list, "C:/16.summary_YrNomPlu.xlsx")


#17.Year Nominative Singular:
  
data= YrNomSin.wide

# Create a summary table for each language variation
summary_tables <- lapply(c("B2", "C2"), create_summary_table)

# Print the summary tables
summary_tables

# Create a list of tables with named elements (the names will be used as sheet names)
tables_list <- list(
  B2 = summary_tables[[1]],
  C2 = summary_tables[[2]]
)

# Write the tables to an Excel file
write_xlsx(tables_list, "C:/17.summary_YrNomSin.xlsx")


# PART TWO: Overall Table

# Define the function
process_dataset <- function(data) {
  
  # No of speakers using cell in conversation
  speakers_using_cell <- sum(data$tot.occ != 0)
  
  # Maximum no of usages of cell by an individual speaker
  max_usages <- max(data$tot.occ)
  
  # No of speaker using cell more than once
  speakers_using_cell_more_than_once <- sum(data$tot.occ > 1)
  
  # No of forms produced by speakers
  start_col <- which(names(data) == "Gender.age")
  end_col <- which(names(data) == "tot.occ")
  num_forms_produced <- end_col - start_col - 1
  
  # No of Speakers using multiple forms
  speakers_using_multiple_forms <- sum(data$tot.form > 1)
  
  # Maximum number of forms used by an individual speaker
  max_forms_by_individual <- max(data$tot.form)
  
  return(c(speakers_using_cell, max_usages, speakers_using_cell_more_than_once, num_forms_produced, 
           speakers_using_multiple_forms, max_forms_by_individual))
}

# 1. Person Accusative Plural
# Test the function with the first dataset
data <- PerAccPlu.wide
result <- process_dataset(data)
print(result)


#Similarly for datasets: PerADMN.wide, PerDatPlu.wide, PerDatSin.wide, PerGenPlu.wide, PerNomSin.wide, PerNomPlu.wide, YrAccPlu.wide, YrAccSin.wide, YrADMN.wide, YrGenPlu.wide, YrGenSin.wide, YrINSPlu.wide, YrLocPlu.wide, YrLocSin.wide, YrNomPlu.wide, YrNomSin.wide


#1. Person Accusitive Plural

data <- PerAccPlu.wide

# Data preparation
# Convert categorical variables to binary format
data <- data %>% select(Gender.age, Area, Length, tot.form) # Select relevant columns
data_dummies <- model.matrix(~ . - 1, data) # Create dummies, dropping the first level of each factor

# Standardize the data
data_standardized <- scale(data_dummies)

# Open a JPEG device
jpeg("Numbercluster_plot.jpg", width=800, height=600, quality=100, pointsize=16)

# Determine the number of clusters using Elbow method
fviz_nbclust(data_standardized, kmeans, method = "wss") + geom_vline(xintercept = 4, linetype = 2)

# Close the JPEG device
dev.off()


set.seed(123) # Setting seed for reproducibility
kclusters <- kmeans(data_standardized, centers = 4)  

# Attach cluster assignment to data
data$cluster <- kclusters$cluster
 

# Analyze clusters
cluster_summary <- data %>% 
  group_by(cluster) %>% 
  summarise(
    avg_tot_form = mean(tot.form),
    count = n(),
    predominant_gender_age = names(sort(table(Gender.age), decreasing = TRUE)[1]),
    predominant_area = names(sort(table(Area), decreasing = TRUE)[1]),
    avg_length = mean(Length)
  )

print(cluster_summary)


# Open a JPEG device
jpeg("Person Accusitive Plural.jpg", width=800, height=600, quality=100, pointsize=16)

fviz_cluster(kclusters, data = data_standardized, 
             geom = "point",  # Only show data points
             show.clust.cent = FALSE)  # Don't show cluster centers


# Close the JPEG device
dev.off()

#Similarly for datasets: PerGenPlu.wide, PerNomSin.wide, YrADMN.wide, YrGenPlu.wide, YrGenSin.wide, YrLocSin.wide, YrNomSin.wide

#Chi-Squared Analysis for Person

# Define the base dataset
person_data <- PerAccPlu.wide

# List of datasets to merge
datasets_to_merge <- list(
  PerADMN.wide = "_PerADMN",
  PerDatPlu.wide = "_PerDatPlu",
  PerDatSin.wide = "_PerDatSing",
  PerGenPlu.wide = "_PerGenPlu",
  PerNomPlu.wide = "_PerNomPlu",
  PerNomSin.wide = "_PerNomSing"
)

# Merge the datasets in the list
for (dataset_name in names(datasets_to_merge)) {
  suffix <- datasets_to_merge[[dataset_name]]
  person_data <- merge(person_data, get(dataset_name)[, c("Nid", "tot.form")], by = "Nid", suffixes = c("", suffix))
}

# List of columns to check
columns_to_check <- c("tot.form", "tot.form_PerADMN", "tot.form_PerDatPlu", "tot.form_PerDatSing", "tot.form_PerGenPlu", "tot.form_PerNomPlu", "tot.form_PerNomSing")

# Create the new binary column
person_data$multiform <- ifelse(rowSums(person_data[, columns_to_check] > 1) > 0, 1, 0)

# Contingency table for Area with person_data
table_area_person <- table(person_data$multiform, person_data$Area)
print(table_area_person)
# Chi-squared test for Area with person_data
chisq_area_person <- chisq.test(table(person_data$multiform, person_data$Area), simulate.p.value = TRUE, B = 2000)
print(chisq_area_person)

# Contingency table for Length with person_data
table_length_person <- table(person_data$multiform, person_data$Length)
print(table_length_person)
# Chi-squared test for Length with person_data
chisq_length_person <- chisq.test(table(person_data$multiform, person_data$Length), simulate.p.value = TRUE, B = 2000)
print(chisq_length_person)

# Contingency table for Gender.age with person_data
table_gender_age_person <- table(person_data$multiform, person_data$Gender.age)
print(table_gender_age_person)
# Chi-squared test for Gender.age with person_data
chisq_gender_age_person <- chisq.test(table(person_data$multiform, person_data$Gender.age), simulate.p.value = TRUE, B = 2000)
print(chisq_gender_age_person)

#Chi-Squared Analysis for Year

# Define the base dataset
year_data <- YrAccPlu.wide

# List of datasets to merge
datasets_to_merge <- list(
  YrAccSin.wide = "_YrAccSin",
  YrADMN.wide = "_YrADMN",
  YrGenPlu.wide = "_YrGenPlu",
  YrGenSin.wide = "_YrGenSin",
  YrINSPlu.wide = "_YrINSPlu",
  YrLocPlu.wide = "_YrLocPlu",
  YrLocSin.wide = "_YrLocSin",
  YrNomPlu.wide = "_YrNomPlu",
  YrNomSin.wide = "_YrNomSin"
)

# Merge the datasets in the list
for (dataset_name in names(datasets_to_merge)) {
  suffix <- datasets_to_merge[[dataset_name]]
  year_data <- merge(year_data, get(dataset_name)[, c("Nid", "tot.form")], by = "Nid", suffixes = c("", suffix))
}

columns_to_check_year <- c("tot.form", "tot.form_YrAccSin", "tot.form_YrADMN", "tot.form_YrGenPlu", "tot.form_YrGenSin", "tot.form_YrINSPlu", "tot.form_YrLocPlu",  "tot.form_YrLocSin", "tot.form_YrNomPlu", "tot.form_YrNomSin")

# Create the new binary column for year_data
year_data$multiform <- ifelse(rowSums(year_data[, columns_to_check_year] > 1) > 0, 1, 0)

# Contingency table for Area with year_data
table_area_year <- table(year_data$multiform, year_data$Area)
print(table_area_year)
# Chi-squared test for Area with year_data
chisq_area_year <- chisq.test(table(year_data$multiform, year_data$Area))
print(chisq_area_year)

# Contingency table for Length with year_data
table_length_year <- table(year_data$multiform, year_data$Length)
print(table_length_year)
# Chi-squared test for Length with year_data
chisq_length_year <- chisq.test(table(year_data$multiform, year_data$Length), simulate.p.value = TRUE, B = 2000)
print(chisq_length_year)

# Contingency table for Gender.age with year_data
table_gender_age_year <- table(year_data$multiform, year_data$Gender.age)
print(table_gender_age_year)
# Chi-squared test for Gender.age with year_data
chisq_gender_age_year <- chisq.test(table(year_data$multiform, year_data$Gender.age))
print(chisq_gender_age_year)
