
# Install and load packages
install.packages("finalfit")
library(finalfit)


#### Missing data analysis - Symptoms ####

# Load data
master_data <- readRDS("clean_data.RDS")

# Check how many participants are missing postop symptom scores
length(which(is.na(master_data[,74]))) 

# Set dependent and explanatory variables for missing data analysis
# Explanatory variables have been split into 6 groups for ease of visualization
dependent <- "Postop_symptoms"

explanatory1 <- c("Age", "Gender", 
                  "LengthSymptoms", "Previous_injections")
explanatory2 <- c("Previous_Operations",
                  "SplintMonths", "SmokingStatus")
explanatory3 <- c("heart", "hbp", "lung", "diabetes",
                  "ulcer")
explanatory4 <- c("kidney", "liver", "anaemia", "cancer",
                  "depression")
explanatory5 <- c("osteoarthritis", "backpain", "rheumatoid",
                  "Thyroid")
explanatory6 <- c("Employment_Status",
                  "BMI", "Diagnosis", "DominantSideSurgery")

# Graphical representation
master_data %>%
  missing_pairs(dependent, explanatory1)    

master_data %>%
  missing_pairs(dependent, explanatory2)   

master_data %>%
  missing_pairs(dependent, explanatory3)   

master_data %>%
  missing_pairs(dependent, explanatory4)   

master_data %>%
  missing_pairs(dependent, explanatory5)   

master_data %>%
  missing_pairs(dependent, explanatory6)   

# Hypothesis tests
master_data %>%
  missing_compare(dependent, c(explanatory1, explanatory2, explanatory3,
                               explanatory4, explanatory5, explanatory6))

# Save missing data analysis to csv
missing_postop_symptoms <- master_data %>%
                           missing_compare(dependent, 
                                           c(explanatory1, explanatory2, explanatory3,
                                             explanatory4, explanatory5, explanatory6)
                                           )

write.csv(missing_postop_symptoms, "missing_postop_symptoms.csv")



#### Missing data analysis - Function ####

# Check how many participants are missing postop function scores
length(which(is.na(master_data[,76]))) 

# Repeat missing data analysis for postop function
dependent <- "Postop_function"

explanatory1 <- c("Age", "Gender", 
                  "LengthSymptoms", "Previous_injections")
explanatory2 <- c("Previous_Operations",
                  "SplintMonths", "SmokingStatus")
explanatory3 <- c("heart", "hbp", "lung", "diabetes",
                  "ulcer")
explanatory4 <- c("kidney", "liver", "anaemia", "cancer",
                  "depression")
explanatory5 <- c("osteoarthritis", "backpain", "rheumatoid",
                  "Thyroid")
explanatory6 <- c("Employment_Status",
                  "BMI", "Diagnosis", "DominantSideSurgery")

# Graphical representation
master_data %>%
  missing_pairs(dependent, explanatory1)    

master_data %>%
  missing_pairs(dependent, explanatory2)   

master_data %>%
  missing_pairs(dependent, explanatory3)   

master_data %>%
  missing_pairs(dependent, explanatory4)   

master_data %>%
  missing_pairs(dependent, explanatory5)   

master_data %>%
  missing_pairs(dependent, explanatory6)   


# Hypothesis tests
master_data %>%
  missing_compare(dependent, c(explanatory1, explanatory2, explanatory3,
                               explanatory4, explanatory5, explanatory6))

# Save to csv
missing_postop_function <- master_data %>%
  missing_compare(dependent, 
                  c(explanatory1, explanatory2, explanatory3,
                    explanatory4, explanatory5, explanatory6)
  )

write.csv(missing_postop_function, "missing_postop_function.csv")



