
# Load packages
library(dplyr)

# Load data
master_data <- read.csv("data.csv", header = T)

# Remove unnecessary columns
master_data[,c(5,10,14,15,20,62:66,67,68,71,73:78,80,81:87,99:111)] <- NULL

# Rename variables
master_data[which(master_data[,3]==1),3] <- "male"
master_data[which(master_data[,3]==2),3] <- "female"

master_data[which(master_data[,4]==0),4] <- "no"
master_data[which(master_data[,4]==1),4] <- "yes"

master_data[which(master_data[,5]==1),5] <- "left"
master_data[which(master_data[,5]==2),5] <- "right"

master_data[which(master_data[,9]==1),9] <- "yes"
master_data[which(master_data[,9]==0),9] <- "no"     

master_data[which(master_data[,59] == 1),59] <- "right"
master_data[which(master_data[,59] == 2),59] <- "left"
master_data[which(master_data[,59] == 3),59] <- "bilateral"

# Collapse co-morbidities
for(i in c(16:29)){
  
  master_data[which(master_data[,i] == 0),i] <- "no"
  master_data[which(master_data[,i] == 1),i] <- "yes"
  master_data[which(master_data[,i] == 2),i] <- "yes"
  master_data[which(master_data[,i] == 3),i] <- "yes"
  
}


# Collapse smoking status
master_data[which(master_data[,13] == "Never smoked"),13] <- "Non_smoker"
master_data[which(master_data[,13] == "Exsmoker"),13] <- "Non_smoker"
master_data[which(master_data[,13] == "Current smoker"),13] <- "Smoker"

# Collapse employment status into "employed/student/houseperson/retired", "unemployed" and "disabled"
master_data[which(master_data[,56] == "Employed"),56] <- "Worker"
master_data[which(master_data[,56] == "Houseperson"),56] <- "Worker"
master_data[which(master_data[,56] == "Retired"),56] <- "Worker"
master_data[which(master_data[,56] == "Student"),56] <- "Worker"

# Add a column for Dominant Side Surgery
DominantSideSurgery <- rep(NA, 1916)
DominantSideSurgery[which(master_data[,5] == master_data[,59])] <- "yes"
DominantSideSurgery[which(master_data[,59] == "bilateral")] <- "yes"
DominantSideSurgery[which(is.na(master_data[,59]))] <- "unknown"
DominantSideSurgery[which(is.na(master_data[,5]))] <- "unknown"
DominantSideSurgery[which(is.na(DominantSideSurgery))] <- "no"
DominantSideSurgery[which(DominantSideSurgery == "unknown")] <- NA
master_data <- master_data %>%
  mutate(DominantSideSurgery = DominantSideSurgery)

# Now change Side.of.Op to laterality
master_data[which(master_data[,59] == "left"),59] <- "unilateral"
master_data[which(master_data[,59] == "right"),59] <- "unilateral"
colnames(master_data)[59] <- "laterality"


# Add a columns for pre and post op symptoms and function, based on respective QD item sum scores
master_data <- master_data %>%
  mutate(Baseline_symptoms = rowSums(master_data[,44:46]),
         Postop_symptoms = rowSums(master_data[,69:71]),
         Baseline_function = rowSums(master_data[,36:41]),
         Postop_function = rowSums(master_data[,61:66]))


# Estimate MIC of QD symptoms subscale - 1.2 (round to 2)
sd(na.omit(master_data$Baseline_symptoms))/2


# Estimate MIC of QD function subscale -  3.045 (round to 3)
sd(na.omit(master_data$Baseline_function))/2


# Add a column for improved symptoms
Improved_Symptoms <- rep(NA, 1916)
Improved_Symptoms[which(master_data[,74] - master_data[,73] < -1)] <- "yes"
Improved_Symptoms[which(master_data[,74] - master_data[,73] > -2)] <- "no"
master_data <- master_data %>%
  mutate(Improved_Symptoms = Improved_Symptoms)


# Add a column for improved function
Improved_Function <- rep(NA, 1916)
Improved_Function[which(master_data[,76] - master_data[,75] < -2)] <- "yes"
Improved_Function[which(master_data[,76] - master_data[,75] > -3)] <- "no"
master_data <- master_data %>%
  mutate(Improved_Function = Improved_Function)


# Make categorical data factors
for (i in c(3,4,5,9,13,16:29,47:56,58:59,72,77,78)){
  master_data[,i] <- as.factor(master_data[,i])
}

# Check factor levels
levels(master_data[,3]) # female, male
levels(master_data[,4]) # no, yes
levels(master_data[,5]) # left, right
levels(master_data[,9]) # no, yes
levels(master_data[,13]) 

# Deal with unused levels
placeholder <- rep(NA, length(master_data[,13]))
placeholder[which(master_data[,13]=="Non_smoker")] <- "Non_smoker"
placeholder[which(master_data[,13]=="Smoker")] <- "Smoker"
placeholder <- factor(placeholder, levels = c("Non_smoker", "Smoker"))
master_data[,13] <- placeholder

levels(master_data[,16]) # no, yes
levels(master_data[,17]) # no, yes
levels(master_data[,18]) # no, yes
levels(master_data[,19]) # no, yes
levels(master_data[,20]) # no, yes
levels(master_data[,21]) # no, yes
levels(master_data[,22]) # no, yes
levels(master_data[,23]) # no, yes
levels(master_data[,24]) # no, yes
levels(master_data[,25]) # no, yes
levels(master_data[,26]) # no, yes
levels(master_data[,27]) # no, yes
levels(master_data[,28]) # no, yes
levels(master_data[,29]) # no, yes

# Deal with unused levels
for (i in c(47:55)){
  placeholder <- rep(NA, length(master_data[,i]))
  placeholder[which(master_data[,i]=="N")] <- "N"
  placeholder[which(master_data[,i]=="Y")] <- "Y"
  placeholder <- factor(placeholder, levels = c("N", "Y"))
  master_data[,i] <- placeholder
}

levels(master_data[,47]) # N, Y
levels(master_data[,48]) # N, Y
levels(master_data[,49]) # N, Y
levels(master_data[,50]) # N, Y
levels(master_data[,51]) # N, Y
levels(master_data[,52]) # N, Y
levels(master_data[,53]) # N, Y
levels(master_data[,54]) # N, Y
levels(master_data[,55]) # N, Y
levels(master_data[,56])

# Deal with unused levels
placeholder <- rep(NA, length(master_data[,56]))
placeholder[which(master_data[,56]=="Long Term Sick / Disabled")] <- "Long Term Sick / Disabled"
placeholder[which(master_data[,56]=="Unemployed")] <- "Unemployed"
placeholder[which(master_data[,56]=="Worker")] <- "Worker"
placeholder <- factor(placeholder, levels = c("Worker", "Unemployed", "Long Term Sick / Disabled"))
master_data[,56] <- placeholder

levels(master_data[,56]) # Worker, unemployed, disabled
levels(master_data[,58]) # CTS, Recurrent CTS
levels(master_data[,59])
levels(master_data[,59]) <- factor(master_data[,59], 
                                   levels = rev(levels(master_data[,59])))# unilateral, bilateral


levels(master_data[,72])
levels(master_data[,77])
levels(master_data[,78])

# Change int to num 

for (i in c(1, 2, 6:8, 10:12, 14:15, 30:46, 60:71)){
  master_data[,i] <- as.numeric(master_data[,i])
}

# Save clean data for later
saveRDS(master_data, "clean_data.RDS")
