
# Load data
master_data <- readRDS("clean_data.RDS")

# Exclude participants that we can't measure symptom change in
symptomdata <- master_data[-which(is.na(master_data$Improved_Symptoms)),]

#### Descriptive statistics - symptom model dataset ####

summary(symptomdata)

shapiro.test(symptomdata$Age)
median(symptomdata$Age)
IQR(symptomdata$Age)

table(symptomdata$Gender)
table(symptomdata$HandDominance)
table(symptomdata$DominantSideSurgery)

table(symptomdata$Diagnosis)

shapiro.test(symptomdata$LengthSymptoms)
median(na.omit(symptomdata$LengthSymptoms))
IQR(na.omit(symptomdata$LengthSymptoms))

shapiro.test(symptomdata$SplintMonths)
median(na.omit(symptomdata$SplintMonths))
IQR(na.omit(symptomdata$SplintMonths))

table(symptomdata$SmokingStatus)
table(symptomdata$heart)
table(symptomdata$hbp)
table(symptomdata$lung)
table(symptomdata$diabetes)
table(symptomdata$ulcer)
table(symptomdata$kidney)
table(symptomdata$liver)
table(symptomdata$anaemia)
table(symptomdata$cancer)
table(symptomdata$depression)
table(symptomdata$osteoarthritis)
table(symptomdata$backpain)
table(symptomdata$rheumatoid)
table(symptomdata$Thyroid)

shapiro.test(symptomdata$EQ5D_VAS)
median(na.omit(symptomdata$EQ5D_VAS))
IQR(na.omit(symptomdata$EQ5D_VAS))

#### Descriptive statistics - function model dataset ####

# Exclude participants that we can't measure function change in
functiondata <- master_data[-which(is.na(master_data$Improved_Function)),]

summary(functiondata)

shapiro.test(functiondata$Age)
median(functiondata$Age)
IQR(functiondata$Age)

table(functiondata$Gender)
table(functiondata$HandDominance)
table(functiondata$DominantSideSurgery)

table(functiondata$Diagnosis)

shapiro.test(functiondata$LengthSymptoms)
median(na.omit(functiondata$LengthSymptoms))
IQR(na.omit(functiondata$LengthSymptoms))

shapiro.test(functiondata$SplintMonths)
median(na.omit(functiondata$SplintMonths))
IQR(na.omit(functiondata$SplintMonths))

table(functiondata$SmokingStatus)
table(functiondata$heart)
table(functiondata$hbp)
table(functiondata$lung)
table(functiondata$diabetes)
table(functiondata$ulcer)
table(functiondata$kidney)
table(functiondata$liver)
table(functiondata$anaemia)
table(functiondata$cancer)
table(functiondata$depression)
table(functiondata$osteoarthritis)
table(functiondata$backpain)
table(functiondata$rheumatoid)
table(functiondata$Thyroid)

shapiro.test(functiondata$EQ5D_VAS)
median(na.omit(functiondata$EQ5D_VAS))
IQR(na.omit(functiondata$EQ5D_VAS))

## Examine QuickDASH change scores calculated in the traditional way 

# Idenitify participants with at least 10 item responses pre op
preopQD <- master_data[,36:46]
atleast10preop <- which(rowSums(is.na(preopQD)) < 2)

# Estimate MIC
MICdata <- master_data[atleast10preop,36:46]
preopQDscores <- (unname(rowMeans(MICdata, na.rm = TRUE)) - 1)*25
sd(preopQDscores)/2 #10.3 - round to 11

# Find records that have enough pre and post op responses to calculate
# pre and postop QuickDASH score
postopQD <- master_data[,61:71]
atleast10postop <- which(rowSums(is.na(postopQD)) < 2)
atleast10prepost <- intersect(atleast10preop, atleast10postop)

# Calculate preop and postop QuickDASH scores, and change scores
pre_op_responses <- master_data[atleast10prepost, 36:46]
pre_op_scores <- (unname(rowMeans(pre_op_responses, na.rm = TRUE)) - 1)*25

post_op_responses <- master_data[atleast10prepost, 61:71]
post_op_scores <- (unname(rowMeans(post_op_responses, na.rm = TRUE)) - 1)*25

change_scores <- post_op_scores - pre_op_scores

# Count change scores that exceed MIC (< -10)
length(which(change_scores < - 10))

# Count change scores that don't exceed MID (> -11)
length(which(change_scores > - 10))
# According to these counts, 705/1117 (63%) patients improved meaningfully, 
# following CTS

## Compare to QD subscale scores
table(symptomdata$Improved_Symptoms)
# According to these counts, 826/1093 (76%) patients have meaningful 
# improvement in symptoms

table(functiondata$Improved_Function)
# And 507/1045 (49%) have a meaningful improvement in function