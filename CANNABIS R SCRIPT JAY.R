#install.packages("readxl")
#install.packages("dplyr")
#install.packages("psych")
#install.packages("ltm")

library(readxl)
library(dplyr)
library(psych)
library(ltm)


#GLP1 Data - Cleaned and Processed
d <- read_excel("C:/Users/jayxp/Downloads/Cannabis Conjoint Survey_Clean.xlsx", sheet = "Clean")

# See all variable names in dataset
names(d)


# -------///////////////// Big Fig Inventory (BFI) Personality Traits 

#See what the actual responses look like in the dataset (for Copy and Paste)
table(d$BFI_1)

### Extraversion
d$Extr1 <- recode(d$BFI_1, 'Completely Agree\r\r\n5\r\r\n' = 1, 'Slightly Agree\r\r\n4\r\r\n' = 2, 'Neither agree nor disagree\r\r\n3\r\r\n' = 3, 'Slightly Disagree\r\r\n2\r\r\n' = 4, 'Completely Disagree\r\r\n1\r\r\n' = 5) #Reverse
d$Extr6 <- recode(d$BFI_6, 'Completely Agree\r\r\n5\r\r\n' = 5, 'Slightly Agree\r\r\n4\r\r\n' = 4, 'Neither agree nor disagree\r\r\n3\r\r\n' = 3, 'Slightly Disagree\r\r\n2\r\r\n' = 2, 'Completely Disagree\r\r\n1\r\r\n' = 1)
### Agreeableness
d$Agree2 <- recode(d$BFI_2, 'Completely Agree\r\r\n5\r\r\n' = 5, 'Slightly Agree\r\r\n4\r\r\n' = 4, 'Neither agree nor disagree\r\r\n3\r\r\n' = 3, 'Slightly Disagree\r\r\n2\r\r\n' = 2, 'Completely Disagree\r\r\n1\r\r\n' = 1)
d$Agree7 <- recode(d$BFI_7, 'Completely Agree\r\r\n5\r\r\n' = 1, 'Slightly Agree\r\r\n4\r\r\n' = 2, 'Neither agree nor disagree\r\r\n3\r\r\n' = 3, 'Slightly Disagree\r\r\n2\r\r\n' = 4, 'Completely Disagree\r\r\n1\r\r\n' = 5) #Reverse
### Conscientiousness
d$Consci3 <- recode(d$BFI_3, 'Completely Agree\r\r\n5\r\r\n' = 1, 'Slightly Agree\r\r\n4\r\r\n' = 2, 'Neither agree nor disagree\r\r\n3\r\r\n' = 3, 'Slightly Disagree\r\r\n2\r\r\n' = 4, 'Completely Disagree\r\r\n1\r\r\n' = 5) #Reverse
d$Consci8 <- recode(d$BFI_8, 'Completely Agree\r\r\n5\r\r\n' = 5, 'Slightly Agree\r\r\n4\r\r\n' = 4, 'Neither agree nor disagree\r\r\n3\r\r\n' = 3, 'Slightly Disagree\r\r\n2\r\r\n' = 2, 'Completely Disagree\r\r\n1\r\r\n' = 1)
### Neuroticism
d$Neuro4 <- recode(d$BFI_4, 'Completely Agree\r\r\n5\r\r\n' = 1, 'Slightly Agree\r\r\n4\r\r\n' = 2, 'Neither agree nor disagree\r\r\n3\r\r\n' = 3, 'Slightly Disagree\r\r\n2\r\r\n' = 4, 'Completely Disagree\r\r\n1\r\r\n' = 5) #Reverse
d$Neuro9 <- recode(d$BFI_9, 'Completely Agree\r\r\n5\r\r\n' = 5, 'Slightly Agree\r\r\n4\r\r\n' = 4, 'Neither agree nor disagree\r\r\n3\r\r\n' = 3, 'Slightly Disagree\r\r\n2\r\r\n' = 2, 'Completely Disagree\r\r\n1\r\r\n' = 1)
### Openness
d$Open5 <- recode(d$BFI_5, 'Completely Agree\r\r\n5\r\r\n' = 1, 'Slightly Agree\r\r\n4\r\r\n' = 2, 'Neither agree nor disagree\r\r\n3\r\r\n' = 3, 'Slightly Disagree\r\r\n2\r\r\n' = 4, 'Completely Disagree\r\r\n1\r\r\n' = 5) #Reverse
d$Open10 <- recode(d$BFI_10, 'Completely Agree\r\r\n5\r\r\n' = 5, 'Slightly Agree\r\r\n4\r\r\n' = 4, 'Neither agree nor disagree\r\r\n3\r\r\n' = 3, 'Slightly Disagree\r\r\n2\r\r\n' = 2, 'Completely Disagree\r\r\n1\r\r\n' = 1)

Extr_Scale <- c("Extr1","Extr6")
Agree_Scale <- c("Agree2","Agree7")
Consci_Scale <- c("Consci3","Consci8")
Neuro_Scale <- c("Neuro4","Neuro9")
Open_Scale <- c("Open5","Open10")

# Compute mean scores for each dimension
d$Extraversion_Score <- rowMeans(d[, Extr_Scale], na.rm = TRUE)
d$Agreeableness_Score <- rowMeans(d[, Agree_Scale], na.rm = TRUE)
d$Conscientiousness_Score <- rowMeans(d[, Consci_Scale], na.rm = TRUE)
d$Neuroticism_Score <- rowMeans(d[, Neuro_Scale], na.rm = TRUE)
d$Openness_Score <- rowMeans(d[, Open_Scale], na.rm = TRUE)

# Preview Final Scores
head(d[, c("Extraversion_Score", "Agreeableness_Score", 
           "Conscientiousness_Score", "Neuroticism_Score", 
           "Openness_Score")])

#################  BIS/BAS ##################
table(d$BISBAS_3)



d$BAS_Drive3 <- recode(d$BISBAS_3,
                       'Very true for me\r\r\n4\r\r\n' = 4,
                       'Somewhat true for me\r\r\n3\r\r\n' = 3,
                       'Somewhat false for me\r\r\n2\r\r\n' = 2,
                       'Very false for me\r\r\n1\r\r\n' = 1) # Reverse scoring
d$BAS_Drive9 <- recode(d$BISBAS_9,
                       'Very true for me\r\r\n4\r\r\n' = 4,
                       'Somewhat true for me\r\r\n3\r\r\n' = 3,
                       'Somewhat false for me\r\r\n2\r\r\n' = 2,
                       'Very false for me\r\r\n1\r\r\n' = 1) # Reverse scoring
d$BAS_Drive12 <- recode(d$BISBAS_12,
                        'Very true for me\r\r\n4\r\r\n' = 4,
                        'Somewhat true for me\r\r\n3\r\r\n' = 3,
                        'Somewhat false for me\r\r\n2\r\r\n' = 2,
                        'Very false for me\r\r\n1\r\r\n' = 1) # Reverse scoring
d$BAS_Drive21 <- recode(d$BISBAS_21,
                        'Very true for me\r\r\n4\r\r\n' = 4,
                        'Somewhat true for me\r\r\n3\r\r\n' = 3,
                        'Somewhat false for me\r\r\n2\r\r\n' = 2,
                        'Very false for me\r\r\n1\r\r\n' = 1) # Reverse scoring

# Recoding BAS Fun Seeking
d$BAS_FunSeek5 <- recode(d$BISBAS_5,
                         'Very true for me\r\r\n4\r\r\n' = 4,
                         'Somewhat true for me\r\r\n3\r\r\n' = 3,
                         'Somewhat false for me\r\r\n2\r\r\n' = 2,
                         'Very false for me\r\r\n1\r\r\n' = 1) # Reverse scoring
d$BAS_FunSeek10 <- recode(d$BISBAS_10,
                          'Very true for me\r\r\n4\r\r\n' = 4,
                          'Somewhat true for me\r\r\n3\r\r\n' = 3,
                          'Somewhat false for me\r\r\n2\r\r\n' = 2,
                          'Very false for me\r\r\n1\r\r\n' = 1) # Reverse scoring

# Recoding BIS
d$BIS2 <- recode(d$BISBAS_2,
                 'Very true for me\r\r\n4\r\r\n' = 4,
                 'Somewhat true for me\r\r\n3\r\r\n' = 3,
                 'Somewhat false for me\r\r\n2\r\r\n' = 2,
                 'Very false for me\r\r\n1\r\r\n' = 1) # Not reverse
d$BIS8 <- recode(d$BISBAS_8,
                 'Very true for me\r\r\n4\r\r\n' = 4,
                 'Somewhat true for me\r\r\n3\r\r\n' = 3,
                 'Somewhat false for me\r\r\n2\r\r\n' = 2,
                 'Very false for me\r\r\n1\r\r\n' = 1) # Reverse scoring

# Compute Final Scores
d$BAS_Drive_Score <- rowMeans(d[, c("BAS_Drive3", "BAS_Drive9", "BAS_Drive12", "BAS_Drive21")], na.rm = TRUE)
d$BAS_FunSeek_Score <- rowMeans(d[, c("BAS_FunSeek5", "BAS_FunSeek10")], na.rm = TRUE)
d$BIS_Score <- rowMeans(d[, c("BIS2", "BIS8")], na.rm = TRUE)

# Preview Final Scores
head(d[, c("BAS_Drive_Score", "BAS_FunSeek_Score", "BIS_Score")])


##########HADS##################

table(d$HADS_1)


# Recoding Anxiety items
d$HADS_A1 <- recode(d$HADS_1,
                    'Often\r\r\n' = 3,
                    'Sometimes\r\r\n' = 2,
                    'Rarely\r\r\n' = 1,
                    'Never\r\r\n' = 0)
d$HADS_A3 <- recode(d$HADS_3,
                    'Often\r\r\n' = 3,
                    'Sometimes\r\r\n' = 2,
                    'Rarely\r\r\n' = 1,
                    'Never\r\r\n' = 0)
d$HADS_A5 <- recode(d$HADS_5,
                    'Often\r\r\n' = 3,
                    'Sometimes\r\r\n' = 2,
                    'Rarely\r\r\n' = 1,
                    'Never\r\r\n' = 0)
d$HADS_A7 <- recode(d$HADS_7,
                    'Often\r\r\n' = 3,
                    'Sometimes\r\r\n' = 2,
                    'Rarely\r\r\n' = 1,
                    'Never\r\r\n' = 0)
d$HADS_A9 <- recode(d$HADS_9,
                    'Often\r\r\n' = 3,
                    'Sometimes\r\r\n' = 2,
                    'Rarely\r\r\n' = 1,
                    'Never\r\r\n' = 0)
d$HADS_A11 <- recode(d$HADS_11,
                     'Often\r\r\n' = 3,
                     'Sometimes\r\r\n' = 2,
                     'Rarely\r\r\n' = 1,
                     'Never\r\r\n' = 0)
d$HADS_A13 <- recode(d$HADS_13,
                     'Never\r\r\n' = 3,
                     'Rarely\r\r\n' = 2,
                     'Sometimes\r\r\n' = 1,
                     'Often\r\r\n' = 0)

# Recoding Depression items
d$HADS_D2 <- recode(d$HADS_2,
                    'Often\r\r\n' = 3,
                    'Sometimes\r\r\n' = 2,
                    'Rarely\r\r\n' = 1,
                    'Never\r\r\n' = 0)
d$HADS_D4 <- recode(d$HADS_4,
                    'Never\r\r\n' = 3,
                    'Rarely\r\r\n' = 2,
                    'Sometimes\r\r\n' = 1,
                    'Often\r\r\n' = 0)
d$HADS_D6 <- recode(d$HADS_6,
                    'Never\r\r\n' = 3,
                    'Rarely\r\r\n' = 2,
                    'Sometimes\r\r\n' = 1,
                    'Often\r\r\n' = 0)
d$HADS_D8 <- recode(d$HADS_8,
                    'Never\r\r\n' = 3,
                    'Rarely\r\r\n' = 2,
                    'Sometimes\r\r\n' = 1,
                    'Often\r\r\n' = 0)
d$HADS_D10 <- recode(d$HADS_10,
                     'Never\r\r\n' = 3,
                     'Rarely\r\r\n' = 2,
                     'Sometimes\r\r\n' = 1,
                     'Often\r\r\n' = 0)
d$HADS_D12 <- recode(d$HADS_12,
                     'Never\r\r\n' = 3,
                     'Rarely\r\r\n' = 2,
                     'Sometimes\r\r\n' = 1,
                     'Often\r\r\n' = 0)

# Compute Final Scores
d$HADS_Anxiety_Score <- rowSums(d[, c("HADS_A1", "HADS_A3", "HADS_A5", "HADS_A7", "HADS_A9", "HADS_A11", "HADS_A13")], na.rm = TRUE)
d$HADS_Depression_Score <- rowSums(d[, c("HADS_D2", "HADS_D4", "HADS_D6", "HADS_D8", "HADS_D10", "HADS_D12")], na.rm = TRUE)

# Preview Final Scores
head(d[, c("HADS_Anxiety_Score", "HADS_Depression_Score")])

#########PSS#################
table (d$PSS_1)

# Regular scoring
d$PSS_1 <- recode(d$PSS_1,
                  'Never\r\r\n' = 0,
                  'Almost never\r\r\n' = 1,
                  'Sometimes\r\r\n' = 2,
                  'Fairly often\r\r\n' = 3,
                  'Very often\r\r\n' = 4)
d$PSS_2 <- recode(d$PSS_2,
                  'Never\r\r\n' = 0,
                  'Almost never\r\r\n' = 1,
                  'Sometimes\r\r\n' = 2,
                  'Fairly often\r\r\n' = 3,
                  'Very often\r\r\n' = 4)
d$PSS_3 <- recode(d$PSS_3,
                  'Never\r\r\n' = 0,
                  'Almost never\r\r\n' = 1,
                  'Sometimes\r\r\n' = 2,
                  'Fairly often\r\r\n' = 3,
                  'Very often\r\r\n' = 4)
d$PSS_6 <- recode(d$PSS_6,
                  'Never\r\r\n' = 0,
                  'Almost never\r\r\n' = 1,
                  'Sometimes\r\r\n' = 2,
                  'Fairly often\r\r\n' = 3,
                  'Very often\r\r\n' = 4)
d$PSS_9 <- recode(d$PSS_9,
                  'Never\r\r\n' = 0,
                  'Almost never\r\r\n' = 1,
                  'Sometimes\r\r\n' = 2,
                  'Fairly often\r\r\n' = 3,
                  'Very often\r\r\n' = 4)
d$PSS_10 <- recode(d$PSS_10,
                   'Never\r\r\n' = 0,
                   'Almost never\r\r\n' = 1,
                   'Sometimes\r\r\n' = 2,
                   'Fairly often\r\r\n' = 3,
                   'Very often\r\r\n' = 4)

# Reverse scoring for items 4, 5, 7, and 8
d$PSS_4 <- recode(d$PSS_4,
                  'Never\r\r\n' = 4,
                  'Almost never\r\r\n' = 3,
                  'Sometimes\r\r\n' = 2,
                  'Fairly often\r\r\n' = 1,
                  'Very often\r\r\n' = 0)
d$PSS_5 <- recode(d$PSS_5,
                  'Never\r\r\n' = 4,
                  'Almost never\r\r\n' = 3,
                  'Sometimes\r\r\n' = 2,
                  'Fairly often\r\r\n' = 1,
                  'Very often\r\r\n' = 0)
d$PSS_7 <- recode(d$PSS_7,
                  'Never\r\r\n' = 4,
                  'Almost never\r\r\n' = 3,
                  'Sometimes\r\r\n' = 2,
                  'Fairly often\r\r\n' = 1,
                  'Very often\r\r\n' = 0)
d$PSS_8 <- recode(d$PSS_8,
                  'Never\r\r\n' = 4,
                  'Almost never\r\r\n' = 3,
                  'Sometimes\r\r\n' = 2,
                  'Fairly often\r\r\n' = 1,
                  'Very often\r\r\n' = 0)

# Compute total PSS score
d$PSS_Total_Score <- rowSums(d[, c("PSS_1", "PSS_2", "PSS_3", "PSS_4", "PSS_5",
                                   "PSS_6", "PSS_7", "PSS_8", "PSS_9", "PSS_10")], na.rm = TRUE)

# Preview Final Scores
head(d[, c("PSS_Total_Score")])

###########MAAS##############
table(d$MAAS_1)

# Recoding MAAS Items (all 16 items)
d$MAAS_1 <- recode(d$MAAS_1, 
                   'Almost always\r\r\n1\r\r\n' = 1, 
                   'Very frequently\r\r\n2\r\r\n' = 2, 
                   'Somewhat frequently\r\r\n3\r\r\n' = 3, 
                   'Somewhat infrequently\r\r\n4\r\r\n' = 4, 
                   'Very infrequently\r\r\n5\r\r\n' = 5, 
                   'Almost never\r\r\n6\r\r\n' = 6)
d$MAAS_2 <- recode(d$MAAS_2, 
                   'Almost always\r\r\n1\r\r\n' = 1, 
                   'Very frequently\r\r\n2\r\r\n' = 2, 
                   'Somewhat frequently\r\r\n3\r\r\n' = 3, 
                   'Somewhat infrequently\r\r\n4\r\r\n' = 4, 
                   'Very infrequently\r\r\n5\r\r\n' = 5, 
                   'Almost never\r\r\n6\r\r\n' = 6)
d$MAAS_3 <- recode(d$MAAS_3, 
                   'Almost always\r\r\n1\r\r\n' = 1, 
                   'Very frequently\r\r\n2\r\r\n' = 2, 
                   'Somewhat frequently\r\r\n3\r\r\n' = 3, 
                   'Somewhat infrequently\r\r\n4\r\r\n' = 4, 
                   'Very infrequently\r\r\n5\r\r\n' = 5, 
                   'Almost never\r\r\n6\r\r\n' = 6)
d$MAAS_4 <- recode(d$MAAS_4, 
                   'Almost always\r\r\n1\r\r\n' = 1, 
                   'Very frequently\r\r\n2\r\r\n' = 2, 
                   'Somewhat frequently\r\r\n3\r\r\n' = 3, 
                   'Somewhat infrequently\r\r\n4\r\r\n' = 4, 
                   'Very infrequently\r\r\n5\r\r\n' = 5, 
                   'Almost never\r\r\n6\r\r\n' = 6)
d$MAAS_5 <- recode(d$MAAS_5, 
                   'Almost always\r\r\n1\r\r\n' = 1, 
                   'Very frequently\r\r\n2\r\r\n' = 2, 
                   'Somewhat frequently\r\r\n3\r\r\n' = 3, 
                   'Somewhat infrequently\r\r\n4\r\r\n' = 4, 
                   'Very infrequently\r\r\n5\r\r\n' = 5, 
                   'Almost never\r\r\n6\r\r\n' = 6)
d$MAAS_6 <- recode(d$MAAS_6, 
                   'Almost always\r\r\n1\r\r\n' = 1, 
                   'Very frequently\r\r\n2\r\r\n' = 2, 
                   'Somewhat frequently\r\r\n3\r\r\n' = 3, 
                   'Somewhat infrequently\r\r\n4\r\r\n' = 4, 
                   'Very infrequently\r\r\n5\r\r\n' = 5, 
                   'Almost never\r\r\n6\r\r\n' = 6)
d$MAAS_7 <- recode(d$MAAS_7, 
                   'Almost always\r\r\n1\r\r\n' = 1, 
                   'Very frequently\r\r\n2\r\r\n' = 2, 
                   'Somewhat frequently\r\r\n3\r\r\n' = 3, 
                   'Somewhat infrequently\r\r\n4\r\r\n' = 4, 
                   'Very infrequently\r\r\n5\r\r\n' = 5, 
                   'Almost never\r\r\n6\r\r\n' = 6)
d$MAAS_8 <- recode(d$MAAS_8, 
                   'Almost always\r\r\n1\r\r\n' = 1, 
                   'Very frequently\r\r\n2\r\r\n' = 2, 
                   'Somewhat frequently\r\r\n3\r\r\n' = 3, 
                   'Somewhat infrequently\r\r\n4\r\r\n' = 4, 
                   'Very infrequently\r\r\n5\r\r\n' = 5, 
                   'Almost never\r\r\n6\r\r\n' = 6)
d$MAAS_9 <- recode(d$MAAS_9, 
                   'Almost always\r\r\n1\r\r\n' = 1, 
                   'Very frequently\r\r\n2\r\r\n' = 2, 
                   'Somewhat frequently\r\r\n3\r\r\n' = 3, 
                   'Somewhat infrequently\r\r\n4\r\r\n' = 4, 
                   'Very infrequently\r\r\n5\r\r\n' = 5, 
                   'Almost never\r\r\n6\r\r\n' = 6)
d$MAAS_10 <- recode(d$MAAS_10, 
                    'Almost always\r\r\n1\r\r\n' = 1, 
                    'Very frequently\r\r\n2\r\r\n' = 2, 
                    'Somewhat frequently\r\r\n3\r\r\n' = 3, 
                    'Somewhat infrequently\r\r\n4\r\r\n' = 4, 
                    'Very infrequently\r\r\n5\r\r\n' = 5, 
                    'Almost never\r\r\n6\r\r\n' = 6)
d$MAAS_11 <- recode(d$MAAS_11, 
                    'Almost always\r\r\n1\r\r\n' = 1, 
                    'Very frequently\r\r\n2\r\r\n' = 2, 
                    'Somewhat frequently\r\r\n3\r\r\n' = 3, 
                    'Somewhat infrequently\r\r\n4\r\r\n' = 4, 
                    'Very infrequently\r\r\n5\r\r\n' = 5, 
                    'Almost never\r\r\n6\r\r\n' = 6)
d$MAAS_12 <- recode(d$MAAS_12, 
                    'Almost always\r\r\n1\r\r\n' = 1, 
                    'Very frequently\r\r\n2\r\r\n' = 2, 
                    'Somewhat frequently\r\r\n3\r\r\n' = 3, 
                    'Somewhat infrequently\r\r\n4\r\r\n' = 4, 
                    'Very infrequently\r\r\n5\r\r\n' = 5, 
                    'Almost never\r\r\n6\r\r\n' = 6)
d$MAAS_13 <- recode(d$MAAS_13, 
                    'Almost always\r\r\n1\r\r\n' = 1, 
                    'Very frequently\r\r\n2\r\r\n' = 2, 
                    'Somewhat frequently\r\r\n3\r\r\n' = 3, 
                    'Somewhat infrequently\r\r\n4\r\r\n' = 4, 
                    'Very infrequently\r\r\n5\r\r\n' = 5, 
                    'Almost never\r\r\n6\r\r\n' = 6)
d$MAAS_14 <- recode(d$MAAS_14, 
                    'Almost always\r\r\n1\r\r\n' = 1, 
                    'Very frequently\r\r\n2\r\r\n' = 2, 
                    'Somewhat frequently\r\r\n3\r\r\n' = 3, 
                    'Somewhat infrequently\r\r\n4\r\r\n' = 4, 
                    'Very infrequently\r\r\n5\r\r\n' = 5, 
                    'Almost never\r\r\n6\r\r\n' = 6)
d$MAAS_15 <- recode(d$MAAS_15, 
                    'Almost always\r\r\n1\r\r\n' = 1, 
                    'Very frequently\r\r\n2\r\r\n' = 2, 
                    'Somewhat frequently\r\r\n3\r\r\n' = 3, 
                    'Somewhat infrequently\r\r\n4\r\r\n' = 4, 
                    'Very infrequently\r\r\n5\r\r\n' = 5, 
                    'Almost never\r\r\n6\r\r\n' = 6)
d$MAAS_16 <- recode(d$MAAS_16, 
                    'Almost always\r\r\n1\r\r\n' = 1, 
                    'Very frequently\r\r\n2\r\r\n' = 2, 
                    'Somewhat frequently\r\r\n3\r\r\n' = 3, 
                    'Somewhat infrequently\r\r\n4\r\r\n' = 4, 
                    'Very infrequently\r\r\n5\r\r\n' = 5, 
                    'Almost never\r\r\n6\r\r\n' = 6)

# Compute the MAAS Total Score as the mean of all 16 items
d$MAAS_Total_Score <- rowMeans(d[, c("MAAS_1", "MAAS_2", "MAAS_3", "MAAS_4", 
                                     "MAAS_5", "MAAS_6", "MAAS_7", "MAAS_8", 
                                     "MAAS_9", "MAAS_10", "MAAS_11", "MAAS_12", 
                                     "MAAS_13", "MAAS_14", "MAAS_15", "MAAS_16")], na.rm = TRUE)

# Preview the Total Score
head(d[, c("MAAS_Total_Score")])


