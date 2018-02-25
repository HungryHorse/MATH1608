#Set working directory
setwd("G:\\MATH1608PP\\rep")

#Install Packages
install.packages("tidyr")
install.packages("ggplot2")
install.packages("readr")
install.packages("dbplyr")
install.packages("dplyr")
install.packages("modelr")
install.packages("likert")
install.packages("RSQLite")


#Load Packages
library(readr)
library(dbplyr) 
library(dplyr)
library(tidyr)
library(ggplot2)
library(modelr)
library(likert)


#Task 1
#Read the database
allergies <- src_sqlite("allergies_db.sqlite3")
allergies

#Rename code column so the two tables can be merged properly
tbl(allergies, "allergy_tests") %>% rename(id = code)

tbl(allergies, "children_info")

#Merge the two tables
dataset <- inner_join(tbl(allergies, "allergy_tests") %>% rename(id = code),
                     tbl(allergies, "children_info"),
                     by = "id")

dataset

dataset <- collect(dataset)

#Look at the columns we need to change
dataset %>% select(id, gender, family_allergy, birth_order, pets, smoke)

#Mutate into factors
dataset <- dataset %>% mutate(gender = factor(gender, levels = c(1, 2), labels = c("Male", "Female")))
dataset <- dataset %>% mutate(family_allergy = factor(family_allergy, levels = c(0, 1), labels = c("No", "Yes")))
dataset <- dataset %>% mutate(birth_order = factor(birth_order, levels = min(birth_order):max(birth_order)))
dataset <- dataset %>% mutate(pets = factor(pets, levels = c(0, 1), labels = c("No", "Yes")))
  dataset <- dataset %>% mutate(smoke = factor(smoke, levels = c(0, 1), labels = c("No", "Yes")))


dataset %>% select(id, gender, family_allergy, birth_order, pets, smoke)


#Task 2

dataset %>% select(pathology)
#Mutate pathology into proper factors
dataset <- dataset %>% mutate(pathology = factor(pathology, levels = 1:7, labels = c("asthma", "pink eye", "eczema", "rash", "rhinitis", "cough", "other")))
dataset %>% select(pathology)

pathologies <- dataset %>% select(pathology, IgE)
set <- summarise(pathologies %>% group_by(pathology), ave_IgE = mean(IgE))
set

ggplot(set, aes(x = pathology, y = ave_IgE)) +
       geom_col(fill = rep(c("darkblue", "blue", "black"), length = 7)) +
      labs(x = "Pathology Type",
           y = "Average IgE Level",
           title = "Mean IgE level for different pathologies")


#Task 3

#Create a new column holding the total number of allergies a child has
dataset
dataset <- dataset %>% mutate(allergy_total = dust_mites_1 + dust_mites_2 + grass + birch + hazel_tree +
                                pellitory + mould + poplar + cypress + sunflower + olive + ragweed + egg_white + egg_yolk +
                                milk + peanut + hazel + tomato + fish + wheat + apple + cat)

#Create another column stating whether or not a child is allergic to anything
dataset <- dataset %>% mutate(allergic = 
                                ifelse(allergy_total > 0, 1, 0))
dataset %>% select(allergy_total, allergic, pathology)

#Create a new table with only the needed columns, for ease of use
set2 <- dataset %>% select(pathology, IgE, allergy_total, allergic, gender)
set2 <- summarise(set2 %>% group_by(pathology), ave_IgE = mean(IgE), ave_allergies = mean(allergy_total))
set2

#
ggplot(set2, aes(x = pathology, y = ave_allergies)) +
  geom_col(fill = rep(c("red", "orange", "yellow"), length = 7)) + 
  labs(x = "Pathology Type",
       y = "Average number of allergies",
       title = "Mean number of allergies for different pathologies")

genderSet <- summarise(dataset %>% group_by(gender), ave_allergies = mean(allergy_total))
ggplot(genderSet, aes(x = gender, y = ave_allergies)) +
  geom_col(fill = c("lightblue","pink")) +
  labs(x = "Gender",
       y = "Mean number of allergies",
       title = "Comparing gender to total number of allergies")

#Task 4
#Create a new table with only the required columns
set4 <- dataset %>% select(pathology, IgE)
set4 <- summarise(set4 %>% group_by(pathology), ave_IgE = mean(IgE))
set4
ggplot(set4, aes(x = pathology, y = ave_IgE)) +
  geom_col() +
  labs(x = "Pathology Type",
       y = "Average IgE level",
       title = "Mean IgE levels for different pathologies")

#Create a dataframe so a P-value can be found
IgE <- dataset %>% select(IgE)
pathology <- dataset %>% select(pathology)

df <- data.frame(IgE, pathology)


m <- lm(IgE ~ pathology, data = df)
m
new <- anova(m)
summary(new)
anova(m)$"Pr(>F)"[1]
#Shows the pathology type has no effect on the average IgE level - the two are independent


#Task 5
set3 <- dataset %>% select(IgE, allergy_total)
set3
IgE <- dataset %>% select(IgE)
totalAllergy <- dataset %>% select(allergy_total)
df <- data.frame(IgE, totalAllergy)
df

ggplot(df, aes(x = IgE, y = allergy_total)) +
  geom_smooth(method = "lm", se = FALSE, col = "blue") +
  geom_point()

cor(df)

m <- lm(allergy_total ~ IgE, data = df)
confint(m)

df_confidence <- data.frame(IgE, totalAllergy, predict(m, interval = "confidence"))
df_confidence

df_prediction <- data.frame(IgE, totalAllergy, predict(m, interval = "prediction"))
df_prediction

ggplot(df, aes(x = IgE, y = allergy_total)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, col = "blue") +
  # Add the lower confidence interval limits
  geom_smooth(aes(x = IgE, y = lwr), # We're plotting age on the x-axis and upr on the y-axis
            data = df_confidence, # We use the data in the data frame df_confidence
            colour = "blue") +
  # Add the upper confidence interval limits
  geom_smooth(aes(x = IgE, y = upr), # We're plotting age on the x-axis and upr on the y-axis
            data = df_confidence, # We use the data in the data frame df_confidence
            colour = "blue") +
  # Add the lower prediction interval limits
  geom_smooth(aes(x = IgE, y = lwr), # We're plotting age on the x-axis and upr on the y-axis
            data = df_prediction, # We use the data in the data frame df_prediction
            colour = "red") + # We're plotting in red
  # Add the upper prediction interval limits
  geom_smooth(aes(x = IgE, y = upr), # We're plotting age on the x-axis and upr on the y-axis
            data = df_prediction, # We use the data in the data frame df_prediction
            colour = "red") + # We're plotting in red
  labs(x = "IgE",
       y = "Total Allergies",
       title = "IgE/Allergy")


#Task 6
set6 <- dataset %>% select(IgE, allergy_total, family_allergy)
set6

ggplot(set6, aes(x = IgE, y = allergy_total, col = family_allergy)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "IgE",
       y = "Total number of allergies",
       title = "dab on those allergies boi",
       colour = "Family Allergies")

#Task 7
dataset

