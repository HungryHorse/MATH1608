setwd("E:\\MATH1608PP\\rep")
library(readr)
library(dbplyr) 
library(dplyr)
library(tidyr)
library(ggplot2)
library(modelr)
#Task 1
allergies <- src_sqlite("allergies_db.sqlite3")
allergies

tbl(allergies, "allergy_tests") %>% rename(id = code)

tbl(allergies, "children_info")

dataset <- inner_join(tbl(allergies, "allergy_tests") %>% rename(id = code),
                     tbl(allergies, "children_info"),
                     by = "id")

dataset

dataset <- collect(dataset)

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
dataset
dataset <- dataset %>% mutate(allergy_total = dust_mites_1 + dust_mites_2 + grass + birch + hazel_tree +
                                pellitory + mould + poplar + cypress + sunflower + olive + ragweed + egg_white + egg_yolk +
                                milk + peanut + hazel + tomato + fish + wheat + apple + cat)

dataset <- dataset %>% mutate(allergic = 
                                ifelse(allergy_total > 0, "Allergic", "Not Allergic"))
dataset %>% select(allergy_total, allergic, pathology)

set2 <- dataset %>% select(pathology, IgE, allergy_total, allergic)
set2 <- summarise(set2 %>% group_by(pathology), ave_IgE = mean(IgE), ave_allergies = mean(allergy_total))
set2

ggplot(set2, aes(x = pathology, y = ave_allergies)) +
  geom_col(fill = rep(c("red", "orange", "yellow"), length = 7)) + 
  labs(x = "Pathology Type",
       y = "Average number of allergies",
       title = "Mean number of allergies for different pathologies")

#Task 4
set4 <- dataset %>% select(pathology, IgE)
set4 <- summarise(set4 %>% group_by(pathology), ave_IgE = mean(IgE))
ggplot(set4, aes(x = pathology, y = ave_IgE)) +
  geom_col() +
  labs(x = "Pathology Type",
       y = "Average IgE level",
       title = "Mean IgE levels for different pathologies")

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

set6A <- set6 %>% filter(family_allergy == "Yes")
set6A
set6B <- set6 %>% filter(family_allergy == "No")
set6B
ggplot(set6, aes(x = IgE, y = allergy_total, col = family_allergy)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

#Task 7
