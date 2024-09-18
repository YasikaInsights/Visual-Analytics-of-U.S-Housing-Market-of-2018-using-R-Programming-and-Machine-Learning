getwd()
setwd("C:/Visual Analytics/Final Exam Spring 2024")

install.packages("haven") 

library(haven)

newdata <- read_dta("acs.dta")

data.1 <- subset(newdata, age >= 18 & age <= 64) #segregated data by age 18-64

data.1 <- na.omit(data.1) #removes missing values


#Discriptive Analysis:

# Plot-1 #Homeownership status by state

install.packages("tidyverse")
library(tidyverse)
library(readr)
library(ggplot2)
library(scales)
library(dplyr)

ownership_proportions <- data.1 %>%
  mutate(
    ownershp = factor(ownershp, levels = c(0, 1, 2), labels = c("Not Bought", "Owned/Bought", "Rented"))
  ) %>%
  group_by(statefip, ownershp) %>%
  summarise(
    count = n(),
    .groups = 'drop'
  ) %>%
  group_by(statefip) %>%
  mutate(total = sum(count)) %>%
  ungroup() %>%
  mutate(proportion = count / total) %>%
  arrange(statefip, ownershp)


p1 <- ggplot(ownership_proportions, aes(x = as.factor(statefip), y = proportion, group = ownershp, color = ownershp)) +
  geom_line(size = 1.1) +
  geom_point(size = 1.9) + # Adding points for clarity
  geom_text(aes(label = scales::percent(proportion, accuracy = 0.1)), 
            position = position_dodge(width = 0.2), 
            vjust = -0.5, check_overlap = TRUE, size = 3) + # Add percentage labels
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    title = "Proportion of Homeownership Status by State of 2018",
    x = "State",
    y = "Proportion of Ownership",
    color = "Ownership Status"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1) # Rotate state labels for better visibility
  )

print(p1)




#Plot- 2 #Avearage household income by state

library(ggplot2)
library(dplyr)

collapsed.data <- data.1 %>%
  group_by(statefip) %>%
  summarise(avg_rate = mean(hhincome, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(statefip = as.factor(statefip))  

max_income <- max(collapsed.data$avg_rate, na.rm = TRUE)

p2 <- ggplot(collapsed.data, aes(x = statefip, y = avg_rate)) +
  geom_line(group = 1) +  
  geom_point() +  
  labs(
    title = "Average Household Income by State",
    x = "State FIP Code",
    y = "Average Income"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  
  scale_y_continuous(
    breaks = seq(0, max_income, by = 100000),  
    limits = c(0, max_income + 100000)  
  )

print(p2)


#Plot-3 #Graph by geographic areas and socioeconomic characteristics

newdata1 <- select(data.1, ownershp, statefip,inctot, sex)

newdata1 <- newdata1 %>%
  mutate(sex = factor(sex, levels = c(1, 2), labels = c("Male", "Female")))

p3 <- ggplot(newdata1, aes(x = as.factor(statefip), y = ownershp, fill = as.factor(sex))) +
  geom_bar(stat = "identity") +
  labs(
    title = "U.S. Homeownership by State of 2018",
    x = "State",
    y = "Homeowner's Count",
    fill = "Sex"
  ) +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_discrete(breaks = unique(newdata1$statefip)) +
  ylim(0, 250000)+
  annotate(geom = "text", x = "6", y = 234707, label = "California", color = "darkgreen", vjust = -6) +
  annotate(geom = "text", x = "48", y = 161120, label = "Texas", color = "darkgreen", vjust = -14) +
  annotate(geom = "text", x = "36", y = 120564, label = "New York", color = "darkgreen", vjust = -15) +
  annotate(geom = "text", x = "56", y = 3312, label = "Wyoming", color = "red", vjust = -15) +
  annotate(geom = "text", x = "50", y = 3829, label = "Vermont", color = "red", vjust = -15) +
  annotate(geom = "text", x = "2", y = 4072, label = "Alaska", color = "red", vjust = -13)+
  annotate(geom = "text", x = "6", y = 234707, label = "234707", color = "black", vjust = -4.5) +
  annotate(geom = "text", x = "48", y = 161120, label = "161120", color = "black", vjust = -12) +
  annotate(geom = "text", x = "36", y = 120564, label = "120564", color = "black", vjust = -13) +
  annotate(geom = "text", x = "56", y = 3312, label = "3312", color = "black", vjust = -13) +
  annotate(geom = "text", x = "50", y = 3829, label = "3829", color = "black", vjust = -13) +
  annotate(geom = "text", x = "2", y = 4072, label = "4072", color = "black", vjust = -11)

print(p3)


# Plot : 4 - Homeownership by state & class of workers
newdata1 <- select(data.1, ownershp, statefip,hhincome,educ, sex, race, classwkr)

newdata2 <- newdata1 %>%
  mutate(classwkr = factor(classwkr, levels = c(2,1,0), labels = c("works for wages","self-employed", "n/a")))

p4 <- ggplot(newdata2, aes(x = as.factor(statefip), y = ownershp, fill = as.factor(classwkr))) +
  geom_bar(stat = "identity") + 
  labs(
    title = "U.S. Homeownership by State and Class of worker of 2018",
    x = "State",
    y = "Homeowner's Count",
    fill = "class of worker"
  ) +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_discrete(breaks = unique(newdata1$statefip)) +
  scale_fill_manual(values = c("works for wages" = "cornflowerblue", "self-employed" = "lightblue", "n/a"= "azure3")) +
  ylim(0, 250000) +
  annotate(geom = "text", x = "6", y = 234707, label = "California", color = "darkgreen", hjust = -1.2) +
  annotate(geom = "text", x = "48", y = 161120, label = "Texas", color = "darkgreen", hjust = -5) +
  annotate(geom = "text", x = "36", y = 120564, label = "New York", color = "darkgreen", hjust = -2.8) +
  annotate(geom = "text", x = "56", y = 3312, label = "Wyoming", color = "red", hjust = -3) +
  annotate(geom = "text", x = "50", y = 3829, label = "Vermont", color = "red", hjust = -5) +
  annotate(geom = "text", x = "2", y = 4072, label = "Alaska", color = "red", hjust = -5)

p4_horizontal <- p4 +
  coord_flip()

print(p4_horizontal)

###Prediction :Linear Regression Model

install.packages("caret")  
library(caret)   
library(haven)              
library(dplyr)               
set.seed(1234)                


data.2 <- sample_frac(data.1, 0.1) 

model.1 <- lm(ownershp ~ hhincome + educ + race + sex + classwkr, data = data.2) 

summary(model.1) 

predict.data <- predict(model.1) 

residuals <- residuals(model.1) 

rmse <- sqrt(mean(model.1$residuals^2)) 

print(rmse)


#Splitting data into training and validation

set.seed(1234)

selected <- sample(1:nrow(data.2), size=nrow(data.2)*0.8, replace=FALSE) 

train.data1 <- data.2[selected,] 
validation.data <- data.2[-selected,] 

model.2 <- lm(ownershp ~ hhincome + educ + race + sex + classwkr, data = train.data1)

summary(model.2)

#Modeling & PREDICTION (VALIDATION DATA)

predict.data <- predict(model.2, newdata= validation.data) 

validation.residuals <- validation.data$ownershp - predict.data 

rmse <- sqrt(mean(validation.residuals^2)) 
print(rmse)


#K- Cross Validation :10-fold cross-validation :
library(caret)
library(dplyr)

set.seed(1234)
data.2 <- sample_frac(data.1, 0.1)

data.2$ownershp <- as.numeric(as.character(data.2$ownershp))  
data.2$race <- as.factor(data.2$race)                         
data.2$sex <- as.factor(data.2$sex)

set.seed(1234)
train_control <- trainControl(method = "cv", number = 10)  

cv_model <- train(ownershp ~ hhincome + educ + race + sex + classwkr, data = data.2, method = "lm", trControl = train_control)

print(cv_model)
summary(cv_model)



#K-NN Model
library(caret)
library(haven)

set.seed(123)  # Set seed for reproducibility

train_index <- createDataPartition(data.2$ownershp, p = 0.75, list = FALSE)

train.data <- data.2[train_index, ]

validation.data <- data.2[-train_index, ]

train.data$ownershp <- as.factor(train.data$ownershp)

predata <- preProcess(train.data[,c("hhincome", "educ", "race", "sex")],
                      method= "range")

Newtrain.data <- predict(predata,train.data)

model.3 <- knn3(ownershp ~ hhincome + educ + race + sex, 
                data = Newtrain.data,
                k = 500)
print(model.3)


best.model <- knn3(ownershp ~ hhincome + educ + race + sex,
                   data = validation.data,
                   k = 31
)



validation.data1 <- predict(predata, validation.data)

validation.prediction <- predict(model.3, newdata = validation.data1)

confusion_matrix <- table(validation_predictions, validation_data$ownershp)