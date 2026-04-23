#load packages#
install.packages("tidyverse")
install.packages("ggplot2")
install.packages("dplyr")
install.packages("xgboost")
install.packages("fastDummies")
library(fastDummies)
library(xgboost)
library(dplyr)
library(tidyverse)
library(ggplot2)

#import excel sheets#
PassModel <- read_excel("#filepath#" +     sheet = "Data Entry")
PassModel1<- read_excel("#filepath#", sheet = "GameSheetCP")  
CPStats <- read_excel("#filepath#", sheet = "StatsCP")

View(PassModel)

#run Logistic regression model for p-values#
Model1<-glm(Accurate~Distance+Direction+Area+`Maj. Def.`+`Min. Def.`+`Body Pt`+Foot+Touches+Thru+`R Air`+`S Air`+Cross, data=PassModel, family=binomial)
summary(Model1)

#turn variables to numeric for xgboost#
PassModel$`...16` <- NULL
PassModel$From <- NULL
PassModel_xg<- PassModel %>%
  filter(complete.cases(.)) %>%
  dummy_cols(select_columns = c("Direction", "Area", "Body Pt", "Foot", "Touches", "Thru", "R Air", "S Air", "H Air", "Cross"),
             remove_selected_columns= TRUE, remove_first_dummy = TRUE)

#Create Data Set with XGBoost#
X<-as.matrix(PassModel_xg %>% select(-Accurate))
Y<-as.factor(PassModel_xg$Accurate)

#Train XGBoost Model#
Accuracy_xg <- xgboost(x=X, y=Y, nrounds=100, objective = "binary:logistic", eval_metric = "logloss",
                    eta=.1, max_depth=4)

#generate xgboost values#
PassModel_xg$xP_xgboost <- predict(Accuracy_xg, X)
PassModel_xg$value <- PassModel_xg$Accurate - PassModel_xg$xP_xgboost

#combine with original table#
PassModel_final <- PassModel %>%
  filter(complete.cases(.)) %>%
  bind_cols(xP_xgboost = PassModel_xg$xP_xgboost,value = PassModel_xg$value)
  view(PassModel_final)

#insert player data#
CPData <- PassModel1 %>%
  dummy_cols(select_columns = c("Direction", "Area", "Body Pt", "Foot", "Touches",
                                "Thru", "R Air", "S Air", "H Air", "Cross"),
             remove_selected_columns = TRUE,
             remove_first_dummy = FALSE) %>%
  select(any_of(colnames(X)))

missing_cols <- setdiff(colnames(X), colnames(CPData))
CPData[missing_cols] <- 0
CPData <- CPData %>% select(all_of(colnames(X)))

CPData_matrix <- as.matrix(CPData)
PassModel1$xP_xgboost <- predict(Accuracy_xg, CPData_matrix)
PassModel1$value <- PassModel1$Accurate - PassModel1$xP_xgboost

player_stats <- PassModel1 %>%
    group_by(Name, Number) %>%
    summarise(
    passes = n(),
    completed = sum(Accurate),
    completion_pct = round(mean(Accurate) * 100, 1),
    total_xP = round(sum(xP_xgboost, na.rm = TRUE), 3),
    xP_per_pass = round(mean(xP_xgboost, na.rm = TRUE), 3),
    total_value = round(sum(value, na.rm = TRUE), 3),
    value_per_pass = round(mean(value, na.rm = TRUE), 3)) %>%
    arrange(desc(value_per_pass))

view(player_stats)


#Merge sheets#
player_qbr <- player_stats %>%
  left_join(CPStats, by = c("Name", "Number"))

player_qbr_ratings<- player_qbr %>%
  mutate(passes_per_90 = round((passes/Minutes)*90,1),
         QBR= round(completion_pct*.30)+
           (completion_pct*.5)+
           (`Total Impact`*.2), 3) %>%
  arrange(desc(QBR))

view(player_qbr_ratings)

#create charts for QBR#
ggplot(data=player_qbr_ratings)+geom_point(aes(x=QBR, y=passes))