#load packages#
install.packages("tidyverse")
install.packages("ggplot2")
install.packages("dplyr")
install.packages("xgboost")
install.packages("fastDummies")
install.packages("ggimage")
install.packages("ggrepel")
library(fastDummies)
library(xgboost)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(readxl)
library(ggimage)
library(ggrepel)

#import excel sheets#
PassModel<- read_excel("Footy/PassModel.xlsx", sheet = "Data Entry")  
PassModelCRY<- read_excel("Footy/PassModel.xlsx", sheet = "GameSheetCRY")  
CPStats <- read_excel("Footy/PassModel.xlsx", sheet = "ImpactStatsCRY")
PassModelAST<- read_excel("Footy/PassModel.xlsx", sheet = "GameSheetAST")  
ASTStats <- read_excel("Footy/PassModel.xlsx", sheet = "ImpactStatsAST")

View(PassModel)

#run Logistic regression model for p-values#
Model1<-glm(Accurate~Distance+Direction+Area+`Maj. Def.`+`Min. Def.`+`Body Pt`+Foot+Touches+Thru+`R Air`+`S Air`+Cross, data=PassModel, family=binomial)
summary(Model1)
view(Model1)

#turn variables to numeric for xgboost#
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
                    learning_rate=.1, max_depth=4)

#generate xgboost values#
PassModel_xg$xP_xgboost <- predict(Accuracy_xg, X)
PassModel_xg$value <- PassModel_xg$Accurate - PassModel_xg$xP_xgboost

#combine with original table#
PassModel_final <- PassModel %>%
  filter(complete.cases(.)) %>%
  bind_cols(xP_xgboost = PassModel_xg$xP_xgboost,value = PassModel_xg$value)
  view(PassModel_final)

#insert player data#
CPData <- PassModelCRY %>%
  dummy_cols(select_columns = c("Direction", "Area", "Body Pt", "Foot", "Touches",
                                "Thru", "R Air", "S Air", "H Air", "Cross"),
             remove_selected_columns = TRUE,
             remove_first_dummy = FALSE) %>%
  select(any_of(colnames(X)))

missing_cols <- setdiff(colnames(X), colnames(CPData))
CPData[missing_cols] <- 0
CPData <- CPData %>% select(all_of(colnames(X)))

CPData_matrix <- as.matrix(CPData)
PassModelCRY$xP_xgboost <- predict(Accuracy_xg, CPData_matrix)
PassModelCRY$value <- PassModelCRY$Accurate - PassModelCRY$xP_xgboost

player_stats <- PassModelCRY %>%
  group_by(Name, Number) %>%
  summarise(
    passes = n(),
    completed = sum(Accurate),
    completion_pct = round(mean(Accurate) * 100, 1),
    total_xP = round(sum(xP_xgboost, na.rm = TRUE), 3),
    xP_per_pass = round(mean(xP_xgboost, na.rm = TRUE), 3),
    total_value = round(sum(value, na.rm = TRUE), 3),
    value_per_pass = round(mean(value, na.rm = TRUE), 3)
  ) %>%
  arrange(desc(value_per_pass))

view(player_stats)

player_qbr_ratings <- player_stats %>%
  left_join(CPStats, by = c("Name", "Number")) %>%
  mutate(
    passes_per90 = round((passes / Minutes) * 90, 1),
    completion_100 = pmin(pmax(((completion_pct - 0) / (100 - 0)) * 100, 0), 100),
    value_100      = pmin(pmax(((value_per_pass - (-0.2)) / (0.2 - (-0.2))) * 100, 0), 100),
    impact_100     = pmin(pmax(((`Total Impact` - (-2.5)) / (2.5 - (-2.5))) * 100, 0), 100),
    QBR_100 = round(
      (completion_100 * 0.30) +
        (value_100 * 0.50) +
        (impact_100 * 0.20),
      1)
  ) %>%
  arrange(desc(QBR_100)) %>%
  select(Name, Number, passes, passes_per90, completed, completion_pct, 
         value_per_pass, `Total Impact`, QBR_100)

View(player_qbr_ratings)

#create charts for QBR#
download.file(
  "https://upload.wikimedia.org/wikipedia/en/a/a2/Crystal_Palace_FC_logo_%282022%29.svg",
  destfile = "palace_logo.svg",
  mode = "wb"
  
CPQBRPlot1<-ggplot(player_qbr_ratings, aes(x=passes_per90, y=QBR_100, image="palace_logo.svg"))+
geom_image(size=.08)+labs(title= "QBR Rating and Per 90 Involvement", subtitle = "Crystal Palace vs. Newcastle MW32", x="Passes Attemped Per 90 Minutes", y="Quarterback Rating")+
  theme_bw()+theme(plot.title=element_text(face="bold", hjust=.5, size=14, family=""), 
                   plot.subtitle = element_text(hjust=.5))
CPQBRPlot2<-CPQBRPlot1+xlim(0,60)+ylim(0,100)+geom_text_repel(aes(label=Name), fontface="bold", max.overlaps=20, box.padding=1)+
  annotate(geom="text", x=2, y=50, fontface="bold", 
           label="Average QBR", color="blue", size=3.5)+
  annotate(geom="text", x=3, y=75, fontface="bold", 
           label="Above Average QBR", color="darkgreen",size=3.5)+
  annotate(geom="text", x=3, y=25, fontface="bold", 
           label="Below Average QBR", color="red",size=3.5)
CPQBRPlot2

#histogram of xP values#
xPHistogram<-ggplot(data=PassModel_final)+geom_histogram(aes(x=xP_xgboost), binwidth = .02, fill="steelblue",color="black")+
  labs(title="Distribution of Expected Pass Values", subtitle="n=2985, Collected from 2025-26 EPL Season", x="Expected Pass Value", y=("Count"))+
  theme_bw()+
  theme(plot.title=element_text(face="bold", hjust=.5, size=14), plot.subtitle=element_text(hjust=.5))

xPHistogramFinal<-xPHistogram+annotate(geom="text", x=.50, y=400, fontface="bold", 
                                       label="Large Amount of High Expected Passes")
xPHistogramFinal

