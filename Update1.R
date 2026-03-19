#load packages, import dataset#
install.packages("tidyverse")
install.packages("ggplot2")
install.packages("dplyr")
library(tidyverse)
library(ggplot2)
library(dplyr)
PassModel <- read_excel("#filepath#" +     sheet = "Data Entry")
View(PassModel)

#run Logistic regression model#
Model1<-glm(Accurate~Distance+Direction+Area+`Maj. Def.`+`Min. Def.`+`Body Pt`+Foot+Touches+Thru+`R Air`+`S Air`+Cross, data=PassModel, family=binomial)
summary(Model1)

#Predict Expected Pass#
PassModel$xP <- predict(Model1, newdata = PassModel, type = "response")
view(PassModel)