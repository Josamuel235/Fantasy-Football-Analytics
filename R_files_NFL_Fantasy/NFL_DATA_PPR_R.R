library(dplyr)
library(tidyverse)
library(readr)
library(ggplot2)

## Cleaning Data sets
PPR<- PPR_cleaned_joined_2
PPR_fullClean<- distinct(PPR,Player_Id, Year, .keep_all = TRUE)

##Filtering for Year 2015
PPR_fullCleaned_2015 <- filter(PPR_fullClean, Year=="2015") 
PPR_fullClean_2015<-  mutate(PPR_fullCleaned_2015, Position = ifelse(Receiving_Yards>Rushing_Yards,"wr","rb"))
View(PPR_fullClean_2015)

## Graphs of 2015 Data
ggplot(data = PPR_fullClean_2015, mapping = aes(x=Rushing_Yards+Receiving_Yards, y=Total_PPR, color=Position))+
  geom_point()+
  geom_smooth(method = "lm", se= FALSE, formula = y~x)+
  labs(
    title = "PPR Scoring Points Distribution 2015",
    subtitle = "WR vs RB",
    y= "Points",
    x=" Total Yards"
  )
## 2015 Points to Scoring Stats correlation
cor(PPR_fullClean_2015$Total_PPR, PPR_fullClean_2015$Rushing_Yards)
cor(PPR_fullClean_2015$Total_PPR, PPR_fullClean_2015$Receiving_Yards)
cor(PPR_fullClean_2015$Total_PPR, PPR_fullClean_2015$Rushing_TD)
cor(PPR_fullClean_2015$Total_PPR, PPR_fullClean_2015$Recieving_TD)

##-----------------------------------------------------------------------------------------------------

##Filtering for Year 2016
PPR_fullCleaned_2016 <- filter(PPR_fullClean, Year=="2016")
PPR_fullClean_2016<-  mutate(PPR_fullCleaned_2016, Position = ifelse(Receiving_Yards>Rushing_Yards,"wr","rb"))
View(PPR_fullClean_2016)

## Graphs of 2016 Data
ggplot(data = PPR_fullClean_2016, mapping = aes(x=Rushing_Yards+Receiving_Yards, y=Total_PPR, color=Position))+
  geom_point()+
  geom_smooth(method = "lm", se= FALSE, formula = y~x)+
  labs(
    title = "PPR Scoring Points Distribution 2016",
    subtitle = "WR vs RB",
    y= "Points",
    x=" Total Yards"
  )
## 2016 Points to Scoring Stats correlation
cor(PPR_fullClean_2016$Total_PPR, PPR_fullClean_2016$Rushing_Yards)
cor(PPR_fullClean_2016$Total_PPR, PPR_fullClean_2016$Receiving_Yards)
cor(PPR_fullClean_2016$Total_PPR, PPR_fullClean_2016$Rushing_TD)
cor(PPR_fullClean_2016$Total_PPR, PPR_fullClean_2016$Recieving_TD)



##-----------------------------------------------------------------------------------------------------

##Filtering for Year 2017
PPR_fullCleaned_2017 <- filter(PPR_fullClean, Year=="2017") 
PPR_fullClean_2017 <- mutate(PPR_fullCleaned_2017, Position = ifelse(Receiving_Yards>Rushing_Yards,"wr","rb"))
View(PPR_fullClean_2017)

## Graphs of 2017 Data
ggplot(data = PPR_fullClean_2017, mapping = aes(x=Rushing_Yards+Receiving_Yards, y=Total_PPR, color=Position))+
  geom_point()+
  geom_smooth(method = "lm", se= FALSE, formula = y~x)+
  labs(
    title = "PPR Scoring Points Distribution 2017",
    subtitle = "WR vs RB",
    y= "Points",
    x=" Total Yards"
  )
## 2017 Points to Scoring Stats correlation
cor(PPR_fullClean_2017$Total_PPR, PPR_fullClean_2017$Rushing_Yards)
cor(PPR_fullClean_2017$Total_PPR, PPR_fullClean_2017$Receiving_Yards)
cor(PPR_fullClean_2017$Total_PPR, PPR_fullClean_2017$Rushing_TD)
cor(PPR_fullClean_2017$Total_PPR, PPR_fullClean_2017$Recieving_TD)

##-----------------------------------------------------------------------------------------------------

##Filtering for Year 2018
PPR_fullCleaned_2018 <- filter(PPR_fullClean, Year=="2018")  
PPR_fullClean_2018<-  mutate(PPR_fullCleaned_2018, Position = ifelse(Receiving_Yards>Rushing_Yards,"wr","rb"))
View(PPR_fullClean_2018)

## Graphs of 2018 Data
ggplot(data = PPR_fullClean_2018, mapping = aes(x=Rushing_Yards+Receiving_Yards, y=Total_PPR, color=Position))+
  geom_point()+
  geom_smooth(method = "lm", se= FALSE, formula = y~x)+
  labs(
    title = "PPR Scoring Points Distribution 2018",
    subtitle = "WR vs RB",
    y= "Points",
    x=" Total Yards"
  )
## 2018 Points to Scoring Stats correlation
cor(PPR_fullClean_2018$Total_PPR, PPR_fullClean_2018$Rushing_Yards)
cor(PPR_fullClean_2018$Total_PPR, PPR_fullClean_2018$Receiving_Yards)
cor(PPR_fullClean_2018$Total_PPR, PPR_fullClean_2018$Rushing_TD)
cor(PPR_fullClean_2018$Total_PPR, PPR_fullClean_2018$Recieving_TD)

#-------------------------------------------------------------------------------------------------------

##Filtering for Year 2019
PPR_fullCleaned_2019 <- filter(PPR_fullClean, Year=="2019") 
PPR_fullClean_2019<-  mutate(PPR_fullCleaned_2019, Position = ifelse(Receiving_Yards>Rushing_Yards,"wr","rb"))
View(PPR_fullClean_2019)

## Graphs of 2019 Data
ggplot(data = PPR_fullClean_2019, mapping = aes(x=Rushing_Yards+Receiving_Yards, y=Total_PPR, color=Position))+
  geom_point()+
  geom_smooth(method = "lm", se= FALSE, formula = y~x)+
  labs(
    title = "PPR Scoring Points Distribution 2019",
    subtitle = "WR vs RB",
    y= "Points",
    x=" Total Yards"
  )
## 2019 Points to Scoring Stats correlation
cor(PPR_fullClean_2019$Total_PPR, PPR_fullClean_2019$Rushing_Yards)
cor(PPR_fullClean_2019$Total_PPR, PPR_fullClean_2019$Receiving_Yards)
cor(PPR_fullClean_2019$Total_PPR, PPR_fullClean_2019$Rushing_TD)
cor(PPR_fullClean_2019$Total_PPR, PPR_fullClean_2019$Recieving_TD)


#-------------------------------------------------------------------------------------------------------

##Filtering for Year 2020
PPR_fullCleaned_2020 <- filter(PPR_fullClean, Year=="2020") 
PPR_fullClean_2020 <- mutate(PPR_fullCleaned_2020, Position = ifelse(Receiving_Yards>Rushing_Yards,"wr","rb"))
View(PPR_fullClean_2020)

## Graphs of 2020 Data
ggplot(data = PPR_fullClean_2020, mapping = aes(x=Rushing_Yards+Receiving_Yards, y=Total_PPR, color=Position))+
  geom_point()+
  geom_smooth(method = "lm", se= FALSE, formula = y~x)+
  labs(
    title = "PPR Scoring Points Distribution 2020",
    subtitle = "WR vs RB",
    y= "Points",
    x=" Total Yards"
  )
## 2020 Points to Scoring Stats correlation
cor(PPR_fullClean_2020$Total_PPR, PPR_fullClean_2020$Rushing_Yards)
cor(PPR_fullClean_2020$Total_PPR, PPR_fullClean_2020$Receiving_Yards)
cor(PPR_fullClean_2020$Total_PPR, PPR_fullClean_2020$Rushing_TD)
cor(PPR_fullClean_2020$Total_PPR, PPR_fullClean_2020$Recieving_TD)