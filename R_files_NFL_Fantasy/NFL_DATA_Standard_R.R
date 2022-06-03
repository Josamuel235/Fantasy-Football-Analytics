## Cleaning Data sets
Standard<- Standard_cleaned_joined 
Standard_fullClean <- distinct(Standard,Player_Id, Year, .keep_all = TRUE)

##Filtering for Year 2015
Standard_fullClean_2015 <- filter(Standard_fullClean, Year=="2015") %>% 
  mutate(Standard_fullClean_2015, Position = ifelse(Receiving_Yards>Rushing_Yards,"wr","rb"))
View(Standard_fullClean_2015)

## Graphs of 2015 Data
ggplot(data = Standard_fullClean_2015, mapping = aes(x=Rushing_Yards+Receiving_Yards, y=Total_Standard, color=Position))+
  geom_point()+
  geom_smooth(method = "lm", se= FALSE, formula = y~x)+
  labs(
    title = "Standard Scoring Points Distribution",
    subtitle = "WR vs RB",
    y= "Points",
    x=" Total Yards"
  )
## 2015 Points to Scoring Stats correlation
cor(Standard_fullClean_2015$Total_Standard, Standard_fullClean_2015$Rushing_Yards)
cor(Standard_fullClean_2015$Total_Standard, Standard_fullClean_2015$Receiving_Yards)
cor(Standard_fullClean_2015$Total_Standard, Standard_fullClean_2015$Rushing_TD)
cor(Standard_fullClean_2015$Total_Standard, Standard_fullClean_2015$Recieving_TD)

##-----------------------------------------------------------------------------------------------------

##Filtering for Year 2016
Standard_fullClean_2016 <- filter(Standard_fullClean, Year=="2016") %>% 
  mutate(Standard_fullClean_2016, Position = ifelse(Receiving_Yards>Rushing_Yards,"wr","rb"))
View(Standard_fullClean_2016)

## Graphs of 2016 Data
ggplot(data = Standard_fullClean_2016, mapping = aes(x=Rushing_Yards+Receiving_Yards, y=Total_Standard, color=Position))+
  geom_point()+
  geom_smooth(method = "lm", se= FALSE, formula = y~x)+
  labs(
    title = "Standard Scoring Points Distribution",
    subtitle = "WR vs RB",
    y= "Points",
    x=" Total Yards"
  )
## 2016 Points to Scoring Stats correlation
cor(Standard_fullClean_2016$Total_Standard, Standard_fullClean_2016$Rushing_Yards)
cor(Standard_fullClean_2016$Total_Standard, Standard_fullClean_2016$Receiving_Yards)
cor(Standard_fullClean_2016$Total_Standard, Standard_fullClean_2016$Rushing_TD)
cor(Standard_fullClean_2016$Total_Standard, Standard_fullClean_2016$Recieving_TD)



##-----------------------------------------------------------------------------------------------------

##Filtering for Year 2017
Standard_fullClean_2017 <- filter(Standard_fullClean, Year=="2017") %>% 
  mutate(Standard_fullClean_2017, Position = ifelse(Receiving_Yards>Rushing_Yards,"wr","rb"))
View(Standard_fullClean_2017)

## Graphs of 2017 Data
ggplot(data = Standard_fullClean_2017, mapping = aes(x=Rushing_Yards+Receiving_Yards, y=Total_Standard, color=Position))+
  geom_point()+
  geom_smooth(method = "lm", se= FALSE, formula = y~x)+
  labs(
    title = "Standard Scoring Points Distribution",
    subtitle = "WR vs RB",
    y= "Points",
    x=" Total Yards"
  )
## 2017 Points to Scoring Stats correlation
cor(Standard_fullClean_2017$Total_Standard, Standard_fullClean_2017$Rushing_Yards)
cor(Standard_fullClean_2017$Total_Standard, Standard_fullClean_2017$Receiving_Yards)
cor(Standard_fullClean_2017$Total_Standard, Standard_fullClean_2017$Rushing_TD)
cor(Standard_fullClean_2017$Total_Standard, Standard_fullClean_2017$Recieving_TD)

##-----------------------------------------------------------------------------------------------------

##Filtering for Year 2018
Standard_fullClean_2018 <- filter(Standard_fullClean, Year=="2018") %>% 
  mutate(Standard_fullClean_2018, Position = ifelse(Receiving_Yards>Rushing_Yards,"wr","rb"))
View(Standard_fullClean_2018)

## Graphs of 2018 Data
ggplot(data = Standard_fullClean_2018, mapping = aes(x=Rushing_Yards+Receiving_Yards, y=Total_Standard, color=Position))+
  geom_point()+
  geom_smooth(method = "lm", se= FALSE, formula = y~x)+
  labs(
    title = "Standard Scoring Points Distribution",
    subtitle = "WR vs RB",
    y= "Points",
    x=" Total Yards"
  )
## 2018 Points to Scoring Stats correlation
cor(Standard_fullClean_2018$Total_Standard, Standard_fullClean_2018$Rushing_Yards)
cor(Standard_fullClean_2018$Total_Standard, Standard_fullClean_2018$Receiving_Yards)
cor(Standard_fullClean_2018$Total_Standard, Standard_fullClean_2018$Rushing_TD)
cor(Standard_fullClean_2018$Total_Standard, Standard_fullClean_2018$Recieving_TD)

#-------------------------------------------------------------------------------------------------------

##Filtering for Year 2019
Standard_fullClean_2019 <- filter(Standard_fullClean, Year=="2019") %>% 
  mutate(Standard_fullClean_2019, Position = ifelse(Receiving_Yards>Rushing_Yards,"wr","rb"))
View(Standard_fullClean_2019)

## Graphs of 2019 Data
ggplot(data = Standard_fullClean_2019, mapping = aes(x=Rushing_Yards+Receiving_Yards, y=Total_Standard, color=Position))+
  geom_point()+
  geom_smooth(method = "lm", se= FALSE, formula = y~x)+
  labs(
    title = "Standard Scoring Points Distribution",
    subtitle = "WR vs RB",
    y= "Points",
    x=" Total Yards"
  )
## 2019 Points to Scoring Stats correlation
cor(Standard_fullClean_2019$Total_Standard, Standard_fullClean_2019$Rushing_Yards)
cor(Standard_fullClean_2019$Total_Standard, Standard_fullClean_2019$Receiving_Yards)
cor(Standard_fullClean_2019$Total_Standard, Standard_fullClean_2019$Rushing_TD)
cor(Standard_fullClean_2019$Total_Standard, Standard_fullClean_2019$Recieving_TD)


#-------------------------------------------------------------------------------------------------------

##Filtering for Year 2020
Standard_fullClean_2020 <- filter(Standard_fullClean, Year=="2020") %>% 
  mutate(Standard_fullClean_2020, Position = ifelse(Receiving_Yards>Rushing_Yards,"wr","rb"))
View(Standard_fullClean_2020)

## Graphs of 2020 Data
ggplot(data = Standard_fullClean_2020, mapping = aes(x=Rushing_Yards+Receiving_Yards, y=Total_Standard, color=Position))+
  geom_point()+
  geom_smooth(method = "lm", se= FALSE, formula = y~x)+
  labs(
    title = "Standard Scoring Points Distribution",
    subtitle = "WR vs RB",
    y= "Points",
    x=" Total Yards"
  )
## 2020 Points to Scoring Stats correlation
cor(Standard_fullClean_2020$Total_Standard, Standard_fullClean_2020$Rushing_Yards)
cor(Standard_fullClean_2020$Total_Standard, Standard_fullClean_2020$Receiving_Yards)
cor(Standard_fullClean_2020$Total_Standard, Standard_fullClean_2020$Rushing_TD)
cor(Standard_fullClean_2020$Total_Standard, Standard_fullClean_2020$Recieving_TD)