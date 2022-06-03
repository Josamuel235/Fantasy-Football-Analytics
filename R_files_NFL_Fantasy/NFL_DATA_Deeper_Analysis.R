
## Standard SCoring

top_Standard_2015<-slice(Standard_fullClean_2015,1:40)
ggplot(data=top_Standard_2015)+
  geom_histogram(mapping=aes(x=Total_Standard, fill=Position),binwidth = 20 )+
  ggtitle("2015 Top 40 Highest Scoring")

top_Standard_2018<-slice(Standard_fullClean_2018,1:40)
ggplot(data=top_Standard_2018)+
  geom_histogram(mapping=aes(x=Total_Standard, fill=Position),binwidth = 20 )+
  ggtitle("2018 Top 40 Highest Scoring")

top_Standard_2020<-slice(Standard_fullClean_2020,1:40)
ggplot(data=top_Standard_2020)+
  geom_histogram(mapping=aes(x=Total_Standard, fill=Position),binwidth = 20 )+
  ggtitle("2020 Top 40 Highest Scoring")

##______________________________________________________________________________________


rb_2015<- filter(Standard_fullClean_2015,Position=="rb")
wr_2015<- filter(Standard_fullClean_2015,Position=="wr")
rbm_2015<- pull(rb_2015,Total_Standard)
wbm_2015<- pull(wr_2015,Total_Standard)

rb_2020<- filter(Standard_fullClean_2020,Position=="rb")
wr_2020<- filter(Standard_fullClean_2020,Position=="wr")
rbm_2020<- pull(rb_2020,Total_Standard)
wbm_2020<- pull(wr_2020,Total_Standard)

plot.function(x = function(t) dnorm(x = t,mean =mean(wbm_2015) , sd =sd(wbm_2015) ),
              from = 0,
              to = 300,
              col = "red",
              xlab = "Points",
              ylab = "Densities",
              main = "2015 RB vs WR Normal Distribution")
plot.function(x = function(t) dnorm(x = t, mean = mean(rbm_2015), sd = sd(rbm_2015) ),
              from = 0,
              to = 300,
              col = "blue",
              add = TRUE)
legend(x = "topright",
       legend = c("WR", "RB"),
       col = c("red", "blue"),
       lty = 1)



plot.function(x = function(t) dnorm(x = t,mean =mean(wbm_2020) , sd =sd(wbm_2020) ),
              from = 0,
              to = 300,
              col = "red",
              xlab = "Points",
              ylab = "Densities",
              main = "2020 RB vs WR Normal Distribution")
plot.function(x = function(t) dnorm(x = t, mean = mean(rbm_2020), sd = sd(rbm_2020) ),
              from = 0,
              to = 300,
              col = "blue",
              add = TRUE)
legend(x = "topright",
       legend = c("WR", "RB"),
       col = c("red", "blue"),
       lty = 1)


##--------------------------------------------------------------------------------------

## PPR SCoring

top_PPR_2015<-slice(PPR_fullClean_2015,1:40)
ggplot(data=top_PPR_2015)+
  geom_histogram(mapping=aes(x=Total_Standard, fill=Position),binwidth = 20 )+
  ggtitle("2015 Top 40 Highest Scoring")

top_PPR_2018<-slice(PPR_fullClean_2018,1:40)
ggplot(data=top_PPR_2018)+
  geom_histogram(mapping=aes(x=Total_Standard, fill=Position),binwidth = 20 )+
  ggtitle("2018 Top 40 Highest Scoring")

top_PPR_2020<-slice(PPR_fullClean_2020,1:40)
ggplot(data=top_PPR_2020)+
  geom_histogram(mapping=aes(x=Total_Standard, fill=Position),binwidth = 20 )+
  ggtitle("2020 Top 40 Highest Scoring")

##----------------------------------------------------------------------------------------

rb_PPR_2020<- filter(Standard_fullClean_2015,Position=="rb")
wr_PPR_2015<- filter(Standard_fullClean_2015,Position=="wr")
rbp_2015<- pull(rb_2015,Total_PPR)
wbp_2015<- pull(wr_2015,Total_PPR)

rb_PPR_2020<- filter(Standard_fullClean_2020,Position=="rb")
wr_PPR_2020<- filter(Standard_fullClean_2020,Position=="wr")
rbp_2020<- pull(rb_2020,Total_PPR)
wbp_2020<- pull(wr_2020,Total_PPR)

plot.function(x = function(t) dnorm(x = t,mean =mean(wbp_2015) , sd =sd(wbp_2015) ),
              from = 0,
              to = 400,
              col = "red",
              xlab = "Points",
              ylab = "Densities",
              main = "2015 RB vs WR Normal Distribution")
plot.function(x = function(t) dnorm(x = t, mean = mean(rbp_2015), sd = sd(rbp_2015) ),
              from = 0,
              to = 400,
              col = "blue",
              add = TRUE)
legend(x = "topright",
       legend = c("WR", "RB"),
       col = c("red", "blue"),
       lty = 1)



plot.function(x = function(t) dnorm(x = t,mean =mean(wbp_2020) , sd =sd(wbp_2020) ),
              from = 0,
              to = 400,
              col = "red",
              xlab = "Points",
              ylab = "Densities",
              main = "2020 RB vs WR Normal Distribution")
plot.function(x = function(t) dnorm(x = t, mean = mean(rbp_2020), sd = sd(rbp_2020) ),
              from = 0,
              to = 400,
              col = "blue",
              add = TRUE)
legend(x = "topright",
       legend = c("WR", "RB"),
       col = c("red", "blue"),
       lty = 1)

