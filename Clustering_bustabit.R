library(tidyverse)
library(dplyr)

#loading the dataset
bustabit <- read_csv("C:/Users/Ideapad/OneDrive/Desktop/bustabit.csv")
str(bustabit)
head(bustabit, n = 5)

#simple EDA
bustabit%>%
  arrange(desc("BustedAt"))%>%
  slice(1)

co <- bustabit$CashedOut
bt <- bustabit$BustedAt
pr <- bustabit$Profit

#creating new features
bustabitf <- bustabit%>%
  mutate(
    CashedOut = ifelse(is.na(co), bt + 0.01, co),
    Profit = ifelse(is.na(pr), 0, pr),
    Losses = ifelse(Profit == 0, -1 * Bet, 0),
    GameWon = ifelse(Profit == 0, 0, 1),
    GameLost = ifelse(Profit == 0, 1, 0)
  )

bustabit

#grouping by players' user to create pre-player stats
bb.cluster<- bustabitf%>%
  group_by(Username)%>%
  summarize(AverageCashedOut = mean(CashedOut),
            AverageBet = mean(Bet),
            TotalProfit = sum(Profit),
            TotalLosses = sum(Losses),
            GamesWon = sum(GameWon),
            GamesLost = sum(GameLost)
  )

head(bb.cluster, n =5)

#scaling and normalization
z_score <- function(x){
  (x - mean(x))/ sd(x)
}

bb.standard<- bb.cluster%>%
  mutate_if(is.numeric, z_score)
summary(bb.standard)

#clustering by K-means algorithm
set.seed(75602315)
final.clus<- kmeans(bb.standard[-1], centers = 5)
bb.cluster$cluster<- factor(final.clus$cluster)
table(bb.cluster$cluster)

#grouping by the clusters and calculate averages
bb.clus.avg<- bb.cluster%>%
  group_by(cluster)%>%
  summarize_if(is.numeric, mean)

bb.clus.avg

#visualizing the clusters
min.max.standard <- function(x){
  (x - min(x)/ max(x) - min(x))
}
bb.avg.max <- bb.clus.avg%>%
  mutate_if(is.numeric,min.max.standard)

library(GGally)
ggparcoord(bb.avg.max, columns = 2:ncol(bb.avg.max),
           groupColumn = "cluster", scale = "uniminmax",order = "skewness")

#analyzing the groups of gamblers
cluster_names<- c(
  "High Rollers",
  "Strategic Addicts",
  "Cautious Commoners",
  "Risk Takers",
  "Risky Commoners"
)

result.bb.cluster<- bb.clus.avg%>%
  cbind(Name = cluster_names)

result.bb.cluster