
# EM 2016
rm(list = ls())
set.seed(4711)

# install.packages("taRifx", "markovchain", "ggplot", "devtools")
library(taRifx)
library(markovchain)
library(ggplot2)
library(gridExtra)
library(RColorBrewer)

library(devtools)
# install_github("plotflow", "trinker")

capgeminiColors = c("#0080AC", "#D5001B", "#83A63C", "#FFBA40")


teams = c("Albania","Austria", "Belgium", "Croatia", "Czech Republic", "England", 
          "France", "Germany", "Hungary", "Iceland", "Italy", "Northern Ireland", 
          "Poland", "Portugal", "Republic of Ireland", "Romania", "Russia", "Slovakia",
          "Spain", "Sweden", "Switzerland", "Turkey", "Ukraine", "Wales")

nTeams = length(teams)

# matchOutcomes = rep(rep((1 / nTeams), nTeams), nTeams)

emptyMatches = rep(0, nTeams^2)
matchOutcomes = matrix(emptyMatches, byrow=TRUE,
                       nrow=nTeams, dimnames=list(teams, teams))

BUSN_GOALS_DISCOUNT_PENALTY = 10

addMatch = function(home = NULL, guest = NULL,
                    goalsHome = 0, goalsGuest = 0,
                    goalsHomePenalty = 0, goalsGuestPenalty = 0) {
  
  matchOutcomes[home, guest] = (matchOutcomes[home, guest] + 
                                  goalsHome + (goalsHomePenalty / BUSN_GOALS_DISCOUNT_PENALTY))
  
  matchOutcomes[guest, home] = (matchOutcomes[guest, home] + 
                                  goalsGuest + (goalsGuestPenalty / BUSN_GOALS_DISCOUNT_PENALTY))
  
  matchOutcomes
}

matchOutcomes = addMatch("France", "Romania", 2, 1)
matchOutcomes = addMatch("Albania", "Switzerland", 0, 1)
matchOutcomes = addMatch("Wales", "Slovakia", 2, 1)
matchOutcomes = addMatch("England", "Russia", 1, 1)
matchOutcomes = addMatch("Turkey", "Croatia", 0, 1)
matchOutcomes = addMatch("Poland", "Northern Ireland", 1, 0)
matchOutcomes = addMatch("Germany", "Ukraine", 2, 0)
matchOutcomes = addMatch("Spain", "Czech Republic", 1, 0)
matchOutcomes = addMatch("Republic of Ireland", "Sweden", 1, 1)
matchOutcomes = addMatch("Belgium", "Italy", 0, 2)
matchOutcomes = addMatch("Austria", "Hungary", 0, 2)
matchOutcomes = addMatch("Portugal", "Iceland", 1, 1)

matchOutcomes = addMatch("Russia", "Slovakia", 1, 2)
matchOutcomes = addMatch("Romania", "Switzerland", 1, 1)
matchOutcomes = addMatch("France", "Albania", 2, 0)
matchOutcomes = addMatch("England", "Wales", 2, 1)
matchOutcomes = addMatch("Ukraine", "Northern Ireland", 0, 2)
matchOutcomes = addMatch("Germany", "Poland", 0, 0)
matchOutcomes = addMatch("Italy", "Sweden", 1, 0)
matchOutcomes = addMatch("Czech Republic", "Croatia", 2, 2)
matchOutcomes = addMatch("Spain", "Turkey", 3, 0)
matchOutcomes = addMatch("Belgium", "Republic of Ireland", 3, 0)
matchOutcomes = addMatch("Iceland", "Hungary", 1, 1)
matchOutcomes = addMatch("Portugal", "Austria", 0, 0)

matchOutcomes = addMatch("Romania", "Albania", 0, 1)
matchOutcomes = addMatch("Switzerland", "France", 0, 0)
matchOutcomes = addMatch("Russia", "Wales", 0, 3)
matchOutcomes = addMatch("Slovakia", "England", 0, 0)
matchOutcomes = addMatch("Ukraine", "Poland", 0, 1)
matchOutcomes = addMatch("Northern Ireland", "Germany", 0, 1)
matchOutcomes = addMatch("Czech Republic", "Turkey", 0, 2)
matchOutcomes = addMatch("Croatia", "Spain", 2, 1)
matchOutcomes = addMatch("Iceland", "Austria", 2, 1)
matchOutcomes = addMatch("Hungary", "Portugal", 3, 3)
matchOutcomes = addMatch("Italy", "Republic of Ireland", 0, 1)
matchOutcomes = addMatch("Sweden", "Belgium", 0, 1)

matchOutcomes = addMatch("Switzerland", "Poland", 1, 1)
matchOutcomes = addMatch("Wales", "Northern Ireland", 1, 0)
matchOutcomes = addMatch("Croatia", "Portugal", 0, 1)
matchOutcomes = addMatch("France", "Republic of Ireland", 2, 1)
matchOutcomes = addMatch("Germany", "Slovakia", 3, 0)
matchOutcomes = addMatch("Hungary", "Belgium", 0, 4)
matchOutcomes = addMatch("Italy", "Spain", 2, 0)
matchOutcomes = addMatch("England", "Iceland", 1, 2)

matchOutcomes = addMatch("Poland", "Portugal", 1, 1, 3, 5)
matchOutcomes = addMatch("Wales", "Belgium", 3, 1)
matchOutcomes = addMatch("Germany", "Italy", 1, 1, 6, 5)
matchOutcomes = addMatch("France", "Iceland", 5, 2)

matchOutcomes = addMatch("Portugal", "Wales", 2, 0)
matchOutcomes = addMatch("Germany", "France", 0, 2)
# matchOutcomes = addMatch("Portugal", "France", 0, 0)


s = NULL

# apply(matchOutcomes, 1, function (v) {
#     
#     s = sum(v)
#     apply(v, 2, function (i) (i / s))
#   })


# apply(matchOutcomes, 1, function (v) print (sum(v)))

matchOutcomes = sweep(matchOutcomes, 2, colSums(matchOutcomes), FUN="/")
# scale(matchOutcomes, center=FALSE, scale=colSums(matchOutcomes))


mc = new("markovchain", states = teams, name = "EM 2016 team ranking using Markovchains",
         transitionMatrix = matrix(matchOutcomes, byrow=TRUE,
                                   nrow=nTeams, dimnames=list(teams, teams)))

fs = steadyStates(mc)


df = as.data.frame(t(fs))
names(df) = c("prob")
df$team = rownames(df)

df$prob = ifelse(df$prob < 0, 0, df$prob)


kmeans = kmeans(df[,c(1)], 4)
df$cluster = as.factor(kmeans$cluster)
boxplot(t(df$prob))




df$rank = -rank(df$prob)
df = df[with(df, order(rank)),]


topDf = subset(df, df$prob > 0.075)
plotTopTeam = ggplot(data = topDf, aes(x=reorder(team, prob), y=prob, fill=cluster)) + 
  xlab("") + 
  ylab("probability of being world's best team") +
  geom_bar(stat="identity") + 
  coord_flip() + 
  scale_fill_manual(values=capgeminiColors) +
  ggtitle("4 Top Teams as of Euro 2016") + 
  theme(legend.position="none") + 
  geom_hline(yintercept = median(topDf$prob), colour="darkgrey")
plotTopTeam

aaDf = subset(df, df$prob > quantile(df$prob, probs = c(.5)))
plotAaTeam = ggplot(data = aaDf, aes(x=reorder(team, prob), y=prob, fill=cluster)) + 
  xlab("") + 
  ylab("probability of being world's best team") +
  geom_bar(stat="identity") + 
  coord_flip() + 
  scale_fill_manual(values=capgeminiColors) +
  ggtitle("Above-average, top 50% Teams as of Euro 2016") + 
  theme(legend.position="none") + 
  geom_hline(yintercept = median(aaDf$prob), colour="darkgrey")
plotAaTeam

grid.arrange(plotTopTeam, plotAaTeam, ncol=1)




plotAllTeams = ggplot(data = df, aes(x=reorder(team, prob), y=prob, fill=cluster)) + 
  xlab("") + 
  ylab("probability of being world's best team") +
  geom_bar(stat="identity") + 
  coord_flip() + 
  scale_fill_manual(values=capgeminiColors) +
  ggtitle("All Teams as of Euro 2016") + 
  theme(legend.position="none") + 
  geom_hline(yintercept = median(df$prob), colour="darkgrey")
plotAllTeams


# plot(df$cluster, df$prob)


# plTeams = ggplot(topDf, aes(prob, fill=cluster))
# plTeams + geom_bar()


# qplot(topDf, prob, geom="bar", stat="identity", fill=I("grey50"))


# View(subset(df, df$prob > 0.025))
# barplot(subset(df, df$prob > 0.04)$prob)
# barplot(subset(df, df$prob > 0.065)$prob)
# View(subset(df, df$prob > 0.065))

# View(subset(df, df$prob > 0.035))
