
# TRANSFORM

matchOutcomes = sweep(matchOutcomes, 2, colSums(matchOutcomes), FUN="/")


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
