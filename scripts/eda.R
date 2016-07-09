
# EDA


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


# odds France wins against Portugal
print("odds France wins against Portugal")
print(df[df$team=="France",]$prob / (df[df$team=="France",]$prob + df[df$team=="Portugal",]$prob)) # about 0.635 at 24h before final


# plot(df$cluster, df$prob)


# plTeams = ggplot(topDf, aes(prob, fill=cluster))
# plTeams + geom_bar()


# qplot(topDf, prob, geom="bar", stat="identity", fill=I("grey50"))


# View(subset(df, df$prob > 0.025))
# barplot(subset(df, df$prob > 0.04)$prob)
# barplot(subset(df, df$prob > 0.065)$prob)
# View(subset(df, df$prob > 0.065))

# View(subset(df, df$prob > 0.035))