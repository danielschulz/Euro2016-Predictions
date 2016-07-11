
# MATCHES

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
matchOutcomes = addMatch("Portugal", "France", 1, 0)
