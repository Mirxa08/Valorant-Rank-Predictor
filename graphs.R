#---------------GRPAPHS-----------

barplot(by(valo$win_percent,valo$region,mean),main="Avg Win% Comparison",ylim=c(0,60))
barplot(by(valo$headshot_percent,valo$region,mean),main="Avg Headshot% Comparison",ylim=c(0,30))

ggplot(valo, aes(Rank=="Radiant", headshot_percent, fill = region, stack=TRUE)) + geom_bar(stat="identity", position = "dodge")+labs(title="Radiant Comparison To Rest",x="Radiant Rank Reached",y="HeadShot%")
ggplot(valo, aes(Rank=="Radiant", win_percent, fill = region, stack=TRUE)) + geom_bar(stat="identity", position = "dodge")+labs(title="Radiant Comparison To Rest",x="Radiant Rank Reached",y="Win%")
ggplot(valo, aes(Rank=="Radiant", damage_round, fill = region, stack=TRUE)) + geom_bar(stat="identity", position = "dodge")+labs(title="Radiant Comparison To Rest",x="Radiant Rank Reached",y="Mean Damage Per Round")
ggplot(valo, aes(Rank=="Radiant", ACS_round, fill = region, stack=TRUE)) + geom_bar(stat="identity", position = "dodge")+labs(title="Radiant Comparison To Rest",x="Radiant Rank Reached",y="Mean Combat Score Per Round")
ggplot(valo, aes(Rank=="Radiant", kd_ratio, fill = region, stack=TRUE)) + geom_bar(stat="identity", position = "dodge")+labs(title="Radiant Comparison To Rest",x="Radiant Rank Reached",y="Mean Kills Per Deaths%")

#INDIVIDUAL GRAPHS BY WIN%
barplot(all_region,names.arg=c("0%-15%","16%-30%","31%-45%","46%-60%","61%-75%","76%-90%","91%-100%"),xlab="Win Percentage",ylab="Players",ylim=c(0,90000),main="Global")
barplot(asiaPacific,names.arg=c("0%-15%","16%-30%","31%-45%","46%-60%","61%-75%","76%-90%","91%-100%"),xlab="Win Percentage",ylab="Players",ylim=c(0,20000),main="Asia Pacific")
barplot(europe,names.arg=c("0%-15%","16%-30%","31%-45%","46%-60%","61%-75%","76%-90%","91%-100%"),xlab="Win Percentage",ylab="Players",ylim=c(0,33000),main="Europe")
barplot(northAmerica,names.arg=c("0%-15%","16%-30%","31%-45%","46%-60%","61%-75%","76%-90%","91%-100%"),xlab="Win Percentage",ylab="Players",ylim=c(0,21000),main="North America")
barplot(korea,names.arg=c("0%-15%","16%-30%","31%-45%","46%-60%","61%-75%","76%-90%","91%-100%"),xlab="Win Percentage",ylab="Players",ylim=c(0,2500),main="Korea")
barplot(latinAmerica,names.arg=c("0%-15%","16%-30%","31%-45%","46%-60%","61%-75%","76%-90%","91%-100%"),xlab="Win Percentage",ylab="Players",ylim=c(0,3500),main="Latin America")
barplot(brazil,names.arg=c("0%-15%","16%-30%","31%-45%","46%-60%","61%-75%","76%-90%","91%-100%"),xlab="Win Percentage",ylab="Players",ylim=c(0,7000),main="Brazil")




