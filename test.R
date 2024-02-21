library(ggplot2)

#--------All In--------------------------

View(valo)

#-----------VARIABLES-----------

chances<-((0.2*valo$win_percent+0.00364*valo$ACS_round+2.86*valo$kd_ratio+0.0556*valo$damage_round+
             0.2*valo$headshot_percent)*1.5)
R<-c("Asia","Brazil","Europe","Korea","Latin-America","North-America")

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





#--------Normal Distribution-------------

ggplot(valoo, aes(Rank, chances, fill = region,stack=TRUE)) + geom_bar(stat="identity", position = "dodge")

m=mean(chances)
s=sd(chances)

range=seq.default((m-(4*s)),(m+(4*s)),0.01)

y=dnorm(range,m,s)

plot(range,y,type='l',ylim=c(0,max(y)+0.01),axes=FALSE)


axis(1,at = seq(m-3*s,m+3*s,s))


#--------Regression-----------
chance2<-data.frame(valo$win_percent,valo$headshot_percent,valo$damage_round,valo$ACS_round,valo$kd_ratio,chances)

head(chance2)

predictor<-lm(chances ~ valo.win_percent + valo.headshot_percent + valo.damage_round +
                valo.ACS_round + valo.kd_ratio, data=chance2)

summary(predictor)

#variables used to predict

head_per<-24.9
win_per<-59.6
dpr<-135.8
acs<-208.8
kd<-1.07

#for manual
guess = -7.457e-13 + (0.3*head_per)+(0.3*win_per)+(4.290*kd)+(0.005460*acs)+(0.08340*dpr)
#guess = -0.0000000000007 + (0.3*head_per)+(0.3*win_per)+(4.3*kd)+(0.005*acs)+(0.08*dpr)----gives low accuracy
#actual value is 42.40607

#for auto prediction
predict(predictor)
#42.40607 49.70200 44.92979 52.41030 44.67593 41.99887----------first 6

#guess2 = -0.0000000000007 + (0.3*chance2$valo.headshot_percent)+(0.3*chance2$valo.win_percent)+
#  (4.30*chance2$valo.kd_ratio)+(0.005*chance2$valo.ACS_round)+
#  (0.08*chance2$valo.damage_round)

#----------------RUBBISH------------------------
library(ggplot2)

valo<-read.csv("C:\\Users\\kingh\\Desktop\\SDA\\valo.csv")

View(valo)

barplot(winbrak2,names.arg=c("0-15","15-30","30-45","45-60","60-75","75-90","90-100"),main=c("Win Percentage"))


ggplot(valo, aes(region, aces, fill = region, stack=TRUE)) + geom_bar(stat="identity", position = "dodge")


chances<-((0.2*valo$win_percent+0.00364*valo$ACS_round+2.86*valo$kd_ratio+0.0556*valo$damage_round+
             0.2*valo$headshot_percent)*1.5)

m_c=mean(chances)
sd_c=sd(chances)
max(chances)

all_region<-1:7
europe<-1:7
northAmerica<-1:7
latinAmerica<-1:7
brazil<-1:7
korea<-1:7
asiaPacific<-1:7

win_cnt=105
counted=100

win_cnt=15
counted=0
vari=0
i=1

while (win_cnt <= 106){
  
  vari<-table(valo$win_percent<=win_cnt)
  vari=vari[2]-counted
  counted=counted+vari
  all_region[i]<-vari
  i=i+1
  win_cnt=win_cnt+15
}
all_region[7]<-1139


val2<-split(valo, valo$Rank)


valoo<-valo

valoo$Rank <- factor(valoo$Rank, levels = c("Unrated"
                                            , "Bronze 1","Bronze 2","Bronze 3"
                                            , "Silver 1","Silver 2","Silver 3"
                                            , "Gold 1","Gold 2","Gold 3"
                                            , "Platinum 1","Platinum 2","Platinum 3"
                                            , "Diamond 1","Diamond 2","Diamond 3"
                                            , "Immortal 1","Immortal 2","Immortal 3"
                                            , "Radiant"))





