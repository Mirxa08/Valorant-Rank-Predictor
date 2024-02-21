
#--------Normal Distribution-------------

ggplot(valoo, aes(Rank, chances, fill = region,stack=TRUE)) + geom_bar(stat="identity", position = "dodge")

m=mean(chances)
s=sd(chances)

range=seq.default((m-(4*s)),(m+(4*s)),0.01)

y=dnorm(range,m,s)

plot(range,y,type='l',ylim=c(0,max(y)+0.01),axes=FALSE)


axis(1,at = seq(m-3*s,m+3*s,s))

