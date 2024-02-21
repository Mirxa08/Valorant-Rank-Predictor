chance2<-data.frame(valo$win_percent,valo$headshot_percent,valo$damage_round,valo$ACS_round,valo$kd_ratio,chances)

head(chance2)

predictor<-lm(chances ~ valo.win_percent + valo.headshot_percent + valo.damage_round +
                valo.ACS_round + valo.kd_ratio, data=chance2)

summary(predictor)

