predictor<-lm(chances ~ valo.win_percent + valo.headshot_percent + valo.damage_round +
                valo.ACS_round + valo.kd_ratio, data=chance2)

summary(predictor)