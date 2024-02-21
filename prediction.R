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
