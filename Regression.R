Covid=read.csv("C:\\Users\\felix\\Desktop\\Uni\\BA\\Daten\\Covid_reg.csv")
RSV=read.csv("C:\\Users\\felix\\Desktop\\Uni\\BA\\Daten\\RSV_reg.csv")
Influenza=read.csv("C:\\Users\\felix\\Desktop\\Uni\\BA\\Daten\\Influenza_reg.csv")
SARI=read.csv("C:\\Users\\felix\\Desktop\\Uni\\BA\\Daten\\sari_reg.csv")
Gesamtverzüge=read.csv("C:\\Users\\felix\\Desktop\\Uni\\BA\\Daten\\gesamtverzüge_reg.csv")
Woche=list()
ges=list()
Woche[[1]]=lm(SARI$value_0w~Covid$value_0w+RSV$value_0w+Influenza$value_0w)
Woche[[2]]=lm(SARI$value_1w~Covid$value_1w+RSV$value_1w+Influenza$value_1w)
Woche[[3]]=lm(SARI$value_2w~Covid$value_2w+RSV$value_2w+Influenza$value_2w)
Woche[[4]]=lm(SARI$value_3w~Covid$value_3w+RSV$value_3w+Influenza$value_3w)
Woche[[5]]=lm(SARI$value_4w~Covid$value_4w+RSV$value_4w+Influenza$value_4w)
#ges[[1]]=lm(SARI$value_0w~Gesamtverzüge$value_0w)                              Wieso solte man das tun wenn man es auch direkt auf die einzelnen beziehen kann?
summary(Woche[[1]])
#summary(ges[[1]])
summary(Woche[[2]])
summary(Woche[[3]])
summary(Woche[[4]])
summary(Woche[[5]])
#plot(Woche[[1]])
#plot(Woche[[2]])
#plot(Woche[[3]])
#plot(Woche[[4]])
#plot(Woche[[5]])

#Darf man  einfach Varianz für aufsummierte Daten berechnen?
Varianz_Matrix=matrix(0,nrow = 5,ncol=5)
row.names(Varianz_Matrix)=c("Covid","RSV","Influenza","Sari","Gesamt")
colnames(Varianz_Matrix)=colnames(Covid[5:9])
Kovarianz=numeric(5)
for (i in 1:5){
  Varianz_Matrix[1,i]=var(Covid[4+i])
  Varianz_Matrix[2,i]=var(RSV[4+i])
  Varianz_Matrix[3,i]=var(Influenza[4+i])
  Varianz_Matrix[4,i]=var(SARI[4+i])
  Varianz_Matrix[5,i]=var(Gesamtverzüge[4+i])
  Kovarianz[i]=cov(SARI[4+i],Gesamtverzüge[4+i])
}
r=numeric(5)
for (i in 1:5){
  r[i]=Kovarianz[i]/(sqrt(Varianz_Matrix[4,i])*sqrt(Varianz_Matrix[5,i]))
}
r
plotten=function(Reg_Modell, y){
  y_hat=predict(Reg_Modell)
  plot(y_hat,y,xlab="Vorhergesagtes y",ylab="Beobachtetes y")
  abline(0,1,col="red",lwd=2)
}
plotten(Woche[[1]],SARI$value_0w)
plotten(Woche[[2]],SARI$value_1w)
plotten(Woche[[3]],SARI$value_2w)           #extrem Krasser Ausreißer
plotten(Woche[[4]],SARI$value_3w)           #extrem Krasser Ausreißer
plotten(Woche[[5]],SARI$value_4w)           #Heteroskedastie?

#Versuch: korregieren der Ausreißer/fehlenden Werten über Weihnachten durch arithmetisches Mittel des Wertes danach
Weihnachtskorrektur=function(Datensatz){
  Woche47=which(Datensatz$week=="47")
  Woche48=which(Datensatz$week=="48")
  Woche49=which(Datensatz$week=="49")
  Woche50=which(Datensatz$week=="50")
  Woche51=which(Datensatz$week=="51")
  Woche52=which(Datensatz$week=="52")
  for (i in 0:4){
    Datensatz[(Woche47+i):(Woche49+i),9-i]=Datensatz[(Woche49+i),9-i]/3
  }
  return(Datensatz)
}
Korr_SARI=Weihnachtskorrektur(SARI)
Korr_Covid=Weihnachtskorrektur(Covid)
Korr_RSV=Weihnachtskorrektur(RSV)
Korr_Influenza=Weihnachtskorrektur(Influenza)
Woche_korrigiert=list()
Woche_korrigiert[[1]]=lm(Korr_SARI$value_0w~Korr_Covid$value_0w+Korr_Influenza$value_0w+Korr_RSV$value_0w)
Woche_korrigiert[[2]]=lm(Korr_SARI$value_1w~Korr_Covid$value_1w+Korr_Influenza$value_1w+Korr_RSV$value_1w)
Woche_korrigiert[[3]]=lm(Korr_SARI$value_2w~Korr_Covid$value_2w+Korr_Influenza$value_2w+Korr_RSV$value_2w)
Woche_korrigiert[[4]]=lm(Korr_SARI$value_3w~Korr_Covid$value_3w+Korr_Influenza$value_3w+Korr_RSV$value_3w)
Woche_korrigiert[[5]]=lm(Korr_SARI$value_4w~Korr_Covid$value_4w+Korr_Influenza$value_4w+Korr_RSV$value_4w)
plotten(Woche_korrigiert[[1]],Korr_SARI$value_0w)
plotten(Woche_korrigiert[[2]],Korr_SARI$value_1w)
plotten(Woche_korrigiert[[3]],Korr_SARI$value_2w)
plotten(Woche_korrigiert[[4]],Korr_SARI$value_3w)
plotten(Woche_korrigiert[[5]],Korr_SARI$value_4w)
summary(Woche_korrigiert[[1]])
summary(Woche_korrigiert[[2]])
summary(Woche_korrigiert[[3]])
summary(Woche_korrigiert[[4]])
summary(Woche_korrigiert[[5]])
#plot(Woche_korrigiert[[1]])
#plot(Woche_korrigiert[[2]])
#plot(Woche_korrigiert[[3]])
#plot(Woche_korrigiert[[4]])
#plot(Woche_korrigiert[[5]])
