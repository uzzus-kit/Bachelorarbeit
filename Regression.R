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
plot(Woche[[1]])
plot(Woche[[2]])
plot(Woche[[3]])
plot(Woche[[4]])
plot(Woche[[5]])

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
