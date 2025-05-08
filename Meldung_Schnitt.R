Covid=read.csv("C:\\Users\\felix\\Desktop\\Uni\\BA\\Daten\\Covid_reg.csv")
Influenza=read.csv("C:\\Users\\felix\\Desktop\\Uni\\BA\\Daten\\Influenza_reg.csv")
RSV=read.csv("C:\\Users\\felix\\Desktop\\Uni\\BA\\Daten\\RSV_reg.csv")
sari=read.csv("C:\\Users\\felix\\Desktop\\Uni\\BA\\Daten\\sari_gesamt.csv")
rest=read.csv("C:\\Users\\felix\\Desktop\\Uni\\BA\\Daten\\Rest_reg.csv")
#Ich mach einmal total und einmal relativ
#aufsummieren=function(Daten){
  Daten_summiert=numeric(5)
  for(i in 1:5){
    for(j in 1:length(Covid[2,])){
      Daten_summiert[i]=Daten_summiert[i]+Covid[j,4+i]
    }
  }
  return(Daten_summiert)
#}
Covid_summiert=aufsummieren(Covid)
