library(ggplot2)
library(tidyr)
Sari_Daten=read.csv("C:\\Users\\felix\\Desktop\\Uni\\BA\\Daten\\SARI-Hospitalisierungsinzidenz.csv",sep = ";",header = TRUE)
Sari_Daten

#Ich muss zuerst Daten vorverarbeiten also die Altersstratifizierung rausbekommen
Kalenderwochen=c(unique(Sari_Daten$Kalenderwoche))
Anzahl_Wochen=length(Kalenderwochen)
Inzidenzen_wochen=matrix(0,nrow=Anzahl_Wochen,ncol=4)
colnames(Inzidenzen_wochen)=c("Gesamt","COVID_19","Influenza","RSV")
rownames(Inzidenzen_wochen)=Kalenderwochen
Inzidenzen_wochen
for(i in 1:Anzahl_Wochen){
  Inzidenzen_wochen[i,1]=sum(Sari_Daten$Hospitalisierungsinzidenz[which(Sari_Daten$Kalenderwoche==Kalenderwochen[i]&Sari_Daten$SARI=="Gesamt")])
  Inzidenzen_wochen[i,2]=sum(Sari_Daten$Hospitalisierungsinzidenz[which(Sari_Daten$Kalenderwoche==Kalenderwochen[i]&Sari_Daten$SARI=="COVID-19")])
  Inzidenzen_wochen[i,3]=sum(Sari_Daten$Hospitalisierungsinzidenz[which(Sari_Daten$Kalenderwoche==Kalenderwochen[i]&Sari_Daten$SARI=="Influenza")])
  Inzidenzen_wochen[i,4]=sum(Sari_Daten$Hospitalisierungsinzidenz[which(Sari_Daten$Kalenderwoche==Kalenderwochen[i]&Sari_Daten$SARI=="RSV")])
}

#Für das Säulendiagramm brauch ich aber den Rest der nicht Covid, Influenza oder RSV ist
Inzidenzen_wochen_Rest=Inzidenzen_wochen
colnames(Inzidenzen_wochen_Rest)=c("Rest","COVID_19","Influenza","RSV")
for(i in 1:Anzahl_Wochen){
  Inzidenzen_wochen_Rest[i,1]=Inzidenzen_wochen_Rest[i,1]-sum(Inzidenzen_wochen_Rest[i,2:4])
}

#Plotten
df=data.frame(
  Woche=Kalenderwochen,
  Rest=Inzidenzen_wochen_Rest[,1],
  COVID_19=Inzidenzen_wochen_Rest[,2],
  Influenza=Inzidenzen_wochen_Rest[,3],
  RSV=Inzidenzen_wochen_Rest[,4]
)
wochen_labels <- Kalenderwochen[seq(1, Anzahl_Wochen, by = 20)]               #Zur Übersicht soll nur jede 20.Zeitperiode auf der x-Achse angezeigt werden. 
df_long = df %>% 
  pivot_longer(cols = -Woche,names_to = "Kategorie",values_to = "Wert")
df_long$Kategorie = factor(df_long$Kategorie, levels = c("Rest", "RSV", "COVID_19", "Influenza"))    #hierüber lässt sich Reihenfolge steuern

df_long$Woche=factor(df_long$Woche, levels=rev(unique(df$Woche)))
levels(df_long$Kategorie)
ggplot(df_long, aes(x = Woche, y = Wert, fill = Kategorie)) +
  geom_bar(stat = "identity") +
  scale_x_discrete(breaks = wochen_labels)+
  labs(title = "Fälle nach Kategorie und Kalenderwoche",
       x = "Kalenderwoche",
       y = "Hospitalisierungsinzidenz",
       fill = "Kategorie") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1,size = 8))

#Das gleiche wie zuvor, diesmal aber nur ab dem Zeitpunkt ab dem die einzel Werte genommen wurden
wert=which(grepl("2020-W10",Kalenderwochen))        #Erster individueller Wert für Covid
Inzidenzen_wochen_Rest_einzel=matrix(0,nrow = wert,ncol =  4)
rownames(Inzidenzen_wochen_Rest_einzel)=Kalenderwochen[1:wert]
colnames(Inzidenzen_wochen_Rest_einzel)=c("Rest","COVID_19","Influenza","RSV")
for(i in 1:4){
  Inzidenzen_wochen_Rest_einzel[,i]=Inzidenzen_wochen_Rest[1:wert,i]
}
Inzidenzen_wochen_Rest_einzel
#Plotten
df_einzel=data.frame(
  Woche=Kalenderwochen[1:wert],
  Rest=Inzidenzen_wochen_Rest_einzel[,1],
  COVID_19=Inzidenzen_wochen_Rest_einzel[,2],
  Influenza=Inzidenzen_wochen_Rest_einzel[,3],
  RSV=Inzidenzen_wochen_Rest_einzel[,4]
)
wochen_labels_einzel <- Kalenderwochen[1:wert][seq(1, wert, by = 10)]
df_long_einzel=df_einzel %>%
  pivot_longer(cols = -Woche,names_to = "Kategorie",values_to = "Wert")
df_long_einzel$Kategorie = factor(df_long_einzel$Kategorie, levels = c("Rest", "RSV", "COVID_19", "Influenza"))    #hierüber lässt sich Reihenfolge steuern
df_long_einzel$Woche=factor(df_long_einzel$Woche, levels=rev(unique(df_einzel$Woche)))
levels(df_long_einzel$Kategorie)
ggplot(df_long_einzel, aes(x = Woche, y = Wert, fill = Kategorie)) +
  geom_bar(stat = "identity") +
  scale_x_discrete(breaks=wochen_labels_einzel)+                                       #Zur übersichtlichkeit nur jede 10 Woche auf x-Achse anzeigen
  scale_fill_manual(values = c("Rest" = "#009682", "RSV" = "black", "COVID_19"="#4664aa", "Influenza"="#A3107C"))+
  labs(title = "Fälle nach Kategorie und Kalenderwoche",
       x = "Kalenderwoche",
       y = "Hospitalisierungsinzidenz",
       fill = "Kategorie") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1,size = 8))

