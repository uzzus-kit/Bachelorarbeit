library(ggplot2)
library(tidyr)
library(dplyr)
Sari_Daten=read.csv("C:\\Users\\felix\\Desktop\\Uni\\BA\\Daten\\target-icosari-sari.csv")
Sari_Daten

#Ich muss zuerst Daten vorverarbeiten also die Altersstratifizierung rausbekommen
Kalenderwochen=c(unique(Sari_Daten$date))
Anzahl_Wochen=length(Kalenderwochen)
Inzidenzen_wochen=matrix(0,nrow=Anzahl_Wochen,ncol=length(unique(Sari_Daten$age_group)))
colnames(Inzidenzen_wochen)=unique(Sari_Daten$age_group)
rownames(Inzidenzen_wochen)=Kalenderwochen
Inzidenzen_wochen
for(i in 1:Anzahl_Wochen){
  k=1
  for (j in unique(Sari_Daten$age_group)){
  if(length(which(as.Date(Sari_Daten$date)==as.Date(Kalenderwochen[i])&Sari_Daten$age_group==j))==0){
      Inzidenzen_wochen[i,k]=0
   }else{
    Inzidenzen_wochen[i,k]=Sari_Daten[which(as.Date(Sari_Daten$date)==as.Date(Kalenderwochen[i])&Sari_Daten$age_group==j),6]
   }
    k=k+1
  }
}

#Für das Säulendiagramm brauch ich aber den Rest der nicht Covid, Influenza oder RSV ist
Inzidenzen_wochen_Rest=Inzidenzen_wochen
colnames(Inzidenzen_wochen_Rest)=c("Rest","00-04","05-14","15-34","35-59","60-79","80+")
for(i in 1:Anzahl_Wochen){
  Inzidenzen_wochen_Rest[i,1]=Inzidenzen_wochen_Rest[i,1]-sum(Inzidenzen_wochen_Rest[i,2:length(colnames(Inzidenzen_wochen_Rest))])
}

#Plotten
df=data.frame(
  Woche=Kalenderwochen,
  Rest=Inzidenzen_wochen_Rest[,1],
  v0004=Inzidenzen_wochen_Rest[,2],
  v0514=Inzidenzen_wochen_Rest[,3],
  v1534=Inzidenzen_wochen_Rest[,4],
  v3559=Inzidenzen_wochen_Rest[,5],
  v6079=Inzidenzen_wochen_Rest[,6],
  v80=Inzidenzen_wochen_Rest[,7]
)
wochen_labels <- Kalenderwochen[seq(1, Anzahl_Wochen, by = 5)]               #Zur Übersicht soll nur jede 20.Zeitperiode auf der x-Achse angezeigt werden. 
df_long = df %>% 
  pivot_longer(cols = -Woche, names_to = "Kategorie", values_to = "Wert") %>%
  mutate(Kategorie = recode(Kategorie,
                            "v0004" = "00-04",
                            "v0514" = "05-14",
                            "v1534" = "15-34",
                            "v3559" = "35-59",
                            "v6079" = "60-79",
                            "v80"   = "80+"))

df_long$Kategorie = factor(df_long$Kategorie, levels = c("Rest", "00-04", "05-14", "15-34","35-59","60-79","80+"))
df_long$Woche=factor(df_long$Woche, levels=unique(df$Woche))
levels(df_long$Kategorie)
ggplot(df_long, aes(x = Woche, y = Wert, fill = Kategorie)) +
  geom_bar(stat = "identity") +
  scale_x_discrete(breaks = wochen_labels)+
  scale_fill_manual(values = c("Rest" = "#009682", "00-04" = "black", "05-14"="#4664aa", "15-34"="#A3107C","35-59"="orange","60-79"="#A7822E","80+"="red"))+
  labs(title = "Fälle nach Kategorie und Kalenderwoche",
       x = "Kalenderwoche",
       y = "Fallzahlen",
       fill = "Kategorie") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1,size = 8))

#Das gleiche wie zuvor, diesmal aber nur ab dem Zeitpunkt ab dem die einzel Werte genommen wurden
wert=which(grepl("2023-10-22",Kalenderwochen))        #Erster individueller Wert für Covid
Inzidenzen_wochen_Rest_einzel=matrix(0,nrow = length(Inzidenzen_wochen_Rest[,1])-wert+1,ncol =  length(Inzidenzen_wochen_Rest[1,]))
rownames(Inzidenzen_wochen_Rest_einzel)=Kalenderwochen[wert:length(Inzidenzen_wochen_Rest[,1])]
colnames(Inzidenzen_wochen_Rest_einzel)=c("Rest","00-04", "05-14", "15-34","35-59","60-79","80+")
for(i in 1:length(Inzidenzen_wochen_Rest[1,])){
  Inzidenzen_wochen_Rest_einzel[,i]=Inzidenzen_wochen_Rest[wert:length(Inzidenzen_wochen_Rest[,1]),i]
}
Inzidenzen_wochen_Rest_einzel
#Plotten
df_einzel=data.frame(
  Woche=Kalenderwochen[wert:length(Inzidenzen_wochen_Rest[,1])],
  Rest=Inzidenzen_wochen_Rest_einzel[,1],
  v0004=Inzidenzen_wochen_Rest_einzel[,2],
  v0514=Inzidenzen_wochen_Rest_einzel[,3],
  v1534=Inzidenzen_wochen_Rest_einzel[,4],
  v3559=Inzidenzen_wochen_Rest_einzel[,5],
  v6079=Inzidenzen_wochen_Rest_einzel[,6],
  v80=Inzidenzen_wochen_Rest_einzel[,7]
)
wochen_labels_einzel <- Kalenderwochen[seq(1, Anzahl_Wochen, by = 3)]               #Zur Übersicht soll nur jede 20.Zeitperiode auf der x-Achse angezeigt werden. 
df_long_einzel = df_einzel %>% 
  pivot_longer(cols = -Woche, names_to = "Kategorie", values_to = "Wert") %>%
  mutate(Kategorie = recode(Kategorie,
                            "v0004" = "00-04",
                            "v0514" = "05-14",
                            "v1534" = "15-34",
                            "v3559" = "35-59",
                            "v6079" = "60-79",
                            "v80"   = "80+"))

df_long_einzel$Kategorie = factor(df_long_einzel$Kategorie, levels = c("Rest","00-04", "05-14", "15-34","35-59","60-79","80+"))    #hierüber lässt sich Reihenfolge steuern
df_long_einzel$Woche=factor(df_long_einzel$Woche, levels=unique(df_einzel$Woche))
levels(df_long_einzel$Kategorie)
ggplot(df_long_einzel, aes(x = Woche, y = Wert, fill = Kategorie)) +
  geom_bar(stat = "identity") +
  scale_x_discrete(breaks=wochen_labels_einzel)+                                       #Zur übersichtlichkeit nur jede 10 Woche auf x-Achse anzeigen
  scale_fill_manual(values = c("Rest" = "#009682", "00-04" = "black", "05-14"="#4664aa", "15-34"="#A3107C","35-59"="orange","60-79"="#A7822E","80+"="red"))+
  labs(title = "Fälle nach Kategorie und Kalenderwoche",
       x = "Kalenderwoche",
       y = "Fallzahlen",
       fill = "Kategorie") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1,size = 8))

