# Apply the KIT-simple_nowcast baseline model to age-stratified data.
# Author: Johannes Bracher, johannes.bracher@kit.edu

# the paths refer to the root of the repository https://github.com/KITmetricslab/RESPINOW-Hub
# setwd("/home/johannes/Documents/RESPINOW/RESPINOW-Hub")


# Set locale to English, otherwise there will be some unnecessary warnings
# (when weekdays are checked)
#Sys.setlocale(category = "LC_TIME", locale = "en_US.UTF8")
Sys.setlocale("LC_TIME", "English_United States")
# note: this is the Linux version of the command, likely different for Windows

# path of the repo:
path_repo <- "."

# get functions:
source("C:\\Users\\felix\\Desktop\\Uni\\BA\\Code\\Fremder Code\\baseline.R")
source("C:\\Users\\felix\\Desktop\\Uni\\BA\\Code\\Fremder Code\\respinow_viz.R")


# define data sources (you need just icosari):
data_source <- "icosari"

# diseases per data source.
#diseases <- c("sari")
# will later likely become:
diseases <- c("sari", "sari_covid", "sari_influenza", "sari_rsv", "Rest")



# dates for which to produce nowcasts:
# for retrospective generation:
#forecast_dates=c(as.Date("2024-10-03"),as.Date("2025-04-10"))
#from=as.Date("2024-11-14") wenn Nowcast Prediction ohne Filter
#ab dem ("2025-01-16") funktioniert das Nowcasting mit Filter
#from=as.Date("2024-10-10") wenn Nowcast Prediction borrow_Delays=borrow_Dispersion=TRUE
forecast_dates <- seq(from = as.Date("2025-01-16"),
                      to = as.Date("2025-04-10"),
                      by = 7)
length(forecast_dates)
# or select an individual forecast_date:
#forecast_dates <- as.Date("2024-10-10")                   #Da Meldungen immer Donnerstags sollte dieses Datum ebenfalls ein Donnerstag sein
# set the sizes of training data sets
n_history_dispersion <- 5
n_history_expectations <- 5
max_delay <- 4
max_horizon <- 3

# specify if plots are to be generated for each nowcast:
plot_all <- TRUE
class(forecast_dates)
# read in reporting triangles:
triangles <- targets <- list()
#for (disease in diseases) {
  # note: we load raw reporting triangles, preprocessing takes place inside compute_nowcast
  triangles[[diseases[1]]] <- read.csv("C:\\Users\\felix\\Desktop\\Uni\\BA\\Daten\\reporting_triangle-icosari-sari.csv",
                                  colClasses = c("date" = "Date"), check.names = FALSE)
  triangles[[diseases[2]]] <- read.csv("C:\\Users\\felix\\Desktop\\Uni\\BA\\Daten\\reporting_triangle-icosari-sari_covid19.csv",
                                   colClasses = c("date" = "Date"), check.names = FALSE)
  triangles[[diseases[3]]] = read.csv("C:\\Users\\felix\\Desktop\\Uni\\BA\\Daten\\reporting_triangle-icosari-sari_influenza.csv",
                                    colClasses = c("date" = "Date"), check.names = FALSE)
  triangles[[diseases[4]]]= read.csv("C:\\Users\\felix\\Desktop\\Uni\\BA\\Daten\\reporting_triangle-icosari-sari_rsv.csv",
                             colClasses = c("date" = "Date"), check.names = FALSE)
  triangles[[diseases[5]]]=triangles[[diseases[4]]]
  sari_gekürzt_triangles=triangles[[diseases[4]]]
  # read in target time series:
  targets[[diseases[1]]] <- read.csv("C:\\Users\\felix\\Desktop\\Uni\\BA\\Daten\\target-icosari-sari.csv",#insgesamt
                                  colClasses = c("date" = "Date"), check.names = FALSE)
  targets[[diseases[2]]] <- read.csv("C:\\Users\\felix\\Desktop\\Uni\\BA\\Daten\\target-icosari-sari_covid19.csv",
                                 colClasses = c("date" = "Date"), check.names = FALSE)
  targets[[diseases[3]]] = read.csv("C:\\Users\\felix\\Desktop\\Uni\\BA\\Daten\\target-icosari-sari_influenza.csv",
                              colClasses = c("date" = "Date"), check.names = FALSE)
  targets[[diseases[4]]] =read.csv("C:\\Users\\felix\\Desktop\\Uni\\BA\\Daten\\target-icosari-sari_rsv.csv",
                       colClasses = c("date" = "Date"), check.names = FALSE)
  targets[[diseases[5]]]=targets[[diseases[4]]]
  sari_gekürzt_targets=targets[[diseases[4]]]
  # this is just the current version of the data in time series format
  for(i in 1:nrow(sari_gekürzt_triangles)){
    match_row <- triangles[[diseases[1]]][
      triangles[[diseases[1]]]$date == sari_gekürzt_triangles[i, 5] & 
        triangles[[diseases[1]]]$age_group == sari_gekürzt_triangles[i, 2], 
    ]
    
    if (nrow(match_row) == 1) {
      sari_gekürzt_triangles[i, ] <- match_row
    } else {
      warning(paste("Kein eindeutiger Match in Zeile", i))
    }
  }
#Rest berechnen
  triangle_Gesamt=triangles[[diseases[2]]]
  for(tag in unique(triangles[[diseases[2]]]$date)){
    Index=which(triangles[[diseases[2]]]$date==tag)
    for (j in which(names(triangles[[diseases[2]]])=="value_0w"):which(names(triangles[[diseases[2]]])=="value_>10w")){
      for(i in Index)
      {
        triangle_Gesamt[i,j]=triangles[[diseases[2]]][i,j]+triangles[[diseases[3]]][i,j]+triangles[[diseases[4]]][i,j]
        triangles[[diseases[5]]][i,j]=sari_gekürzt_triangles[i,j]-triangle_Gesamt[i,j]
      }  
    }
  }
  for(i in 1:nrow(sari_gekürzt_targets)){
    match_row <- targets[[diseases[1]]][
      targets[[diseases[1]]]$date == sari_gekürzt_targets[i, 5] & 
        targets[[diseases[1]]]$age_group == sari_gekürzt_targets[i, 2], 
    ]
    
    if (nrow(match_row) == 1) {
      sari_gekürzt_targets[i, ] <- match_row
    } else {
      warning(paste("Kein eindeutiger Match in Zeile", i))
    }
  }
  target_Gesamt=targets[[diseases[2]]]
  for(tag in unique(targets[[diseases[2]]]$date)){
    Index=which(targets[[diseases[2]]]$date==tag)
      for(i in Index)
      {
        target_Gesamt[i,6]=targets[[diseases[2]]][i,6]+targets[[diseases[3]]][i,6]+targets[[diseases[4]]][i,6]
        targets[[diseases[5]]][i,6]=sari_gekürzt_targets[i,6]-target_Gesamt[i,6]
      }  
  }
#Bis hier müsste Code passen
triangles
targets
SARI_liste=list()
# run over forecast dates to generate nowcasts:
for (j in 1:5){
for(i in seq_along(forecast_dates)){                    #Durchläuft alle Prognosedaten der Liste forecast_dates
  forecast_date <- forecast_dates[i]                    #Speichert aktuelles Prognosedatum in forecast_date
  cat(as.character(forecast_dates[i]), "\n")            #Gibt Prognosedatum auf Console aus
  
  # run over diseases:
  for (disease in diseases[[j]]) {                           #Durchlaufe Sari, Covid, Rsv und Influenza Daten
    #assign(paste0("liste",disease),list())
    # generate nowcasts for age group 00+
    # note: pre-processing and subsetting of RT takes place inside
    # note: observed is the reporting triangle for which to generate a nowcast,
    # observed 2 is the triangle used to estimate the delay pattern (to do this,
    # borrow_delays and borrow_dispersion need to be set to TRUE)
    
    nc <- compute_nowcast(observed = triangles[[disease]], # this is the reporting triangle for which to compute nowcasts
                          location = "DE",              #Wir haben doch sowieso nur DE Daten?
                          age_group = "00+",            #Für meine Arbeit raus nehmen.
                          forecast_date = forecast_date,
                          observed2 = triangles[[disease]], # use complete sari to estimate delays
                          location2 = "DE",
                          age_group2 = "00+",
                          type = "additions",
                          borrow_delays = FALSE,          #Verzögerungsdaten von Datensatz[1:5,] nutzen
                          borrow_dispersion = FALSE,      #Verzögerungsverteilung von Datensatz[1:5,] nutzen
                          # note using n_history_expectations_, n_history_dispersion_,
                          # which may be reduced to fit shorter triangle.
                          n_history_expectations = n_history_expectations,
                          n_history_dispersion = n_history_dispersion,
                          max_delay = max_delay)
    # note: message "Reporting triangle contains dates later than forecast_date..."
    # is normal when using the function on earlier dates.
    
    # keep only result element:
    nc <- nc$result                                       #Extrahiert nur die Ergebnisse aus dem Nowcasting Objekt
    # generate a plot if desired:
    if(plot_all){
      # truth data as of forecast_date, subset to relevant stratum
      observed_back_in_time <- data_as_of(dat_truth = triangles[[disease]], date = forecast_date,                                           #data_as_of():Extrahiert die historischen Daten zum Zeitpunkt des Prognosedatums
                                          location = "DE", age_group = "00+", max_lag = max_delay)
      plot_data_back_in_time <- data.frame(date = as.Date(observed_back_in_time$date),                                                      #Erstellt Data Frame mit  Datum und zugehörig aufsummierten Werten
                                           value = rowSums(observed_back_in_time[, grepl("value_", colnames(observed_back_in_time))],
                                                           na.rm = TRUE))
      target_current <- subset(targets[[disease]], age_group == "00+" & location == "DE")
      
      plot_forecast(forecasts = nc,                       #Erstellt den Plot der vergangenen Daten inklusive Nowcast
                    location = "DE", age_group = "00+",
                    truth = plot_data_back_in_time,
                    levels_coverage = c(0.5, 0.95),       #Gibt "Schlauch" um die Daten an mit 50% und 95% Quantil
                    start = as.Date(forecast_date) - 135,      #Zeitfenster um Prognosedatum -135 Tage bis Prognosedatum bis 28 nach Prognosedatum
                    end = as.Date(forecast_date) + 28,
                    forecast_date = forecast_date,
                    ylim = c(0, 1.2*max(tail(plot_data_back_in_time$value, 20)))        #Passt y-Achse an jüngste Datenwerte an
      )
      # add the most recent data:
      lines(target_current$date, target_current$value, col = "red", lty  ="solid")      #aktuelle Daten
      title(paste0(disease, ", ", "00+", ", ", forecast_date))                          #Titel
    }
    r=r+1
    SARI_liste[[r]]=cbind(nc,disease)
    # write out:
    # need to adapt path if nowcasts shall be written out.
    #write.csv(nc, file = paste0("C:\\Users\\felix\\Desktop\\Uni\\BA\\Daten_",
                                 #forecast_date, "-", data_source, "-", disease, "-KIT-simple_nowcast.csv"), row.names = FALSE)
  }
  }
}
write.csv(do.call(rbind,SARI_liste),"C:\\Users\\felix\\Desktop\\Uni\\BA\\Daten\\Nowcast_gesamt_Wochenbasiert.csv",row.names = FALSE)
Nowcast=read.csv("C:\\Users\\felix\\Desktop\\Uni\\BA\\Daten\\Nowcast_gesamt_Wochenbasiert.csv")
Nowcast_einzel=list()
i=1
for (disease in diseases){
  Nowcast_einzel[[i]]=Nowcast[which(Nowcast$disease==disease),]
  i=i+1
}
Nowcast_aufbrechen=function(Nowcast_Krankheit){
Nowcast_aufgebrochen=list()
untere_Grenze=1
obere_Grenze=4*8
for(i in 1:floor(nrow(Nowcast_Krankheit)/(4*8))){
  Nowcast_aufgebrochen[[i]]=Nowcast_Krankheit[untere_Grenze:obere_Grenze,]
  untere_Grenze=untere_Grenze+(4*8)
  obere_Grenze=obere_Grenze+(4*8)
}
return(Nowcast_aufgebrochen)
}
Nowcast_sari=Nowcast_aufbrechen(Nowcast_einzel[[1]])
Nowcast_covid=Nowcast_aufbrechen(Nowcast_einzel[[2]])
Nowcast_influenza=Nowcast_aufbrechen(Nowcast_einzel[[3]])
Nowcast_RSV=Nowcast_aufbrechen(Nowcast_einzel[[4]])
Nowcast_Rest=Nowcast_aufbrechen(Nowcast_einzel[[5]])

plotten=function(Nowcast,Zahl,disease){
# Basisdaten vorbereiten
observed_back_in_time <- data_as_of(dat_truth = triangles[[Zahl]],
                                    date = forecast_date,
                                    location = "DE", age_group = "00+", max_lag = max_delay)

plot_data_back_in_time <- data.frame(
  date = as.Date(observed_back_in_time$date),
  value = rowSums(observed_back_in_time[, grepl("value_", colnames(observed_back_in_time))], na.rm = TRUE)
)

target_current <- subset(targets[[Zahl]], age_group == "00+" & location == "DE")

# 1. Plot mit einem Forecast erstellen (z. B. dem aktuellsten)
plot_forecast(forecasts = Nowcast[[length(Nowcast)]],
              location = "DE", age_group = "00+",
              truth = plot_data_back_in_time,
              levels_coverage = c(0.5, 0.95),
              start = as.Date(forecast_dates[1]),
              end = as.Date(forecast_dates[length(forecast_dates)]),
              forecast_date = forecast_date,
              ylim = c(0, 1.2 * max(tail(plot_data_back_in_time$value, 20)))
)

# 2. Jetzt zusätzliche Nowcasts mit Linien hinzufügen (Medianlinien z. B.)
farben <- rainbow(length(Nowcast))
for (i in 1:(length(Nowcast)-1)) {
  forecast_i <- Nowcast[[i]]
  median_forecast <- subset(forecast_i, quantile == 0.5)
  
  lines(as.Date(median_forecast$target_end_date), median_forecast$value,
        col = farben[i], lty = "dashed")
}

# Aktuelle Daten hinzufügen
lines(target_current$date, target_current$value, col = "red", lty = "solid")
title(paste0(disease, ", 00+, ", forecast_date))

for (i in 1:(length(Nowcast_sari)-1)) {
  forecast_i <- Nowcast[[i]]
  
  # 95% Intervall extrahieren
  lower <- subset(forecast_i, quantile == 0.025)
  upper <- subset(forecast_i, quantile == 0.975)
  
  # Sicherstellen, dass Daten in gleicher Reihenfolge vorliegen
  stopifnot(all(as.Date(lower$target_end_date) == as.Date(upper$target_end_date)))
  
  # Polygon (Konfidenzband) zeichnen
  polygon(
    c(as.Date(lower$target_end_date), rev(as.Date(upper$target_end_date))),
    c(lower$value, rev(upper$value)),
    col = adjustcolor(farben[i], alpha.f = 0.2),
    border = NA
  )
  
  # Medianlinie hinzufügen (wie bisher)
  median_forecast <- subset(forecast_i, quantile == 0.5)
  lines(as.Date(median_forecast$target_end_date), median_forecast$value,
        col = farben[i], lty = "dashed")
}
}

plotten(Nowcast_sari,1,diseases[1])
plotten(Nowcast_covid,2,diseases[2])
plotten(Nowcast_influenza,3,diseases[3])
plotten(Nowcast_RSV,4,diseases[4])
plotten(Nowcast_Rest,5,diseases[5])
