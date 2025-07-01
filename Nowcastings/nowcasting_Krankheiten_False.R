#In diesem Programm werden die Nowcasts based auf den einzelnen Erkrankungen berechnet.

# Apply the KIT-simple_nowcast baseline model to age-stratified data.
# Inspired by: Johannes Bracher, johannes.bracher@kit.edu

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
#für Nowcasting ohne Filter: "C:\\Users\\felix\\Desktop\\Uni\\BA\\Code\\eigener Code\\nowcasting_Wochenbasiert.R"
source("C:\\Users\\felix\\Desktop\\Uni\\BA\\Code\\Fremder Code\\baseline.R")
source("C:\\Users\\felix\\Desktop\\Uni\\BA\\Code\\Fremder Code\\respinow_viz.R")


# define data sources (you need just icosari):
data_source <- "icosari"

# diseases per data source.
#diseases <- c("sari")
# will later likely become:
diseases <- c("sari", "sari_covid", "sari_influenza", "sari_rsv","Rest")



# dates for which to produce nowcasts:
# for retrospective generation:
#forecast_dates=c(as.Date("2024-10-03"),as.Date("2025-04-10"))
#from=as.Date("2024-11-14") wenn Nowcast Prediction ohne Filter
#ab dem ("2025-01-16") funktioniert das Nowcasting mit Filter
#from=as.Date("2024-10-10") wenn Nowcast Prediction borrow_Delays=borrow_Dispersion=TRUE
# or select an individual forecast_date:
#forecast_dates <- as.Date("2024-10-10")                   #Da Meldungen immer Donnerstags sollte dieses Datum ebenfalls ein Donnerstag sein
# set the sizes of training data sets
n_history_dispersion <- 5
n_history_expectations <-5
max_delay <- 4
max_horizon <- 4
if(n_history_dispersion==5){
  forecast_dates=seq(from = as.Date(as.Date("2024-10-06")+4+7*6),
                                       to = as.Date("2025-04-10"),
                                       by = 7)
}else if(n_history_dispersion==10){
  forecast_dates <- seq(from = as.Date(as.Date("2024-10-06")+4+7*11),
                        to = as.Date("2025-04-10"),
                        by = 7)
}
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
triangles[[diseases[5]]]=read.csv("C:\\Users\\felix\\Desktop\\Uni\\BA\\Daten\\reporting_triangle-icosari-Rest.csv",
                                  colClasses = c("date" = "Date"), check.names = FALSE)
# read in target time series:
targets[[diseases[1]]] <- read.csv("C:\\Users\\felix\\Desktop\\Uni\\BA\\Daten\\target-icosari-sari.csv",#insgesamt
                                   colClasses = c("date" = "Date"), check.names = FALSE)
targets[[diseases[2]]] <- read.csv("C:\\Users\\felix\\Desktop\\Uni\\BA\\Daten\\target-icosari-sari_covid19.csv",
                                   colClasses = c("date" = "Date"), check.names = FALSE)
targets[[diseases[3]]] = read.csv("C:\\Users\\felix\\Desktop\\Uni\\BA\\Daten\\target-icosari-sari_influenza.csv",
                                  colClasses = c("date" = "Date"), check.names = FALSE)
targets[[diseases[4]]] =read.csv("C:\\Users\\felix\\Desktop\\Uni\\BA\\Daten\\target-icosari-sari_rsv.csv",
                                 colClasses = c("date" = "Date"), check.names = FALSE)
targets[[diseases[5]]]=read.csv("C:\\Users\\felix\\Desktop\\Uni\\BA\\Daten\\target-icosari-rest.csv",
                                colClasses = c("date" = "Date"), check.names = FALSE)
#Bis hier müsste Code passen
triangles
targets
SARI_liste=list()
# run over forecast dates to generate nowcasts:
r=1
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
    if(disease=="sari_rsv"&forecast_date<=as.Date("2025-01-09")){               #RSV erst berechenbar ab dem 2025-01-16
      next
    }
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
                    start = as.Date(forecast_date) -135 ,      #Zeitfenster um Prognosedatum -135 Tage bis Prognosedatum bis 28 nach Prognosedatum
                    end = as.Date(forecast_date) + 28,
                    forecast_date = forecast_date,
                    ylim = c(0, 1.2*max(tail(plot_data_back_in_time$value, 20)))        #Passt y-Achse an jüngste Datenwerte an
      )
      # add the most recent data:
      lines(target_current$date, target_current$value, col = "red", lty  ="solid")      #aktuelle Daten
      title(paste0(disease, ", ", forecast_date))                          #Titel
      }
    r=r+1
    SARI_liste[[r]]=cbind(nc,disease)
    # write out:
    # need to adapt path if nowcasts shall be written out.
    #write.csv(nc, file = paste0("C:\\Users\\felix\\Desktop\\Uni\\BA\\Daten_",
                                 #forecast_date, "-", data_source, "-", disease, "-KIT-simple_nowcast.csv"), row.names = FALSE)
  }
  #browser()
  }
}
write.csv(do.call(rbind,SARI_liste),paste0("C:\\Users\\felix\\Desktop\\Uni\\BA\\Daten\\Nowcasts\\Nowcast_Krankheit_False",n_history_dispersion,".csv"),row.names = FALSE)
Nowcast=read.csv(paste0("C:\\Users\\felix\\Desktop\\Uni\\BA\\Daten\\Nowcasts\\Nowcast_Krankheit_False",n_history_dispersion,".csv"))
Nowcast_einzel=list()
i=1
for (disease in diseases){
  Nowcast_einzel[[disease]]=Nowcast[which(Nowcast$disease==disease),]
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
Nowcast_darstellung=list()

for(disea in diseases){
  Nowcast_darstellung[[disea]]=subset(Nowcast_einzel[[disea]],Nowcast_einzel[[disea]]$forecast_date %in% sort(unique(Nowcast_einzel[[disea]]$forecast_date))[seq(3, length(unique(Nowcast_einzel[[disea]]$forecast_date)), by = 3)],)
  Nowcast_darstellung[[disea]]=Nowcast_aufbrechen(Nowcast_darstellung[[disea]])
  }
for (disea in diseases) {
  targets[[disea]]=targets[[disea]][which(targets[[disea]]$age_group=="00+"),]
  #triangles[[disea]]$date=as.Date(triangles[[disea]]$date+4)
}
plotten=function(Nowcast,Zahl,disease){

  # Basisdaten vorbereiten
  #Nowcast=Nowcast_sari
  #Zahl=1
  #disease="sari"
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
            start = (as.Date(forecast_dates[1])-35),
            end = as.Date(forecast_dates[length(forecast_dates)]),
            forecast_date = forecast_date,
            ylim = c(0, 1.2 * max(tail(plot_data_back_in_time$value, 20)))
)
# Aktuelle Daten hinzufügen
#lines(target_current$date, target_current$value, col = "red", lty = "solid")
title(disease)

# 2. Jetzt zusätzliche Nowcasts mit Linien hinzufügen (Medianlinien z. B.)
for (i in 1:(length(Nowcast))) {
  
  forecast_i <- Nowcast[[i]]
  median_forecast <- subset(forecast_i, quantile == 0.5)
  
  lines(as.Date(median_forecast$target_end_date), median_forecast$value,
        col = "#A3107C", lty = "solid")
  
  #draw_truths <- function(truth, location){
  #  lines(truth$date, truth$value, pch = 20, lwd = 2)
  #}
  
  
  # 95% Intervall extrahieren
  lower1 <- subset(forecast_i, quantile == 0.025)
  upper1 <- subset(forecast_i, quantile == 0.975)
  
  lower2=subset(forecast_i, quantile == 0.25)
  upper2=subset(forecast_i, quantile == 0.75)
  
  # Sicherstellen, dass Daten in gleicher Reihenfolge vorliegen
  stopifnot(all(as.Date(lower1$target_end_date) == as.Date(upper1$target_end_date)))
  stopifnot(all(as.Date(lower2$target_end_date) == as.Date(upper2$target_end_date)))
  # Polygon (Konfidenzband) zeichnen
  polygon(
    c(as.Date(lower1$target_end_date), rev(as.Date(upper1$target_end_date))),
    c(lower1$value, rev(upper1$value)),
    col = adjustcolor("#009682", alpha.f = 0.2),
    border = NA
  )
  polygon(
    c(as.Date(lower2$target_end_date), rev(as.Date(upper2$target_end_date))),
    c(lower2$value, rev(upper2$value)),
    col = adjustcolor("#009682", alpha.f = 0.5),
    border = NA
  )
  # 3. Damalige Wahrheit extrahieren (BLACK LINE)
  observed_back_in_time <- data_as_of(dat_truth = triangles[[Zahl]],  # oder triangles[[disease]]
                                      date = as.Date(unique(forecast_i$forecast_date)),
                                      location = "DE", age_group = "00+",
                                      max_lag = max_delay)
  
  plot_data_back_i <- data.frame(
    date = as.Date(observed_back_in_time$date),
    value = rowSums(observed_back_in_time[, grepl("value_", colnames(observed_back_in_time))],
                    na.rm = TRUE)
  )
  
  # 4. Schwarze Linie plotten
  lines(plot_data_back_i$date, plot_data_back_i$value, col = "yellow", lty = "solid")
}
  #browser()
}
i=1
for (disea in diseases){
plotten(Nowcast_darstellung[[disea]],i,disea)
i=i+1
}

