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
diseases <- c("sari", "sari_covid", "sari_influenza", "sari_rsv")



# dates for which to produce nowcasts:
# for retrospective generation:
#forecast_dates=c(as.Date("2024-10-03"),as.Date("2025-04-10"))
forecast_dates <- seq(from = as.Date("2024-10-10"),
                      to = as.Date("2025-04-10"),
                      by = 7)
# or select an individual forecast_date:
#forecast_dates <- as.Date("2024-10-10")                   #Da Meldungen immer Donnerstags sollte dieses Datum ebenfalls ein Donnerstag sein
# set the sizes of training data sets
n_history_dispersion <- 15
n_history_expectations <- 15
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
  
  # read in target time series:
  targets[[diseases[1]]] <- read.csv("C:\\Users\\felix\\Desktop\\Uni\\BA\\Daten\\target-icosari-sari.csv",#insgesamt
                                  colClasses = c("date" = "Date"), check.names = FALSE)
  targets[[diseases[2]]] <- read.csv("C:\\Users\\felix\\Desktop\\Uni\\BA\\Daten\\target-icosari-sari_covid19.csv",
                                 colClasses = c("date" = "Date"), check.names = FALSE)
  targets[[diseases[3]]] = read.csv("C:\\Users\\felix\\Desktop\\Uni\\BA\\Daten\\target-icosari-sari_influenza.csv",
                              colClasses = c("date" = "Date"), check.names = FALSE)
  targets[[diseases[4]]] =read.csv("C:\\Users\\felix\\Desktop\\Uni\\BA\\Daten\\target-icosari-sari_rsv.csv",
                       colClasses = c("date" = "Date"), check.names = FALSE)
  # this is just the current version of the data in time series format
#}
#Bis hier müsste Code passen
triangles
targets
# run over forecast dates to generate nowcasts:
for(i in seq_along(forecast_dates)){                    #Durchläuft alle Prognosedaten der Liste forecast_dates
  forecast_date <- forecast_dates[i]                    #Speichert aktuelles Prognosedatum in forecast_date
  cat(as.character(forecast_dates[i]), "\n")            #Gibt Prognosedatum auf Console aus
  
  # run over diseases:
  for (disease in diseases) {                           #Durchlaufe Sari, Covid, Rsv und Influenza Daten
    
    # generate nowcasts for age group 00+
    # note: pre-processing and subsetting of RT takes place inside
    # note: observed is the reporting triangle for which to generate a nowcast,
    # observed 2 is the triangle used to estimate the delay pattern (to do this,
    # borrow_delays and borrow_dispersion need to be set to TRUE)
    nc <- compute_nowcast(observed = triangles[[disease]], # this is the reporting triangle for which to compute nowcasts
                          location = "DE",              #Wir haben doch sowieso nur DE Daten?
                          age_group = "00+",            #Für meine Arbeit raus nehmen.
                          forecast_date = forecast_date,
                          observed2 = triangles$sari, # use complete sari to estimate delays
                          location2 = "DE",
                          age_group2 = "00+",
                          type = "additions",
                          borrow_delays = TRUE,          #Verzögerungsdaten von Sari nutzen
                          borrow_dispersion = TRUE,      #Verzögerungsverteilung von Sari nutzen
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
    
    # write out:
    # need to adapt path if nowcasts shall be written out.
     #write.csv(nc, file = paste0(path_repo, "/submissions/", data_source, "/", disease, "/KIT-simple_nowcast/",
                                # forecast_date, "-", data_source, "-", disease, "-KIT-simple_nowcast.csv"), row.names = FALSE)
  }
}

  