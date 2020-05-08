#Deploy App innerhalb eines Netzwerks

# Durch Proxy-Freigabe kann der eigene APC genutzt werden,
# innerhalb eines Netzwerks den Zugriff für Kolleginnen und Kollegen
# zu ermöglichen.
# Benötigt wird die App sowie das Package: shiny

#Pfad zur Package-Bibliothek
.libPaths()
library(shiny)
setwd('H:/R-Stuff/NRWoelkchen-master') # ordnerpfad zu app.R

x = system("ipconfig", intern = TRUE)
z = x[grep("IPv4", x)]
ip = gsub(".*? ([[:digit:]])", "\\1", z)

options(shiny.host = '0.0.0.0')        # standard (nicht anpassen)
options(shiny.port = 8888)             # port muss freigegeben sein (ggf. anpassen)

message(paste0("running on: http://", ip, ":", options("shiny.port"), "/"))
runApp()