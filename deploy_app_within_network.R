#Deploy App innerhalb eines Netzwerks

# Durch Proxy-Freigabe kann der eigene APC genutzt werden,
# innerhalb eines Netzwerks den Zugriff für Kolleginnen und Kollegen
# zu ermöglichen.
# Benötigt wird die App sowie das Package: shiny

#Pfad zur Package-Bibliothek
.libPaths()

folder_address = 'H:/NRWoelkchen/app.R' # ordnerpfad zu app.R

x = system("ipconfig", intern = TRUE)
z = x[grep("IPv4", x)]
ip = gsub(".*? ([[:digit:]])", "\\1", z)
port = 80L
cat(paste0("running on: http://", ip, ":", port, "/"))

runApp(folder_address, launch.browser = TRUE, port = port, host = ip[2])
