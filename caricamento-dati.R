library(data.table)

#### Base dati relativa ai collegi elettorali ####

# TODO: verificare se serve davvero

# Ottenuta da 
# http://www.riformeistituzionali.gov.it/media/1367/proposta_commissione_13112020.zip
# File Access da cui è stato estratto il csv

# Carico i dati
base_dati <- fread("dati/BaseDati_Proposta_Commissione.csv", na.strings = "")

# Rinomino le colonne per coerenza con le altre fonti
setnames(base_dati, c("DEN_PRO_CM20", "DEN_COM20"), c("PROVINCIA", "COMUNE"))

# Trasformo in maiuscolo per coerenza
base_dati$PROVINCIA <- toupper(base_dati$PROVINCIA)
base_dati$COMUNE <- toupper(base_dati$COMUNE)

#### Camera 2018 ####

# Preparo il nome del file temporaneo da scaricare
file_path <- file.path(tempdir(), "camera-20180304.zip")

# Scarico il file zip
download.file(
  "https://elezionistorico.interno.gov.it/daithome/documenti/opendata/camera/camera-20180304.zip",
  file_path
)

# Estraggo il file con i risultati degli scrutini a livello comunale
unzip(
  file_path,
  "Camera2018_livComune.txt",
  exdir = tempdir()
)

# Leggo il file appena estratto e creo un data.table
camera_2018 <- fread(file.path(tempdir(), "Camera2018_livComune.txt"))
