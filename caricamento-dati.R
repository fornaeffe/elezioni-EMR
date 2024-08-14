library(data.table)
library(parallel)

#### Base dati relativa ai collegi elettorali ####

# TODO: verificare se serve davvero

# Ottenuta da 
# http://www.riformeistituzionali.gov.it/media/1367/proposta_commissione_13112020.zip
# File Access da cui è stato estratto il csv

# # Carico i dati
# base_dati <- fread("dati/BaseDati_Proposta_Commissione.csv", na.strings = "")
# 
# # Rinomino le colonne per coerenza con le altre fonti
# setnames(base_dati, c("DEN_PRO_CM20", "DEN_COM20"), c("PROVINCIA", "COMUNE"))
# 
# # Trasformo in maiuscolo per coerenza
# base_dati$PROVINCIA <- toupper(base_dati$PROVINCIA)
# base_dati$COMUNE <- toupper(base_dati$COMUNE)

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
# Purtroppo il file è codificato male all'origine, penso,
# e non penso ci si possa fare nulla...
camera_2018 <- fread(
  file.path(tempdir(), "Camera2018_livComune.txt"),
  encoding = "Latin-1"
)


#### Camera 2022 ####


# Preparo il nome del file temporaneo da scaricare
file_path <- file.path(tempdir(), "camera-20220925.zip")

# Scarico il file zip
download.file(
  "https://elezionistorico.interno.gov.it/daithome/documenti/opendata/camera/camera-20220925.zip",
  file_path
)

# Estraggo il file con i risultati degli scrutini a livello comunale
unzip(
  file_path,
  c("Camera_Italia_LivComune.txt",  "Camera_VAosta_LivComune.txt"),
  exdir = tempdir()
)

# Leggo il file appena estratto e creo un data.table
camera_2022 <- fread(file.path(tempdir(), "Camera_Italia_LivComune.txt"))
camera_2022_Aosta <- fread(file.path(tempdir(), "Camera_VAosta_LivComune.txt"))

#### Codici statistici e unità territoriali ####

# http://www.istat.it/storage/codici-unita-amministrative/Elenco-codici-statistici-e-denominazioni-delle-unit%C3%A0-territoriali.zip

# Preparo il nome del file temporaneo da scaricare
file_path <- tempfile(pattern = "ISTAT", fileext = ".zip")

# Scarico il file zip
download.file(
  "http://www.istat.it/storage/codici-unita-amministrative/Elenco-codici-statistici-e-denominazioni-delle-unit%C3%A0-territoriali.zip",
  file_path
)

interal_file_path <- file.path(
  "Elenco-codici-statistici-e-denominazioni-delle-unità-territoriali",
  "Codici-statistici-e-denominazioni-al-30_06_2024.csv"
)

# Estraggo il file csv
unzip(
  file_path,
  interal_file_path,
  exdir = tempdir(),
  unzip = "unzip"
)

# Leggo il file appena estratto e creo un data.table
ISTAT <- fread(file.path(tempdir(), interal_file_path), encoding = "Latin-1")

#### Unione ####
find_best_matches <- function(v, v_ref) {
  
  v <- unique(v)
  v_ref <- unique(v_ref)
  
  find_matches <- function(stringa, v_ref) {
    
    if (stringa %in% v_ref) return(c(stringa, stringa))
    
    matches <- agrep(
      stringa, 
      v_ref, 
      max.distance = 0.1,
      value = TRUE, 
      ignore.case = TRUE
    )
    
    if (length(matches) == 0) {
      return(c(stringa, NA))
    }
    
    return(c(stringa, matches[1]))
  }

  cl <- makeCluster(parallel::detectCores())
  
  results <- t(parSapplyLB(cl, v, find_matches, v_ref = v_ref))
  
  stopCluster(cl)
  
  results <- results[is.na(results[,2]) | results[,1] != results[,2],]
  
  return(results)
}

results <- find_best_matches(camera_2018$COMUNE, c(camera_2022$COMUNE, camera_2022_Aosta$COMUNE))

results