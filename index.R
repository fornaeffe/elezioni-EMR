
library(readxl)
library(stringr)
library(data.table)
library(parallel)

#### Importazione dati ####

##### Base dati #####

base_dati <- read.csv2(
  "dati/BaseDati_Proposta_Commissione.csv",
  fileEncoding = "utf-8",
  
  # TO DO: verificare che le righe seguenti siano davvero utili
  # colClasses = c(
  #   CIRCOCAM_20_DEN = "factor",
  #   CP20_DEN = "factor",
  #   CU20_DEN = "factor"
  # ),
  
  na.strings = ""
)

base_dati$SP20_DEN[base_dati$DEN_REG20 == "Trentino-Alto Adige"] <- 
  "Trentino-Alto Adige/Südtirol - P01"

base_dati$DEN_REG20[base_dati$DEN_REG20 == "Trentino-Alto Adige"] <- 
  "Trentino-Alto Adige/Südtirol"

base_dati$CIRCOCAM_20_DEN <- 
  str_remove(base_dati$CIRCOCAM_20_DEN, "/Vallée d'Aoste")
base_dati$SU20_DEN <- 
  str_remove(base_dati$SU20_DEN, "/Vallée d'Aoste")
base_dati$CU20_DEN <- 
  str_remove(base_dati$CU20_DEN, "/Vallée d'Aoste")

#Trasformo in maiuscolo per compatibilità con i dati dei candidati
base_dati$DEN_REG20 <- toupper(base_dati$DEN_REG20)
base_dati$CIRCOCAM_20_DEN <- toupper(base_dati$CIRCOCAM_20_DEN)
base_dati$CP20_DEN <- toupper(base_dati$CP20_DEN)
base_dati$SP20_DEN <- toupper(base_dati$SP20_DEN)
base_dati$CU20_DEN <- toupper(base_dati$CU20_DEN)
base_dati$SU20_DEN <- toupper(base_dati$SU20_DEN)


names(base_dati)[names(base_dati) == "DEN_PRO_CM20"] <- "PROVINCIA"
names(base_dati)[names(base_dati) == "DEN_COM20"] <- "COMUNE"

base_dati$PROVINCIA <- toupper(base_dati$PROVINCIA)
base_dati$COMUNE <- toupper(base_dati$COMUNE)

province <- aggregate(
  POP_2011 ~ PROVINCIA,
  base_dati,
  sum
)


comuni <- unique(base_dati[, c("PROVINCIA", "COMUNE")])

##### Politiche #####

camera_2018 <- read.csv2(
  "dati/2018/camera-20180304_2.txt",
  fileEncoding = "utf-8"
)

camera_2018$PROV_TEMP <- str_remove(camera_2018$COLLEGIOUNINOMINALE, "\\A[0-9]{2} (- )?")
camera_2018$PROV_TEMP <- str_remove(camera_2018$PROV_TEMP, " - .*\\Z")
camera_2018$PROV_TEMP <- str_remove(camera_2018$PROV_TEMP, " AREA STATISTICA .*\\Z")

camera_2018 <- merge(
  camera_2018,
  comuni,
  by.x = "PROV_TEMP",
  by.y = "COMUNE",
  all.x = TRUE
)

camera_2018$PROVINCIA[camera_2018$PROV_TEMP == ""] <- "AOSTA"
camera_2018$PROVINCIA[camera_2018$PROV_TEMP == "BOLZANO/BOZEN"] <- "BOLZANO"
camera_2018$PROVINCIA[camera_2018$PROV_TEMP == "BRESSANONE/BRIXEN"] <- "BOLZANO"
camera_2018$PROVINCIA[camera_2018$PROV_TEMP == "CANT+"] <- "COMO"
camera_2018$PROVINCIA[camera_2018$PROV_TEMP == "CORIGLIANO CALABRO"] <- "COSENZA"
camera_2018$PROVINCIA[camera_2018$PROV_TEMP == "FORL¦"] <- "FORLI'-CESENA"
camera_2018$PROVINCIA[camera_2018$PROV_TEMP == "MERANO/MERAN"] <- "BOLZANO"
camera_2018$PROVINCIA[camera_2018$PROV_TEMP == "NARDÊ"] <- "LECCE"
camera_2018$PROVINCIA[camera_2018$PROV_TEMP == "PATERNÊ"] <- "CATANIA"
camera_2018$PROVINCIA[camera_2018$PROV_TEMP == "SAN DONA' DI PIAVE"] <- "VENEZIA"

# Checks
# TO DO: trasformare in un controllo che innesca un errore
setdiff(unique(camera_2018$PROVINCIA), province$PROVINCIA)
sum(is.na(camera_2018$PROVINCIA))


###### 2022 ########

carica_dati <- function(ramo) {
  
  anagrafica_comuni <- read.csv(
    paste0(
      "dati/2022/",
      ramo,
      "-italia-comune_anagrafica.csv"
    )
  )
  
  anagrafica_comuni$desc_cl_uni <- str_remove(
    anagrafica_comuni$desc_cl_uni,
    " \\(.*"
  )
  
  anagrafica_per_merge <- anagrafica_comuni[, c(
    "codice",
    "desc_com",
    "desc_prov",
    "desc_circ",
    "ele_t",
    "vot_t"
  )]
  
  names(anagrafica_per_merge) <- c(
    "codice",
    "COMUNE",
    "PROVINCIA",
    "CIRCOSCRIZIONE",
    "ELETTORI",
    "VOTANTI"
  )
  
  liste_comune <- read.csv(
    paste0(
      "dati/2022/",
      ramo,
      "-italia-comune.csv"
    )
  )
  
  liste_comune <- merge(
    liste_comune,
    anagrafica_per_merge
  )
  
  names(liste_comune)[names(liste_comune) == "desc_lis"] <- "LISTA"
  names(liste_comune)[names(liste_comune) == "voti"] <- "VOTI_LISTA"
  
  cand_comune <- aggregate(
    VOTI_LISTA ~
      codice +
      cogn +
      nome +
      voti_solo_can +
      COMUNE +
      PROVINCIA +
      CIRCOSCRIZIONE,
    liste_comune,
    sum
  )
  cand_comune$VOTI_TOT_CANDIDATO <- cand_comune$VOTI_LISTA
  cand_comune$VOTI_LISTA <- NULL
  
  liste_comune <- merge(
    liste_comune,
    cand_comune
  )
  
  liste_comune$PERC_NEL_CANDIDATO <- liste_comune$VOTI_LISTA / liste_comune$VOTI_TOT_CANDIDATO
  liste_comune$VOTI_LISTA <- liste_comune$VOTI_LISTA + liste_comune$voti_solo_can * liste_comune$PERC_NEL_CANDIDATO
  
  liste_comune$PROVINCIA[liste_comune$PROVINCIA == "REGGIO NELL' EMILIA"] <- "REGGIO NELL'EMILIA"
  liste_comune$PROVINCIA[liste_comune$PROVINCIA == "MASSA-CARRARA"] <- "MASSA CARRARA"
  names(liste_comune)[names(liste_comune) == "cogn"] <- "COGNOME"
  names(liste_comune)[names(liste_comune) == "nome"] <- "NOME"

  return(liste_comune[,c(
    "CIRCOSCRIZIONE",
    "PROVINCIA",
    "COMUNE",
    "ELETTORI",
    "VOTANTI",
    "COGNOME",
    "NOME",
    "LISTA",
    "VOTI_LISTA"
  )])
}

camera_2022 <- carica_dati("camera")


##### Amministrative #####

lista_files <- list.files("dati/eur_reg")

lista_dataframes <- lapply(
  paste0("dati/eur_reg/", lista_files),
  read.csv2,
  fileEncoding = "utf-8"
)

lista_dataframes <- mapply(
  function(df, nome_file) {
    names(df) <- toupper(names(df))
    if (is.null(df$COGNOME)) df$COGNOME <- NA
    if (is.null(df$NOME)) df$NOME <- NA
    names(df)[names(df) == "VOTILISTA"] <- "VOTI_LISTA"
    df <- df[,c(
      "REGIONE",
      "PROVINCIA",
      "COMUNE",
      "ELETTORI",
      "VOTANTI",
      "COGNOME",
      "NOME",
      "LISTA",
      "VOTI_LISTA"
    )]
    df$ELEZIONE <- nome_file
    df
  },
  df = lista_dataframes,
  nome_file = lista_files,
  SIMPLIFY = FALSE
)

amministrative <- rbindlist(lista_dataframes)
lista_dataframes <- NULL

amministrative$PROVINCIA[amministrative$PROVINCIA == "REGGIO NELL' EMILIA"] <- "REGGIO NELL'EMILIA"
amministrative$PROVINCIA[amministrative$PROVINCIA == "MASSA-CARRARA"] <- "MASSA CARRARA"

# Checks
# TO DO: trasformare in un controllo che innesca un errore
setdiff(unique(amministrative$PROVINCIA), province$PROVINCIA)

##### Unione camera e amministrative #####

camera_2018$REGIONE <- str_remove(camera_2018$CIRCOSCRIZIONE, " [0-9]\\Z")
camera_2018$REGIONE <- str_remove(camera_2018$REGIONE, "/.*")
camera_2018$ELEZIONE <- "camera_2018"

camera_2022$REGIONE <- str_remove(camera_2022$CIRCOSCRIZIONE, " [0-9]\\Z")
camera_2022$REGIONE <- str_remove(camera_2022$REGIONE, "/.*")
camera_2022$ELEZIONE <- "camera_2022"


###### Calcolo astensione ######

camera_2018_astensione <- aggregate(
  VOTI_LISTA ~ 
    CIRCOSCRIZIONE +
    COLLEGIOPLURINOMINALE +
    COLLEGIOUNINOMINALE +
    REGIONE +
    PROVINCIA +
    COMUNE +
    ELETTORI +
    VOTANTI + 
    ELEZIONE,
  camera_2018,
  sum
)

camera_2018_astensione$LISTA <- "astensione"
camera_2018_astensione$COGNOME <- NA
camera_2018_astensione$NOME <- NA
camera_2018_astensione$VOTI_LISTA <- 
  camera_2018_astensione$ELETTORI - camera_2018_astensione$VOTI_LISTA

camera_2022_astensione <- aggregate(
  VOTI_LISTA ~ 
    REGIONE +
    PROVINCIA +
    COMUNE +
    ELETTORI +
    VOTANTI + 
    ELEZIONE,
  camera_2022,
  sum
)

camera_2022_astensione$LISTA <- "astensione"
camera_2022_astensione$COGNOME <- NA
camera_2022_astensione$NOME <- NA
camera_2022_astensione$VOTI_LISTA <- 
  camera_2022_astensione$ELETTORI - camera_2022_astensione$VOTI_LISTA

amministrative_astensione <- aggregate(
  VOTI_LISTA ~ 
    REGIONE +
    PROVINCIA +
    COMUNE +
    ELETTORI +
    VOTANTI +
    ELEZIONE,
  amministrative,
  sum
)

amministrative_astensione$LISTA <- "astensione"
amministrative_astensione$COGNOME <- NA
amministrative_astensione$NOME <- NA
amministrative_astensione$VOTI_LISTA <- 
  amministrative_astensione$ELETTORI - amministrative_astensione$VOTI_LISTA





camera_2022$CIRCOSCRIZIONE <- NULL

dati_precedenti <- rbind(
  amministrative,
  amministrative_astensione,
  camera_2018[, c(
    "REGIONE",
    "PROVINCIA",
    "COMUNE",
    "ELETTORI",
    "VOTANTI",
    "COGNOME",
    "NOME",
    "LISTA",
    "VOTI_LISTA",
    "ELEZIONE"
  )],
  camera_2018_astensione[, c(
    "REGIONE",
    "PROVINCIA",
    "COMUNE",
    "ELETTORI",
    "VOTANTI",
    "COGNOME",
    "NOME",
    "LISTA",
    "VOTI_LISTA",
    "ELEZIONE"
  )],
  camera_2022,
  camera_2022_astensione
)
amministrative <- NULL
camera_2018 <- NULL
camera_2022 <- NULL

dati_precedenti <- dati_precedenti[dati_precedenti$REGIONE == "EMILIA-ROMAGNA"]
province <- province[province$PROVINCIA %in% dati_precedenti$PROVINCIA, ]

# Questo è servito per esportare i nomi delle liste
write.csv2(
  dati_precedenti[!duplicated(dati_precedenti$LISTA), ],
  "_output/liste_precedenti_elezioni.csv",
  fileEncoding = "utf-8"
)

##### Corrispondenza liste - aree #####

liste <- read_xlsx("dati/liste_precedenti_elezioni.xlsx", "aree")


#### Calcolo distribuzione spaziale elettori di area ####

dati_precedenti$AREA <- 
  factor(dati_precedenti$LISTA, levels = liste$LISTA, labels = liste$AREA)

prov_area_elezione <- aggregate(
  VOTI_LISTA ~ PROVINCIA + AREA + ELEZIONE,
  dati_precedenti,
  sum
)

prov_area_elezione <- merge(
  prov_area_elezione,
  aggregate(
    VOTI_LISTA ~ PROVINCIA + ELEZIONE,
    prov_area_elezione, 
    sum
  ),
  by = c("PROVINCIA", "ELEZIONE"),
  suffixes = c("", "_PROV")
)

prov_area_elezione$PERCENTUALE <-
  prov_area_elezione$VOTI_LISTA / prov_area_elezione$VOTI_LISTA_PROV

prov_area_elezione$LOG_P <- log(prov_area_elezione$PERCENTUALE)

area_elezione <- aggregate(
  VOTI_LISTA ~ AREA + ELEZIONE,
  prov_area_elezione,
  sum
)

area_elezione <- merge(
  area_elezione,
  aggregate(
    VOTI_LISTA ~ ELEZIONE,
    area_elezione,
    sum
  ),
  by = "ELEZIONE",
  suffixes = c("", "_TOT")
)

area_elezione$PERCENTUALE <-
  area_elezione$VOTI_LISTA / area_elezione$VOTI_LISTA_TOT

area_elezione$LOG_P <- log(area_elezione$PERCENTUALE)

prov_area_elezione <- merge(
  prov_area_elezione,
  area_elezione[, c("AREA", "ELEZIONE", "LOG_P")],
  by = c("AREA", "ELEZIONE"),
  suffixes = c("", "_ELEZIONE")
)

prov_area_elezione$DELTA_LOG_P <-
  prov_area_elezione$LOG_P - prov_area_elezione$LOG_P_ELEZIONE


prov_area <- aggregate(
  VOTI_LISTA ~ PROVINCIA + AREA,
  dati_precedenti,
  sum
)

prov_area <- merge(
  prov_area,
  aggregate(
    VOTI_LISTA ~ PROVINCIA,
    prov_area,
    sum
  ),
  by = "PROVINCIA",
  suffixes = c("", "_TOT")
)

prov_area$PERCENTUALE_STORICA <- prov_area$VOTI_LISTA / prov_area$VOTI_LISTA_TOT

prov_area$LOG_P <- log(prov_area$PERCENTUALE_STORICA)

prov_area <- merge(
  prov_area,
  province
)

prov_area$POP_AREA <- prov_area$PERCENTUALE_STORICA * prov_area$POP_2011

aree <- aggregate(
  POP_AREA ~ AREA,
  prov_area,
  sum
)


popolazione <- sum(aree$POP_AREA)

aree$LOG_P <- log(aree$POP_AREA / popolazione)


prov_area <- merge(
  prov_area,
  aree,
  by = "AREA",
  suffixes = c("", "_TOT")
)

prov_area$PERCENTUALE_AREA <- prov_area$POP_AREA / prov_area$POP_AREA_TOT

prov_area$DELTA_LOG_P <-
  prov_area$LOG_P - prov_area$LOG_P_TOT

prov_area_elezione <- merge(
  prov_area_elezione,
  prov_area[, c("PROVINCIA", "AREA", "DELTA_LOG_P")],
  by = c("PROVINCIA", "AREA"),
  suffixes = c("", "_GLOBALE")
)

prov_area_elezione$DELTA2_LOG_P <-
  prov_area_elezione$DELTA_LOG_P - prov_area_elezione$DELTA_LOG_P_GLOBALE

variab <- sd(prov_area_elezione$DELTA2_LOG_P)

##### Carico liste regionali #####

liste_reg <- read_xlsx("dati/liste.xlsx")
aree <- merge(aree, liste_reg, by.x = "AREA", by.y = "LISTA")

#### Simulazione ####



simula <- function(
    iterazioni = 200
) {
  
  iterazione <- function(
    iter = 1,
    aree,
    prov_area,
    popolazione,
    variab,
    province
  ) {
    #### Simulazione percentuali regionali ####
    
    aree$LOG_P_ITER <- rnorm(
      aree$LOG_P,
      aree$LOG_P,
      0.25
    )
    
    aree$PERCENTUALE <- exp(aree$LOG_P_ITER) / sum(exp(aree$LOG_P_ITER))
    
    aree$VOTANTI <- aree$PERCENTUALE * popolazione
    
    #### Calcolo percentuali per provincia ####
    
    prov_area <- merge(
      prov_area,
      aree[,c("AREA", "VOTANTI")]
    )
    
    prov_area$VOTANTI_LOCALI <- prov_area$VOTANTI * prov_area$PERCENTUALE_AREA
    
    prov_area$PERCENTUALE_BASE <- prov_area$VOTANTI_LOCALI / prov_area$POP_2011
    
    prov_area$LOG_P_BASE <- log(prov_area$PERCENTUALE_BASE)
    
    prov_area$LOG_P_ITER <- rnorm(
      prov_area$LOG_P_BASE,
      prov_area$LOG_P_BASE,
      variab
    )
    
    prov_area$PERCENTUALE_ITER <- ave(
      prov_area$LOG_P_ITER,
      prov_area$PROVINCIA,
      FUN = function(x) exp(x) / sum(exp(x))
    )
    
    prov_area$VOTI_LISTA_ITER <- prov_area$POP_2011 * prov_area$PERCENTUALE_ITER
    
    scrutinio <- Scrutinio(
      prov_area,
      province
    )
    
    # TODO continua
    
  }
  
  cl <- makeCluster(parallel::detectCores())
  
  clusterEvalQ(
    cl,
    source("scrutinio.R")
  )
  
  lista_risultati <- parLapply(
    cl,
    seq_len(iterazioni),
    iterazione,
    aree = aree,
    prov_area = prov_area,
    popolazione = popolazione,
    variab = variab,
    province = province
  )
  
  stopCluster(cl)
  
  # TODO continua
  
}



