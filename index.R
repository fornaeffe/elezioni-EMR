
library(readxl)
library(stringr)
library(data.table)
library(parallel)

#### Importazione dati ####

##### Base dati #####

# TODO usare una base dati più aggiornata, soprattutto per quanto riguarda
# la popolazione

importa_dati <- function() {
  base_dati <- read.csv2(
    "dati/BaseDati_Proposta_Commissione.csv",
    fileEncoding = "utf-8",
    na.strings = ""
  )
  
  
  # Rinomino le colonne per coerenza con le altre fonti
  names(base_dati)[names(base_dati) == "DEN_PRO_CM20"] <- "PROVINCIA"
  names(base_dati)[names(base_dati) == "DEN_COM20"] <- "COMUNE"
  
  # Trasformo in maiuscolo per coerenza
  base_dati$PROVINCIA <- toupper(base_dati$PROVINCIA)
  base_dati$COMUNE <- toupper(base_dati$COMUNE)
  
  # Ottengo l'elenco delle province
  province <- aggregate(
    POP_2011 ~ PROVINCIA,
    base_dati,
    sum
  )
  
  # Ottengo l'elenco dei comuni
  comuni <- unique(base_dati[, c("PROVINCIA", "COMUNE")])
  
  ##### Politiche 2018 #####
  
  # Carico i dati delle elezioni politiche del 2018
  camera_2018 <- read.csv2(
    "dati/2018/camera-20180304_2.txt",
    fileEncoding = "utf-8"
  )
  
  # Collego ciascun collegio uninominale alla provincia di riferimento
  # TODO: sarebbe più opportuno svolgere questa operazione comune per comune,
  # ma ci sono troppe differenze tra i nomi dei comuni nei dati delle politiche
  # 2018 e i nomi dei comuni nella base dati
  camera_2018$COMUNE_COLLEGIO <- str_remove(camera_2018$COLLEGIOUNINOMINALE, "\\A[0-9]{2} (- )?")
  camera_2018$COMUNE_COLLEGIO <- str_remove(camera_2018$COMUNE_COLLEGIO, " - .*\\Z")
  camera_2018$COMUNE_COLLEGIO <- str_remove(camera_2018$COMUNE_COLLEGIO, " AREA STATISTICA .*\\Z")
  
  camera_2018 <- merge(
    camera_2018,
    comuni,
    by.x = "COMUNE_COLLEGIO",
    by.y = "COMUNE",
    all.x = TRUE
  )
  
  camera_2018$PROVINCIA[camera_2018$COMUNE_COLLEGIO == ""] <- "AOSTA"
  camera_2018$PROVINCIA[camera_2018$COMUNE_COLLEGIO == "BOLZANO/BOZEN"] <- "BOLZANO"
  camera_2018$PROVINCIA[camera_2018$COMUNE_COLLEGIO == "BRESSANONE/BRIXEN"] <- "BOLZANO"
  camera_2018$PROVINCIA[camera_2018$COMUNE_COLLEGIO == "CANT+"] <- "COMO"
  camera_2018$PROVINCIA[camera_2018$COMUNE_COLLEGIO == "CORIGLIANO CALABRO"] <- "COSENZA"
  camera_2018$PROVINCIA[camera_2018$COMUNE_COLLEGIO == "FORL¦"] <- "FORLI'-CESENA"
  camera_2018$PROVINCIA[camera_2018$COMUNE_COLLEGIO == "MERANO/MERAN"] <- "BOLZANO"
  camera_2018$PROVINCIA[camera_2018$COMUNE_COLLEGIO == "NARDÊ"] <- "LECCE"
  camera_2018$PROVINCIA[camera_2018$COMUNE_COLLEGIO == "PATERNÊ"] <- "CATANIA"
  camera_2018$PROVINCIA[camera_2018$COMUNE_COLLEGIO == "SAN DONA' DI PIAVE"] <- "VENEZIA"
  
  # Checks
  # TO DO: trasformare in un controllo che innesca un errore
  setdiff(unique(camera_2018$PROVINCIA), province$PROVINCIA)
  sum(is.na(camera_2018$PROVINCIA))
  
  
  ##### Politiche 2022 #######
  
  # Carico l'anagrafica dei comuni e la formatto coerentemente con gli altri dati
  anagrafica_comuni <- read.csv("dati/2022/camera-italia-comune_anagrafica.csv")
  anagrafica_comuni <- anagrafica_comuni[, c(
    "codice",
    "desc_com",
    "desc_prov",
    "desc_circ",
    "ele_t",
    "vot_t"
  )]
  names(anagrafica_comuni) <- c(
    "codice",
    "COMUNE",
    "PROVINCIA",
    "CIRCOSCRIZIONE",
    "ELETTORI",
    "VOTANTI"
  )
  
  # Carico i dati elettorali per comune
  camera_2022 <- read.csv("dati/2022/camera-italia-comune.csv")
  
  # Aggiungo i dati relativi ai comuni
  camera_2022 <- merge(
    camera_2022,
    anagrafica_comuni
  )
  
  # Rinomino le colonne
  names(camera_2022)[names(camera_2022) == "desc_lis"] <- "LISTA"
  names(camera_2022)[names(camera_2022) == "voti"] <- "VOTI_LISTA"
  names(camera_2022)[names(camera_2022) == "cogn"] <- "COGNOME"
  names(camera_2022)[names(camera_2022) == "nome"] <- "NOME"
  
  # Rinomino alcune province, per coerenza
  camera_2022$PROVINCIA[camera_2022$PROVINCIA == "REGGIO NELL' EMILIA"] <- "REGGIO NELL'EMILIA"
  camera_2022$PROVINCIA[camera_2022$PROVINCIA == "MASSA-CARRARA"] <- "MASSA CARRARA"
  
  
  # Distribuisco i voti al solo candidato proporzionalmente tra le liste
  # che lo sostengono
  camera_2022 <- merge(
    camera_2022,
    aggregate(
      VOTI_LISTA ~
        codice +
        COGNOME +
        NOME,
      camera_2022,
      sum
    ),
    by = c("codice", "COGNOME", "NOME"),
    suffixes = c("", "_TOT")
  )
  
  camera_2022$VOTI_LISTA <- 
    camera_2022$VOTI_LISTA +
    camera_2022$voti_solo_can * 
    camera_2022$VOTI_LISTA / camera_2022$VOTI_LISTA_TOT
  
  
  camera_2022 <- camera_2022[,c(
    "CIRCOSCRIZIONE",
    "PROVINCIA",
    "COMUNE",
    "ELETTORI",
    "VOTANTI",
    "COGNOME",
    "NOME",
    "LISTA",
    "VOTI_LISTA"
  )]
  
  
  
  ###### Trovo la regione #######
  
  camera_2018$REGIONE <- str_remove(camera_2018$CIRCOSCRIZIONE, " [0-9]\\Z")
  camera_2018$REGIONE <- str_remove(camera_2018$REGIONE, "/.*")
  camera_2018$ELEZIONE <- "camera_2018"
  
  camera_2022$REGIONE <- str_remove(camera_2022$CIRCOSCRIZIONE, " [0-9]\\Z")
  camera_2022$REGIONE <- str_remove(camera_2022$REGIONE, "/.*")
  camera_2022$ELEZIONE <- "camera_2022"
  
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
  
  # Rinomino alcune province
  amministrative$PROVINCIA[amministrative$PROVINCIA == "REGGIO NELL' EMILIA"] <- "REGGIO NELL'EMILIA"
  amministrative$PROVINCIA[amministrative$PROVINCIA == "MASSA-CARRARA"] <- "MASSA CARRARA"
  
  # Checks
  # TO DO: trasformare in un controllo che innesca un errore
  setdiff(unique(amministrative$PROVINCIA), province$PROVINCIA)
  
  
  ##### Unisco i dati #####
  
  dati_precedenti <- rbindlist(
    lapply(
      list(
        amministrative,
        camera_2018,
        camera_2022
      ),
      function(df) df[, c(
        "REGIONE",
        "PROVINCIA",
        "COMUNE",
        "ELETTORI",
        "VOTANTI",
        "LISTA",
        "VOTI_LISTA",
        "ELEZIONE"
      )]
    )
  )
  
  ###### Calcolo astensione ######
  
  calcola_astensione <- function(df) {
    astensione <- aggregate(
      VOTI_LISTA ~
        REGIONE +
        PROVINCIA +
        COMUNE +
        ELETTORI +
        VOTANTI +
        ELEZIONE,
      df,
      sum
    )
    
    astensione$LISTA <- "astensione"
    astensione$VOTI_LISTA <- astensione$ELETTORI - astensione$VOTI_LISTA
    
    astensione
  }
  
  dati_precedenti <- rbind(
    dati_precedenti,
    calcola_astensione(dati_precedenti)
  )
  
  ##### Filtro (solo Emilia-Romagna) #####
  
  dati_precedenti <- dati_precedenti[dati_precedenti$REGIONE == "EMILIA-ROMAGNA"]
  province <- province[province$PROVINCIA %in% dati_precedenti$PROVINCIA, ]
  
  return(list(province = province, dati_precedenti = dati_precedenti))
}

dati <- importa_dati()

dati_precedenti <- dati$dati_precedenti
province <- dati$province


# Questo è servito per esportare i nomi delle liste
write.csv2(
  dati$dati_precedenti[!duplicated(dati$dati_precedenti$LISTA), ],
  "_output/liste_precedenti_elezioni.csv",
  fileEncoding = "utf-8"
)

##### Corrispondenza liste - aree #####

# Carico la tabella di corrispondenza tra liste e partiti, e la appiattisco

corrispondenza_liste <- as.data.table(read_xlsx("dati/corrispondenza_liste.xlsx"))
corrispondenza_liste[is.na(corrispondenza_liste)] <- 0

corrispondenza_liste <- melt(
  corrispondenza_liste,
  c("LISTA_ORIGINALE", "ELEZIONE"),
  variable.name = "LISTA",
  value.name = "FATTORE"
)

corrispondenza_liste <- corrispondenza_liste[corrispondenza_liste$FATTORE > 0, ]


# Unisco questa ai dati precedenti, e calcolo i voti per le nuove liste

names(dati_precedenti)[names(dati_precedenti) == "LISTA"] <- "LISTA_ORIGINALE"

corrispondenza_liste$ELEZIONE <- NULL

dati_precedenti <- merge(
  dati_precedenti,
  corrispondenza_liste,
  allow.cartesian = TRUE
)

dati_precedenti$VOTI_LISTA <- dati_precedenti$VOTI_LISTA * dati_precedenti$FATTORE

#### Calcolo distribuzione spaziale elettori di area ####

prov_lista_elezione <- aggregate(
  VOTI_LISTA ~ PROVINCIA + LISTA + ELEZIONE,
  dati_precedenti,
  sum
)

prov_lista_elezione <- merge(
  prov_lista_elezione,
  aggregate(
    VOTI_LISTA ~ PROVINCIA + ELEZIONE,
    prov_lista_elezione, 
    sum
  ),
  by = c("PROVINCIA", "ELEZIONE"),
  suffixes = c("", "_PROV")
)

prov_lista_elezione$PERCENTUALE <-
  prov_lista_elezione$VOTI_LISTA / prov_lista_elezione$VOTI_LISTA_PROV

prov_lista_elezione$LOG_P <- log(prov_lista_elezione$PERCENTUALE)

lista_elezione <- aggregate(
  VOTI_LISTA ~ LISTA + ELEZIONE,
  prov_lista_elezione,
  sum
)

lista_elezione <- merge(
  lista_elezione,
  aggregate(
    VOTI_LISTA ~ ELEZIONE,
    lista_elezione,
    sum
  ),
  by = "ELEZIONE",
  suffixes = c("", "_TOT")
)

lista_elezione$PERCENTUALE <-
  lista_elezione$VOTI_LISTA / lista_elezione$VOTI_LISTA_TOT

lista_elezione$LOG_P <- log(lista_elezione$PERCENTUALE)

prov_lista_elezione <- merge(
  prov_lista_elezione,
  lista_elezione[, c("LISTA", "ELEZIONE", "LOG_P")],
  by = c("LISTA", "ELEZIONE"),
  suffixes = c("", "_ELEZIONE")
)

prov_lista_elezione$DELTA_LOG_P <-
  prov_lista_elezione$LOG_P - prov_lista_elezione$LOG_P_ELEZIONE


prov_lista <- aggregate(
  VOTI_LISTA ~ PROVINCIA + LISTA,
  dati_precedenti,
  sum
)

prov_lista <- merge(
  prov_lista,
  aggregate(
    VOTI_LISTA ~ PROVINCIA,
    prov_lista,
    sum
  ),
  by = "PROVINCIA",
  suffixes = c("", "_TOT")
)

prov_lista$PERCENTUALE_STORICA <- prov_lista$VOTI_LISTA / prov_lista$VOTI_LISTA_TOT

prov_lista$LOG_P <- log(prov_lista$PERCENTUALE_STORICA)

prov_lista <- merge(
  prov_lista,
  province
)

prov_lista$POP_LISTA <- prov_lista$PERCENTUALE_STORICA * prov_lista$POP_2011

liste <- aggregate(
  POP_LISTA ~ LISTA,
  prov_lista,
  sum
)


popolazione <- sum(liste$POP_LISTA)

liste$PERCENTUALE_STORICA <- liste$POP_LISTA / popolazione
liste$LOG_P <- log(liste$PERCENTUALE_STORICA)


prov_lista <- merge(
  prov_lista,
  liste,
  by = "LISTA",
  suffixes = c("", "_TOT")
)

prov_lista$PERCENTUALE_LISTA <- prov_lista$POP_LISTA / prov_lista$POP_LISTA_TOT

prov_lista$DELTA_LOG_P <-
  prov_lista$LOG_P - prov_lista$LOG_P_TOT

prov_lista_elezione <- merge(
  prov_lista_elezione,
  prov_lista[, c("PROVINCIA", "LISTA", "DELTA_LOG_P")],
  by = c("PROVINCIA", "LISTA"),
  suffixes = c("", "_GLOBALE")
)

prov_lista_elezione$DELTA2_LOG_P <-
  prov_lista_elezione$DELTA_LOG_P - prov_lista_elezione$DELTA_LOG_P_GLOBALE

variab <- sd(prov_lista_elezione$DELTA2_LOG_P)

# Pulizia
rm(corrispondenza_liste, dati, dati_precedenti, lista_elezione, prov_lista_elezione, liste)

prov_lista <- prov_lista[, c(
  "PROVINCIA",
  "LISTA",
  "POP_2011",
  "PERCENTUALE_LISTA"
)]

##### Carico liste regionali #####

liste <- read_xlsx("dati/liste.xlsx")

liste$PERCENTUALE_CORRETTA <- liste$PERCENTUALE
liste$PERCENTUALE_CORRETTA[liste$LISTA != "astensione"] <-
  liste$PERCENTUALE[liste$LISTA != "astensione"] * 
  ( 1 - liste$PERCENTUALE[liste$LISTA == "astensione"])

stopifnot(sum(liste$PERCENTUALE_CORRETTA) == 1)
stopifnot(setequal(liste$LISTA, unique(prov_lista$LISTA)))


liste$LOG_P <- log(liste$PERCENTUALE_CORRETTA)

# pulizia

liste <- liste[, c(
  "COALIZIONE",
  "LISTA",
  "LOG_P"
)]

#### Simulazione ####

simula <- function(
    iterazioni = 200
) {
  
  iterazione <- function(
    iter = 1,
    liste,
    prov_lista,
    popolazione,
    variab,
    province
  ) {
    #### Simulazione percentuali regionali ####
    
    liste$LOG_P_ITER <- rnorm(
      liste$LOG_P,
      liste$LOG_P,
      0.25
    )
    
    liste$PERCENTUALE <- exp(liste$LOG_P_ITER) / sum(exp(liste$LOG_P_ITER))
    
    liste$VOTANTI <- liste$PERCENTUALE * popolazione
    
    #### Calcolo percentuali per provincia ####
    
    prov_lista <- merge(
      prov_lista,
      liste[,c("LISTA", "VOTANTI")]
    )
    
    prov_lista$VOTANTI_LOCALI <- prov_lista$VOTANTI * prov_lista$PERCENTUALE_LISTA
    
    prov_lista$PERCENTUALE_BASE <- prov_lista$VOTANTI_LOCALI / prov_lista$POP_2011
    
    prov_lista$LOG_P_BASE <- log(prov_lista$PERCENTUALE_BASE)
    
    prov_lista$LOG_P_ITER <- rnorm(
      prov_lista$LOG_P_BASE,
      prov_lista$LOG_P_BASE,
      variab
    )
    
    prov_lista$PERCENTUALE_ITER <- ave(
      prov_lista$LOG_P_ITER,
      prov_lista$PROVINCIA,
      FUN = function(x) exp(x) / sum(exp(x))
    )
    
    # TODO sostituire la popolazione con gli elettori
    prov_lista$VOTI_LISTA_ITER <- prov_lista$POP_2011 * prov_lista$PERCENTUALE_ITER
    
    # prov_lista <- prov_lista[, c(
    #   "PROVINCIA",
    #   "LISTA",
    #   "VOTI_LISTA_ITER"
    # )]
    # 
    # liste <- liste[, c(
    #   "COALIZIONE",
    #   "LISTA"
    # )]
    
    # save(liste, prov_lista, province, file = "dati_per_scrutinio.RData")
    
    scrutinio <- Scrutinio(
      prov_lista[, c(
        "PROVINCIA",
        "LISTA",
        "VOTI_LISTA_ITER"
      )],
      province,
      liste[, c(
        "COALIZIONE",
        "LISTA"
      )]
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
    liste = liste,
    prov_lista = prov_lista,
    popolazione = popolazione,
    variab = variab,
    province = province
  )
  
  stopCluster(cl)
  
  # TODO continua
  
}



