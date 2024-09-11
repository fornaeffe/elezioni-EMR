library(data.table)
library(stringr)

# Funzione per scaricare ed estrarre i files
scarica <- function(
    url,
    internal_file_paths,
    unzip = "internal",
    encoding = "unknown",
    colClasses = NULL
) {
  # Preparo il nome del file temporaneo da scaricare
  file_path <- tempfile(fileext = ".zip")
  
  # Scarico il file zip
  download.file(url, file_path)
  
  # Estraggo i file
  unzip(file_path, internal_file_paths, exdir = tempdir(), unzip = unzip)
  
  # Se mi interessa solo un file lo restituisco un data.table
  if (length(internal_file_paths) == 1) {
    return(
      fread(
        file.path(tempdir(), internal_file_paths), 
        encoding = encoding,
        colClasses = colClasses
      )
    )
  }
  
  # Se mi interessano più file restituisco una lista di data.tables
  return(
    lapply(
      internal_file_paths,
      function(internal_file_path) {
        fread(
          file.path(tempdir(), internal_file_path), 
          encoding = encoding,
          colClasses = colClasses
        )
      }
    )
  )
}

#### Codici statistici e unità territoriali ####

ISTAT <- scarica(
  "http://www.istat.it/storage/codici-unita-amministrative/Elenco-codici-statistici-e-denominazioni-delle-unit%C3%A0-territoriali.zip",
  file.path(
    "Elenco-codici-statistici-e-denominazioni-delle-unità-territoriali",
    "Codici-statistici-e-denominazioni-al-30_06_2024.csv"
  ),
  unzip = "unzip",
  encoding = "Latin-1",
  colClasses = c(`Codice Comune formato alfanumerico` = "character")
)


#### Variazioni amministrative territoriali ####

ISTAT_variazioni <- scarica(
  "https://www.istat.it/storage/codici-unita-amministrative/Variazioni%20amministrative%20e%20territoriali%20dal%201991.zip",
  file.path(
    "Variazioni amministrative e territoriali dal 1991",
    "Variazioni_amministrative_territoriali_dal_01011991.csv"
  ),
  unzip = "unzip",
  encoding = "Latin-1",
  colClasses = c(
    `Codice Comune formato alfanumerico` = "character",
    `Codice del Comune associato alla variazione o nuovo codice Istat del Comune` = "character"
  )
)


# Tengo solo le variazioni che mi interessano
ISTAT_variazioni_pulito <- ISTAT_variazioni[
  !(ISTAT_variazioni$`Tipo variazione` %in% c("AQ", "CE", "CECS")),
]

# Fix typo
ISTAT_variazioni_pulito$`Provvedimento e Documento` <- gsub(
  "novenbre",
  "novembre",
  ISTAT_variazioni_pulito$`Provvedimento e Documento`
)
ISTAT_variazioni_pulito$`Provvedimento e Documento` <- gsub(
  "maggio1992",
  "maggio 1992",
  ISTAT_variazioni_pulito$`Provvedimento e Documento`
)

# Definisci la funzione per estrarre e convertire la data
estrai_data <- function(testo) {
  # Pattern per riconoscere la data
  pattern <- "\\b\\d{1,2} [a-z]+ \\d{4}\\b"
  
  # Estrazione della data della pubblicazione in Gazzetta come stringa
  data_string_list <- str_extract_all(testo, pattern)
  data_string <- sapply(
    data_string_list, 
    function(x) {
      if (length(x) > 0) {
        return(tail(x, 1))
      } else {
        return(NA)
      }
    }
  )
  
  # Conversione della stringa in un oggetto Date
  data <- as.Date(data_string, format = "%d %B %Y")
  
  return(data)
}

# Estraggo le date
ISTAT_variazioni_pulito$DATA <- estrai_data(ISTAT_variazioni_pulito$`Provvedimento e Documento`)
ISTAT_variazioni_pulito[
  `Data decorrenza validità amministrativa` != "",
  DATA := as.Date(`Data decorrenza validità amministrativa`, format="%d/%m/%Y")
]

# Ordino la tabella
ISTAT_variazioni_pulito <- ISTAT_variazioni_pulito[order(DATA)]

# Controllo che non ci siano comuni che vengono spezzettati in altri
tabella <- ISTAT_variazioni_pulito[
  `Tipo variazione` %in% c("CS", "AQES"),
  .(.N),
  by = .(
    `Provvedimento e Documento`, 
    `Denominazione Comune associata alla variazione o nuova denominazione`
  )
]

tabella <- tabella[N > 1,]

if (nrow(tabella) > 0) warning("Alcuni comuni sono stati spezzettati")

# Controllo che non ci siano comuni che vengono spezzettati in altri
tabella <- ISTAT_variazioni_pulito[
  `Tipo variazione` %in% c("ES"),
  .(.N),
  by = .(`Provvedimento e Documento`, `Denominazione Comune`)
]

tabella <- tabella[N > 1,]

if (nrow(tabella) > 0) warning("Alcuni comuni sono stati spezzettati")


# Funzione che aggiorna i nomi dei comuni e aggiunge i codici
aggiorna_comuni <- function(DT, data_elezione, colonna_nome_comune = "COMUNE") {
  
  cat("Uniformo e aggiorno i nomi dei comuni...\n")
  
  nomi_comuni <- unique(DT[,..colonna_nome_comune][[1]])
  
  
  # Considero solo le variazioni avvenute da poco prima l'elezione a oggi
  variazioni <- ISTAT_variazioni_pulito[DATA > data_elezione - 180]
  
  tutti_i_nomi <- c(
    ISTAT$`Denominazione (Italiana e straniera)`,
    ISTAT$`Denominazione in italiano`,
    variazioni$`Denominazione Comune`
  )
  
  # Funzione che ricostruisce le variazioni nel codice e nel nome del comune
  comune_attuale <- function(indice) {
    
    # Recupero il codice del comune corrispondente al nome trovato
    codice <- variazioni$`Codice Comune formato alfanumerico`[indice]
    
    # Scorre tutte le variazioni successive alla riga trovata
    for (i in indice:nrow(variazioni)) {
      # Se la variazione non si riferisce al comune vai avanti
      if (variazioni$`Codice Comune formato alfanumerico`[i] != codice) next
      
      # Solo se la variazione comporta un cambio di codice del comune,
      # Aggiorna il codice del comune
      if (variazioni$`Tipo variazione`[i] %in% c("ES", "CD", "AP")) {
        codice <- 
          variazioni$`Codice del Comune associato alla variazione o nuovo codice Istat del Comune`[i]
      }
    }
    
    # Restituisce il codice finale e il corrispondente nome del comune
    return(list(
      comune = ISTAT$`Denominazione in italiano`[
        match(codice, ISTAT$`Codice Comune formato alfanumerico`)
      ],
      codice = codice
    ))
  }
  
  # Funzione che cerca un nome di comune scritto nello stesso modo
  cerca_nome_identico <- function(nome) {
    # Cerco il nome nei comuni attuali
    matches <- which(
      toupper(ISTAT$`Denominazione (Italiana e straniera)`) == nome
    )
    
    if (length(matches) > 0) return(list(
      comune = ISTAT$`Denominazione in italiano`[matches[1]],
      codice = ISTAT$`Codice Comune formato alfanumerico`[matches[1]]
    ))
    
    # Cerco il nome nei comuni attuali
    matches <- which(
      toupper(ISTAT$`Denominazione in italiano`) == nome
    )
    
    if (length(matches) > 0) return(list(
      comune = ISTAT$`Denominazione in italiano`[matches[1]],
      codice = ISTAT$`Codice Comune formato alfanumerico`[matches[1]]
    ))
    
    # Cerco il nome nei comuni variati,
    # se lo trovo aggiorno il nome con il nome attuale
    matches <- which(
      toupper(variazioni$`Denominazione Comune`) == nome
    )
    
    if (length(matches) > 0) return(comune_attuale(matches[1]))
    
    return(NA)
  }
  
  # Funzione che cerca il nome del comune anche scritto in modo diverso
  cerca_nome <- function(nome) {
    
    # Comincio cercando il nome così come è scritto
    risultato <- cerca_nome_identico(nome)
    
    if (length(risultato) > 1) return (risultato)
    
    # Se non lo ho trovato, converto gli apostrofi in accenti
    # e lo cerco nuovamente
    risultato <- cerca_nome_identico(
      str_replace_all(
        nome,
        c(
          "A'" = toupper("à"),
          "E'" = toupper("è"),
          "I'" = toupper("ì"),
          "O'" = toupper("ò"),
          "U'" = toupper("ù")
        )
      )
    )
    
    if(length(risultato) > 1) return (risultato)
    
    # Controlla se il nome è da cambiare manualmente
    if (nome == "MALE'") return(cerca_nome_identico(toupper("Malé")))
    if (nome == "S+N JAN DI FASSA") return(cerca_nome_identico("SAN GIOVANNI DI FASSA"))
    if (nome == "HONE") return(cerca_nome_identico(toupper("Hône")))
    
    # Se ancora non lo ho trovato,
    # cerco nomi simili tra tutti i nomi possibili
    distanze <- adist(
      tutti_i_nomi, 
      nome, 
      ignore.case = TRUE
    )
    matches <- which(distanze == min(distanze))
    
    # Se non lo trovo avviso e restituisco NA
    if (length(matches) == 0) {
      warning("Comune ", nome, " non trovato negli elenchi ISTAT")
      
      return(list(
        comune = NA,
        codice = NA
      ))
    }
    
    # Se lo trovo avviso della corrispondenza trovata
    cat(nome, "corrisponde a", tutti_i_nomi[matches[1]], "\n")
    
    # In base alla posizione dentro "tutti_i_nomi", recupero il codice
    # e il nome del comune dagli elenchi ISTAT
    if (matches[1] <= nrow(ISTAT)) return(list(
      comune = ISTAT$`Denominazione in italiano`[matches[1]],
      codice = ISTAT$`Codice Comune formato alfanumerico`[matches[1]]
    ))
    
    if (matches[1] <= nrow(ISTAT) * 2) return(list(
      comune = ISTAT$`Denominazione in italiano`[matches[1] - nrow(ISTAT)],
      codice = ISTAT$`Codice Comune formato alfanumerico`[matches[1] - nrow(ISTAT)]
    ))
    
    # Se devo andarlo a cercare nei nomi passati, aggiorno il nome
    # e il codice a quelli attuali
    return(comune_attuale(matches[1] - nrow(ISTAT) * 2))
    
  }
  
  # Creo un data.table con tutti i nomi e codici aggiornati, associati al
  # nome come è scritto nella tabella dei dati elettorali
  risultato <- rbindlist(lapply(nomi_comuni, cerca_nome))
  risultato$nome_originario <- nomi_comuni
  
  cat("\nTerminato l'aggiornamento dei nomi dei comuni\n")
  
  # Associo i nomi e i codici nuovi alla tabella dei dati elettorali
  return(merge(DT, risultato, by.x = colonna_nome_comune, by.y = "nome_originario"))
  
}

#### Inizializzo data.table dati ####

dati <- data.table(
  DATA = as.POSIXct(character(0)),
  ELEZIONE = character(0),
  COMUNE = character(0),
  CODICE_COMUNE = character(0),
  LISTA = character(0),
  VOTI = numeric(0)
)

#### Camera 2018 ####

data_camera_2018 <- "2018-03-04"

tryCatch(
  {
    cat("\nDownload dei dati delle elezioni della Camera del 2018...\n")
    camera_2018 <- scarica(
      "https://elezionistorico.interno.gov.it/daithome/documenti/opendata/camera/camera-20180304.zip",
      "Camera2018_livComune.txt",
      encoding = "Latin-1"
    )
    
    # Poiché VOTI_LISTA è NA per la circoscrizione AOSTA, copio in quella 
    # colonna i voti per il candidato (Nella circoscrizione Aosta ci sono solo 
    # candidati uninominali)
    camera_2018[
      CIRCOSCRIZIONE == "AOSTA",
      VOTI_LISTA := VOTI_CANDIDATO
    ]
    
    # Calcolo l'astensione
    astensione <- camera_2018[
      ,
      .(
        VOTI_LISTA = ELETTORI - sum(VOTI_LISTA),
        LISTA = "astensione"
      ),
      by = .(
        CIRCOSCRIZIONE,
        COLLEGIOPLURINOMINALE,
        COLLEGIOUNINOMINALE,
        COMUNE,
        ELETTORI,
        VOTANTI,
        SCHEDE_BIANCHE
      )
    ]
    camera_2018 <- rbind(camera_2018, astensione, fill = TRUE)
    
    
    # Aggiorno il nome dei comuni
    camera_2018 <- aggiorna_comuni(camera_2018, as.Date(data_camera_2018))
    
    
    
    dati <- rbind(
      dati,
      data.table(
        DATA = as.POSIXct(data_camera_2018),
        ELEZIONE = "camera 2018",
        COMUNE = camera_2018$comune,
        CODICE_COMUNE = camera_2018$codice,
        LISTA = camera_2018$LISTA,
        VOTI = camera_2018$VOTI_LISTA
      )
    )
  },
  error = function(e) warning(
    "Non sono riuscito a caricare i dati delle elezioni ",
    "della Camera del 2018, a causa di questo errore: ", e
  )
)





#### Camera 2022 ####

data_camera_2022 <- "2022-09-25"

tryCatch(
  {
    cat("\nDownload dei dati delle elezioni della Camera del 2022...\n")
    camera_2022 <- scarica(
      "https://elezionistorico.interno.gov.it/daithome/documenti/opendata/camera/camera-20220925.zip",
      "Camera_Italia_LivComune.txt"
    )
    
    
    # Calcolo l'astensione
    camera_2022 <- rbind(
      camera_2022,
      camera_2022[
        ,
        .(
          VOTILISTA = ELETTORITOT - sum(VOTILISTA),
          DESCRLISTA = "astensione"
        ),
        by = .(
          COLLUNINOM,
          COMUNE,
          ELETTORITOT
        )
      ],
      fill = TRUE
    )
    
    # Aggiorno il nome dei comuni
    camera_2022 <- aggiorna_comuni(camera_2022, as.Date(data_camera_2022))
    
    dati <- rbind(
      dati,
      data.table(
        DATA = as.POSIXct(data_camera_2022),
        ELEZIONE = "camera 2022",
        COMUNE = camera_2022$comune,
        CODICE_COMUNE = camera_2022$codice,
        LISTA = camera_2022$DESCRLISTA,
        VOTI = camera_2022$VOTILISTA
      )
    )
  },
  error = function(e) warning(
    "Non sono riuscito a caricare i dati delle elezioni ",
    "della Camera del 2022, a causa di questo errore: ", e
  )
)


#### Regionali 2020 ####

tryCatch(
  {
    cat("\nDownload dei dati delle elezioni regionali del 2020...\n")
    regionali_2020 <- scarica(
      "https://elezionistorico.interno.gov.it/daithome/documenti/opendata/regionali/regionali-20200126.zip",
      "regionali-20200126.txt"
    )
    
    
    # Calcolo l'astensione
    regionali_2020 <- rbind(
      regionali_2020,
      regionali_2020[
        ,
        .(
          VOTI_LISTA = ELETTORI - sum(VOTI_LISTA),
          LISTA = "astensione"
        ),
        by = .(
          REGIONE,
          CIRCOSCRIZIONE,
          COMUNE,
          ELETTORI
        )
      ],
      fill = TRUE
    )
    
    # Aggiorno il nome dei comuni
    regionali_2020 <- aggiorna_comuni(regionali_2020, as.Date("2020-01-26"))
    
    dati <- rbind(
      dati,
      data.table(
        DATA = as.POSIXct("2020-01-26"),
        ELEZIONE = "regionali 2020",
        COMUNE = regionali_2020$comune,
        CODICE_COMUNE = regionali_2020$codice,
        LISTA = regionali_2020$LISTA,
        VOTI = regionali_2020$VOTI_LISTA
      )
    )
  },
  error = function(e) warning(
    "Non sono riuscito a caricare i dati delle elezioni ",
    "regionali del 2020, a causa di questo errore: ", e
  )
)

#### Europee 2019 ####

tryCatch(
  {
    cat("\nDownload dei dati delle elezioni europee del 2019...\n")
    europee_2019 <- scarica(
      "https://elezionistorico.interno.gov.it/daithome/documenti/opendata/europee/europee-20190526.zip",
      "europee-20190526.txt",
      encoding = "Latin-1"
    )
    
    
    # Calcolo l'astensione
    europee_2019 <- rbind(
      europee_2019,
      europee_2019[
        ,
        .(
          VOTI_LISTA = ELETTORI - sum(VOTI_LISTA),
          LISTA = "astensione"
        ),
        by = .(
          COMUNE,
          ELETTORI
        )
      ],
      fill = TRUE
    )
    
    # Aggiorno il nome dei comuni
    europee_2019 <- aggiorna_comuni(europee_2019, as.Date("2019-05-26"))
    
    dati <- rbind(
      dati,
      data.table(
        DATA = as.POSIXct("2019-05-26"),
        ELEZIONE = "europee 2019",
        COMUNE = europee_2019$comune,
        CODICE_COMUNE = europee_2019$codice,
        LISTA = europee_2019$LISTA,
        VOTI = europee_2019$VOTI_LISTA
      )
    )
  },
  error = function(e) warning(
    "Non sono riuscito a caricare i dati delle elezioni ",
    "europee del 2019, a causa di questo errore: ", e
  )
)

#### Europee 2024 ####

tryCatch(
  {
    cat("\nDownload dei dati delle elezioni europee del 2024...\n")
    europee_2024 <- fread(
      "https://elezioni.interno.gov.it/daithome/documenti/Europee_Scrutini_ITALIA_20240609.csv",
      encoding = "Latin-1"
    )
    
    
    # Calcolo l'astensione
    europee_2024 <- rbind(
      europee_2024,
      europee_2024[
        ,
        .(
          VOTI_LISTA = ELETTORI - sum(VOTI_LISTA),
          DESCR_LISTA = "astensione"
        ),
        by = .(
          COMUNE,
          ELETTORI
        )
      ],
      fill = TRUE
    )
    
    # Aggiorno il nome dei comuni
    europee_2024 <- aggiorna_comuni(europee_2024, as.Date("2024-06-08"))
    
    dati <- rbind(
      dati,
      data.table(
        DATA = as.POSIXct("2024-06-08"),
        ELEZIONE = "europee 2024",
        COMUNE = europee_2024$comune,
        CODICE_COMUNE = europee_2024$codice,
        LISTA = europee_2024$DESCR_LISTA,
        VOTI = europee_2024$VOTI_LISTA
      )
    )
  },
  error = function(e) warning(
    "Non sono riuscito a caricare i dati delle elezioni ",
    "europee del 2024, a causa di questo errore: ", e
  )
)

# Controlla che non siano presenti codici comune sconosciuti
stopifnot(length(setdiff(dati$CODICE_COMUNE, ISTAT$`Codice Comune formato alfanumerico`)) == 0)

# Aggiungo i codici e i nomi di provincia e regione
dati <- merge(
  dati,
  ISTAT[
    ,
    .(
      CODICE_COMUNE = `Codice Comune formato alfanumerico`,
      CODICE_PROVINCIA = `Codice dell'Unità territoriale sovracomunale \n(valida a fini statistici)`,
      PROVINCIA = `Denominazione dell'Unità territoriale sovracomunale \n(valida a fini statistici)`,
      CODICE_REGIONE = `Codice Regione`,
      REGIONE = `Denominazione Regione`
    )
  ]
)

# Sommo le righe riferite allo stesso comune nella stessa elezione
# (dovute ad esempio a comuni che si sono fusi insieme successivamente)
dati <- dati[
  ,
  .(VOTI = sum(VOTI)),
  by = .(
    DATA,
    ELEZIONE,
    CODICE_REGIONE,
    REGIONE,
    CODICE_PROVINCIA,
    PROVINCIA,
    CODICE_COMUNE,
    COMUNE,
    LISTA
  )
]

# Tiene solo i dati dell'Emilia-Romagna
dati <- dati[CODICE_REGIONE == 8]

# Salvo il file
save(dati, file = "dati/dati.RData")

