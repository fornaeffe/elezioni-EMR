library(shiny)
library(bslib)
library(editbl)
library(tibble)
library(data.table)

# Define UI ----
ui <- fluidPage(
  titlePanel("Simulatore delle elezioni regionali dell'Emilia-Romagna"),
  
  # TODO: inserimento sondaggi
  
  # Pulsante salva scenario e carica scenario
  
  # Tabella di aggiunta delle liste
  eDTOutput('liste')
  
  # Tabella di corrispondenza liste attuali e precedenti
  
  # Impostazioni simulazione
  
  # Pulsante simula
  
  # Grafici output
  
  # Pulsante salva report
)

# Define server logic ----
server <- function(input, output) {
  
  # Se non sono ancora stati scaricati dati, li scarica
  # TODO: verificare se serve un indicatore di avanzamento
  # TODO: aggiungere un pulsante che forza l'aggiornamento dei dati
  if (!file.exists("dati/dati.RData")) {
    
    # Creo un nuovo ambiente in modo da non riempire il server di oggetti
    # inutili
    myEnv <- new.env()
    
    source("caricamento-dati.R", local = myEnv)
  }
  
  # Carica i dati
  load("dati/dati.RData")
  
  # Tiene solo i dati dell'Emilia-Romagna
  setkey(dati, CODICE_REGIONE, DATA, ELEZIONE, LISTA)
  dati <- dati[.(8)]
  
  # Elenca le liste nelle passate elezioni
  liste_passate <- unique(dati[, c("DATA", "ELEZIONE", "LISTA")])
  
  # Prepara una tibble con l'elenco delle liste alle prossime elezioni
  # TODO: verifica se esistono già degli scenari salvati
  liste <- tibble(
    LISTA = "astensione",
    COALIZIONE = as.character(NA),
    COLORE = as.character(NA)
  )
  
  modifiedData <- eDT(
    id = "liste", 
    data = liste,
    options = list(
      paging = FALSE, 
      dom = "Brt",
      buttons = list("add", "undo", "redo")
    )
  )
  
  observe({
    liste <- modifiedData$state()
  })
}

# Run the app ----
shinyApp(ui = ui, server = server)