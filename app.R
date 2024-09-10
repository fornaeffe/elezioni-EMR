library(shiny)
library(bslib)
library(editbl)
library(tibble)

# Define UI ----
ui <- fluidPage(
  titlePanel("Simulatore delle elezioni regionali dell'Emilia-Romagna"),
  
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
  liste <- tibble(
    LISTA = "astensione",
    COALIZIONE = as.character(NA),
    COLORE = as.character(NA)
  )
  
  modifiedData <- eDT(
    id = "liste", 
    data = liste
  )
  
  observe({
    liste <- modifiedData$result()
    print(liste)
  })
}

# Run the app ----
shinyApp(ui = ui, server = server)