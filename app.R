library(shiny)
library(bslib)
library(editbl)

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
  liste <- data.frame(
    LISTA = "astensione",
    COALIZIONE = NA,
    COLORE = NA
  )
  
  modifiedData <- eDT(id = "liste", data = liste)
  
  observe({
    print(modifiedData$result())
  })
}

# Run the app ----
shinyApp(ui = ui, server = server)