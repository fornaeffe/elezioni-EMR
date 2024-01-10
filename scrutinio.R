Hare.Niemeyer <- function(votes, seats, details = FALSE) {
  if (seats > 0) {
    q <- sum(votes) / seats
    quotients <- votes %/% q
    still.to.assign <- seats - sum(quotients)
    remainders <- votes %% q
    remainders.order <- order(remainders, votes, runif(length(votes)), decreasing = TRUE)
    remainders.seats <- rep(0, length(votes))
    remainders.seats[remainders.order[1:still.to.assign]] <- 1
    assigned <- quotients + remainders.seats 
  } else {
    assigned <- rep(0, length(votes))
    remainders <- votes
    remainders.seats <- rep(0, length(votes))
  }
  
  if (details) {
    return( data.frame(assigned = assigned, remainders = remainders, remainders.seats = remainders.seats ))
  } else return(assigned)
}


Scrutinio <- function(
  prov_area,
  province
) {
  
  prov_lista <- prov_area
  prov_lista$LISTA <- prov_lista$AREA
  prov_lista <- prov_lista[prov_lista$LISTA != "astensione", ]
  
  # Art. 3
  # Individuazione dei seggi e delle circoscrizioni provinciali
  
  # 1. Quaranta dei consiglieri assegnati all'Assemblea legislativa sono eletti
  # con criterio proporzionale sulla base di liste circoscrizionali concorrenti ai
  # sensi delle disposizioni di cui all'articolo 12, comma 3, e articolo 13, comma
  # 1, mediante riparto nelle singole circoscrizioni e recupero dei voti residui
  # nel collegio unico regionale. Nove dei consiglieri assegnati alla Regione sono
  # eletti con sistema maggioritario nell'ambito dei candidati concorrenti nelle
  # liste circoscrizionali in base ai voti conseguiti dalle coalizioni di liste o
  # gruppi di liste collegati ai candidati alla carica di Presidente della Giunta
  # regionale ai sensi dell'articolo 13, comma 2, lettere da b) a f). Un seggio è
  # riservato al candidato alla carica di Presidente della Giunta regionale che ha
  # conseguito un numero di voti validi immediatamente inferiore a quello del
  # candidato proclamato eletto Presidente ai sensi dell'articolo 13, comma 3.
  
  # 2. Le circoscrizioni elettorali coincidono con i territori delle province
  # emiliano-romagnole di cui all'articolo 1, comma 2, dello Statuto regionale. La
  # ripartizione dei seggi tra le circoscrizioni è effettuata dividendo il numero
  # degli abitanti della regione per i quaranta seggi di cui al primo comma del
  # presente articolo e assegnando i seggi in proporzione alla popolazione di ogni
  # circoscrizione sulla base dei quozienti interi e dei più alti resti. La
  # popolazione è determinata in base ai risultati dell'ultimo censimento generale
  # della stessa, riportati dalla più recente pubblicazione ufficiale
  # dell'Istituto nazionale di statistica.
  
  
  
  
  province$seggi_proporzionali <- Hare.Niemeyer(province$POP_2011, 40)
  
  
  
  # Art. 11
  # Soglie di sbarramento
  
  # 1. Non sono ammesse all'assegnazione dei seggi le liste circoscrizionali il
  # cui gruppo abbia ottenuto, nell'intera regione, meno del tre per cento dei
  # voti validi, se non collegato ad un candidato Presidente che ha ottenuto
  # almeno il cinque per cento dei voti nella relativa elezione.
  
  # TODO continua
  
  
}