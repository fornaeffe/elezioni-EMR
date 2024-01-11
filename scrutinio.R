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
  province,
  liste
) {
  
  # load("dati_per_scrutinio.RData")
  
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
  
  liste <- merge(
    liste,
    aggregate(VOTI_LISTA_ITER ~ LISTA, data = prov_lista, sum)
  )
  
  voti_validi <- sum(liste$VOTI_LISTA_ITER)
  
  liste$PERCENTUALE_DEI_VOTI_VALIDI <- liste$VOTI_LISTA_ITER / voti_validi
  
  liste$SOGLIA_3 <- liste$PERCENTUALE_DEI_VOTI_VALIDI >= 0.03
  
  coalizioni <- aggregate(
    VOTI_LISTA_ITER ~ COALIZIONE,
    liste,
    sum
  )
  
  coalizioni$PERCENTUALE_DEI_VOTI_VALIDI <- 
    coalizioni$VOTI_LISTA_ITER / voti_validi
  
  coalizioni$SOGLIA_COALIZIONE <- coalizioni$PERCENTUALE_DEI_VOTI_VALIDI >= 0.05
  
  liste <- merge(
    liste,
    coalizioni[, c(
      "COALIZIONE",
      "SOGLIA_COALIZIONE"
    )]
  )
  
  liste$SOGLIA <- liste$SOGLIA_3 | liste$SOGLIA_COALIZIONE
  
  liste_ammesse <- liste[
    liste$SOGLIA,
    c(
      "COALIZIONE",
      "LISTA",
      "VOTI_LISTA_ITER"
    )
  ]
  
  prov_lista_ammesse <- prov_lista[
    prov_lista$LISTA %in% liste_ammesse$LISTA,
    c(
      "LISTA",
      "PROVINCIA",
      "VOTI_LISTA_ITER"
    )
  ]
  
  # Art. 12
  # Operazioni degli uffici centrali circoscrizionali
  

  # 3. Compiute le suddette operazioni, l'ufficio centrale circoscrizionale:

  # a) somma i voti validi, compresi quelli assegnati ai sensi del comma 1,
  # lettera b), ottenuti da ciascun candidato alla carica di Presidente della
  # Giunta regionale nelle singole sezioni della circoscrizione;

  # b) determina la cifra elettorale circoscrizionale di ciascuna lista
  # circoscrizionale. La cifra elettorale circoscrizionale di ogni lista
  # circoscrizionale è data dalla somma dei voti di lista validi, compresi
  # quelli assegnati ai sensi del comma 1, lettera b), ottenuti da ciascuna
  # lista nelle singole sezioni della circoscrizione;

  # c) procede al riparto dei seggi tra le liste in base alla cifra elettorale
  # di ciascuna lista. A tal fine divide il totale delle cifre elettorali di
  # tutte le liste per il numero dei seggi assegnati alla circoscrizione più
  # uno, ottenendo così il quoziente elettorale circoscrizionale;
  # nell'effettuare la divisione trascura la eventuale parte frazionaria del
  # quoziente.
  
  
  
  province <- merge(
    province,
    aggregate(
      VOTI_LISTA_ITER ~
        PROVINCIA,
      prov_lista_ammesse,
      sum
    )
  )
  
  province$QUOZIENTE_1 <- 
    floor(province$VOTI_LISTA_ITER / (province$seggi_proporzionali + 1))
  
  # Attribuisce quindi ad ogni lista tanti seggi quante volte il
  # quoziente elettorale risulti contenuto nella cifra elettorale di ciascuna
  # lista.
  
  prov_lista_ammesse <- merge(
    prov_lista_ammesse,
    province[, c(
      "PROVINCIA",
      "QUOZIENTE_1"
    )]
  )
  
  prov_lista_ammesse$SEGGI_1 <- 0
  
  prov_lista_ammesse$SEGGI_1 <- 
    floor(prov_lista_ammesse$VOTI_LISTA_ITER / prov_lista_ammesse$QUOZIENTE_1)
  
  # Se, con il quoziente così calcolato, il numero dei seggi da
  # attribuire in complesso alle liste superi quello dei seggi assegnati alla
  # circoscrizione, le operazioni si ripetono con un nuovo quoziente ottenuto
  # diminuendo di una unità il divisore. 
  
  province <- merge(
    province,
    aggregate(
      SEGGI_1 ~ PROVINCIA,
      prov_lista_ammesse,
      sum
    )
  )
  
  province$QUOZIENTE_2 <-
    floor(province$VOTI_LISTA_ITER / province$seggi_proporzionali)
  
  prov_lista_ammesse <- merge(
    prov_lista_ammesse,
    province[, c(
      "PROVINCIA",
      "QUOZIENTE_2"
    )]
  )
  
  prov_lista_ammesse$SEGGI_2 <- 0
  
  prov_lista_ammesse$SEGGI_2 <- 
    floor(prov_lista_ammesse$VOTI_LISTA_ITER / prov_lista_ammesse$QUOZIENTE_2)
  
  province$USA_2 <- province$SEGGI_1 > province$seggi_proporzionali
  
  prov_lista_ammesse <- merge(
    prov_lista_ammesse,
    province[, c(
      "PROVINCIA",
      "USA_2"
    )]
  )
  
  prov_lista_ammesse$SEGGI_CIRC <- ifelse(
    prov_lista_ammesse$USA_2,
    prov_lista_ammesse$SEGGI_2,
    prov_lista_ammesse$SEGGI_1
  )
  
  # I seggi che rimangono non assegnati
  # vengono attribuiti al collegio unico regionale;
  
  province <- merge(
    province,
    aggregate(
      SEGGI_CIRC ~ PROVINCIA,
      prov_lista_ammesse,
      sum
    )
  )
  
  seggi_non_assegnati <- 
    sum(province$seggi_proporzionali) - sum(province$SEGGI_CIRC)
  
  # d) stabilisce la somma dei voti residuati di ogni lista e il numero dei
  # seggi non potuti attribuire ad alcuna lista per insufficienza di quozienti o
  # di candidati. La determinazione della somma dei voti residuati deve essere
  # fatta anche nel caso che tutti i seggi assegnati alla circoscrizione vengano
  # attribuiti. Si considerano voti residuati anche quelli delle liste che non
  # abbiano raggiunto alcun quoziente ed i voti che, pur raggiungendo il
  # quoziente, rimangano inefficienti per mancanza di candidati;
  
  prov_lista_ammesse$VOTI_RESIDUATI <- 
    prov_lista_ammesse$VOTI_LISTA_ITER -
    ifelse(
      prov_lista_ammesse$USA_2,
      prov_lista_ammesse$QUOZIENTE_2 * prov_lista_ammesse$SEGGI_2,
      prov_lista_ammesse$QUOZIENTE_1 * prov_lista_ammesse$SEGGI_1
    )
  
  # Art.13
  # Operazioni dell'ufficio centrale regionale
  
  # 1. L'ufficio centrale regionale, ricevuti gli estratti dei verbali da tutti
  # gli uffici centrali circoscrizionali:
  
  # a) determina il numero dei seggi non attribuiti nelle circoscrizioni;
  
  # b) determina, per ciascuna lista, il numero dei voti residuati.
  # Successivamente procede alla somma dei predetti voti per tutte le liste
  # aventi lo stesso contrassegno;
  
  liste_ammesse <- merge(
    liste_ammesse,
    aggregate(
      VOTI_RESIDUATI ~ LISTA,
      prov_lista_ammesse,
      sum
    )
  )
  
  # c) procede alla assegnazione ai predetti gruppi di liste dei seggi indicati
  # alla lettera a). A tal fine divide la somma dei voti residuati di tutti i
  # gruppi di liste per il numero dei seggi da attribuire; nell'effettuare la
  # divisione, trascura la eventuale parte frazionaria del quoziente. Il
  # risultato costituisce il quoziente elettorale regionale. Divide, poi, la
  # somma dei voti residuati di ogni gruppo di liste per tale quoziente: il
  # risultato rappresenta il numero dei seggi da assegnare a ciascun gruppo. I
  # seggi che rimangono ancora da attribuire sono rispettivamente assegnati ai
  # gruppi per i quali queste ultime divisioni hanno dato maggiori resti e, in
  # caso di parità di resti, a quei gruppi che abbiano avuto maggiori voti
  # residuati. A parità anche di questi ultimi si procede a sorteggio.
  
  sr <- Hare.Niemeyer(liste_ammesse$VOTI_RESIDUATI, seggi_non_assegnati, TRUE)
  liste_ammesse$SEGGI_DA_VOTI_RESIDUATI <- sr$assigned
  liste_ammesse$RESTI_VOTI_RESIDUATI <- sr$remainders
  liste_ammesse$SEGGI_DA_RESTI_VOTI_RESIDUATI <- sr$remainders.seats
  
  # I seggi
  # spettanti a ciascun gruppo di liste vengono attribuiti alle rispettive liste
  # nelle singole circoscrizioni seguendo la graduatoria decrescente dei voti
  # residuati espressi in percentuale del relativo quoziente circoscrizionale. A
  # tal fine si moltiplica per cento il numero dei voti residuati di ciascuna
  # lista e si divide il prodotto per il quoziente circoscrizionale. Qualora in
  # una circoscrizione fosse assegnato un seggio ad una lista i cui candidati
  # fossero già stati tutti esauriti, l'ufficio centrale regionale attribuisce
  # il seggio alla lista di un'altra circoscrizione proseguendo nella
  # graduatoria anzidetta.
  
  # 2. L'ufficio centrale regionale procede al riparto della restante quota di
  # seggi. A tal fine effettua le seguenti operazioni:
  
  # a) proclama eletto alla carica di Presidente della Giunta regionale il
  # candidato Presidente che nella Regione ha ottenuto il maggior numero di voti
  # validi sommando i voti ottenuti da ciascun candidato alla carica di
  # Presidente della Giunta regionale nelle singole circoscrizioni di cui
  # all'articolo 12, comma 3, lettera a). Individua, altresì, il candidato alla
  # carica di Presidente che ha ottenuto il totale dei voti validi
  # immediatamente inferiore al candidato proclamato eletto, ai fini della
  # riserva di un seggio da effettuare con le modalità di cui al comma 3;
  
  coalizioni$CLASSIFICA <- rank(
    - coalizioni$VOTI_LISTA_ITER, 
    ties.method = "random"
  )
  liste_ammesse <- merge(
    liste_ammesse,
    coalizioni[, c(
      "COALIZIONE",
      "CLASSIFICA"
    )]
  )
  
  # b) determina la cifra elettorale regionale di ciascun gruppo di liste
  # circoscrizionali, sommando le cifre elettorali circoscrizionali attribuite
  # alle liste circoscrizionali di ogni gruppo ai sensi dell'articolo 12, comma
  # 3, lettera b);
  coalizioni <- merge(
    coalizioni
  )
  
  # c) determina la cifra elettorale regionale attribuita alla coalizione di
  # liste ovvero al gruppo di liste non riunito in coalizione con cui il
  # Presidente della Giunta regionale eletto ha dichiarato collegamento sommando
  # le cifre elettorali circoscrizionali attribuite alle singole liste
  # circoscrizionali che ne fanno parte;
  
  # TODO continua
  
  
}