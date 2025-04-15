


################################################################################
################################################################################
#####                                                                      #####
#####                    SCRAPING VON WG-GESUCHT Hamburg                   #####
#####                                                                      #####
################################################################################
################################################################################


library(tidyverse)
library(rvest)






################################################################################
#####                                                                      #####
#####                        VORBEREITUNG DES LOOPS                        #####
#####                                                                      #####
################################################################################


## Link WG-Gesucht Hamburg

Link_Stadt <- "https://www.wg-gesucht.de/wg-zimmer-in-Hamburg.55.0.1."



## Vektor für Selektionslinks nicht älter als 60 Tage erstellen

Selektionslinks <- read_csv("Hamburg/Daten/Rohdaten/Rohdaten.csv", 
                            col_select = c("Link", "Datum_Scraping"),
                            show_col_types = FALSE) %>%
  
  bind_rows() %>%
  filter(Datum_Scraping > Sys.Date() - 60) %>%
  select(-Datum_Scraping) %>%
  distinct() %>%
  pull()



## Leeren Datensatz erstellen

Datensatz_Rohdaten <- tibble()




## Funktion für die einzelnen Variablen der Subdaten schreiben


Fun_Subdata = function(Link_Subdata) {
  
  WG_Angebot <- read_html(Link_Subdata)
  
  
  Titel <- WG_Angebot %>%
    html_node("h1.headline.headline-detailed-view-title span:last-child") %>%
    html_text(trim = TRUE)
  
  WG_Konstellation <- WG_Angebot %>%
    html_node("span.mr5") %>%
    html_attr("title")
  
  Zimmergröße_Gesamtmiete <- WG_Angebot %>%
    html_nodes("b.key_fact_value") %>%
    html_text(trim = TRUE) %>%
    paste(collapse = "|")
  
  Adresse <- WG_Angebot %>%
    html_node(".col-sm-6 .col-xs-12 .section_panel_detail") %>%
    html_text(trim = TRUE)
  
  Datum <- WG_Angebot %>%
    html_node(".col-sm-6+ .col-sm-6:nth-child(2)") %>%
    html_text(trim = TRUE)
  
  WG_Details <- WG_Angebot %>%
    html_nodes(".pl15 .section_panel_detail") %>%
    html_text(trim = TRUE) %>%
    paste(collapse = "|")
  
  Kostenfeld <- WG_Angebot %>%
    html_nodes(".row:nth-child(6) .section_panel") %>%
    html_text(trim = TRUE) %>%
    paste(collapse = "|")
  
  Angaben_zum_Objekt <- WG_Angebot %>%
    html_nodes(".utility_icons") %>%
    html_text(trim = TRUE) %>%
    paste(collapse = "|")
  
  Freitext_Zimmer <- WG_Angebot %>%
    html_nodes("#freitext_0 p") %>%
    html_text(trim = TRUE) %>%
    paste(collapse = "|")
  
  Freitext_Lage <- WG_Angebot %>%
    html_nodes("#freitext_1 p") %>%
    html_text(trim = TRUE) %>%
    paste(collapse = "|")
  
  Freitext_WG_Leben <- WG_Angebot %>%
    html_nodes("#freitext_2 p") %>%
    html_text(trim = TRUE) %>%
    paste(collapse = "|")
  
  Freitext_Sonstiges <- WG_Angebot %>%
    html_nodes("#freitext_3 p") %>%
    html_text(trim = TRUE) %>%
    paste(collapse = "|")
  
  return(c(Titel, WG_Konstellation, Zimmergröße_Gesamtmiete, Adresse, Datum,
           WG_Details, Kostenfeld, Angaben_zum_Objekt, Freitext_Zimmer,
           Freitext_Lage, Freitext_WG_Leben, Freitext_Sonstiges))
}





################################################################################
#####                                                                      #####
#####                             Scrapingloop                             #####
#####                                                                      #####
################################################################################


## Scrapinglog mit Ergebnissen speichern

sink(
  paste0("Hamburg/Logs/Scrapinglog ", format(Sys.time(), "%Y.%m.%d {%H-%M}"), ".txt"),
  split = TRUE
)


print(paste0("--- Scrapinglog ", format(Sys.time(), "%Y.%m.%d {%H-%M}"), " ---"))
print("")

message(" ")
message("---------- BEGINNE SCRAPING ------------")
message(" ")


for(Seite in seq(0, 4, 1)) {
  
  
  # Verzögerung der Seitenabfrage einbauen:
  
  Sys.sleep(3)
  
  message(paste0("------------ Starte Loop ",Seite + 1, " -------------"))
  
  # Zusammensetzung der Links bestimmen und als html auslesen
  
  print(paste0("Erstellen Makrolink | Seite: ", Seite + 1))
  
  link = paste0(Link_Stadt,Seite,".html")
  Url = read_html(link)
  
  
  
  
  # Linksammlung Subdaten und aussortieren bereits gescrapter Links
  
  print(paste0("Linksammlung erstellen | Seite: ", Seite + 1))
  
  Sublinks <- Url %>%
    html_nodes(".offer_list_item .truncate_title a") %>%
    html_attr("href") %>%
    paste0("https://www.wg-gesucht.de", .) %>%
    setdiff(Selektionslinks)
  
  
  
  
  ## Prüfen, ob neue Links generiert wurden
  
  if (length(Sublinks) > 0) {
    
    print(paste0("Neue Links scrapen | Seite: ", Seite + 1))
    
    WG_Subdaten <- sapply(Sublinks, Fun_Subdata)
    
    
    Datensatz_Rohdaten <- rbind(Datensatz_Rohdaten, 
                                tibble(Link = as.vector(Sublinks),
                                       Titel = WG_Subdaten[1,], 
                                       WG_Konstellation = WG_Subdaten[2,],
                                       Zimmergröße_Gesamtmiete = WG_Subdaten[3,], 
                                       Adresse = WG_Subdaten[4,], 
                                       Datum = WG_Subdaten[5,], 
                                       WG_Details = WG_Subdaten[6,],
                                       Kostenfeld = WG_Subdaten[7,],
                                       Angaben_zum_Objekt = WG_Subdaten[8,],
                                       Freitext_Zimmer = WG_Subdaten[9,],
                                       Freitext_Lage = WG_Subdaten[10,],
                                       Freitext_WG_Leben = WG_Subdaten[11,],
                                       Freitext_Sonstiges = WG_Subdaten[12,],
                                       Datum_Scraping = Sys.Date()))
    
    
    print({
      zeile <- replace(WG_Subdaten[1, ], is.na(WG_Subdaten[1, ]), "")
      
      if (all(zeile == "")) {
        paste0("S. ", Seite + 1, " | Kein Scraping")
        
      } else if (any(zeile == "")) {
        paste0("S. ", Seite + 1, " | Scraping teilweise erfolgreich: Erfolgreich ", 
               sum(zeile != ""), " / Fehlgeschlagen ", sum(zeile == ""))
        
      } else {
        paste0("S. ", Seite + 1, " | Scraping erfolgreich: ", length(zeile), 
               " Link(s) gescraped.")
      }
    })
    
    
  } else {
    print(paste0("Für Seite ", Seite + 1, " keine neuen Sublinks"))
  }
  
  
  print(" ")
  
}  


sink()




################################################################################
#####                                                                      #####
#####                          Rohaten speichern                           #####
#####                                                                      #####
################################################################################


## Nicht vollständig gescrapte Fälle entfernen

Datensatz_final <- Datensatz_Rohdaten[1:length(as.vector(na.omit(Datensatz_Rohdaten$Titel))),]


## Neue Daten mit altem Datensatz verbinden 

Rohdaten_neu <- read_csv("Hamburg/Daten/Rohdaten/Rohdaten.csv",
                         show_col_types = FALSE) %>%
  rbind(Datensatz_final) # %>%
#  distinct(Link, .keep_all = TRUE)


# if (length(Selektionslinks) + nrow(Datensatz_final) != nrow(Rohdaten_neu)) {
#   beepr::beep(9)
#   stop("Eigener Abbruch wegen fehlerhafter Dateien")
# }


## Alten Datensatz mit neuem Überschreiben 

write.csv(Rohdaten_neu, "Hamburg/Daten/Rohdaten/Rohdaten.csv", row.names = FALSE)



################################################################################
#####                                                                      #####
#####                          Datenaufbereitung                           #####
#####                                                                      #####
################################################################################


message("---------- DATENAUFBEREITUNG -----------")


# Aufbereitungsskripte durchlaufen lassen

source("Hamburg/Skripte/Datenscraping/Aufbereitungskript neue_Rohdaten Hamburg.R")

source("Hamburg/Skripte/Datenscraping/Aufbereitungsskript Hamburg.R")


message(" ")

beepr::beep(4)

