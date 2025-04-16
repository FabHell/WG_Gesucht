


################################################################################
###                                                                          ###
###                          Einzelnen Link Scrapen                          ###
###                                                                          ###
################################################################################




Einzellink <- "https://www.wg-gesucht.de/wg-zimmer-in-Hamburg-Wilhelmsburg.11964734.html"




## Ggf. alten Link entfernen ---------------------------------------------------

Analysedaten_gesamt <- read_csv("Hamburg/Daten/Analysedaten/Analysedaten.csv",
                                show_col_types = FALSE)

Analysedaten_gesamt <- Analysedaten_gesamt %>%
  filter(Link != Einzellink)

write.csv(Analysedaten_gesamt, "Hamburg/Daten/Analysedaten/Analysedaten.csv", 
          row.names = FALSE)


Rohdaten_gesamt <- read_csv("Hamburg/Daten/Rohdaten/Rohdaten.csv",
                            show_col_types = FALSE)

Rohdaten_gesamt <- Rohdaten_gesamt %>%
  filter(Link != Einzellink)

write.csv(Rohdaten_gesamt, "Hamburg/Daten/Rohdaten/Rohdaten.csv", row.names = FALSE)



# Liste mit gescrapten Einzellinks 
#   (ursprüngliche Links sind noch in Backups enthalten)
#
#
#
#
#


## Scraping des Links ----------------------------------------------------------


Datensatz_Rohdaten <- tibble()

WG_Angebot <- read_html(Einzellink)



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

Datum_Scraping = Sys.Date()


Rohdaten_neu <- tibble(Link, Titel, WG_Konstellation, Zimmergröße_Gesamtmiete, 
                       Adresse, Datum, WG_Details, Kostenfeld, Angaben_zum_Objekt, 
                       Freitext_Zimmer, Freitext_Lage, Freitext_WG_Leben,
                       Freitext_Sonstiges, Datum_Scraping)



## In Rohdaten speichern -------------------------------------------------------

Rohdaten_gesamt <- read_csv("Hamburg/Daten/Rohdaten/Rohdaten.csv",
                             show_col_types = FALSE) %>%
  rbind(Rohdaten_neu) 

write.csv(Rohdaten_gesamt, "Hamburg/Daten/Rohdaten/Rohdaten.csv", 
          row.names = FALSE)



## Datenbereinigung ausführen und in Analysedaten speichern --------------------

source("Hamburg/Skripte/Datenscraping/Aufbereitungsskript Hamburg.R")

