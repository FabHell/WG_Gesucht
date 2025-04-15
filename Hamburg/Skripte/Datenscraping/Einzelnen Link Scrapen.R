


################################################################################
###                                                                          ###
###                          Einzelnen Link Scrapen                          ###
###                                                                          ###
################################################################################


Link <- " "


## Ggf. alten Link entfernen ---------------------------------------------------

# Rohdaten_neu <- read_csv("Hamburg/Daten/Rohdaten/Rohdaten.csv", 
#                          show_col_types = FALSE) 
# 
# Rohdaten_neu <- Rohdaten_neu %>%
#   filter(!(Link == Link))
# 
# write.csv(Rohdaten_neu, "Hamburg/Daten/Rohdaten/Rohdaten.csv", row.names = FALSE)




## Scraping des Links ----------------------------------------------------------


Datensatz_Rohdaten <- tibble()

WG_Angebot <- read_html(Link)



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


Datensatz_final <- tibble(Link, Titel, WG_Konstellation, Zimmergröße_Gesamtmiete, 
                          Adresse, Datum, WG_Details, Kostenfeld, Angaben_zum_Objekt, 
                          Freitext_Zimmer, Freitext_Lage, Freitext_WG_Leben,
                          Freitext_Sonstiges, Datum_Scraping)


Rohdaten_neu <- read_csv("Hamburg/Daten/Rohdaten/Rohdaten.csv",
                         show_col_types = FALSE) %>%
  rbind(Datensatz_final)

write.csv(Rohdaten_neu, "Hamburg/Daten/Rohdaten/Rohdaten.csv", row.names = FALSE)
