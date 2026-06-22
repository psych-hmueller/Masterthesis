#Arbeitsverzeichnis zu aktuellem Ordner ändern
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#Daten laden
data_all <- read.csv("allData.csv")
head(data_all)
str(data_all)

#Package laden
#install.packages("dplyr")
library(dplyr)

#Datensatz um Durchgänge mit ungültigen Amplituden (sowohl für Sakkaden, als auch Fixationen) und Latenzen bereinigen
data_clean <- data_all %>%
  filter(
    !(exp %in% c(31,32) & (sacc_amp < 7 | sacc_amp > 13)) &
    !(exp %in% c(31, 32) & (sacc_lat < 100 | sacc_lat > 500)) &
    !(exp %in% c(33, 34, 35, 36) & (fix_error == 1))
  )
head(data_clean)
str(data_clean)

##Übersicht über die ausgeschlossenen Daten
#Datensatz mit Ausschlüssen als Spalten erstellen
data_exclusions <- data_all %>%
  mutate(excl_sacc_amp = exp %in% c(31,32) & (sacc_amp < 7 | sacc_amp > 13),
        excl_sacc_lat = exp %in% c(31, 32) & (sacc_lat < 100 | sacc_lat > 500),
        excl_fix_error = exp %in% c(33, 34, 35, 36) & (fix_error == 1),
        excluded = excl_sacc_amp | excl_sacc_lat | excl_fix_error
  )

#Wie viele Durchgänge pro Experiment ausgeschlossen:
excl_gesamt <- data_exclusions %>%
  group_by(exp) %>%
  summarise(
    gesamt = n(),
    ausgeschlossen = sum(excluded, na.rm = TRUE),
    behalten = sum(!excluded, na.rm = TRUE)
  )
print(excl_gesamt)

#Aufgeschlüsselt nach Ausschlussregel pro Experiment -> Achtung: Durchgänge die gegen mehrere Regeln verstoßen, werden hier auch mehrmals gezählt
excl_nach_regel <- data_exclusions %>%
  group_by(exp) %>%
  summarise(
    excl_sacc_amp = sum(excl_sacc_amp, na.rm=TRUE),
    excl_sacc_lat = sum(excl_sacc_lat, na.rm = TRUE),
    excl_fix_error = sum(excl_fix_error, na.rm = TRUE)
  )
print(excl_nach_regel)

##proportion correct berechnen
proportion_correct <- data_clean %>%
  group_by(subject, exp, delay_2, contrast_cond, stim) %>%
  summarise(
    proportion_correct = sum(sc_hit) / n(),
    n_trials = n(),
    .groups = "drop"
  )
head(proportion_correct)
str(proportion_correct)

#neuen Datensatz speichern
write.csv(proportion_correct, "prop_correct.csv", row.names = FALSE)
