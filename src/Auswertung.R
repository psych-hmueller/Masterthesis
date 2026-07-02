#Arbeitsverzeichnis zu aktuellem Ordner ändern
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#Packages laden
#install.packages("car")
library(car)
#install.packages("emmeans")
library(emmeans)
#install.packages("dplyr")
library(dplyr)
#install.packages("ggplot2")
library(ggplot2)


#Daten laden
data <- read.csv("prop_correct.csv")
head(data)
str(data)

##Vergleich mean fitting vs logarithmisches fitting für die passive Bedingung

#mittlere prop_corr pro Person in der passiven Bedingung
passive_agg <- data %>%
  filter(stim == 2) %>%
  group_by(subject,contrast_cond) %>%
  summarise(mean_prop = mean(proportion_correct), .groups = "drop")

#subjects den Gruppen zuteilen
passive_agg$gruppe <- ifelse(passive_agg$subject %in% c(5:14),
                             "Mittelwert", "Logarithmisch")

#NV prüfen
#Gruppe Mittelwert - Offset
shapiro.test(passive_agg$mean_prop[passive_agg$gruppe == "Mittelwert" & 
                                     passive_agg$contrast_cond == 1])
#Gruppe Mittelwert - Onset
shapiro.test(passive_agg$mean_prop[passive_agg$gruppe == "Mittelwert" & 
                                     passive_agg$contrast_cond == 2])
#Gruppe logarithmisch - Offset
shapiro.test(passive_agg$mean_prop[passive_agg$gruppe == "Logarithmisch" & 
                                     passive_agg$contrast_cond == 1])
#Gruppe logarithmisch - Onset
shapiro.test(passive_agg$mean_prop[passive_agg$gruppe == "Logarithmisch" & 
                                     passive_agg$contrast_cond == 2])

#auch Überprüfung mittels QQ-Plot
par(mar = c(2,2,2,2))
par(mfrow = c(2,2))

qqnorm(passive_agg$mean_prop[passive_agg$gruppe == "Mittelwert" &
                               passive_agg$contrast_cond == 1],
       main = "Mean curve fitting - Offset")
qqline(passive_agg$mean_prop[passive_agg$gruppe == "Mittelwert" &
                               passive_agg$contrast_cond == 1])

qqnorm(passive_agg$mean_prop[passive_agg$gruppe == "Mittelwert" &
                               passive_agg$contrast_cond == 2],
       main = "Mean curve fitting - Onset")
qqline(passive_agg$mean_prop[passive_agg$gruppe == "Mittelwert" &
                               passive_agg$contrast_cond == 2])

qqnorm(passive_agg$mean_prop[passive_agg$gruppe == "Logarithmisch" &
                               passive_agg$contrast_cond == 1],
       main = "Logarithmic curve fitting - Offset")
qqline(passive_agg$mean_prop[passive_agg$gruppe == "Logarithmisch" &
                               passive_agg$contrast_cond == 1])

qqnorm(passive_agg$mean_prop[passive_agg$gruppe == "Logarithmisch" &
                               passive_agg$contrast_cond == 2],
       main = "Logarithmic curve fitting - Onset")
qqline(passive_agg$mean_prop[passive_agg$gruppe == "Logarithmisch" &
                               passive_agg$contrast_cond == 2])

#für weitere Plots Aufteilung wieder zurück 
par(mfrow = c(1,1))

#da Normalverteilungsannahme in min. 2 Gruppen verletzt ist: Man-Whitney-U-Test mit Hodges-Lehmann-Schätzer (positiv = alphabetisch erste Gruppe hatte größere Werte)
p_offset <- wilcox.test(mean_prop ~ gruppe,
             data = filter(passive_agg, contrast_cond == 1),
             conf.int = TRUE)$p.value
p_onset <- wilcox.test(mean_prop ~ gruppe,
            data = filter(passive_agg, contrast_cond == 2),
            conf.int = TRUE)$p.value
#Holm-Korrektur
p.adjust(c(p_offset, p_onset), method = "holm")

#zusätzlich zu Hodges-Lehmann-Schätzer: Mediane der Gruppen
passive_agg %>%
  group_by(gruppe, contrast_cond) %>%
  summarise(median_prop = median(mean_prop),
            mean_prop = mean (mean_prop),
            .groups = "drop")

##Zum Vergleich: mean curve fitting vs logarithmic curve fitting in der aktiven Bedingung
#mittlere prop_corr pro Person in der aktiven Bedingung
active_agg <- data %>%
  filter(stim == 1) %>%
  group_by(subject,contrast_cond) %>%
  summarise(mean_prop = mean(proportion_correct), .groups = "drop")

#subjects den Gruppen zuteilen
active_agg$gruppe <- ifelse(active_agg$subject %in% c(5:14),
                             "Mittelwert", "Logarithmisch")

#NV prüfen
#Gruppe Mittelwert - Offset
shapiro.test(active_agg$mean_prop[active_agg$gruppe == "Mittelwert" & 
                                    active_agg$contrast_cond == 1])
#Gruppe Mittelwert - Onset
shapiro.test(active_agg$mean_prop[active_agg$gruppe == "Mittelwert" & 
                                     active_agg$contrast_cond == 2])
#Gruppe logarithmisch - Offset
shapiro.test(active_agg$mean_prop[active_agg$gruppe == "Logarithmisch" & 
                                    active_agg$contrast_cond == 1])
#Gruppe logarithmisch - Onset
shapiro.test(active_agg$mean_prop[active_agg$gruppe == "Logarithmisch" & 
                                     active_agg$contrast_cond == 2])

#auch Überprüfung mittels QQ-Plot
par(mar = c(2,2,2,2))
par(mfrow = c(2,2))

qqnorm(active_agg$mean_prop[active_agg$gruppe == "Mittelwert" &
                               active_agg$contrast_cond == 1],
       main = "Mean curve fitting - Offset")
qqline(active_agg$mean_prop[active_agg$gruppe == "Mittelwert" &
                               active_agg$contrast_cond == 1])

qqnorm(active_agg$mean_prop[active_agg$gruppe == "Mittelwert" &
                               active_agg$contrast_cond == 2],
       main = "Mean curve fitting - Onset")
qqline(active_agg$mean_prop[active_agg$gruppe == "Mittelwert" &
                               active_agg$contrast_cond == 2])

qqnorm(active_agg$mean_prop[active_agg$gruppe == "Logarithmisch" &
                               active_agg$contrast_cond == 1],
       main = "Logarithmic curve fitting - Offset")
qqline(active_agg$mean_prop[active_agg$gruppe == "Logarithmisch" &
                               active_agg$contrast_cond == 1])

qqnorm(active_agg$mean_prop[active_agg$gruppe == "Logarithmisch" &
                               active_agg$contrast_cond == 2],
       main = "Logarithmic curve fitting - Onset")
qqline(active_agg$mean_prop[active_agg$gruppe == "Logarithmisch" &
                               active_agg$contrast_cond == 2])

#für weitere Plots Aufteilung wieder zurück 
par(mfrow = c(1,1))

#da Normalverteilungsannahme in min. 2 Gruppen verletzt ist: Man-Whitney-U-Test
p_offset <- wilcox.test(mean_prop ~ gruppe,
                        data = filter(active_agg, contrast_cond == 1),
                        conf.int = TRUE)$p.value
p_onset <- wilcox.test(mean_prop ~ gruppe,
                       data = filter(active_agg, contrast_cond == 2),
                       conf.int = TRUE)$p.value
#Holm-Korrektur
p.adjust(c(p_offset, p_onset), method = "holm")
#-> UNterschied wird in der aktiven Bedingung nicht sig. 
#--> Unterschied in der passiven Bedingung kann nicht auf individuelle Unterschiede
#zwischen den beiden Personengruppen zurückgeführt werden


#ab hier Vorbereitung glm ohne Gruppentrennung 

#Change asychrony zentrieren, um später Interaktionen berechnen zu können
delay_mean <- mean(data$delay_2)
data$delay_2 <- data$delay_2 - mean(data$delay_2)


#Effekt-kodieren des change type
data$contrast_cond <- as.factor (data$contrast_cond)
levels(data$contrast_cond)
contrasts(data$contrast_cond) <-  cbind(c(-1, 1))#Onset = 1; Offset = -1
contrasts(data$contrast_cond)

#Effekt-kodieren der Bedingung 
data$stim <- as.factor (data$stim)
levels(data$stim)
contrasts(data$stim) <- 
  cbind("Active_vs_Mean"   = c(1, 0, -1), # Kontrast 1
        "Passive_vs_Mean" = c(0, 1, -1))    # Kontrast 2
contrasts(data$stim)

#Modelle aufstellen und vergleichen
model1 <- glm(proportion_correct ~ delay_2 * contrast_cond * stim,
             family = binomial(link="logit"), 
             data = data,
             weights = n_trials) 
summary(model1)

model2 <- glm(proportion_correct ~ delay_2 + contrast_cond + stim + delay_2*contrast_cond + delay_2*stim + contrast_cond*stim,
              family = binomial(link="logit"), 
              data = data,
              weights = n_trials) 
summary(model2)

#Überdispersion prüfen
deviance(model1) / df.residual(model1)
deviance(model2) / df.residual(model2)

#Modelle aufgrund der Überdispersion mit Quasibinomial-Modell

model3 <- glm(proportion_correct ~ delay_2 * contrast_cond * stim,
              family = quasibinomial(link = "logit"), 
              data = data,
              weights = n_trials) 
summary(model3)

model4 <- glm(proportion_correct ~ delay_2 + contrast_cond + stim + delay_2*contrast_cond + delay_2*stim + contrast_cond*stim,
              family = quasibinomial(link="logit"), 
              data = data,
              weights = n_trials) 
summary(model4)

#Modellvergleich
anova(model3, model4, test = "F")
#->das komplexere Model3 ist sig. besser, deshalb ab jetzt weiter hiermit

#Residualannahmen überprüfen
plot(model3, which = 1)

#main effects 
summary(model3)

#Post-Hoc Vergleiche:

#Offset vs Onset über alles andere hinweg
emmeans(model3, pairwise ~ contrast_cond, type = "response", adjust = "holm")

#Unterschied zwischen Onset und Offset INNERHALB der exp. Bedingungen
emmeans(model3, pairwise ~contrast_cond | stim, type = "response", adjust = "holm") 

#Vergleich Differenz zwischen Onset und Offset zwischen Bedingungen auf Whk.Skala
delta <- emmeans(model3, ~ contrast_cond * stim, type = "response", adjust = "holm")
contrast(delta, interaction = c("pairwise", "pairwise"))

#Eplorativ: Unterscheiden sich auch NUR die Offsets sig. zwischen den Bedingungen 
#--> Liegt sig. Differenz daran, dass Untersch. zwischen Offsets so groß ist?
offset <- emmeans(model3, ~stim, at = list(contrast_cond = "1"), type ="response", adjust = "holm")
pairs(offset, adjust = "holm")

#das selbe für Onset
onset <- emmeans(model3, ~stim, at = list(contrast_cond = "2"), type ="response", adjust = "holm")
pairs(onset, adjust = "holm")

#ab hier Graphen

#neues Dataframe mit den Daten auf denen vorhersagen basieren sollen
predict_df <- expand.grid(
  delay_2 = unique(data$delay_2),
  contrast_cond = factor(c("1", "2")),
  stim = factor(c(1, 2, 3))
)

#Vorhergesagte Wahrscheinlichkeiten dem dataframe hinzufügen
pred <- predict(model3, newdata = predict_df, type = "link", se.fit = TRUE)

#Konfidenz-Intervalle auf Log-Odds-Skala
predict_df$fit <- pred$fit
predict_df$se <- pred$se.fit
predict_df$lwr <- pred$fit - 1.96 * pred$se.fit
predict_df$upr <- pred$fit + 1.96 * pred$se.fit

#auf Whk.-Skala zurückrechnen
logistic <- function(x) exp(x) / (1 + exp (x))
predict_df$prob <- logistic(predict_df$fit)
predict_df$lwr <- logistic(predict_df$lwr)
predict_df$upr <- logistic(predict_df$upr)

#ursprüngliche Skala für delay_2 hinzufügen
predict_df$delay_original <- predict_df$delay_2 + delay_mean

#Labels für Graphen dem Dataframe hinzufügen
predict_df$contrast_label <- factor(predict_df$contrast_cond,
                                levels = c("1", "2"),
                                labels = c("Offset", "Onset"))
predict_df$stim_label <- factor(predict_df$stim,
                                    levels = c(1, 2, 3),
                                    labels = c("Active", "Passive", "Static"))

#Plot: vorhergesagte prop_corr getrennt für jede Bedingung für Onset und Offset
plot_1 <- ggplot(predict_df, aes(x = delay_original, y = prob, color = contrast_label)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = lwr, ymax = upr),
                width = 3, linewidth = 0.5)+
  scale_color_manual(values = c("Offset" = "hotpink", "Onset" = "orange")) +
  coord_cartesian(ylim = c(NA, 1.01))+
  facet_wrap(~ stim_label) +
  labs(x = "Change Asynchrony (ms)",
       y = "Proportion Correct",
       color = "Change type") +
  theme_minimal() +
  theme(legend.position = "bottom",
        strip.text = element_text (size = 14),
        axis.title = element_text(size = 12),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 12))

#ggsave("OnsetOffset_proBedingung.png", width = 600, height = 400, units = "mm")

#ab hier für Plot_2 der Differenzen von Onset und Offset

#Paket laden
#install.packages("tidyr")
library(tidyr)


#Dataframe in wide-Format bringen -> nach contrast_label
predict_wide_con <- predict_df %>%
  select(delay_original, stim_label, contrast_label, prob, lwr, upr, se) %>%
  pivot_wider(names_from = contrast_label,
              values_from = c(prob, lwr, upr, se))

#Differenz und Konfidenzintervalle berechnen 
predict_wide_con$diff <- predict_wide_con$prob_Offset - predict_wide_con$prob_Onset
predict_wide_con$diff_se <- sqrt(predict_wide_con$se_Offset^2 + predict_wide_con$se_Onset^2)
predict_wide_con$diff_lwr <- predict_wide_con$diff - 1.96 * predict_wide_con$diff_se
predict_wide_con$diff_upr <- predict_wide_con$diff + 1.96 * predict_wide_con$diff_se

#Plot2: Differenz Offset-Onset in prop_corr getrennt für jede Bedingung 
plot_2 <- ggplot(predict_wide_con, aes(x = delay_original, y = diff, color = stim_label)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = diff_lwr, ymax = diff_upr),
                width = 3, linewidth = 0.5)+
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50")+
 scale_color_manual(values = c("Active" = "seagreen2",
                                "Passive" = "dodgerblue1",
                                "Static" = "red2")) +
  labs(x = "Change Asynchrony (ms)",
       y = "Proportion Correct (Offset-Onset)",
       color = "Condition") +
  theme_minimal() +
  theme(legend.position = "bottom",
        strip.text = element_text (size = 14),
        axis.title = element_text(size = 12),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 12))

#ggsave("DifferenzOffsetOnset_proBedingung.png", width = 12, height = 8)

#ab hier für Plot_3 der Differenzen der Bedingungen getrennt für Offset und Onset

#Dataframe in wide-Format bringen -> nach stim_label
predict_wide_stim <- predict_df %>%
  select(delay_original, stim_label, contrast_label, prob, lwr, upr, se) %>%
  pivot_wider(names_from = stim_label,
              values_from = c(prob, lwr, upr, se))

#Differenzen berechnen
predict_wide_stim$diff_ActPass <- predict_wide_stim$prob_Active - predict_wide_stim$prob_Passive
predict_wide_stim$diff_ActStat <- predict_wide_stim$prob_Active - predict_wide_stim$prob_Static
predict_wide_stim$diff_PassStat <- predict_wide_stim$prob_Passive - predict_wide_stim$prob_Static

#KI der Differenzen berechnen
predict_wide_stim$se_ActPass <- sqrt(predict_wide_stim$se_Active^2 + predict_wide_stim$se_Passive^2)
predict_wide_stim$se_ActStat <- sqrt(predict_wide_stim$se_Active^2 + predict_wide_stim$se_Static^2)
predict_wide_stim$se_PassStat <- sqrt(predict_wide_stim$se_Passive^2 + predict_wide_stim$se_Static^2)

predict_wide_stim$lwr_ActPass <- predict_wide_stim$diff_ActPass - 1.96 * predict_wide_stim$se_ActPass
predict_wide_stim$upr_ActPass <- predict_wide_stim$diff_ActPass + 1.96 * predict_wide_stim$se_ActPass

predict_wide_stim$lwr_ActStat <- predict_wide_stim$diff_ActStat - 1.96 * predict_wide_stim$se_ActStat
predict_wide_stim$upr_ActStat <- predict_wide_stim$diff_ActStat + 1.96 * predict_wide_stim$se_ActStat

predict_wide_stim$lwr_PassStat <- predict_wide_stim$diff_PassStat - 1.96 * predict_wide_stim$se_PassStat
predict_wide_stim$upr_PassStat <- predict_wide_stim$diff_PassStat + 1.96 * predict_wide_stim$se_PassStat

#Für ggplot in Longformat überführen
predict_long_stim <- predict_wide_stim %>%
  select(delay_original, contrast_label,
         diff_ActPass, lwr_ActPass, upr_ActPass,
         diff_ActStat, lwr_ActStat, upr_ActStat,
         diff_PassStat, lwr_PassStat, upr_PassStat) %>%
  pivot_longer(cols = -c(delay_original, contrast_label),
               names_to = c(".value", "comparison"),
               names_pattern = "(diff|lwr|upr)_(.*)")

#Labels für Subplots
predict_long_stim$comparison <- factor(predict_long_stim$comparison,
                                       levels = c("ActPass", "ActStat", "PassStat"),
                                       labels = c("Active - Passive",
                                                  "Active - Static",
                                                  "Passive - Static"))

#Plot3: Differenzen der Bedingungen für Onset und Offset getrennt
plot_3 <- ggplot(predict_long_stim, aes(x = delay_original, y = diff, color = contrast_label)) +
  geom_line(linewidth = 1) +
  geom_point(data = predict_long_stim, size =2) +
  geom_errorbar(data = predict_long_stim, 
                aes(ymin = lwr, ymax = upr),
                width = 3, linewidth = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  scale_color_manual(values = c ("Offset" = "hotpink", "Onset" = "orange")) +
  facet_wrap(~ comparison) +
  labs(x = "Change Asynchrony (ms)",
       y = "Proportion Correct (Difference of Conditions)",
       color = "Change type") +
  theme_minimal() +
  theme(legend.position = "bottom",
        strip.text = element_text (size = 14),
        axis.title = element_text(size = 12),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 12))

#ggsave("DifferenzBedingungen_proOffsetOnset.png", width = 12, height = 8)






