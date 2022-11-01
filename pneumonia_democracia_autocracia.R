
# Pneumonia em países capitalistas e comunistas --------------------------------------------------------------------------------------------
# Autoria do script: Jeanne Franco ---------------------------------------------------------------------------------------------------------
# Data: 21/10/22 ---------------------------------------------------------------------------------------------------------------------------
# Referência: https://ourworldindata.org/pneumonia -----------------------------------------------------------------------------------------

# Sobre os dados ---------------------------------------------------------------------------------------------------------------------------

### Em 2019, houve 2,5 milhões de pessoas que morreram de peneumonia. Quase um terço das
### vítimas foram crianças com menos de 5 anos, essa é a principal causa de morte para
### crianças de até 5 anos.

### Pneumonia é uma infecção nos pequenos sacos aéreos do pulmão, chamados de alvéolos.
### Em uma pessoa com pneumonia, os alvéolos são preenchidos com pus e fluidos, o que torna
### a respiração dolorosa e reduz a ingestão de oxigênio. Pneumonia é causada por um diferente
### número de agentes infecciosos, incluindo vírus, bactérias e fungos. 

### Neste registro nós observamos quem está sofrendo de pneumonia e porquê - e o que nós
### podemos fazer para reduzir o número de pessoas morrendo dessa doença.

# Carregar pacotes -------------------------------------------------------------------------------------------------------------------------

library(tidyverse)
library(cols4all)
library(hrbrthemes)
library(ggthemes)

# Carregar dados ---------------------------------------------------------------------------------------------------------------------------

pne <- read.csv("pneumonia-death-rates-age-standardized.csv")
view(pne)
names(pne)

# Manipular dados --------------------------------------------------------------------------------------------------------------------------

pne <- pne %>%
  select(-Code) %>%
  rename(taxa_mortes_pneu = Deaths...Lower.respiratory.infections...Sex..Both...Age..Age.standardized..Rate.) %>%
  view()

pne1 <- pne %>%
  filter(Entity %in% c("United States", "Japan", "Germany",
                       "China", "Cuba", "North Korea")) %>%
  group_by(Entity) %>%
  summarise(media = mean(taxa_mortes_pneu),
            sd = sd(taxa_mortes_pneu), n = n(),
            se = sd/sqrt(n)) %>%
  view()

pne2 <- pne %>%
  filter(Entity %in% c("United States", "Japan", "Germany",
                       "China", "Cuba", "North Korea")) %>%
  view()

pne3 <- pne %>%
  filter(Entity %in% c("United States", "China")) %>%
  view()

pne4 <- pne %>%
  filter(Entity %in% c("United States", "China", "Brazil")) %>%
  view()

# Gráficos ---------------------------------------------------------------------------------------------------------------------------------

c4a("safe", 6)

ggplot(pne1, aes(x = fct_reorder(Entity, media), 
                 y = media, fill = Entity)) +
  geom_col(width = 0.9) +
  geom_errorbar(aes(ymin = media - se, ymax = media + se),
                size = 0.8, width = 0.3) +
  scale_fill_manual(values = c("#88CCEE", "#CC6677",
                             "#DDCC77", "#117733",
                             "#332288", "#AA4499")) +
  scale_y_continuous(expand = expansion(mult = c(0,0))) +
  scale_x_discrete(labels = c("Alemanha", "Estados Unidos", "Japão",
                              "China", "Cuba", "Coreia do Norte")) +
  labs(x = "Países", y = "Taxa de mortes por pneumonia (%)") +
  theme_ipsum(axis_text_size = 14, axis_title_size = 16) +
  theme(legend.position = "none",
        axis.text = element_text(color = "black"))

ggplot(pne2, aes(x = Year, y = taxa_mortes_pneu,
                 group = Entity, col = Entity)) +
  geom_point(shape = 15, size = 2.5) +
  geom_line(size = 1.2) +
  scale_color_manual(values = c("#88CCEE", "#CC6677",
                             "#DDCC77", "#117733",
                             "#332288", "#AA4499"),
                     labels = c("China", "Cuba", "Alemanha",
                                "Japão", "Coreia do Norte", "Estados Unidos")) +
  labs(x = "Tempo (anos)", y = "Taxa de mortes por pneumonia (%)",
       color = "Países") +
  theme_ipsum(axis_text_size = 14, axis_title_size = 16) +
  theme(axis.text = element_text(color = "black"))

c4a("dark2", 2)

ggplot(pne3, aes(x = Year, y = taxa_mortes_pneu,
                 group = Entity, col = Entity)) +
  geom_line(size = 2.2) +
  scale_color_manual(values = c("#1B9E77", "#D95F02"),
                     labels = c("China", "Estados Unidos")) +
  labs(x = "Tempo (anos)", y = "Taxa de mortes por pneumonia (%)",
       color = "Países") +
  theme_hc() +
  theme(axis.title = element_text(size = 18),
        axis.text = element_text(color = "black", size = 15),
        legend.text = element_text(size = 12))

ggplot(pne4, aes(x = Year, y = taxa_mortes_pneu,
                 group = Entity, col = Entity)) +
  geom_line(size = 2) +
  scale_color_manual(values = c('#1B9E77', '#999999','#E69F00'), 
                     labels = c("Brasil", "China", "Estados Unidos")) +
  labs(x = "Tempo (anos)", y = "Taxa de mortes por pneumonia",
       color = "Países") +
  theme_light() +
  theme(axis.title = element_text(size = 18),
        axis.text = element_text(color = "black", size = 15),
        legend.text = element_text(size = 12))
