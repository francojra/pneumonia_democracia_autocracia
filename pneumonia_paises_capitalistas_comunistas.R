
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

# Gráficos ---------------------------------------------------------------------------------------------------------------------------------


