library(shiny)
library(shinydashboard)
library(tidyverse)
library(lubridate)
library(patchwork)
library(xtable)
library(knitr)
theme_set(theme_light())

# Carregando a base de dados ----
dados <- readRDS("dados6.rds")

# Somente COVID ----
dados <- dados %>%
  filter(CLASSI_FIN == "5")

#criando variável chamada variante(original,gama,delta,omicron)
in_gama <- as.Date("01-02-2021", format = "%d-%m-%Y")
in_delta <- as.Date("01-08-2021", format = "%d-%m-%Y")
in_omicron <- as.Date("01-01-2022", format = "%d-%m-%Y")

dados <- dados %>%
  mutate(
    variante = case_when(
      dt_sint < in_gama ~ "original",
      dt_sint >= in_gama &
        dt_sint < in_delta ~ "gama",
      dt_sint >= in_delta &
        dt_sint < in_omicron ~ "delta",
      dt_sint >= in_omicron ~ "omicron"
    )
  ) %>%
  mutate(month_year = paste(
    formatC(
      month(dt_sint),
      width = 2,
      format = "d",
      flag = "0"
    ),
    year(dt_sint),
    sep = "/"
  )) %>%
  mutate(mes = month(dt_sint))


dados <- dados %>% 
  rename(vacina_cov1=vacina_cov) %>% 
  mutate(vacina_cov = case_when(
    variante == "original" ~ "não",
    vacina_cov1 == "sim" ~ "sim",
    vacina_cov1 == "não" 
    ~ "não",
    TRUE ~ NA_character_
  ))

dados <- dados %>%
  mutate(vacina_cov = ifelse(variante == "original", "não", vacina_cov))

dados$variante <- factor(dados$variante,
                         levels = c("original", "gama", "delta", "omicron"))

dados <- dados %>%
  mutate(dt_1dose = as.Date(DOSE_1_COV, format = "%d/%m/%Y")) %>%
  mutate(dt_2dose = as.Date(DOSE_2_COV, format = "%d/%m/%Y")) %>%
  mutate(
    doses = case_when(
      vacina_cov == "sim" & is.na(dt_1dose)
      &
        is.na(dt_2dose) ~ "pelo menos uma dose",
      !is.na(dt_2dose) ~ "duas doses",
      !is.na(dt_1dose) &
        is.na(dt_2dose) ~ "pelo menos uma dose",
      TRUE ~ "não informado"
    )
  )

# Alterações iniciais na base ----


dados$suport_ven <- factor(dados$suport_ven,
                           levels = c("não", "não invasivo", "invasivo"))



dados$CLASSI_FIN <- as.factor(dados$CLASSI_FIN)

dados$DT_SIN_PRI <- dmy(dados$DT_SIN_PRI)
dados$DT_EVOLUCA <- dmy(dados$DT_EVOLUCA)

dados <- dados %>%
  mutate(variante2 = as.numeric(
    case_when(
      variante == "original" ~ "1",
      variante == "gama" ~ "2",
      variante == "delta" ~ "3",
      variante == "omicron" ~ "4",
      TRUE ~ NA_character_
    )
  ))

dados <- dados %>%
  mutate(vacina_cov2 = as.numeric(
    case_when(
      vacina_cov == "sim"  ~ "1",
      vacina_cov == "não"  ~ "0",
      TRUE ~ NA_character_
    )
  ))

dados$vacinacov_variante <-
  as.factor(dados$vacina_cov2 * dados$variante2)

