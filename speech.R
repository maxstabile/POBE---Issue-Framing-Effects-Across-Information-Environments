# -- CARREGANDO PACOTES E ABRINDO A BASE DE DADOS -----------------------------------


# Lista de pacotes necessários
pacotes <- c("tidyverse", "quanteda", "rainette", "readr", "dplyr", "ggplot2", 
             "scales", "gridExtra", "lubridate", "ggthemes", "stringi")

# Função para verificar e instalar pacotes
instalar_pacotes <- function(pacotes) {
  pacotes_ausentes <- pacotes[!(pacotes %in% installed.packages()[, "Package"])]
  if(length(pacotes_ausentes)) install.packages(pacotes_ausentes)
}

# Chamar a função
instalar_pacotes(pacotes)

# Carregar os pacotes
lapply(pacotes, library, character.only = TRUE)


set.seed(100)



speech <- read_delim("data/discursos/data.csv", 
                   delim = ";", escape_double = FALSE, col_types = cols(Data = col_date(format = "%d/%m/%Y")), 
                   trim_ws = TRUE)

# Carregar as bibliotecas necessárias
#library(dplyr)
#library(tidyr)
#library(lubridate)

all_months <- tibble(mes_ano = seq(from = as.Date("2018-01-01"), 
                                   to = floor_date(Sys.Date(), "month"), 
                                   by = "month"))


############### Pegando Wave das datas

library(dplyr)
library(lubridate)

# Converter a coluna de datas para formato Date
speech <- speech %>%
  mutate(data = as.Date(data, format="%d/%m/%Y"))

speech <- subset(speech, select = -wave)

speech <- speech |>
  mutate(
    wave = case_when(
      Data >= "2018-10-29" & Data <= "2018-12-07" ~ 1,
      Data >= "2018-12-14" & Data <= "2019-01-22" ~ 2,
      Data >= "2019-02-16" & Data <= "2019-03-27" ~ 3,
      Data >= "2019-10-31" & Data <= "2019-12-09" ~ 4,
      Data >= "2022-02-11" & Data <= "2022-03-23" ~ 5,
      TRUE ~ NA
    )
  )


# Filtrar os dados a partir de 2018 e contar os termos agrupados por wave
speech_consolidated_Wave <- speech %>%
  filter(ano >= 2018) %>%
  group_by(wave) %>%
  summarise(
    pandemia_count = sum(grepl("pandemia", termo, ignore.case = TRUE), na.rm = TRUE),
    reforma_previdencia_count = sum(grepl("reforma da previdencia", termo, ignore.case = TRUE), na.rm = TRUE),
    .groups = "drop"
  )



# Exibir resultado
print(speech_consolidated_Wave)
