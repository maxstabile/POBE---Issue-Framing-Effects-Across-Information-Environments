
# # --------------------------------------------------------------------------------------
#  Issue Framing Effects Across Information Environments
# # --------------------------------------------------------------------------------------
#  Out Files: Figure 1 and Figure 2
# # --------------------------------------------------------------------------------------


# -- CARREGANDO PACOTES E ABRINDO A BASE DE DADOS -----------------------------------

rm(list=ls()); gc(); 
# Lista de pacotes necessários
pacotes <- c("tidyverse", "quanteda", "rainette", "readr", "dplyr", "ggplot2", 
             "scales", "gridExtra", "lubridate", "ggthemes", "stringi")


instalar_pacotes <- function(pacotes) {
  pacotes_ausentes <- pacotes[!(pacotes %in% installed.packages()[, "Package"])]
  if(length(pacotes_ausentes)) install.packages(pacotes_ausentes)
}

instalar_pacotes(pacotes)
lapply(pacotes, library, character.only = TRUE)


set.seed(100)


# Abertura da base de dados
fssr_noticias_lemmatizado <- read_delim(
  "data/press/fssr_noticias_lemmatizado.csv",
  delim = ";",
  escape_double = FALSE,
  locale = locale(encoding = "WINDOWS-1252"),
  trim_ws = TRUE
)

#fazendo a limpeza de notícias quem nao tenham conteúdo
fssr_noticias_lemmatizado <- fssr_noticias_lemmatizado[!is.na(fssr_noticias_lemmatizado$texto_lemmatizado_limpo), ]
fssr_noticias_lemmatizado<- fssr_noticias_lemmatizado %>% dplyr::filter(texto_lemmatizado_limpo != "nan")
fssr_noticias_lemmatizado$veiculo <- gsub("Folha de S. Paulo", "Folha de S.Paulo", fssr_noticias_lemmatizado$veiculo)


# --- DESCRITIVOS INICIAIS -------------------------


#NÚMERO TOTAL DE NOTÍCIAS

nrow(fssr_noticias_lemmatizado)


# DESCRITIVO DOS VEÍCULOS
prop.table(table(fssr_noticias_lemmatizado$veiculo))



#CRIANIDO A MÉDIA
news_by_date <- fssr_noticias_lemmatizado %>% 
  filter(wave > 0) %>%
  select(data, wave) %>%
  group_by(data, wave) %>%
  summarize(qtde = n()) %>%
  ungroup()

# news_by_date$wave <- case_when(
#   news_by_date$wave == 1 ~ "Wave 1",
#   news_by_date$wave == 2 ~ "Wave 2",
#   news_by_date$wave == 3 ~ "Wave 3",
#   news_by_date$wave == 4 ~ "Wave 4",
#   news_by_date$wave == 5 ~ "Wave 5",
#   TRUE ~ as.character(news_by_date$wave)
# )


# Criando as médias por período (wave) para plotar no gráfico
dMean <- news_by_date %>%
  group_by(wave) %>%
  summarise(MN = mean(qtde))

# Garantindo que 'data' está no formato Date
news_by_date$data <- as.Date(news_by_date$data, format = "%d/%m/%Y")


table(dMean)

# Ajustando o gráfico para melhorar a visualização do eixo X

news_by_date$waveDescription <- news_by_date$wave
news_by_date$waveDescription <- case_when(
  news_by_date$waveDescription == 1 ~ "Post-election \n (Survey 1)",
  news_by_date$waveDescription == 2 ~ "Inauguration \n (Survey 2)", 
  news_by_date$waveDescription == 3 ~ "Bill introduction \n (Survey 3)",
  news_by_date$waveDescription == 4 ~ "Bill approval \n (Survey 4)",
  news_by_date$waveDescription == 5 ~ "Post-reform cycle \n (Survey 5)", 
  TRUE ~ as.character(news_by_date$waveDescription)
)

news_by_date$waveDescription <-factor(news_by_date$waveDescription, levels=c("Post-election \n (Survey 1)", "Inauguration \n (Survey 2)", 
                                     "Bill introduction \n (Survey 3)", "Bill approval \n (Survey 4)", "Post-reform cycle \n (Survey 5)"), ordered = T)

table(news_by_date$waveDescription)

dSummary <- news_by_date %>%
  group_by(waveDescription) %>%
  summarise(MN = mean(qtde, na.rm = TRUE),
            min_date = min(data),
            max_date = max(data),
            max_qtde = max(qtde, na.rm = TRUE))  # Para definir escala do Y


png(filename="images/fig1.png", width = 1200, height = 300)

news_by_date %>%
  ggplot(aes(x = data, y = qtde)) +
  geom_line(color = "black", linewidth = 0.8) + # Aumenta a espessura da linha
  xlab("Days") + 
  ylab("Number of News Articles") + 
  
  # Ajuste eixo X: apenas a data mínima e máxima com rótulo, marcação diária sem rótulo
  scale_x_date(
    breaks = unique(c(dSummary$min_date, dSummary$max_date)),  # Apenas min e max têm rótulos
    minor_breaks = seq(min(news_by_date$data), max(news_by_date$data), by = "1 day"),  # Marcações diárias
    labels = date_format("%d/%b/%y"),
    expand = expansion(mult = c(0.02, 0.08))  # Dá mais espaço para evitar corte no último rótulo
  ) +
  
  # Ajuste eixo Y: marcações de 10 em 10
  scale_y_continuous(
    breaks = seq(0, max(dSummary$max_qtde, na.rm = TRUE), by = 10)
  ) +
  
  theme_light() +  # Mantém o quadrado ao redor dos gráficos
  theme(
    axis.text.x = element_text(angle = 90, size = 14, hjust = 0.2),  # Move os rótulos levemente para dentro
    axis.text.y = element_text(size = 8),  # Mantém eixo Y legível
    axis.ticks.x = element_line(),  # Mantém ticks do eixo X
    axis.ticks.y = element_line(),  # Mantém ticks do eixo Y
    panel.grid.major.x = element_blank(),  # Remove grades maiores no X
    panel.grid.minor.x = element_blank(),  # Remove grades menores no X
    strip.background = element_rect(fill = "gray30"),  # Fundo dos títulos dos facets
    strip.text = element_text(color = "white", size = 16, face = "bold")
  ) +
  geom_hline(data = dSummary, aes(yintercept = MN), linetype = "dashed", color = "gray", linewidth = 1) +
  facet_wrap(~ waveDescription, scales = "free_x", nrow = 1, ncol = 5, shrink = TRUE)  # Ajusta espaçamento entre facets

dev.off()


### MEANS TEST OF THE QTDE OF NEWS ACROSS WAVES.
dfcontent1<-subset(dfcontent, wave=="Post-election \n (Survey 1)")
dfcontent2<-subset(dfcontent, wave=="Presidential Inauguration \n (Survey 2)")
dfcontent3<-subset(dfcontent, wave=="SSR Bill introduction \n (Survey 3)")
dfcontent4<-subset(dfcontent, wave=="SSR Bill approval \n (Survey 4)")

t.test(dfcontent1$qtde, dfcontent2$qtde) # SIG
t.test(dfcontent1$qtde, dfcontent3$qtde) # SIG
t.test(dfcontent1$qtde, dfcontent4$qtde) # N.S. 0.864
t.test(dfcontent2$qtde, dfcontent3$qtde)# SIG
t.test(dfcontent2$qtde, dfcontent4$qtde) # SIG
t.test(dfcontent3$qtde, dfcontent4$qtde) # SIG

summary(aov(qtde ~ wave, data = dfcontent)) # SIG




#####################################################

# -----ANÁLISE DE CONTEÚDO -------------

#####################################################

#código para rodar a análise hierarquica para cada wave.
#ATENÇÃO, COMO HÁ UM COMPONENTE ALEATÓRIO, SALVEI A LISTA AGRUPADA DOS RESULTADOS PARA SEMPRE ABRIR A MESMA

# {
# waves <- seq_len(5)
# 
# lista <- list()
# 
#   for (x in waves){
#     
#     print(paste0("Wave ", x))
#     
#     wave_name <- paste0("Wave_", x)
#     
#     df <- fssr_noticias_lemmatizado |> 
#       filter(wave == x)
#     
#     corpus <- corpus(df, text_field = "texto_lemmatizado_limpo")
#     
#     if (x != 5) {
#       
#       corpus_split <- split_segments(corpus, segment_size = 40)
#       dtm <- dfm(tokens(corpus_split))
#       
#       dtm <- dfm_trim(dtm, min_docfreq = 50)
#       res1 <- rainette(dtm, k = 10, min_segment_size = 10, min_split_members = 50)
#     }
#     
#     if (x == 5){
#       corpus_split <- split_segments(corpus, segment_size = 10)
#       dtm <- dfm(tokens(corpus_split))
#       
#       dtm <- dfm_trim(dtm, min_docfreq = 2)
#       res1 <- rainette(dtm, k = 10, min_segment_size = 10, min_split_members = 100)
#       
#     } 
#     
#     
#     lista[[wave_name]]$dtm <- dtm
#     lista[[wave_name]]$res1 <- res1
#     lista[[wave_name]]$corpus_split <- corpus_split
#     
#     lista[[wave_name]]$show <- show
#     
#     if (x == 4){
#       
#       cluster_assignments <- cutree(res1, k = 5)
#       
#     } else {
#       
#       cluster_assignments <- cutree(res1, k = 4)
#       
#     }
#     
#     cluster_counts <- table(cluster_assignments)
#     
#     lista[[wave_name]]$cluster_counts <- cluster_counts
#     
#     print(paste0("Contagem de casos por cluster para Wave ", x, ":"))
#     print(cluster_counts)
#     
#   }
# }
# 
# 
#
#saveRDS(lista, "data/press/rainette.rds")


lista <- readRDS("data/press/rainette.rds")


# WAVE 1 - FAZENDO A ANÁLISE DE CADA RODADA.

#rainette_explor(
#  res = lista[["Wave_1"]]$res1,
#  dtm = lista[["Wave_1"]]$dtm,
#  corpus_src = lista[["Wave_1"]]$corpus_split
#) 

#CRIANDO UMA LISTA
cluster_categories <- list()

#fazendo a designação manual dos clusters
cluster_categories[["Wave_1"]] <- c("SSR Rhetoric", "Macroeconomic", "Goverment Deliberation", "Political Debate")


###
# WAVE 2 
###

#rainette_explor(
#  res = lista[["Wave_2"]]$res1,
#  dtm = lista[["Wave_2"]]$dtm,
#  corpus_src = lista[["Wave_2"]]$corpus_split
#)


cluster_categories[["Wave_2"]] <- c("SSR Rhetoric", "Macroeconomic", "Political Debate", "Goverment Deliberation")



###
# WAVE 3 
###

#rainette_explor(
#  res = lista[["Wave_3"]]$res1,
#  dtm = lista[["Wave_3"]]$dtm,
#  corpus_src = lista[["Wave_3"]]$corpus_split
#)

cluster_categories[["Wave_3"]] <- c("Macroeconomic", "SSR Rhetoric", "Political Debate", "Legislative Process")



###
# WAVE 4 
###


#rainette_explor(
#  res = lista[["Wave_4"]]$res1,
#  dtm = lista[["Wave_4"]]$dtm,
#  corpus_src = lista[["Wave_4"]]$corpus_split
#)

cluster_categories[["Wave_4"]] <- c("NA", "Macroeconomic", "State & Municipal Reform", "Legislative Process", "Political Debate")



###
# WAVE 5 
###


#rainette_explor(
#  res = lista[["Wave_5"]]$res1,
#  dtm = lista[["Wave_5"]]$dtm,
#  corpus_src = lista[["Wave_5"]]$corpus_split
#)


cluster_categories[["Wave_5"]] <- c("SSR Rhetoric", "Political Debate", "Goverment Deliberation", "NA")

# --- juntando as definições manuais em um data.frame

waves_tab <- list()
# 
for (i in 1:5) {
  wave_name <- paste0("Wave_", i)
  wave_data <- data.frame(lista[[wave_name]]$cluster_counts) %>%
    mutate(wave = wave_name)
  waves_tab[[i]] <- wave_data
}


# Atualizando waves_tab com a categorização de clusters
for (i in 1:5) {
  wave_name <- paste0("Wave_", i)
  wave_data <- waves_tab[[i]]
  
  wave_data <- wave_data %>%
    mutate(cluster_category = case_when(
      cluster_assignments == 1 ~ cluster_categories[[wave_name]][1],
      cluster_assignments == 2 ~ cluster_categories[[wave_name]][2],
      cluster_assignments == 3 ~ cluster_categories[[wave_name]][3],
      cluster_assignments == 4 ~ cluster_categories[[wave_name]][4],
      cluster_assignments == 5 ~ cluster_categories[[wave_name]][5],
      TRUE ~ "NA"
    ))
  
  waves_tab[[i]] <- wave_data
}

waves_df <- bind_rows(waves_tab)

waves_df <- as.data.frame(waves_df)


waves_df$Freq <- as.numeric(waves_df$Freq)
waves_df$cluster_assignments <- as.numeric(as.character(waves_df$cluster_assignments))

str(waves_df)


# ---- criando a análise descritiva
## NUMBER OF text segments 20,062 
sum(waves_df$Freq, na.rm = TRUE)


waves_df %>%
  filter(cluster_category != "NA") %>%
  group_by(cluster_category) %>%
  summarise(total = sum(Freq)) %>%
  mutate(proportion = (total / sum(total)) * 100) -> proportions_df




# -----CRIANDO O GRÁFICO ----




waves_df$wave <- case_when(
  waves_df$wave == "Wave_1"  ~ "Post-election \n (Survey 1)",
  waves_df$wave == "Wave_2" ~ "Inauguration \n (Survey 2)", 
  waves_df$wave == "Wave_3" ~ "Bill introduction \n (Survey 3)",
  waves_df$wave == "Wave_4" ~ "Bill approval \n (Survey 4)",
  waves_df$wave == "Wave_5" ~ "Post-reform cycle \n (Survey 5)", 
  TRUE ~ as.character(waves_df$wave)
)

waves_df$wave <-factor(waves_df$wave, levels=c("Post-election \n (Survey 1)", "Inauguration \n (Survey 2)", 
                                                                             "Bill introduction \n (Survey 3)", "Bill approval \n (Survey 4)", "Post-reform cycle \n (Survey 5)"), ordered = T)


#dev.off()  
png(filename="images/fig2.png", width = 1200, height = 300)

waves_df |>
  mutate(
 #   wave = str_replace(wave, "Wave_", "Survey "),  # Ajusta nome dos surveys
    cluster_category = fct_relevel(cluster_category, 
                                   "Goverment Deliberation", 
                                   "SSR Rhetoric", 
                                   "Political Debate", 
                                   "Macroeconomic", 
                                   "Legislative Process", 
                                   "State & Municipal Reform")
  ) |>
  filter(cluster_category != "NA") |>
  ggplot(aes(x = Freq, y = fct_rev(cluster_category))) +  # Inverte a ordem de exibição
  geom_bar(stat = "identity", color = "black", fill = "#C0C0C0", width = 0.6) +  
  facet_grid(. ~ wave, scales = "free_x", space = "free_x") +
  
  # Define limite do eixo X, remove decimais e gruda a barra na lateral esquerda
  scale_x_continuous(limits = c(0, 3000), labels = label_number(accuracy = 1), expand = c(0, 0)) +
  
  # Adiciona rótulos
  labs(x = "Number of text segments", y = NULL) +
  
  theme_light() + 
  theme(
    legend.position = "none",
    axis.text.y = element_text(size = 18, face = "bold"),
    axis.text.x = element_text(size = 14, face = "italic", angle = 0, hjust = 0.5),
    strip.text = element_text(size = 16, face = "bold"),
    plot.background = element_rect(colour = "white"),
    strip.background = element_rect(fill = "gray35"),  
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(color = "gray", size = 0.5),
    panel.border = element_rect(color = "black", fill = NA, size = 1),
    panel.spacing = unit(1.5, "lines"),  # Espaçamento maior entre facets
    plot.margin = margin(t = 10, r = 20, b = 10, l = 10)  # Aumenta margem à direita
  )


dev.off()

# -----------------

#proporções

prop <- waves_df  %>%
  filter(!is.na(cluster_category)) %>%  # Excluindo NAs de cluster_category
  group_by(cluster_category) %>%
  summarise(total_freq = sum(Freq)) %>%
  ungroup() %>%
  mutate(proportion = total_freq / sum(total_freq))  # Calculando proporção geral


# Supondo que o dataframe original seja o waves_df
waves_df_wide <- waves_df %>%
  filter(cluster_category != "NA") %>% # Remover valores "NA" em cluster_category
  group_by(cluster_category, wave) %>% # Agrupar pelas categorias e waves
  summarise(Freq = sum(Freq)) %>% # Somar as frequências para categorias iguais
  ungroup() %>%
  mutate(cluster_category = fct_relevel(cluster_category, 
                                        "Goverment Deliberation", 
                                        "SSR Rhetoric", 
                                        "Political Debate", 
                                        "Macroeconomic", 
                                        "Legislative Process", 
                                        "State & Municipal Reform")) %>% # Reordenar as categorias
  pivot_wider(
    names_from = wave, # As colunas serão as waves
    values_from = Freq, # Os valores das células serão os valores de Freq
    values_fill = list(Freq = 0) # Preencher com 0 caso não tenha valores
  ) %>%
  rowwise() %>%
  mutate(TOTAL = sum(c_across(where(is.numeric)))) # Adiciona a coluna de TOTAL
# Visualizar a tabela transformada
print(waves_df_wide)

total_geral <- sum(waves_df_wide %>% select(-cluster_category, -TOTAL), na.rm = TRUE)

# Calcular o percentual de cada célula em relação ao total geral, excluindo a coluna TOTAL
waves_df_percentual <- waves_df_wide %>%
  mutate(across(where(is.numeric) & !matches("TOTAL"), ~ (. / total_geral) * 100))



