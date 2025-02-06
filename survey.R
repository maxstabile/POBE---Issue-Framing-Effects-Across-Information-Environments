# # --------------------------------------------------------------------------------------
#  Issue Framing Effects Across Information Environments
# # --------------------------------------------------------------------------------------
#  Out Files: Figure 3 and Figure 4. 
#  Author Robert Vidigal & Max Stabile.
#  Machine: Mac Book PRO (Silicon M1 Chip)
# # --------------------------------------------------------------------------------------
rm(list=ls()); gc(); 

pacotes <- c("ggplot2", "ggthemes", "dplyr", "purrr")


instalar_pacotes <- function(pacotes) {
  pacotes_ausentes <- pacotes[!(pacotes %in% installed.packages()[, "Package"])]
  if(length(pacotes_ausentes)) install.packages(pacotes_ausentes)
}

instalar_pacotes(pacotes)
lapply(pacotes, library, character.only = TRUE)


# Loading SURVEY DATA
ssr_complete<-read.csv("data/survey/SSR_Dataset_Merged.csv", header=T)
ssr<-subset(ssr_complete, !is.na(op_previdência_AFAVOR)) # Removing empty DVs, final sample 8,277

# # --------------------------------------------------------------------------------------
# Treatment 0: control
# Treatment 1: frame increasing investment in public health and education (silent).
# Treatment 2: frame reducing expenditure on pensions (salient).
# # --------------------------------------------------------------------------------------

# --- Issue Salience in the Environment
ssr$salient<-ifelse(ssr$ONDA==1, 0,
                 ifelse(ssr$ONDA==2,0,
                        ifelse(ssr$ONDA==3,1,
                               ifelse(ssr$ONDA==4,0,0))))
#ssr$infonews<-ifelse(ssr$ONDA==1,12.4,
#                     ifelse(ssr$ONDA==2,15.8,
#                            ifelse(ssr$ONDA==3,31.3,
#                                   ifelse(ssr$ONDA==4,12.2,0)))) 
# Actual levels from content analyses, does not change salience coding results.

# --- Treatment
ssr$treat<-ifelse(ssr$tratamento==1,1,
                  ifelse(ssr$tratamento==2,-1,0))
ssr$treat<-factor(ssr$treat, levels=c(-1,0,1), labels=c("reduce","control", "increase"))
ssr <- within(ssr, treat <- relevel(treat, ref = 2)) # 0

# --- SSR Opinion
table(ssr$op_previdência_AFAVOR)

# --- Bolsonaro Opinion
psych::describe(ssr$bolsonaro) # Higher values indicate more negative views towards govt.
colnames(ssr)[colnames(ssr) == 'bolsonaro'] <- 'neg_govt_eval'

# Survey Mode (0 = telephone, 1 = online) --- W2 and W3 and W5 online
ssr$mode<-ifelse(ssr$ONDA==1, 0,
                 ifelse(ssr$ONDA==2,1,
                        ifelse(ssr$ONDA==3,1,
                               ifelse(ssr$ONDA==5,1,0)))) # 1 = telephone 0 = online
table(ssr$mode)

# Individual dummies
# # --------------------------------------------------------------------------------------
ssr$wave1<-ifelse(ssr$ONDA==1,1,0)
ssr$wave2<-ifelse(ssr$ONDA==2,1,0)
ssr$wave3<-ifelse(ssr$ONDA==3,1,0)
ssr$wave4<-ifelse(ssr$ONDA==4,1,0)
ssr$wave5<-ifelse(ssr$ONDA==5,1,0)

# # --------------------------------------------------------------------------------------
# SUBSETS
# # --------------------------------------------------------------------------------------
antibolso<-subset(ssr, neg_govt_eval>2) # 5858
probolso<-subset(ssr, neg_govt_eval <3) # 3503

# Individual Waves
# # --------------------------------------------------------------------------------------
table(ssr$ONDA)
w1<-subset(ssr, ONDA==1 & !is.na(ssr$op_previdência_AFAVOR)) # 905
w2<-subset(ssr, ONDA==2 & !is.na(ssr$op_previdência_AFAVOR)) # 1,456
w3<-subset(ssr, ONDA==3 & !is.na(ssr$op_previdência_AFAVOR)) # 775
w4<-subset(ssr, ONDA==4 & !is.na(ssr$op_previdência_AFAVOR)) # 1,636
w5<-subset(ssr, ONDA==5 & !is.na(ssr$op_previdência_AFAVOR)) # 3,494

# By experimental condition
w1c <-subset(w1, tratamento==0) # 297
w1t1<-subset(w1, tratamento==1) # 307
w1t2<-subset(w1, tratamento==2) # 301

w2c <-subset(w2, tratamento==0) # 505
w2t1<-subset(w2, tratamento==1) # 473
w2t2<-subset(w2, tratamento==2) # 478

w3c <-subset(w3, tratamento==0) # 262
w3t1<-subset(w3, tratamento==1) # 261
w3t2<-subset(w3, tratamento==2) # 252

w4c <-subset(w4, tratamento==0) # 545
w4t1<-subset(w4, tratamento==1) # 553
w4t2<-subset(w4, tratamento==2) # 538

w5c <-subset(w5, tratamento==0) # 1714
w5t1<-subset(w5, tratamento==1) # 1673
w5t2<-subset(w5, tratamento==2) # 1620

allc<-subset(ssr, tratamento==0)
allt1<-subset(ssr, tratamento==1)
allt2<-subset(ssr, tratamento==2)

w1234allc<-subset(allc, tratamento==0 & ONDA!=5)
w1234allt1<-subset(allt1, tratamento==1 & ONDA!=5)
w1234allt2<-subset(allt2, tratamento==2 & ONDA!=5)

# # --------------------------------------------------------------------------------------
### PAIRWISE COMPARISONS
# # --------------------------------------------------------------------------------------

# --- W1
w1ct1<-t.test(w1c$op_previdência_AFAVOR,  w1t1$op_previdência_AFAVOR) # sig .01
w1ct2<-t.test(w1c$op_previdência_AFAVOR,  w1t2$op_previdência_AFAVOR) # sig
w1t1t2<-t.test(w1t1$op_previdência_AFAVOR, w1t2$op_previdência_AFAVOR) # sig 
summary(glm(op_previdência_AFAVOR~tratamento+neg_govt_eval+mulher+idade+ensino, 
            family="binomial", data=w1))
# Both frames different from control and each other.

# --- W2
w2ct1<-t.test(w2c$op_previdência_AFAVOR,  w2t1$op_previdência_AFAVOR) # NS
w2ct2<-t.test(w2c$op_previdência_AFAVOR,  w2t2$op_previdência_AFAVOR) # sig 
w2t1t2<-t.test(w2t1$op_previdência_AFAVOR, w2t2$op_previdência_AFAVOR) # sig
summary(glm(op_previdência_AFAVOR~tratamento+neg_govt_eval+mulher+idade+ensino, 
            family="binomial", data=w2))
# Only Silent Frame not different from control

# --- W3
w3ct1<-t.test(w3c$op_previdência_AFAVOR,  w3t1$op_previdência_AFAVOR) # NS
w3ct2<-t.test(w3c$op_previdência_AFAVOR,  w3t2$op_previdência_AFAVOR) # NS
w3t1t2<-t.test(w3t1$op_previdência_AFAVOR, w3t2$op_previdência_AFAVOR) # NS
summary(glm(op_previdência_AFAVOR~tratamento+neg_govt_eval+mulher+idade+ensino, 
            family="binomial", data=w3))
# None significant

# --- W4
w4ct1<-t.test(w4c$op_previdência_AFAVOR,  w4t1$op_previdência_AFAVOR) # sig
w4ct2<-t.test(w4c$op_previdência_AFAVOR,  w4t2$op_previdência_AFAVOR) # NS 
w4t1t2<-t.test(w4t1$op_previdência_AFAVOR, w4t2$op_previdência_AFAVOR) # sig
summary(glm(op_previdência_AFAVOR~tratamento+neg_govt_eval+mulher+idade+ensino, 
            family="binomial", data=w4))
# Silent Frame different but Salient not different from control anymore

# --- W5
w5ct1<-t.test(w4c$op_previdência_AFAVOR,   w5t1$op_previdência_AFAVOR) # sig
w5ct2<-t.test(w4c$op_previdência_AFAVOR,   w5t2$op_previdência_AFAVOR) # sig
w5t1t2<-t.test(w4t1$op_previdência_AFAVOR, w4t2$op_previdência_AFAVOR) # sig
summary(glm(op_previdência_AFAVOR~tratamento+neg_govt_eval+mulher+idade+ensino, 
            family="binomial", data=w5))

# --- POOLED
w1234allct1<-t.test(w1234allc$op_previdência_AFAVOR,  w1234allt1$op_previdência_AFAVOR) # sig
w1234allct2<-t.test(w1234allc$op_previdência_AFAVOR,  w1234allt2$op_previdência_AFAVOR) # sig 
w1234allt1t2<-t.test(w1234allt1$op_previdência_AFAVOR, w1234allt2$op_previdência_AFAVOR) # sig




# # --------------------------------------------------------------------------------------
### FIGURE 3 - ISSUE FRAMING EFFECTS
# # --------------------------------------------------------------------------------------
# Dataframe for Fig 3
# BUILD TABLE HERE WITH DESCRIBE() FOR FIGURE
fig1tab <- data.frame(
  "Perc"=c(psych::describe(w1c$op_previdência_AFAVOR)[[3]],
           psych::describe(w1t1$op_previdência_AFAVOR)[[3]],
           psych::describe(w1t2$op_previdência_AFAVOR)[[3]],
           psych::describe(w2c$op_previdência_AFAVOR)[[3]],
           psych::describe(w2t1$op_previdência_AFAVOR)[[3]],
           psych::describe(w2t2$op_previdência_AFAVOR)[[3]],
           psych::describe(w3c$op_previdência_AFAVOR)[[3]],
           psych::describe(w3t1$op_previdência_AFAVOR)[[3]],
           psych::describe(w3t2$op_previdência_AFAVOR)[[3]],
           psych::describe(w4c$op_previdência_AFAVOR)[[3]],
           psych::describe(w4t1$op_previdência_AFAVOR)[[3]],
           psych::describe(w4t2$op_previdência_AFAVOR)[[3]],
           #psych::describe(w1234allc$op_previdência_AFAVOR)[[3]],
           #psych::describe(w1234allt1$op_previdência_AFAVOR)[[3]], # pooled 1-4 surveys
           #psych::describe(w1234allt2$op_previdência_AFAVOR)[[3]]
           psych::describe(w5c$op_previdência_AFAVOR)[[3]],
           psych::describe(w5t1$op_previdência_AFAVOR)[[3]],
           psych::describe(w5t2$op_previdência_AFAVOR)[[3]]
  ),
  "se"=c(psych::describe(w1c$op_previdência_AFAVOR)[[13]],
         psych::describe(w1t1$op_previdência_AFAVOR)[[13]],
         psych::describe(w1t2$op_previdência_AFAVOR)[[13]],
         psych::describe(w2c$op_previdência_AFAVOR)[[13]],
         psych::describe(w2t1$op_previdência_AFAVOR)[[13]],
         psych::describe(w2t2$op_previdência_AFAVOR)[[13]],
         psych::describe(w3c$op_previdência_AFAVOR)[[13]],
         psych::describe(w3t1$op_previdência_AFAVOR)[[13]],
         psych::describe(w3t2$op_previdência_AFAVOR)[[13]],
         psych::describe(w4c$op_previdência_AFAVOR)[[13]],
         psych::describe(w4t1$op_previdência_AFAVOR)[[13]],
         psych::describe(w4t2$op_previdência_AFAVOR)[[13]],
         #psych::describe(w1234allc$op_previdência_AFAVOR)[[13]],
         #psych::describe(w1234allt1$op_previdência_AFAVOR)[[13]], # pooled 1-4 surveys
         #psych::describe(w1234allt2$op_previdência_AFAVOR)[[13]]
         psych::describe(w5c$op_previdência_AFAVOR)[[13]],
         psych::describe(w5t1$op_previdência_AFAVOR)[[13]],
         psych::describe(w5t2$op_previdência_AFAVOR)[[13]]
  ),
  "tratamento"=c(rep(c("control", "increase investment frame", 
                       "reduce expenditure frame"), 5)),
  "ONDA"=c(rep("Post-election \n (Survey 1) \n (telephone)",3), 
           rep("Inauguration \n (Survey 2) \n (online)",3 ), 
           rep("Bill introduction \n (Survey 3) \n (online)",3 ), 
           rep("Bill approval \n (Survey 4) \n (telephone)",3),
           rep("March 2022 \n (Survey 5) \n (online)",3)) # rep("Pooled \n Surveys", 3)) #, 
)


fig1tab <- fig1tab %>%
  mutate(
    tratamento = factor(tratamento, levels = c("control", "increase investment frame", "reduce expenditure frame")),
    ONDA = factor(ONDA, levels = c(
      "Post-election \n (Survey 1) \n (telephone)", 
      "Inauguration \n (Survey 2) \n (online)", 
      "Bill introduction \n (Survey 3) \n (online)", 
      "Bill approval \n (Survey 4) \n (telephone)",
      "March 2022 \n (Survey 5) \n (online)"
    ))
  )





png(filename="images/fig3.png", width = 800, height = 400)
ggplot(fig1tab, aes(x = tratamento, y = Perc, group = tratamento, shape = tratamento, color = tratamento)) + 
  theme_light() +  
  geom_point(size = 4, position = position_dodge(.15)) +  # Pontos menores
  geom_errorbar(aes(ymin = Perc - 1.64 * se, ymax = Perc + 1.64 * se), 
                width = 0, linewidth = 1, position = position_dodge(.15)) +  # Linhas mais finas
  
  # Linhas de referência horizontais
  geom_hline(yintercept = .7, linetype = 'dotted', color = "gray") + 
  geom_hline(yintercept = .5, linetype = 'dotted', color = "gray") + 
  geom_hline(yintercept = .3, linetype = 'dotted', color = "gray") +
  geom_hline(yintercept = .1, linetype = 'dotted', color = "gray") + 
  
  # Ajuste no eixo Y para exibir porcentagem
  scale_y_continuous(limits = c(0, .80), labels = scales::percent_format(accuracy = 1)) +
  
  # Cores das categorias
  scale_color_manual(values = c("black", "#a8a9ab", "#7e7f80")) +
  
  # Facet_wrap para separar os surveys
  facet_wrap(~ ONDA, scales = "free_x", nrow = 1) +
  
  # Ajuste nos rótulos
  labs(x = NULL, y = "% Support Social Security Reform") + 
  
  # Tema ajustado para manter a aparência dos gráficos anteriores
  theme(
    legend.position = "bottom",  # Move a legenda para baixo
    legend.title = element_blank(),
    legend.text = element_text(size = 16),  # Aumenta o tamanho da legenda
    legend.key.size = unit(1, "cm"),  # Aumenta o tamanho dos símbolos na legenda
    legend.text.align = 0.5,
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    strip.background = element_rect(fill = "gray30"),  # Fundo escuro nos títulos dos facets
    strip.text = element_text(color = "white", size = 12, face = "bold"),
    axis.line = element_line(colour = "black"),  
    axis.text.x = element_blank(),  # Remove os rótulos do eixo X
    axis.ticks.x = element_blank(),  # Remove os ticks do eixo X
    axis.text.y = element_text(face = 'bold', size = 10),
    panel.border = element_rect(color = "black", fill = NA, size = 1),
    panel.spacing = unit(1, "lines")  # Espaço entre os facets
  )
dev.off()

# # --------------------------------------------------------------------------------------
# FIGURE 4 - POOLED MODEL (Linear Probability Model for SSR approval)
# # --------------------------------------------------------------------------------------
ssr1234<-subset(ssr, ONDA!=5)

model1234<-glm(op_previdência_AFAVOR~treat+
                 neg_govt_eval+mode+mulher+idade+ensino+
                 salient*treat,  
               data=ssr1234) # wave4 ref category # no interaction effects of mode and treat
summary(model1234) # TABLE A5
pscl::pR2(model1234)
stargazer::stargazer(model1234, type="text")

# Plot
# jpeg(filename="images/fig4.png", units="in", width = 8, height = 5, res=300)
# sjPlot::plot_model(model1234, type="std2", rm.terms=c("mode", "idade", "ensino",
#                                                       "mulher", "neg_govt_eval")) + 
# theme_base() + 
#   ylim(-0.5,0.5) + ggtitle("") +
#   scale_x_discrete(labels=list("treat:reduce x salient:issue", "treat:increase x salient:issue", 
#                                "High Issue Salience \n(salient:issue)",
#                                "Increase Investment \n(treat:increase)",
#                                "Reduce Expenditure \n(treat:reduce)"
#                                )) + ylab("") +
#   geom_hline(yintercept=0, linetype = 'dotted', color="gray") +
#   theme(axis.text.x = element_text(size=12), 
#         axis.text.y = element_text(size=12),
#         axis.title.x = element_text(size=12)) 
# dev.off()

# # --------------------------------------------------------------------------------------
# BLACK AND WHITE PLOT
# # --------------------------------------------------------------------------------------
tidy_model <- broom::tidy(model1234, conf.int = TRUE)  
tidy_model_filtered <- tidy_model %>%
  filter(!term %in% c("mode", "idade", "ensino", "mulher", "neg_govt_eval", "(Intercept)"))

png(filename="images/fig4.png", units="in", width = 8, height = 5, res=300)
ggplot(tidy_model_filtered, aes(x = term, y = estimate)) + 
  geom_point(size = 4, color = "black") +  # Pontos um pouco maiores
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.01, size = 1.2, color = "black") +  # Linhas de erro mais grossas
  theme_bw() + 
  ylim(-0.25, 0.25) + 
  labs(title = "", x = "", y = "") + 
  coord_flip() +
  
  # Ajuste dos rótulos do eixo X
  scale_x_discrete(labels = c(
    "treatreduce:salient" = "treat:reduce x salient:issue", 
    "treatincrease:salient" = "treat:increase x salient:issue", 
    "salient" = "High Issue Salience\n(salient:issue)", 
    "treatincrease" = "Increase Investment\n(treat:increase)", 
    "treatreduce" = "Reduce Expenditure\n(treat:reduce)"
  ),
  limits = rev(c("treatincrease", "treatreduce", "salient",
                 "treatincrease:salient", "treatreduce:salient"))) + 
  
  # Linha de referência no zero mais destacada
  geom_hline(yintercept = 0, linetype = 'dashed', color = "gray55", size = 1.2) + 
  
  # Ajuste nos tamanhos dos textos e grades do gráfico
  theme(
    panel.grid.major.x = element_blank(), 
    panel.grid.minor.x = element_blank(), 
    panel.grid.major.y = element_line(color = "gray", linetype = "dotted"), 
    panel.grid.minor.y = element_blank(), 
    axis.text.x = element_text(size = 16),  # Aumentei tamanho do texto do eixo X
    axis.text.y = element_text(size = 16),  # Aumentei tamanho do texto do eixo Y
    axis.title.x = element_text(size = 16),  # Aumentei tamanho do título do eixo X
    plot.title = element_text(hjust = 0.5, size = 14)  # Ajustei o tamanho do título
  )


dev.off()

# # ----------------------rev.default()# # --------------------------------------------------------------------------------------
# # --------------------------------------------------------------------------------------
# # --------------------------------------------------------------------------------------

# ----------------------------------------------------------------------------------------
# APPENDIX
# # --------------------------------------------------------------------------------------

# # --------------------------------------------------------------------------------------
# TABLE A1
# TABLES A2-A3 - WLAD
# # --------------------------------------------------------------------------------------
table(w5$mulher); prop.table(table(w5$mulher))
mean(w5$idade); sd(w5$idade)
table(w5$ensino); prop.table(table(w5$ensino))
table(w5$region); prop.table(table(w5$region))
table(w5$neg_govt_eval); prop.table(table(w5$neg_govt_eval))
table(w5$op_previdência_AFAVOR); prop.table(table(w5$op_previdência_AFAVOR))
table(w5$treat); prop.table(table(w5$treat))

### BALANCE TESTS
# # --------------------------------------------------------------------------------------


# function for pairwise t-tests, storing only p-values
pairwise_t_tests <- function(subsets, variables_list) {
  results <- data.frame()  # Initialize an empty data frame for results
  
  # Get all possible pairs of subsets
  subset_names <- names(subsets)
  subset_pairs <- expand.grid(subset_names, subset_names, stringsAsFactors = FALSE) %>%
    filter(Var1 < Var2) # Ensure only unique pairs
  
  # Iterate over pair of subsets
  for (i in 1:nrow(subset_pairs)) {
    subset1_name <- subset_pairs$Var1[i]
    subset2_name <- subset_pairs$Var2[i]
    subset1 <- subsets[[subset1_name]]
    subset2 <- subsets[[subset2_name]]
    
    # Iterate over each variable tested
    for (variable in variables_list) {
      # Check if the variable exists in both subsets and is numeric or logical
      if (is.numeric(subset1[[variable]]) && is.numeric(subset2[[variable]])) {
        test_result <- t.test(subset1[[variable]], subset2[[variable]])
        result_row <- data.frame(
          Subset1 = subset1_name,
          Subset2 = subset2_name,
          Variable = variable,
          P_Value = test_result$p.value
        )
        results <- bind_rows(results, result_row)
      }
    }
  }
  
  return(results)
}

# Create a list of your subsets
exp_subsets <- list(
  control = subset(ssr, treat == "control"),
  reducecue = subset(ssr, treat == "reduce"),
  increasecue = subset(ssr, treat == "increase")
)

# Define the variables_list to be tested
variables_list <- c("mulher", "ensino", "mode", "region", "idade")

# Perform the pairwise t-tests and store p-values
t_test_results <- pairwise_t_tests(exp_subsets, variables_list)

# Display the results
t_test_results

# Filter the results
significant_results <- t_test_results %>%
  filter(P_Value < 0.05) %>% print(significant_results) # NONE


# ----------------------------------------------------------------------------------------
# REGRESSION MODELS APPENDIX (TABLE A4)
# # --------------------------------------------------------------------------------------

# ----------
# W1
# ----------
w1model<-glm(op_previdência_AFAVOR~treat+neg_govt_eval+mulher+idade+ensino, 
             family="binomial", data=w1) 
stargazer::stargazer(w1model, type="text")

# ----------
# W2
# ----------
# WAVE2 no education, thus not included in the model.
w2model<-glm(op_previdência_AFAVOR~treat+neg_govt_eval+mulher+idade+ensino,
             family="binomial", data=w2) 
stargazer::stargazer(w2model, type="text")

# ----------
# W3
# ----------
w3model<-glm(op_previdência_AFAVOR~treat+neg_govt_eval+mulher+idade+ensino,
             family="binomial", data=w3)
stargazer::stargazer(w3model, type="text")

# ----------
# W4
# ----------
w4model<-glm(op_previdência_AFAVOR~treat+neg_govt_eval+mulher+idade+ensino,
             family="binomial", data=w4) 
stargazer::stargazer(w4model, type="text")

# ----------
# W5
# ----------
w5model<-glm(op_previdência_AFAVOR~treat+neg_govt_eval+mulher+idade+ensino,
             family="binomial", data=w5) # 
stargazer::stargazer(w5model, type="text")

# # --------------------------------------------------------------------------------------
# REGRESSION ANALYSIS FOR APPENDIX
# # --------------------------------------------------------------------------------------
# OLS/LPM MODEL
ols1234mode<-lm(op_previdência_AFAVOR~treat+neg_govt_eval+mode+ensino+mulher+idade+
                     salient*treat,  
               data=ssr1234) 
summary(ols1234mode) # TABLE A5
stargazer::stargazer(ols1234mode, type="text")

# LOGIT MODEL (NOT IN APPENDIX SAME RESULTS)
logit1234mode<-glm(op_previdência_AFAVOR~treat+neg_govt_eval+mode+ensino+mulher+idade+
                     salient*treat, family=binomial(link = "logit"), 
               data=ssr1234) 
summary(logit1234mode)
pscl::pR2(logit1234mode)

# PROBIT MODEL  (NOT IN APPENDIX SAME RESULTS)
probit1234mode<-glm(op_previdência_AFAVOR~treat+neg_govt_eval+mode+ensino+mulher+idade+
                     salient*treat, family=binomial(link = "probit"), 
               data=ssr1234) 
summary(probit1234mode)
pscl::pR2(probit1234mode)

# A MULTILEVEL MODEL FOR SURVEY MODE
# # --------------------------------------------------------------------------------------
require(plm)

# 2nd-level: Survey Mode (FIXED EFFECTS) - TABLE A6
# # --------------------------------------------------------------------------------------
fixed1<-plm(op_previdência_AFAVOR~treat+neg_govt_eval+ensino+mulher+idade+
              salient*treat, 
             index=c("mode"), data=ssr1234, model="within") # FE
summary(fixed1) # Controlling for time-invariant variables (FE).
fixef(fixed1)

# # --------------------------------------------------------------------------------------
# FIGURE A.1
# # --------------------------------------------------------------------------------------
# FULL FIGURE 3 PLOT 
jpeg(filename="images/figA1_appendix.png", units="in", width = 8, height = 5, res=300)
sjPlot::plot_model(model1234, type="std2", rm.terms=c()) + 
theme_base() + 
  ylim(-0.5,0.5) + ggtitle("") + geom_point(size=3) +
  scale_x_discrete(labels=list("treat:reduce x salient:issue", 
                               "treat:increase x salient:issue",
                               "High Issue Salience)",
                               "Education (3-levels)",
                               "Age (in years)", "Gender (Women)", 
                               "Survey Mode (online)", "Govt. Evaluation", 
                               "Reduce Expenditure",
                               "Increase Investment")) +
  geom_hline(yintercept=0, linetype = 'dotted', color="gray") + ylab("") +
  theme(axis.text.x = element_text(size=12), 
        axis.text.y = element_text(size=12),
        axis.title.x = element_text(size=12)) 
dev.off()

# # --------------------------------------------------------------------------------------
# FIGURE A.2
# # --------------------------------------------------------------------------------------
# Predicted Values of Support for Social Security Reform by Education
jpeg(filename="images/figA2_appendix.png", units="in", width = 7, height = 5, res=300)
sjPlot::plot_model(model1234, type = "pred", terms = c("ensino")) + 
  theme_clean() + ylab("Support for Social Security Reform \n") + xlab("\n Education") + 
  scale_x_continuous(breaks=c(1,2,3), labels = c("Primary", "Secondary", "Tertiary")) +
  scale_y_continuous(breaks=c(.425,.45,.475,.5,.525,.55)) +
  theme(axis.text.x = element_text(size=12), 
        axis.text.y = element_text(size=12),
        axis.title.y = element_text(size=14),
        axis.title.x = element_text(size=12)) + ggtitle("")
dev.off()

# # --------------------------------------------------------------------------------------
# # --------------------------------------------------------------------------------------
# # --------------------------------------------------------------------------------------

print("Code logged successfully")

