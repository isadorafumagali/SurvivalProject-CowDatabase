
library(readxl)
library(tidyverse)
library(survival)
library(survminer)
library(missForest)
library(mice)
library(splines)


#carrega os dados da planilha
caminho <- "Downloads/Projeto_Vacas.xlsx"
dados_raw <- read_excel(caminho)

#calcula o percentual de dados faltantes por variável
missing_summary <- colMeans(is.na(dados_raw)) * 100
print(round(missing_summary, 2))

#resume a variável contínua (leite_dia)
dados_raw %>%
  summarise(
    Media = mean(leite_dia, na.rm = TRUE),
    DP = sd(leite_dia, na.rm = TRUE),
    Mediana = median(leite_dia, na.rm = TRUE),
    IQR = IQR(leite_dia, na.rm = TRUE)
  )

#calcula a proporção de Censura vs Concepção
table(dados_raw$indic_tempo_concep)
prop.table(table(dados_raw$indic_tempo_concep)) * 100


#lista as Variáceis Categóricas
categoricas <- c("Cidade", "Estacao", "problema_pre_parto", 
                 "problema_pos_parto", "sistema_reproducao", 
                 "tempo_parto_insem", "producao_leite")

#roda a frequência para todas as categóricas (com NAs)
tabela_cat <- map_df(categoricas, function(var) {
  dados_raw %>%
    count(Categoria = get(var)) %>%
    mutate(
      Variavel = var,
      Pct = round((n / sum(n)) * 100, 2)
    ) %>%
    select(Variavel, Categoria, n, Pct)
})

print(tabela_cat, n = 30)


#calcula o Min e Max de leite dia
min_max_leite <- dados_raw %>%
  summarise(
    Minimo = min(leite_dia, na.rm = TRUE),
    Maximo = max(leite_dia, na.rm = TRUE)
  )

print(min_max_leite)


#remove os registros sem o desfecho no estudo
dados <- dados_raw %>%
  filter(!is.na(tempo_concep) & !is.na(indic_tempo_concep))

#converte variáveis categóricas para fatores
dados <- dados %>%
  mutate(
    Estacao = factor(Estacao, labels = c("Outono", "Inverno", "Primavera", "Verão")),
    Cidade = factor(Cidade, labels = c("Cidade A", "Cidade B")),
    problema_pos_parto = factor(problema_pos_parto, labels = c("Não", "Sim")),
    problema_pre_parto = factor(problema_pre_parto, labels = c("Não", "Sim")),
    sistema_reproducao = factor(sistema_reproducao, labels = c("IA", "Monta")),
    tempo_parto_insem = factor(tempo_parto_insem, labels = c("Até 85d", "Mais de 85d")),
    producao_leite = factor(producao_leite, labels = c("Abaixo Mediana", "Acima Mediana"))
  )


#semente para a imputação com missForest
set.seed(123)

#remove a coluna ID para não enviesar a imputação de padrões
dados_para_imputar <- dados %>% select(-ID_vaca)

#executa o missForest
imputacao_mf <- missForest(as.data.frame(dados_para_imputar))
dados_mf <- imputacao_mf$ximp

#reinsere o ID se necessário
dados_mf$ID_vaca <- dados$ID_vaca
dados_mf

#abaixo segue a Análise Não Paramétrica por Kaplan-Meier após a imputação por missForest

#cria o objeto de sobrevivência
surv_obj_mf <- Surv(time = dados_mf$tempo_concep, event = dados_mf$indic_tempo_concep)

#curva Kaplan-Meier por Problema Pós-Parto
fit_km_pos <- survfit(surv_obj_mf ~ problema_pos_parto, data = dados_mf)

#plot da Curva acumulada (1 - S(t))
ggsurvplot(
  fit_km_pos, 
  data = dados_mf,
  fun = "event",                     
  pval = TRUE,                      
  conf.int = TRUE,                  
  legend.title = "Problema Pós-Parto",
  xlab = "Dias pós-parto",
  ylab = "Proporção de Vacas Prenhes Acumulada",
  ggtheme = theme_minimal()
)

#Teste de Log-Rank para Estação do Ano
logrank_estacao <- survdiff(surv_obj_mf ~ Estacao, data = dados_mf)
print(logrank_estacao)

#semente para a Imputação Múltipla com MICE
set.seed(123)

#selecionando apenas as variáveis explicativas e o desfecho (ignorando ID_vaca)
#usei 'leite_dia' (contínua) em vez de 'producao_leite' para evitar multicolinearidade
dados_mice_prep <- dados %>% 
  select(-ID_vaca, -producao_leite)

#gera 5 bancos de dados imputados
dados_imp_mice <- mice(dados_mice_prep, m = 5, printFlag = FALSE)


#abaixo segue a Análise Descritiva

#histograma do tempo até a concepção
ggplot(dados, aes(x = tempo_concep)) +
  geom_histogram(
    bins = 30,
    fill = 'darkolivegreen',
    color = "black"
  ) +
  labs(
    title = "Distribuição do tempo até a concepção",
    x = "Tempo até a concepção (dias)",
    y = "Frequência"
  ) +
  theme_minimal()

#boxplot do tempo até a concepção
ggplot(dados, aes(y = tempo_concep)) +
  geom_boxplot() +
  labs(
    title = "Boxplot do tempo até a concepção",
    y = "Tempo até a concepção (dias)"
  ) +
  theme_minimal()

#distribuição da produção diária de leite
ggplot(dados, aes(x = leite_dia)) +
  geom_histogram(
    bins = 20,
    color = "black"
  ) +
  labs(
    title = "Distribuição da produção diária de leite",
    x = "Produção diária de leite (litros)",
    y = "Frequência"
  ) +
  theme_minimal()

#produção de leite vs tempo até concepção
ggplot(
  dados,
  aes(
    x = leite_dia,
    y = tempo_concep
  )
) +
  geom_point(
    aes(shape = factor(indic_tempo_concep)),
    alpha = 0.7
  ) +
  geom_smooth(
    method = "lm",
    se = TRUE
  ) +
  labs(
    title = "Produção diária de leite e tempo até a concepção",
    x = "Produção diária de leite (litros)",
    y = "Tempo até a concepção (dias)",
    shape = "Evento"
  ) +
  scale_shape_discrete(
    labels = c(
      "0" = "Censurada",
      "1" = "Concepção"
    )
  ) +
  theme_minimal()

#sistema reprodutivo
ggplot(
  dados,
  aes(
    x = sistema_reproducao,
    y = tempo_concep
  )
) +
  geom_boxplot() +
  labs(
    title = "Tempo até a concepção segundo sistema reprodutivo",
    x = "Sistema reprodutivo",
    y = "Tempo até a concepção (dias)"
  ) +
  theme_minimal()

#problema pós parto
ggplot(
  dados,
  aes(
    x = problema_pos_parto,
    y = tempo_concep
  )
) +
  geom_boxplot() +
  labs(
    title = "Tempo até a concepção segundo problema pós-parto",
    x = "Problema pós-parto",
    y = "Tempo até a concepção (dias)"
  ) +
  theme_minimal()

#produção de leite
ggplot(
  dados,
  aes(
    x = producao_leite,
    y = tempo_concep
  )
) +
  geom_boxplot() +
  labs(
    title = "Tempo até a concepção segundo produção de leite",
    x = "Produção de leite",
    y = "Tempo até a concepção (dias)"
  ) +
  theme_minimal()

#estação do ano
ggplot(
  dados,
  aes(
    x = Estacao,
    y = tempo_concep
  )
) +
  geom_boxplot() +
  labs(
    title = "Tempo até a concepção segundo estação",
    x = "Estação",
    y = "Tempo até a concepção (dias)"
  ) +
  theme_minimal()

#abaixo segue a Análise Não Paramétrica por Kaplan-Meier após a imputação por MICE

#cria o objeto de sobrevivência
surv_obj_mf <- Surv(
  time = dados_mf$tempo_concep,
  event = dados_mf$indic_tempo_concep
)

#KM por sistema reprodutivo
fit_km_repro <- survfit(
  surv_obj_mf ~ sistema_reproducao,
  data = dados_mf
)

#o gráfico abaixo mostra a proporção acumulada de vacas que já apresentaram concepção
#isso é mais intuitivo neste estudo do que mostrar a probabilidade de "sobrevivência" sem concepção
ggsurvplot(
  fit_km_repro,
  data = dados_mf,
  fun = "event",
  pval = TRUE,
  conf.int = TRUE,
  risk.table = TRUE,
  legend.title = "Sistema reprodutivo",
  xlab = "Dias pós-parto",
  ylab = "Proporção de vacas com concepção acumulada",
  title = "Curva de concepção acumulada segundo sistema reprodutivo",
  ggtheme = theme_minimal()
)

#KM problema pós parto
fit_km_pos <- survfit(
  surv_obj_mf ~ problema_pos_parto,
  data = dados_mf
)

ggsurvplot(
  fit_km_pos,
  data = dados_mf,
  fun = "event",
  pval = TRUE,
  conf.int = TRUE,
  risk.table = TRUE,
  legend.title = "Problema pós-parto",
  xlab = "Dias pós-parto",
  ylab = "Proporção de vacas com concepção acumulada",
  title = "Curva de concepção acumulada segundo problema pós-parto",
  ggtheme = theme_minimal()
)


#KM por produção de leite
fit_km_leite <- survfit(
  surv_obj_mf ~ producao_leite,
  data = dados_mf
)

ggsurvplot(
  fit_km_leite,
  data = dados_mf,
  fun = "event",
  pval = TRUE,
  conf.int = TRUE,
  risk.table = TRUE,
  legend.title = "Produção de leite",
  xlab = "Dias pós-parto",
  ylab = "Proporção de vacas com concepção acumulada",
  title = "Curva de concepção acumulada segundo produção de leite",
  ggtheme = theme_minimal()
)


#KM por estação do ano
fit_km_estacao <- survfit(
  surv_obj_mf ~ Estacao,
  data = dados_mf
)

ggsurvplot(
  fit_km_estacao,
  data = dados_mf,
  fun = "event",
  pval = TRUE,
  conf.int = TRUE,
  risk.table = TRUE,
  legend.title = "Estação",
  xlab = "Dias pós-parto",
  ylab = "Proporção de vacas com concepção acumulada",
  title = "Curva de concepção acumulada segundo estação",
  ggtheme = theme_minimal()
)

#Km por cidade
fit_km_cidade <- survfit(
  surv_obj_mf ~ Cidade,
  data = dados_mf
)

ggsurvplot(
  fit_km_cidade,
  data = dados_mf,
  fun = "event",
  pval = TRUE,
  conf.int = TRUE,
  risk.table = TRUE,
  legend.title = "Cidade",
  xlab = "Dias pós-parto",
  ylab = "Proporção de vacas com concepção acumulada",
  title = "Curva de concepção acumulada segundo cidade",
  ggtheme = theme_minimal()
)

#km pro problema pré parto
fit_km_pre <- survfit(
  surv_obj_mf ~ problema_pre_parto,
  data = dados_mf
)

ggsurvplot(
  fit_km_pre,
  data = dados_mf,
  fun = "event",
  pval = TRUE,
  conf.int = TRUE,
  risk.table = TRUE,
  legend.title = "Problema pré-parto",
  xlab = "Dias pós-parto",
  ylab = "Proporção de vacas com concepção acumulada",
  title = "Curva de concepção acumulada segundo problema pré-parto",
  ggtheme = theme_minimal()
)

#KM por tempo parto-inseminação
fit_km_insem <- survfit(
  surv_obj_mf ~ tempo_parto_insem,
  data = dados_mf
)

ggsurvplot(
  fit_km_insem,
  data = dados_mf,
  fun = "event",
  pval = TRUE,
  conf.int = TRUE,
  risk.table = TRUE,
  legend.title = "Tempo parto–inseminação",
  xlab = "Dias pós-parto",
  ylab = "Proporção de vacas com concepção acumulada",
  title = "Curva de concepção acumulada segundo tempo parto–inseminação",
  ggtheme = theme_minimal()
)


#log-rank por sistema reprodutivo
logrank_repro <- survdiff(
  surv_obj_mf ~ sistema_reproducao,
  data = dados_mf
)

print(logrank_repro)

#log-rank por problema pós parto
logrank_pos <- survdiff(
  surv_obj_mf ~ problema_pos_parto,
  data = dados_mf
)

print(logrank_pos)

#log-rank por problema pré parto
logrank_pre <- survdiff(
  surv_obj_mf ~ problema_pre_parto,
  data = dados_mf
)

print(logrank_pre)

#log-rank por produção de leite
logrank_leite <- survdiff(
  surv_obj_mf ~ producao_leite,
  data = dados_mf
)

print(logrank_leite)


#log-rank por estação
logrank_estacao <- survdiff(
  surv_obj_mf ~ Estacao,
  data = dados_mf
)

print(logrank_estacao)


#log-rank por cidade
logrank_cidade <- survdiff(
  surv_obj_mf ~ Cidade,
  data = dados_mf
)

print(logrank_cidade)

#log-rank por tempo parto-inseminação
logrank_insem <- survdiff(
  surv_obj_mf ~ tempo_parto_insem,
  data = dados_mf
)

print(logrank_insem)

#tabela de resultados dos log-ranks
extrair_p_logrank <- function(formula, data) {
  
  teste <- survdiff(formula, data = data)
  
  p_valor <- 1 - pchisq(
    teste$chisq,
    df = length(teste$n) - 1
  )
  
  return(p_valor)
}
p_repro <- extrair_p_logrank(
  surv_obj_mf ~ sistema_reproducao,
  dados_mf
)

p_pos <- extrair_p_logrank(
  surv_obj_mf ~ problema_pos_parto,
  dados_mf
)

p_pre <- extrair_p_logrank(
  surv_obj_mf ~ problema_pre_parto,
  dados_mf
)

p_leite <- extrair_p_logrank(
  surv_obj_mf ~ producao_leite,
  dados_mf
)

p_estacao <- extrair_p_logrank(
  surv_obj_mf ~ Estacao,
  dados_mf
)

p_cidade <- extrair_p_logrank(
  surv_obj_mf ~ Cidade,
  dados_mf
)

p_insem <- extrair_p_logrank(
  surv_obj_mf ~ tempo_parto_insem,
  dados_mf
)
resultado_logrank <- tibble(
  Variavel = c(
    "Sistema reprodutivo",
    "Problema pós-parto",
    "Problema pré-parto",
    "Produção de leite",
    "Estação",
    "Cidade",
    "Tempo parto–inseminação"
  ),
  
  p_valor = c(
    p_repro,
    p_pos,
    p_pre,
    p_leite,
    p_estacao,
    p_cidade,
    p_insem
  )
)

resultado_logrank


###
#Modelos de Cox univariado
###

#cox sistema reprodutivo
cox_repro <- coxph(
  surv_obj_mf ~ sistema_reproducao,
  data = dados_mf
)

summary(cox_repro)

#cox problemas pós parto
cox_pos <- coxph(
  surv_obj_mf ~ problema_pos_parto,
  data = dados_mf
)

summary(cox_pos)


#cox problemas pré parto
cox_pre <- coxph(
  surv_obj_mf ~ problema_pre_parto,
  data = dados_mf
)

summary(cox_pre)

#cox produção de leite por dia
cox_leite <- coxph(
  surv_obj_mf ~ leite_dia,
  data = dados_mf
)

summary(cox_leite)

#cox para estação
cox_estacao <- coxph(
  surv_obj_mf ~ Estacao,
  data = dados_mf
)

summary(cox_estacao)

#cox para cidade
cox_cidade <- coxph(
  surv_obj_mf ~ Cidade,
  data = dados_mf
)

summary(cox_cidade)

#cox para tempo parto-inseminação
cox_insem <- coxph(
  surv_obj_mf ~ tempo_parto_insem,
  data = dados_mf
)

summary(cox_insem)


##
#Hazard Ratios dos cox univariados
##

resultados_uni <- bind_rows(
  
  tidy(cox_repro, exponentiate = TRUE, conf.int = TRUE) %>%
    mutate(Modelo = "Sistema reprodutivo"),
  
  tidy(cox_pos, exponentiate = TRUE, conf.int = TRUE) %>%
    mutate(Modelo = "Problema pós-parto"),
  
  tidy(cox_pre, exponentiate = TRUE, conf.int = TRUE) %>%
    mutate(Modelo = "Problema pré-parto"),
  
  tidy(cox_leite, exponentiate = TRUE, conf.int = TRUE) %>%
    mutate(Modelo = "Produção de leite"),
  
  tidy(cox_estacao, exponentiate = TRUE, conf.int = TRUE) %>%
    mutate(Modelo = "Estação"),
  
  tidy(cox_cidade, exponentiate = TRUE, conf.int = TRUE) %>%
    mutate(Modelo = "Cidade"),
  
  tidy(cox_insem, exponentiate = TRUE, conf.int = TRUE) %>%
    mutate(Modelo = "Tempo parto–inseminação")
)

resultados_uni
resultados_uni %>%
  select(
    Modelo,
    term,
    estimate,
    conf.low,
    conf.high,
    p.value
  )


##
#cox multivariado
##

fit_cox_mice <- with(
  dados_imp_mice,
  coxph(
    Surv(tempo_concep, indic_tempo_concep) ~
      Estacao +
      Cidade +
      problema_pos_parto +
      problema_pre_parto +
      sistema_reproducao +
      tempo_parto_insem +
      leite_dia
  )
)
resultado_final_cox <- pool(fit_cox_mice)
summary(resultado_final_cox)
hr_resultados <- summary(
  resultado_final_cox,
  exponentiate = TRUE,
  conf.int = TRUE
)

print(hr_resultados)

tabela_cox_final <- summary(
  resultado_final_cox,
  exponentiate = TRUE,
  conf.int = TRUE
) %>%
  select(
    term,
    estimate,
    `2.5 %`,
    `97.5 %`,
    p.value
  ) %>%
  rename(
    Variavel = term,
    HR = estimate,
    IC95_inf = `2.5 %`,
    IC95_sup = `97.5 %`,
    P_valor = p.value
  )

print(tabela_cox_final)


##
#análise de diagnóstico
##

modelos_cox <- fit_cox_mice$analyses

#teste de riscos proporcionais
diagnostico_ph <- lapply(
  modelos_cox,
  cox.zph
)

diagnostico_ph

#gráfico da suposição dos riscos proporcionais
plot(
  diagnostico_ph[[1]],
  var = 1
)

#teste de Schoenfeld
#permite verificar se uma determinada variável apresenta violação em várias das cinco imputações.
ph_resultados <- lapply(
  seq_along(diagnostico_ph),
  function(i) {
    
    teste <- diagnostico_ph[[i]]
    
    data.frame(
      Imputacao = i,
      Variavel = rownames(teste$table),
      Qui_quadrado = teste$table[, "chisq"],
      P_valor = teste$table[, "p"]
    )
  }
) %>%
  bind_rows()

print(ph_resultados)


#análise de disgnóticos - resíiduos Schoenfeld
cox.zph()
plot(diagnostico_ph[[1]])

#análise de disgnóticos - resíiduos deviance
ggcoxdiagnostics(
  modelos_cox[[1]],
  type = "deviance",
  linear.predictions = FALSE,
  ggtheme = theme_minimal()
)

#análise de disgnóticos - resíiduos martingale
ggcoxdiagnostics(
  modelos_cox[[1]],
  type = "martingale",
  linear.predictions = FALSE,
  ggtheme = theme_minimal()
)

#análise de disgnóticos - resíiduos DFBeta
ggcoxdiagnostics(
  modelos_cox[[1]],
  type = "dfbeta",
  linear.predictions = FALSE,
  ggtheme = theme_minimal()
)


#concordância dos modelos
concordancia <- sapply(
  modelos_cox,
  function(modelo) {
    summary(modelo)$concordance[1]
  }
)

concordancia


##
#modelo sem tempo_parto_insem
##

dados_mice_prep2 <- dados %>%
  select(
    -ID_vaca,
    -producao_leite,
    -tempo_parto_insem
  )

dados_imp_mice2 <- mice(
  dados_mice_prep2,
  m = 5,
  printFlag = FALSE
)

fit_cox_mice2 <- with(
  dados_imp_mice2,
  coxph(
    Surv(tempo_concep, indic_tempo_concep) ~
      Estacao +
      Cidade +
      problema_pos_parto +
      problema_pre_parto +
      sistema_reproducao +
      leite_dia
  )
)

resultado_final_cox2 <- pool(fit_cox_mice2)

summary(
  resultado_final_cox2,
  exponentiate = TRUE,
  conf.int = TRUE
)


modelos_cox2 <- fit_cox_mice2$analyses

diagnostico_ph2 <- lapply(
  modelos_cox2,
  cox.zph
)

diagnostico_ph2


##
#linearidade da lete_dia
##
modelo_linear <- with(
  dados_imp_mice2,
  coxph(
    Surv(tempo_concep, indic_tempo_concep) ~
      Estacao +
      Cidade +
      problema_pos_parto +
      problema_pre_parto +
      sistema_reproducao +
      leite_dia
  )
)

modelo_spline <- with(
  dados_imp_mice2,
  coxph(
    Surv(tempo_concep, indic_tempo_concep) ~
      Estacao +
      Cidade +
      problema_pos_parto +
      problema_pre_parto +
      sistema_reproducao +
      ns(leite_dia, df = 3)
  )
)

for (i in 1:5) {
  
  cat("\n============================\n")
  cat("IMPUTAÇÃO", i, "\n")
  cat("============================\n")
  
  print(
    anova(
      modelo_linear$analyses[[i]],
      modelo_spline$analyses[[i]],
      test = "LRT"
    )
  )
}


par(mfrow = c(2, 3))

for (i in 1:5) {
  
  modelo <- fit_cox_mice2$analyses[[i]]
  
  martingale <- residuals(modelo, type = "martingale")
  
  x <- dados_imp_mice2$data[[i]]$leite_dia
  
  plot(
    x,
    martingale,
    xlab = "Leite por dia",
    ylab = "Resíduos de Martingale",
    main = paste("Imputação", i),
    pch = 19
  )
  
  abline(h = 0, lty = 2)
  
  lines(
    lowess(x, martingale),
    lwd = 2
  )
}

par(mfrow = c(1, 1))


#diagnóstico do modelo de Cox
modelos_cox2 <- fit_cox_mice2$analyses


#teste de riscos proporcionais - res[iduos de Schoenfeld
diagnostico_ph2 <- lapply(
  modelos_cox2,
  cox.zph
)

diagnostico_ph2


#tabela resumindo os testes de Schoenfeld
ph_resultados2 <- lapply(
  seq_along(diagnostico_ph2),
  function(i) {
    
    teste <- diagnostico_ph2[[i]]$table
    
    data.frame(
      Imputacao = i,
      Variavel = rownames(teste),
      Qui_quadrado = teste[, "chisq"],
      GL = teste[, "df"],
      p_valor = teste[, "p"]
    )
  }
) %>%
  bind_rows()

ph_resultados2


#gráficos dos resíduos de Schoenfeld

#gera os gráficos da primeira imputação
par(mfrow = c(2, 3))

plot(
  diagnostico_ph2[[1]],
  main = "Resíduos de Schoenfeld - Imputação 1"
)

par(mfrow = c(1, 1))


#gera o gráfico específico para leite_dia
plot(
  diagnostico_ph2[[1]],
  var = "leite_dia"
)


#resíduos de Martingale
par(mfrow = c(2, 3))

for (i in 1:5) {
  
  modelo <- modelos_cox2[[i]]
  
  residuos_martingale <- residuals(
    modelo,
    type = "martingale"
  )
  
  x <- dados_imp_mice2$data[[i]]$leite_dia
  
  plot(
    x,
    residuos_martingale,
    xlab = "Produção de leite por dia",
    ylab = "Resíduos de Martingale",
    main = paste("Imputação", i),
    pch = 19
  )
  
  abline(
    h = 0,
    lty = 2
  )
  
  linhas_lowess <- lowess(
    x,
    residuos_martingale
  )
  
  lines(
    linhas_lowess,
    lwd = 2
  )
}

par(mfrow = c(1, 1))

#deviance
par(mfrow = c(2, 3))

for (i in 1:5) {
  
  modelo <- modelos_cox2[[i]]
  
  residuos_deviance <- residuals(
    modelo,
    type = "deviance"
  )
  
  plot(
    residuos_deviance,
    xlab = "Observação",
    ylab = "Resíduos de Deviance",
    main = paste("Imputação", i),
    pch = 19
  )
  
  abline(
    h = 0,
    lty = 2
  )
}

par(mfrow = c(1, 1))

#identifica observações atípicas
for (i in 1:5) {
  
  modelo <- modelos_cox2[[i]]
  
  residuos_deviance <- residuals(
    modelo,
    type = "deviance"
  )
  
  indices <- which(
    abs(residuos_deviance) > 2
  )
  
  cat("\n============================\n")
  cat("IMPUTAÇÃO", i, "\n")
  cat("============================\n")
  
  if (length(indices) == 0) {
    
    cat("Nenhuma observação com |Deviance| > 2\n")
    
  } else {
    
    cat("Observações com |Deviance| > 2:\n")
    print(indices)
  }
}

#confere a influência das observações
par(mfrow = c(2, 3))

for (i in 1:5) {
  
  modelo <- modelos_cox2[[i]]
  
  dfbeta <- residuals(
    modelo,
    type = "dfbeta"
  )
  
  matplot(
    dfbeta,
    type = "h",
    lty = 1,
    xlab = "Observação",
    ylab = "DFBeta",
    main = paste("DFBeta - Imputação", i)
  )
  
  abline(
    h = 0,
    lty = 2
  )
}

par(mfrow = c(1, 1))


influencia_dfbeta <- lapply(
  seq_along(modelos_cox2),
  function(i) {
    
    modelo <- modelos_cox2[[i]]
    
    dfbeta <- residuals(
      modelo,
      type = "dfbeta"
    )
    
    influencia <- apply(
      abs(dfbeta),
      1,
      max
    )
    
    data.frame(
      Imputacao = i,
      Observacao = seq_along(influencia),
      DFBeta_max = influencia
    ) %>%
      arrange(desc(DFBeta_max))
  }
)

influencia_dfbeta



#mostra as 10 observações mais influentes de cada imputação
influencia_dfbeta %>%
  bind_rows() %>%
  group_by(Imputacao) %>%
  slice_max(
    order_by = DFBeta_max,
    n = 10
  ) %>%
  arrange(Imputacao, desc(DFBeta_max))



#calcula o índice de concordância do modelo
concordancias2 <- sapply(
  modelos_cox2,
  function(modelo) {
    summary(modelo)$concordance[1]
  }
)

concordancias2

media_concordancia2 <- mean(
  concordancias2
)

media_concordancia2

data.frame(
  Imputacao = 1:5,
  Concordancia = concordancias2
)


#plota a curva de sobrevivência estimada
modelo1 <- modelos_cox2[[1]]

ajuste_surv <- survfit(
  modelo1
)

plot(
  ajuste_surv,
  xlab = "Tempo até concepção (dias)",
  ylab = "Probabilidade de permanecer sem concepção",
  main = "Curva de sobrevivência estimada",
  lwd = 2
)

#investiga as observações mais influentes
for (i in 1:5) {
  
  modelo <- modelos_cox2[[i]]
  
  dfbeta <- residuals(modelo, type = "dfbeta")
  
  influencia <- apply(abs(dfbeta), 1, max)
  
  maiores <- order(influencia, decreasing = TRUE)[1:10]
  
  cat("\n========================================\n")
  cat("IMPUTAÇÃO", i, "\n")
  cat("========================================\n")
  
  resultado <- data.frame(
    Observacao = maiores,
    DFBeta_max = influencia[maiores]
  )
  
  print(resultado)
  
  cat("\nDFBeta por variável:\n")
  
  print(
    dfbeta[maiores, , drop = FALSE]
  )
}

#identifica as observações mais influentes da imputação 1
modelo <- modelos_cox2[[1]]

dfbeta <- residuals(modelo, type = "dfbeta")

influencia <- apply(abs(dfbeta), 1, max)

maiores <- order(influencia, decreasing = TRUE)[1:10]

dados_imp_mice2$data[[1]][maiores, ]


#identifica as observações com resíduos de Deviance elevados
for (i in 1:5) {
  
  modelo <- modelos_cox2[[i]]
  
  residuos_deviance <- residuals(
    modelo,
    type = "deviance"
  )
  
  indices <- which(
    abs(residuos_deviance) > 2
  )
  
  cat("\n============================\n")
  cat("IMPUTAÇÃO", i, "\n")
  cat("============================\n")
  
  if (length(indices) == 0) {
    
    cat("Nenhuma observação com |Deviance| > 2\n")
    
  } else {
    
    resultado <- data.frame(
      Observacao = indices,
      Deviance = residuos_deviance[indices]
    )
    
    print(resultado)
  }
}


#dados completos da imputação 1
dados_comp1 <- complete(dados_imp_mice2, 1)

#checa a observação 78
dados_comp1

dados_comp1[maiores, ]


#análise de sensibilidade da observação 78
#dados completos da imputação 1
dados_comp1 <- complete(dados_imp_mice2, 1)

#modelo original
modelo_original <- modelos_cox2[[1]]

#retira a observação 78
dados_sem78 <- dados_comp1[-78, ]

#reajusta o modelo
modelo_sem78 <- coxph(
  Surv(tempo_concep, indic_tempo_concep) ~
    Estacao +
    Cidade +
    problema_pos_parto +
    problema_pre_parto +
    sistema_reproducao +
    leite_dia,
  data = dados_sem78
)

#compara os modelos
summary(modelo_original)
summary(modelo_sem78)

#HR e IC do modelo original
exp(cbind(
  HR = coef(modelo_original),
  confint(modelo_original)
))

#HR e IC sem a observação 78
exp(cbind(
  HR = coef(modelo_sem78),
  confint(modelo_sem78)
))
