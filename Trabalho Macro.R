# Pacotes requeridos e Dados ----
library(stats)
library(readxl)
library("dplyr")
library("lmtest")
library("stargazer")
library('stringr')
library('sandwich')
library('car')
library('rstudioapi')



# Importando os dados coletados pela turma 
# As duas funções abaixo fazem a mesma coisa
# A primeira escolhe por meio de dialógos
# Se já souber o caminho a segunda é mais rápida
# setwd(selectDirectory())
setwd("/home/pbnnr/Peu/Documentos/UFMG/Econometria I/Trabalho Conjunto/deb")
Turma <- read_excel("turma.xlsx")

# Tratando os Dados # ----

# Removendo Colunas não utilizadas na Regressão:
Turma <- select(Turma, -any_of('capital social'))
# Removendo a tabela de Dummies para corrigir depois das transformações
Turma <- select(Turma, -((ncol(Turma)-nrow(Turma)+1):ncol(Turma)))
# Removendo Linhas que constam N/As
Turma <- na.omit(Turma)
# Recriando tabela de dummies
dum_pais <- diag(nrow(Turma))
colnames(dum_pais) <- Turma$País



# Trocando todas as vírgulas por pontos para que o decimal seja padronizado
i <- c(3:ncol(Turma))
Turma[ ,i] <- apply(Turma[ ,i], 2, function(x) str_replace_all(x, ",", "."))

# Convertendo as dados da turma para númericos
Turma[ ,i] <- apply(Turma[ ,i], 2,function(x) as.numeric(x))

# Convertendo o nome das colunas para nomes convencionais
colnames(Turma)[colnames(Turma)
                == "Trust (proxy de capital social)"] <- "trust"
colnames(Turma)[colnames(Turma) 
                == "Law $ Order"] <- "law"
colnames(Turma)[colnames(Turma) 
                == "inserção no mercado internacional"] <- "trade"
colnames(Turma)[colnames(Turma) 
                == "Taxa de Investimento"] <- "s"
colnames(Turma)[colnames(Turma) 
                == "Taxa de crescimento populacional Geométrica"] <- "n"
colnames(Turma)[colnames(Turma) 
                == "Escolaridade (anos médio de estudo)"] <- "escola"
colnames(Turma)[colnames(Turma) 
                == "Coeficiente de Gini"] <- "Gini"


# Regressões # ----

# Estabelecendo variáveis para as regressões com valores propostos por MRW
d <- 0.03
g <-0.02

# Regredindo
modelo <- lm(log(Turma$y) ~ log(Turma$s) + log(Turma$n+g+d))

# Conferindo Heteroscedasticidade
# Se a estatística p do Breusch-Pagan test for baixa 
# significa que há heteroscedasticidade
bptest(modelo)

# Essas duas próximas funções demonstram a normalidade dos resíduos
qqnorm(residuals(modelo), main="Solow Clássico")  
qqline(residuals(modelo))

# Uma forma mais apresentável de visualizar os resultados da regressão
# Essa função mostra tanto as estatísticas corrigidas pela heterocedasticidade
stargazer(modelo, coeftest(modelo, vcovHC(modelo, type = "HC0")), 
          type="text", out="default.htm", 
          covariate.labels=c("Intercepto", "log(s)","log(n+g+d)"),
          dep.var.caption =c("log do PIB por trabalhador"),
          dep.var.labels = c("OLS","Ajustado por White"),
          model.names = FALSE,
          intercept.bottom = FALSE,
          single.row = TRUE,
          title = "Solow Clássico")

# Repetindo os passos regredindo log y contra s, n e escolaridade
modelo_esc <- lm(log(Turma$y) ~ log(Turma$s) + log(Turma$n+g+d) 
                 + log(Turma$escola))
bptest(modelo_esc)
qqnorm(residuals(modelo_esc), main = "MRW")  
qqline(residuals(modelo_esc))
stargazer(modelo_esc, coeftest(modelo_esc, vcovHC(modelo_esc, type = "HC0")), 
          type="text", out="default.htm", 
          covariate.labels=c("Intercepto", "log(s)","log(n+g+d)","Escolaridade"),
          dep.var.caption =c("log do PIB por trabalhador"),
          dep.var.labels = c("OLS","Ajustado por White"),
          model.names = FALSE,
          intercept.bottom = FALSE,
          single.row = TRUE,
          title = "MRW")

# Repetindo os passos regredindo log y contra as proxies todas
modelo_y <- lm(log(Turma$y) ~ Turma$trade + 
                              Turma$trust + 
                              Turma$Gini + Turma$Latitude + Turma$law)
bptest(modelo_y)

# stargazer(modelo_y, coeftest(modelo_y, vcovHC(modelo_y, type = "HC0")),
#           type="text", out="default.htm", 
#           covariate.labels=c("Intercepto", "Inserção Internacional",
#                              "Índice de Confiança", "Índice de Gini",
#                              "Latitude",
#                              "Índice Law and Order"),
#           dep.var.caption =c("log do PIB por trabalhador"),
#           dep.var.labels = c("OLS","Ajustado por White"),
#           model.names = FALSE,
#           intercept.bottom = FALSE,
#           single.row = TRUE,
#           title = "PIB contra proxies",
#           omit.table.layout = "#")

# Repetindo regressão das proxies contra log k
modelo_k <- lm(log(Turma$k) ~ Turma$trade + 
                 Turma$trust + 
                 Turma$Gini + Turma$Latitude + Turma$law)
bptest(modelo_k)

# stargazer(modelo_k, coeftest(modelo_k, vcovHC(modelo_k, type = "HC0")),
#           type="text", out="default.htm", 
#           covariate.labels=c("Intercepto", "Inserção Internacional",
#                              "Índice de Confiança", "Índice de Gini",
#                              "Latitude",
#                              "Índice Law and Order"),
#           dep.var.caption =c("log do Capital por trabalhador"),
#           dep.var.labels = c("OLS","Ajustado por White"),
#           intercept.bottom = FALSE,
#           single.row = TRUE,
#           title = "Capital contra proxies",
#           model.names = FALSE,
#           omit.table.layout = "#")


# Repetindo regressão das proxies contra log h
modelo_h <- lm(log(Turma$h) ~ Turma$trade + 
                 Turma$trust + 
                 Turma$Gini + Turma$Latitude + Turma$law)
bptest(modelo_h)
qqnorm(residuals(modelo_y), main = "PIB contra proxies")  
qqline(residuals(modelo_y))
qqnorm(residuals(modelo_k), main = "Capital contra proxies")  
qqline(residuals(modelo_k))
qqnorm(residuals(modelo_h), main = "Capital Humano contra proxies")  
qqline(residuals(modelo_h))
# stargazer(modelo_h, coeftest(modelo_h, vcovHC(modelo_h, type = "HC0")),
#           type="text", out="default.htm", 
#           covariate.labels=c("Intercepto", "Inserção Internacional",
#                              "Índice de Confiança", "Índice de Gini",
#                              "Latitude",
#                              "Índice Law and Order"),
#           dep.var.caption =c("log do Capital Humano"),
#           dep.var.labels = c("OLS","Ajustado por White"),
#           intercept.bottom = FALSE,
#           single.row = TRUE,
#           title = "Capital Humano contra Proxies",
#           model.names = FALSE,
#           omit.table.layout = "#")


# Repetindo regressão das proxies contra log PTF
modelo_a <- lm(log(Turma$PTF) ~ Turma$trade + 
                 Turma$trust + 
                 Turma$Gini + Turma$Latitude + Turma$law)
bptest(modelo_a)

# stargazer(modelo_a, coeftest(modelo_a, vcovHC(modelo_a, type = "HC0")),
#           type="text", out="default.htm", 
#           covariate.labels=c("Intercepto", "Inserção Internacional",
#                              "Índice de Confiança", "Índice de Gini",
#                              "Latitude",
#                              "Índice Law and Order"),
#           dep.var.caption =c("log da PTF"),
#           intercept.bottom = FALSE,
#           single.row = TRUE,
#           title = "PTF contra proxies",
#           model.names = FALSE,
#           omit.table.layout = "#")
# Gráficos de Normalidade dos Resíduos
par(mfrow=c(2,2))
qqnorm(residuals(modelo_y), main = "PIB contra proxies")  
qqline(residuals(modelo_y))
qqnorm(residuals(modelo_k), main = "Capital contra proxies")  
qqline(residuals(modelo_k))
qqnorm(residuals(modelo_h), main = "Capital Humano contra proxies")  
qqline(residuals(modelo_h))
qqnorm(residuals(modelo_a), main = "PTF contra proxies")  
qqline(residuals(modelo_a))
# Apresentando todos os gráficos em uma tabela só
# Primeiro pegando o número de coeficientes no modelo
row_count <- nrow(coeftest(modelo_y, vcovHC(modelo_y, type = "HC0")))
# Então atualizando a lista de coeficientes com os erros de White
erros <- list(coeftest(modelo_y, vcovHC(modelo_y, 
                                               type = "HC0"))[1:row_count,2])
erros <- c(erros, list(coeftest(modelo_k, vcovHC(modelo_k,
                                                 type = "HC0"))[1:row_count,2]))
erros <- c(erros, list(coeftest(modelo_h,vcovHC(modelo_h,
                                                type = "HC0"))[1:row_count,2]))
erros <- c(erros, list(coeftest(modelo_a,vcovHC(modelo_a,
                                                type = "HC0"))[1:row_count,2]))
stargazer(modelo_y,
          modelo_k,
          modelo_h,
          modelo_a,
          se = erros,
          type="text", out="default.htm", 
          covariate.labels=c("Intercepto", "Inserção Internacional",
                             "Índice de Confiança", "Índice de Gini",
                             "Latitude",
                             "Índice Law and Order", "Dummy do País"),
          dep.var.caption = c("Comparação regressão com proxies"),
          dep.var.labels = c("Log Pib"),
          column.labels = c("Log(y)", "Log(k)","Log(h)","Log(PTF)"),
          model.names = FALSE,
          intercept.bottom = FALSE,
          single.row = FALSE,
          no.space = TRUE,
          title = "Diferentes Variáveis Dependentes",
          omit.table.layout = "d#",
          notes = c("Erros padrão Robustos de White"),
          omit.stat = c("ser","f"))

# Repetindo os modelos com as dummies países específicos # ----


# Vetor de Dummies
# Lembre-se de colocar na Lista o(s) nome(s) do(s) seu(s) país(es)
# Igual está na tabela da turma
# Exemplo: Lista <- c('Japão')
# Exemplo 2: Lista <- c('Nigéria', 'Israel)
Lista <- c('Austrália','USA')
for(pais in Lista){
# Regredindo
assign(paste("modelo_dum_",pais, sep=""), lm(log(Turma$y) ~ log(Turma$s) + log(Turma$n+g+d) 
                                  + dum_pais[ , pais]))

#Ajustando por Heteroscedasticidade
bptest(get(paste("modelo_dum_",pais, sep="")))
qqnorm(residuals(get(paste("modelo_dum_",pais, sep=""))), main = "Solow com dummy para país")  
qqline(residuals(get(paste("modelo_dum_",pais, sep=""))))

# Uma forma mais apresentável de visualizar os resutlados da regressão
# stargazer(modelo_y, coeftest(get(paste("modelo_dum_",pais, sep="")), vcovHC(get(paste("modelo_dum_",pais, sep="")), type = "HC0")),
#           type="text", out="default.htm", 
#           covariate.labels=c("Intercepto", "log(s)","log(n+g+d)", pais),
#           dep.var.caption = paste("log do PIB por trabalhador", pais),
#           dep.var.labels = c("OLS","Ajustado por White"),
#           model.names = FALSE,
#           intercept.bottom = FALSE,
#           single.row = TRUE,
#           title = "Solow com dummy para país")


# Repetindo os passos regredindo log y contra s, n e escolaridade
assign(paste("modelo_dum_esc",pais, sep=""), lm(log(Turma$y) ~ log(Turma$s) + log(Turma$n+g+d) 
                                             + Turma$escola + dum_pais[ , pais]))
bptest(get(paste("modelo_dum_esc",pais, sep="")))
qqnorm(residuals(get(paste("modelo_dum_esc",pais, sep=""))), main = "MRW com dummy para país")  
qqline(residuals(get(paste("modelo_dum_esc",pais, sep=""))))
# stargazer(get(paste("modelo_dum_esc",pais, sep="")), 
#           coeftest(get(paste("modelo_dum_esc",pais, sep="")), vcovHC(get(paste("modelo_dum_esc",pais, sep="")), type = "HC0")),
#           type="text", out="default.htm", 
#           covariate.labels=c("Intercepto", "log(s)",
#                              "log(n+g+d)","Escolaridade", pais),
#           dep.var.caption = paste("log do PIB por trabalhador", pais),
#           dep.var.labels = c("OLS","Ajustado por White"),
#           model.names = FALSE,
#           intercept.bottom = FALSE,
#           single.row = TRUE,
#           title = "MRW com dummy para país")

# Repetindo os passos regredindo log y contra as proxies todas
assign(paste("modelo_dum_y",pais, sep=""), lm(log(Turma$k) ~ Turma$trade 
                                              + Turma$trust + Turma$Gini 
                                              + Turma$Latitude + Turma$law 
                                              + dum_pais[ , pais]))
bptest(get(paste("modelo_dum_y",pais, sep="")))
qqnorm(residuals(get(paste("modelo_dum_y",pais, sep=""))), main = "Capital contra proxies e dummy")  
qqline(residuals(get(paste("modelo_dum_y",pais, sep=""))))
# stargazer(get(paste("modelo_dum_y",pais, sep="")), 
#           coeftest(get(paste("modelo_dum_y",pais, sep="")), vcovHC(get(paste("modelo_dum_y",pais, sep="")), type = "HC0")),
#           type="text", out="default.htm", 
#           covariate.labels=c("Intercepto", "Inserção Internacional",
#                              "Índice de Confiança", "Índice de Gini",
#                              "Latitude",
#                              "Índice Law and Order", pais),
#           dep.var.caption = paste("Log do PIB por trabalhador", pais),
#           dep.var.labels = c("OLS","Ajustado por White"),
#           model.names = FALSE,
#           intercept.bottom = FALSE,
#           single.row = TRUE,
#           title = "Pib contra proxies e dummy")

# Repetindo regressão das proxies contra log k
assign(paste("modelo_dum_k",pais, sep=""), lm(log(Turma$k) ~ Turma$trade 
                                              + Turma$trust + Turma$Gini 
                                              + Turma$Latitude + Turma$law 
                                              + dum_pais[ , pais]))
bptest(get(paste("modelo_dum_k",pais, sep="")))
qqnorm(residuals(get(paste("modelo_dum_k",pais, sep=""))), main = "Capital contra proxies e dummy")  
qqline(residuals(get(paste("modelo_dum_k",pais, sep=""))))
# stargazer(get(paste("modelo_dum_k",pais, sep="")), 
#           coeftest(get(paste("modelo_dum_k",pais, sep="")), vcovHC(get(paste("modelo_dum_k",pais, sep="")), type = "HC0")),
#           type="text", out="default.htm", 
#           covariate.labels=c("Intercepto", "Inserção Internacional",
#                              "Índice de Confiança", "Índice de Gini",
#                              "Latitude",
#                              "Índice Law and Order", pais),
#           dep.var.caption = paste("Log do Capital por trabalhador", pais),
#           dep.var.labels = c("OLS","Ajustado por White"),
#           model.names = FALSE,
#           intercept.bottom = FALSE,
#           single.row = TRUE,
#           title = "Capital contra proxies e dummy")

# Repetindo regressão das proxies contra log h
assign(paste("modelo_dum_h",pais, sep=""), lm(log(Turma$h) ~ Turma$trade 
                                              + Turma$trust + Turma$Gini 
                                              + Turma$Latitude + Turma$law
                                              + dum_pais[ , pais]))
bptest(get(paste("modelo_dum_h",pais, sep="")))
qqnorm(residuals(get(paste("modelo_dum_h",pais, sep=""))), main = "Capital Humano contra proxies e dummy")  
qqline(residuals(get(paste("modelo_dum_h",pais, sep=""))))
# stargazer(get(paste("modelo_dum_h",pais, sep="")), 
#           coeftest(get(paste("modelo_dum_h",pais, sep="")), vcovHC(get(paste("modelo_dum_h",pais, sep="")), type = "HC0")),
#           type="text", out="default.htm", 
#           covariate.labels=c("Intercepto", "Inserção Internacional",
#                              "Índice de Confiança", "Índice de Gini",
#                              "Latitude",
#                              "Índice Law and Order", pais),
#           dep.var.caption = paste("log do Capital Humano por trabalhador", pais),
#           dep.var.labels = c("OLS","Ajustado por White"),
#           model.names = FALSE,
#           intercept.bottom = FALSE,
#           single.row = TRUE,
#           title = "Capital Humano contra proxies e dummy")

# Repetindo regressão das proxies contra log PTF
assign(paste("modelo_dum_a",pais, sep=""), lm(log(Turma$PTF) ~ Turma$trade 
                                              + Turma$trust + Turma$Gini 
                                              + Turma$Latitude + Turma$law
                                              + dum_pais[ , pais]))
bptest(get(paste("modelo_dum_a",pais, sep="")))
qqnorm(residuals(get(paste("modelo_dum_a",pais, sep=""))), main = "PTF contra proxies e dummy")  
qqline(residuals(get(paste("modelo_dum_a",pais, sep=""))))
# stargazer(get(paste("modelo_dum_a",pais, sep="")), 
#           coeftest(get(paste("modelo_dum_a",pais, sep="")), vcovHC(get(paste("modelo_dum_a",pais, sep="")), type = "HC0")),
#           type="text", out="default.htm", 
#           covariate.labels=c("Intercepto", "Inserção Internacional",
#                              "Índice de Confiança", "Índice de Gini",
#                              "Latitude",
#                              "Índice Law and Order", pais),
#           dep.var.caption = paste("log da PTF", pais),
#           dep.var.labels = c("OLS","Ajustado por White"),
#           model.names = FALSE,
#           intercept.bottom = FALSE,
#           single.row = TRUE,
#           title = "PTF contra proxies e dummy")
}
# Pegando os coeficientes dinâmicos para colocar tudo num atabela só
# Primeiro o erro padrão de White do modelo padrão sem dummies
row_count <- nrow(coeftest(modelo_y, vcovHC(modelo_y, type = "HC0")))
erros_dum <- list(coeftest(modelo_y, vcovHC(modelo_y, type = "HC0"))[1:row_count,2])
# Agora os erros padrão das dummies
for (i in 1:length(Lista)) {
  row_count <- nrow(coeftest(get(paste("modelo_dum_y", Lista[i], sep="")), 
                             vcovHC(get(paste("modelo_dum_y", Lista[i], sep="")), 
                                    type = "HC0")))
  erros_dum <- c(erros_dum, 
                    list(coeftest(get(paste("modelo_dum_y", Lista[i], sep="")), 
                                 vcovHC(get(paste("modelo_dum_y", Lista[i], sep="")), 
                                         type = "HC0"))[1:row_count,2]))
}
stargazer(modelo_y,
          get(paste("modelo_dum_y", Lista[1], sep="")),
          get(paste("modelo_dum_y", Lista[2], sep="")),
          get(paste("modelo_dum_y", Lista[3], sep="")),
          get(paste("modelo_dum_y", Lista[4], sep="")),
          get(paste("modelo_dum_y", Lista[5], sep="")),
          se = erros_dum,
          type="text", out="default.htm", 
          covariate.labels=c("Intercepto", "Inserção Internacional",
                             "Índice de Confiança", "Índice de Gini",
                             "Latitude",
                             "Índice Law and Order", "Dummy do País"),
          dep.var.caption = c("Comparação modelos Dummy"),
          dep.var.labels = c("Log Pib"),
          column.labels = c("Sem Dummy", Lista),
          model.names = FALSE,
          intercept.bottom = FALSE,
          single.row = FALSE,
          no.space = TRUE,
          title = "Log(y) contra proxies e dummy",
          omit.table.layout = "d#",
          notes = c("Erros padrão Robustos de White"),
          omit.stat = c("ser","f"))
row_count <- nrow(coeftest(modelo_k, vcovHC(modelo_k, type = "HC0")))
erros_dum <- list(coeftest(modelo_k, vcovHC(modelo_k, type = "HC0"))[1:row_count,2])
# Agora os erros padrão das dummies
for (i in 1:length(Lista)) {
  row_count <- nrow(coeftest(get(paste("modelo_dum_k", Lista[i], sep="")), 
                             vcovHC(get(paste("modelo_dum_k", Lista[i], sep="")), 
                                    type = "HC0")))
  erros_dum <- c(erros_dum, 
                 list(coeftest(get(paste("modelo_dum_k", Lista[i], sep="")), 
                               vcovHC(get(paste("modelo_dum_k", Lista[i], sep="")), 
                                      type = "HC0"))[1:row_count,2]))
}
stargazer(modelo_k,
          get(paste("modelo_dum_k", Lista[1], sep="")),
          get(paste("modelo_dum_k", Lista[2], sep="")),
          get(paste("modelo_dum_k", Lista[3], sep="")),
          get(paste("modelo_dum_k", Lista[4], sep="")),
          get(paste("modelo_dum_k", Lista[5], sep="")),
          se = erros_dum,
          type="text", out="default.htm", 
          covariate.labels=c("Intercepto", "Inserção Internacional",
                             "Índice de Confiança", "Índice de Gini",
                             "Latitude",
                             "Índice Law and Order", "Dummy do País"),
          dep.var.caption = c("Comparação modelos Dummy"),
          dep.var.labels = c("Log k"),
          column.labels = c("Sem Dummy", Lista),
          model.names = FALSE,
          intercept.bottom = FALSE,
          single.row = FALSE,
          no.space = TRUE,
          title = "Log(k) contra proxies e dummy",
          omit.table.layout = "d#",
          notes = c("Erros padrão Robustos de White"),
          omit.stat = c("ser","f"))
row_count <- nrow(coeftest(modelo_h, vcovHC(modelo_h, type = "HC0")))
erros_dum <- list(coeftest(modelo_h, vcovHC(modelo_h, type = "HC0"))[1:row_count,2])
# Agora os erros padrão das dummies
for (i in 1:length(Lista)) {
  row_count <- nrow(coeftest(get(paste("modelo_dum_h", Lista[i], sep="")), 
                             vcovHC(get(paste("modelo_dum_h", Lista[i], sep="")), 
                                    type = "HC0")))
  erros_dum <- c(erros_dum, 
                 list(coeftest(get(paste("modelo_dum_h", Lista[i], sep="")), 
                               vcovHC(get(paste("modelo_dum_h", Lista[i], sep="")), 
                                      type = "HC0"))[1:row_count,2]))
}
stargazer(modelo_h,
          get(paste("modelo_dum_h", Lista[1], sep="")),
          get(paste("modelo_dum_h", Lista[2], sep="")),
          get(paste("modelo_dum_h", Lista[3], sep="")),
          get(paste("modelo_dum_h", Lista[4], sep="")),
          get(paste("modelo_dum_h", Lista[5], sep="")),
          se = erros_dum,
          type="text", out="default.htm", 
          covariate.labels=c("Intercepto", "Inserção Internacional",
                             "Índice de Confiança", "Índice de Gini",
                             "Latitude",
                             "Índice Law and Order", "Dummy do País"),
          dep.var.caption = c("Comparação modelos Dummy"),
          dep.var.labels = c("Log h"),
          column.labels = c("Sem Dummy", Lista),
          model.names = FALSE,
          intercept.bottom = FALSE,
          single.row = FALSE,
          no.space = TRUE,
          title = "Log(h) contra proxies e dummy",
          omit.table.layout = "d#",
          notes = c("Erros padrão Robustos de White"),
          omit.stat = c("ser","f"))
row_count <- nrow(coeftest(modelo_a, vcovHC(modelo_a, type = "HC0")))
erros_dum <- list(coeftest(modelo_a, vcovHC(modelo_a, type = "HC0"))[1:row_count,2])
# Agora os erros padrão das dummies
for (i in 1:length(Lista)) {
  row_count <- nrow(coeftest(get(paste("modelo_dum_a", Lista[i], sep="")), 
                             vcovHC(get(paste("modelo_dum_a", Lista[i], sep="")), 
                                    type = "HC0")))
  erros_dum <- c(erros_dum, 
                 list(coeftest(get(paste("modelo_dum_a", Lista[i], sep="")), 
                               vcovHC(get(paste("modelo_dum_a", Lista[i], sep="")), 
                                      type = "HC0"))[1:row_count,2]))
}
stargazer(modelo_a,
          get(paste("modelo_dum_a", Lista[1], sep="")),
          get(paste("modelo_dum_a", Lista[2], sep="")),
          get(paste("modelo_dum_a", Lista[3], sep="")),
          get(paste("modelo_dum_a", Lista[4], sep="")),
          get(paste("modelo_dum_a", Lista[5], sep="")),
          se = erros_dum,
          type="text", out="default.htm", 
          covariate.labels=c("Intercepto", "Inserção Internacional",
                             "Índice de Confiança", "Índice de Gini",
                             "Latitude",
                             "Índice Law and Order", "Dummy do País"),
          dep.var.caption = c("Comparação modelos Dummy"),
          dep.var.labels = c("Log PTF"),
          column.labels = c("Sem Dummy", Lista),
          model.names = FALSE,
          intercept.bottom = FALSE,
          single.row = FALSE,
          no.space = TRUE,
          title = "log(PTF) contra proxies e dummy",
          omit.table.layout = "d#",
          notes = c("Erros padrão Robustos de White"),
          omit.stat = c("ser","f"))

# Dummies Para os Continentes ----
library("WDI")
library(data.table)
continentes <- read.csv("continentes.csv")
rownames(continentes) <- continentes$País
continentes$País <- NULL
continentes$ameŕica_norte <- NULL

# Rodando modelo utilizando o crescimento da América do Norte (USA + Canadá) 
# como a dummy base
modelo_cont <- lm(log(Turma$y) ~ continentes$america_latina 
                  + continentes$europa + continentes$africa
                  + continentes$ásia)
qqnorm(residuals(modelo_cont), main = "PIB contra dummy para continentes")  
qqline(residuals(modelo_cont))
stargazer(modelo_cont, 
          coeftest(modelo_cont, vcovHC(modelo_cont, type = "HC0")),
          type="text", out="default.htm", 
          covariate.labels=c("América do Norte", "América Latina", 
                             "Europa", "África", "Ásia"),
          dep.var.caption = paste("log do PIB"),
          dep.var.labels = c("OLS","Ajustado por White"),
          model.names = FALSE,
          intercept.bottom = FALSE,
          single.row = TRUE,
          title = "PIB contra dummy para continentes")

# Nova Escolaridade ----
# Nova variável para escola calculando 
# conforme o método original proposto em MRW
# Tem-se a proporção da população na idade do nível "secondary" que está 
# estudando no nível "secondary". Então essa proporção é multiplicada pela 
# proporção da população na idade de trabalhar que também está na idade do nível
# "secondary" de educação.
# Pegando número de empregados
pwt10 <- read_xlsx("pwt100.xlsx", sheet = 3)
pwt10 <- select(pwt10, any_of(c('country','year','emp')))
pwt10 <- subset(pwt10, pwt10$year == 2010)
pwt10$year <- NULL
paises <- read.csv("pais.csv")
paises[4, 1] <- c("China, Hong Kong SAR")
eng_pais <- c(as.list(paises[,1]))
pwt10 <- pwt10[c(pwt10$country %in% eng_pais),]

# A proporção da pop do "secondary" que está no "secondary"
# Dados obtidos através da UNESCO (UIS)
second_ratio <- read.csv("second_ratio.csv")
second_ratio$NATMON_IND <- NULL
second_ratio$Indicator <- NULL
second_ratio$LOCATION <- NULL
second_ratio$Time <- NULL
second_ratio$TIME <- NULL
second_ratio$Flag.Codes <- NULL
second_ratio$Flags <- NULL
second_ratio <-second_ratio[c(second_ratio$Country %in% eng_pais),]
# Adicionando países faltantes ou que possuem nomes não condizentes
second_ratio <- rbind(second_ratio, c("Russia", "101.08"))
second_ratio <- rbind(second_ratio, c("China, Hong Kong SAR", "88.03878"))
second_ratio <- rbind(second_ratio, c("United Kingdom", "102.90468"))
second_ratio <- rbind(second_ratio, c("United States", "94.84594"))
second_ratio <- second_ratio[!(second_ratio$Country %in% 
                                 c("China, Hong Kong Special Administrative Region",
                                   "United States of America", 
                                   "United Kingdom of Great Britain and Northern Ireland")),]

# Dados da população em idade "secondary"
eng_pais <- c(eng_pais)

second_pop <- read.csv("second_pop.csv")
second_pop$NATMON_IND <- NULL
second_pop <-second_pop[c(second_pop$Indicator %in% "School age population, secondary education, both sexes (number)"),]
second_pop$Indicator <- NULL
second_pop$LOCATION <- NULL
second_pop$Time <- NULL 
second_pop$TIME <- NULL
second_pop$Flag.Codes <- NULL
second_pop$Flags <- NULL
second_pop <-second_pop[c(second_pop$Country %in% eng_pais),]
second_pop <- rbind(second_pop, c("China, Hong Kong SAR", "577324"))
second_pop <- rbind(second_pop, c("United Kingdom", "5381903"))
second_pop <- rbind(second_pop, c("United States", "25507455"))
second_pop <- second_pop[!(second_pop$Country %in% 
                             c("China, Hong Kong Special Administrative Region",
                               "United States of America", 
                               "United Kingdom of Great Britain and Northern Ireland")),]
# Tudo numa tabela só
# Fazendo com que todos tenham os mesmos países e ordenados na mesma ordem e 
# mesmo nome de coluna
# Atualizando o vetor de países com dados

paises <- paises[c(paises$Country %in% as.list(second_pop$Country)),]
pwt10 <- pwt10[c(pwt10$country %in% as.list(second_pop$Country)),]
second_pop <- second_pop[order(second_pop$Country),]
second_ratio <- second_ratio[order(second_ratio$Country),]
colnames(pwt10)[colnames(pwt10)
                == "country"] <- "Country"
pwt10 <- pwt10[order(pwt10$Country),]
colnames(second_pop)[colnames(second_pop)
                     == "Value"] <- "pop"
colnames(second_ratio)[colnames(second_ratio)
                       == "Value"] <- "ratio"
second_pop[2] <- apply(second_pop[2], 2,function(x) as.numeric(as.character(x)))
second_ratio[2] <- apply(second_ratio[2], 2,function(x) as.numeric(as.character(x)))
second_pop[2] <- apply(second_pop[2], 2,function(x) as.numeric(as.character(x)))

# Calculando o novo escola de MRW
hc_tabela <- cbind(second_pop, second_ratio, pwt10)
hc_tabela$emp <- hc_tabela$emp*10^6
hc_tabela$hc <- c(hc_tabela$ratio*(hc_tabela$pop/hc_tabela$pop))

# Atualizando tabela com novo valor de escola
Turmahc <- cbind(Turma)
Turmahc <- Turmahc[c(Turmahc$País %in% as.list(paises$País)),]
Turmahc <- Turmahc[order(Turmahc$País),]
paises <- paises[order(paises$País),]
Turmahc <- Turmahc[order(paises$Country),]
paises <- paises[order(paises$Country),]
Turmahc <- cbind(Turmahc, hc_tabela$hc)
colnames(Turmahc)[colnames(Turmahc)
                  == "hc_tabela$hc"] <- "escolaMRW"
# Porquê pegar uma nova variável?
plot(Turmahc$h,Turmahc$escola)
plot(Turmahc$h, Turmahc$escolaMRW)
cor.test(Turmahc$h, Turmahc$escola)
cor.test(Turmahc$h, Turmahc$escolaMRW)
# Regressão com nova escolaridade
y <- log(Turmahc$y)
s <- log(Turmahc$s)
n <- log(Turmahc$n+g+d)
escola <- log(Turmahc$escolaMRW)
modelo_escMRW <- lm(y ~ s + n + escola)
escola <- log(Turmahc$escola)
modelo_esc <- lm(y ~ s + n + escola)
seMRW <- list(coeftest(modelo_escMRW, vcovHC(modelo_escMRW, type = "HC0"))[1:4,2])
seMRW <- c(seMRW, list(coeftest(modelo_esc, vcovHC(modelo_esc, type = "HC0"))[1:4,2]))
qqnorm(residuals(modelo_escMRW), main = "Modelo MRW com Escolaridade Corrigida")  
qqline(residuals(modelo_escMRW))
stargazer(modelo_escMRW, se = seMRW,
          modelo_esc,
          type="text", out="default.htm",
          covariate.labels=c("Intercepto", "log(s)","log(n+g+d)",
                             "Escolaridade"),
          dep.var.caption =c("Comparação entre valores diferentes para Escola"),
          dep.var.labels = c("Escola MRW"),
          model.names = FALSE,
          intercept.bottom = FALSE,
          single.row = TRUE,
          column.labels = c("Escola MRW", "Anos de Escolaridade"),
          omit.table.layout = "d#",
          notes = c("Erros padrão Robustos de White"))




