# Trabalho de Séries - ARIMA - ID 1211
setwd("D:/Users/Mathews/Documents/UNB_mestrado/Series/Trabalho_1")

# Pacotes
library(Mcomp)
library(forecast) 
library(tidyverse)
library(tseries)
library(xtable)
library(cowplot)

data(M3)


# ID - 1936
id <- 1936
M3[[id]]
h <- M3[[id]]$h
treino <- M3[[id]]$x
teste <- M3[[id]]$xx

M3[[id]]$period
M3[[id]]$st
M3[[id]]$type
M3[[id]]$n
# Apresentação da Série
autoplot(treino) +
  labs(x = "Ano", y = "Valor Observado") +
  theme_bw()+
  autolayer(teste, series="Teste") +
  scale_colour_manual(values = c("Teste" = "#A01400"), breaks = c("Teste"), name = "")

ggsave("imagens//descricao-original.png", width = 158, height = 93, units = "mm")

lambada <- treino %>% BoxCox.lambda()

treino_box <- treino %>% BoxCox(lambda = lambada)

autoplot(treino_box) +
  labs(x = "Ano", y = "Valor Observado Transformado") +
  theme_bw()

# Decomposição
decomp.stl <- stl(treino, s.window = 13)
autoplot(decomp.stl) +
  labs(x = "Ano") +
  theme_bw()
ggsave("imagens/decomposicao-stl.png", width = 158, height = 93, units = "mm")


# Teste de estacionaridade
kpss.test(treino)

# Teste de sazonalidade
ocsb.test(treino)


# Seleção manual do modelo ARIMA
ndiffs(treino) # ndiffs(diff(treino, lag = 4)) ## precisa da segunda diferença, mas não sazional
nsdiffs(treino) # nsdiffs(diff(treino))

ndiffs(treino_box) # ndiffs(diff(treino, lag = 4)) ## precisa da segunda diferença, mas não sazional
nsdiffs(treino_box) # nsdiffs(diff(treino))

cbind(`Série Original` = treino, `Primeiras Diferenças` = diff(treino), `Primeiras Diferenças e Sazonais` = diff(diff(treino), lag = 4)) %>% 
  autoplot(facets = T) +
  labs(x = "Ano", y = "Valor Observado") +
  theme_bw() +
  ggsave("Graficos/selecao-diff.png", width = 158, height = 93, units = "mm")

d_treino <- diff(treino) # série estacionária seria a d_treino com apenas uma dif
# d_treino <- treino
#w <- diff(treino) %>% diff(lag=12)
ndiffs(d_treino) 
kpss.test(d_treino) ### depois de tirar a diferença é estacionário

nsdiffs(d_treino)

ocsb.test(d_treino)

par(mfrow=c(1,1))

plot_acf<- ggAcf(d_treino, lag.max = 12*3) +
  scale_y_continuous(limits = c(-1, 1)) +
  scale_x_continuous(breaks = seq(0,36,6))+
  labs(title = "") +
  theme_bw()

plot_pacf<- ggPacf(d_treino, lag.max = 12*3) +
  scale_y_continuous(limits = c(-1, 1)) +
  scale_x_continuous(breaks = seq(0,36,6))+
  labs(title = "") +
  theme_bw()


ggAcf(treino, lag.max = 12*3) +
  scale_y_continuous(limits = c(-1, 1)) +
  scale_x_continuous(breaks = c(1:36))+
  labs(title = "") +
  theme_bw()

ggPacf(treino, lag.max = 12*3) +
  scale_y_continuous(limits = c(-1, 1)) +
  scale_x_continuous(breaks = c(1:36))+
  labs(title = "") +
  theme_bw()



plot_grid(plot_acf, plot_pacf, ncol = 2)
ggsave("imagens/acf_pacf.png", width = 178, height = 93, units = "mm")


# Sabemos que d=1 e D=0
#
# Demais argumentos:
# p = 2 ou 1
# q = 2 ou 1
# P = 0     
# Q = 2 ou 1    

## Modelo candidato para {x}
# --> SARIMA (2,1,2) x (0,0,0)



### testador de modelos hahaha
melhor_AICc =Inf
for(p in 0:2){
  for(q in 0:2){
    for(Q in 1:2){
      fit = Arima(treino,order=c(p,1,q),seasonal=c(1,0,Q))
      if(fit$aicc < melhor_AICc){
        melhor_AICc = fit$aicc
        cat("p =" ,p, ", q =" ,q, ", P = " ,P,  ",Q = " ,Q, ", AICc =", fit$aicc, "\n")
      } 
    }
  }
}

#Arima(treino,order=c(0,1,2),seasonal=c(1,0,2))
#Arima(treino,order=c(0,1,2),seasonal=c(1,0,1))


mod_arima <- Arima(treino, order = c(0, 1, 1), seasonal = c(1,0,2))
mod_arima

mode_auto_arima <- auto.arima(treino)

res1 <- residuals(mod_arima)

res_auto <- residuals( mode_auto_arima)

mod_box_cox <- auto.arima(treino,lambda = lambada)

res_cox <- residuals(mod_box_cox)

shapiro.test(res_cox)

kpss.test(res1)

Box.test(res1,lag=20,type = 'Ljung-Box')

shapiro.test(res_auto)

kpss.test(res_auto)

Box.test(res_auto, lag=20, type = 'Ljung-Box', fitdf=4)

####### Análise de resíduos do ARIMA #########

### Manual 

res1 <- residuals(mod_arima)


# Estacionaridade
residual <- autoplot(res1) +
            labs(x = "Ano", y = "Resíduos") +
            theme_bw()

kpss.test(res1)

# Independência
ACF_res <- ggAcf(res1, lag.max = 4*5) +
           scale_y_continuous(limits = c(-.5, .5)) +
           labs(title = "") + theme_bw() 

PACF_res <- ggPacf(res1, lag.max = 4*5) +
            scale_y_continuous(limits = c(-.5, .5)) +
            labs(title = "") + theme_bw() 

Box.test(res1, lag = 20, type = "Ljung-Box")


# Normalidade
QQplot_res <- data.frame(res1) %>%
              ggplot(aes(sample = res1)) +
              stat_qq() +
              stat_qq_line() + labs(x = "Quantis Teóricos", y = "Quantis Amostrais") + theme_bw() 


shapiro.test(res1)


plot_grid(residual, QQplot_res,ACF_res,PACF_res, nrow = 2)
ggsave("imagens/residuos_arima.png", width = 178, height = 93, units = "mm")



### Auto_arima 

res_auto <- residuals(mode_auto_arima)


# Estacionaridade
residual <- autoplot(res_auto) +
  labs(x = "Ano", y = "Resíduos") +
  theme_bw()

kpss.test(res_auto)

# Independência
ACF_res <- ggAcf(res_auto, lag.max = 4*5) +
  scale_y_continuous(limits = c(-.5, .5)) +
  labs(title = "") + theme_bw() 

PACF_res <- ggPacf(res_auto, lag.max = 4*5) +
  scale_y_continuous(limits = c(-.5, .5)) +
  labs(title = "") + theme_bw() 

Box.test(res_auto, lag = 20, type = "Ljung-Box")


# Normalidade
QQplot_res <- data.frame(res_auto) %>%
  ggplot(aes(sample = res_auto)) +
  stat_qq() +
  stat_qq_line() + labs(x = "Quantis Teóricos", y = "Quantis Amostrais") + theme_bw() 

shapiro.test(res_auto)

plot_grid(residual, QQplot_res,ACF_res,PACF_res, nrow = 2)
ggsave("imagens/residuos_auto.png", width = 178, height = 93, units = "mm")


### Transformada Box-Cox 

res_cox <- residuals(mod_box_cox)

# Estacionaridade
residual <- autoplot(res_cox) +
  labs(x = "Ano", y = "Resíduos") +
  theme_bw()

kpss.test(res_cox)

# Independência
ACF_res <- ggAcf(res_cox, lag.max = 4*5) +
  scale_y_continuous(limits = c(-.5, .5)) +
  labs(title = "") + theme_bw() 

PACF_res <- ggPacf(res_cox, lag.max = 4*5) +
  scale_y_continuous(limits = c(-.5, .5)) +
  labs(title = "") + theme_bw() 

Box.test(res_cox, lag = 20, type = "Ljung-Box")

# Normalidade
QQplot_res <- data.frame(res_cox) %>%
  ggplot(aes(sample = res_cox)) +
  stat_qq() +
  stat_qq_line() + labs(x = "Quantis Teóricos", y = "Quantis Amostrais") + theme_bw() 

shapiro.test(res_cox)

plot_grid(residual, QQplot_res,ACF_res,PACF_res, nrow = 2)
ggsave("imagens/residuos_cox.png", width = 178, height = 93, units = "mm")




######## Seleção manual do modelo ETS ######

ets1 <- ets(treino, model = "AAA", damped = FALSE, restrict = FALSE) # ETS(A,A ,A)   
ets2 <- ets(treino, model = "AAA", damped = TRUE, restrict = FALSE)  # ETS(A,Ad,A)  
ets3 <- ets(treino, model = "AMA", damped = FALSE, restrict = FALSE) # ETS(A,M ,A)
ets4 <- ets(treino, model = "AMA", damped = TRUE, restrict = FALSE)  # ETS(A,Md,A)
ets5 <- ets(treino, model = "MAA", damped = FALSE, restrict = FALSE) # ETS(M,A ,A)
ets6 <- ets(treino, model = "MAA", damped = TRUE, restrict = FALSE)  # ETS(M,Ad,A)
ets7 <- ets(treino, model = "MMA", damped = FALSE, restrict = FALSE) # ETS(M,M ,A)
ets8 <- ets(treino, model = "MMA", damped = TRUE, restrict = FALSE)  # ETS(M,Md,A)
ets9 <- ets(treino, model = "MAM", damped = FALSE, restrict = FALSE) # ETS(M,A ,M)
ets10 <- ets(treino, model = "MAM", damped = TRUE, restrict = FALSE)  # ETS(M,Ad,M)
ets11 <- ets(treino, model = "MMM", damped = FALSE, restrict = FALSE) # ETS(M,M ,M)
ets12 <- ets(treino, model = "MMM", damped = TRUE, restrict = FALSE)  # ETS(M,Md,M)


aics<- rbind(ets1$aic,ets2$aic,ets3$aic,ets4$aic,ets5$aic,ets6$aic,
             ets7$aic,ets8$aic,ets9$aic,ets10$aic,ets11$aic,ets12$aic)

bics<- rbind(ets1$bic,ets2$bic,ets3$bic,ets4$bic,ets5$bic,ets6$bic,
             ets7$bic,ets8$bic,ets9$bic,ets10$bic,ets11$bic,ets12$bic)

aiccs<- rbind(ets1$aicc,ets2$aicc,ets3$aicc,ets4$aicc,ets5$aicc,ets6$aicc,
              ets7$aicc,ets8$aicc,ets9$aicc,ets10$aicc,ets11$aicc,ets12$aicc)

Criterios_Ets <- data.frame(aics,aiccs,bics)
Criterios_Ets$modelos <- c("AAA","AAdA","AMA",'AMdA','MAA','MAdA','MMA','MMdA','MAM','MAdM','MMM','MMdM')
Criterios_Ets$mods <- c(1:12)
Criterios_Ets <- Criterios_Ets %>% arrange(aiccs)
Criterios_Ets <- Criterios_Ets[,-5]
Criterios_Ets <- Criterios_Ets[1:4,]

xtable(Criterios_Ets)

#### transformação 

ets1 <- ets(treino, model = "AAA", damped = FALSE, restrict = FALSE, lambda = lambada) # ETS(A,A ,A)   
ets2 <- ets(treino, model = "AAA", damped = TRUE, restrict = FALSE, lambda = lambada)  # ETS(A,Ad,A)  
ets3 <- ets(treino, model = "AMA", damped = FALSE, restrict = FALSE, lambda = lambada) # ETS(A,M ,A)
ets4 <- ets(treino, model = "AMA", damped = TRUE, restrict = FALSE, lambda = lambada)  # ETS(A,Md,A)
ets5 <- ets(treino, model = "MAA", damped = FALSE, restrict = FALSE, lambda = lambada) # ETS(M,A ,A)
ets6 <- ets(treino, model = "MAA", damped = TRUE, restrict = FALSE, lambda = lambada)  # ETS(M,Ad,A)
ets7 <- ets(treino, model = "MMA", damped = FALSE, restrict = FALSE, lambda = lambada) # ETS(M,M ,A)
ets8 <- ets(treino, model = "MMA", damped = TRUE, restrict = FALSE, lambda = lambada)  # ETS(M,Md,A)
ets9 <- ets(treino, model = "MAM", damped = FALSE, restrict = FALSE, lambda = lambada) # ETS(M,A ,M)
ets10 <- ets(treino, model = "MAM", damped = TRUE, restrict = FALSE, lambda = lambada)  # ETS(M,Ad,M)
ets11 <- ets(treino, model = "MMM", damped = FALSE, restrict = FALSE, lambda = lambada) # ETS(M,M ,M)
ets12 <- ets(treino, model = "MMM", damped = TRUE, restrict = FALSE, lambda = lambada)  # ETS(M,Md,M)


aics<- rbind(ets1$aic,ets2$aic,ets3$aic,ets4$aic,ets5$aic,ets6$aic,
             ets7$aic,ets8$aic,ets9$aic,ets10$aic,ets11$aic,ets12$aic)

bics<- rbind(ets1$bic,ets2$bic,ets3$bic,ets4$bic,ets5$bic,ets6$bic,
             ets7$bic,ets8$bic,ets9$bic,ets10$bic,ets11$bic,ets12$bic)

aiccs<- rbind(ets1$aicc,ets2$aicc,ets3$aicc,ets4$aicc,ets5$aicc,ets6$aicc,
              ets7$aicc,ets8$aicc,ets9$aicc,ets10$aicc,ets11$aicc,ets12$aicc)

Criterios_Ets <- data.frame(aics,aiccs,bics)
Criterios_Ets$modelos <- c("AAA","AAdA","AMA",'AMdA','MAA','MAdA','MMA','MMdA','MAM','MAdM','MMM','MMdM')
Criterios_Ets$mods <- c(1:12)
Criterios_Ets <- Criterios_Ets %>% arrange(aiccs)
xtable(Criterios_Ets)


###### Análise de resíduos do ETS #################

mod_ets <- ets(treino, model = "MAM", damped = TRUE, restrict = FALSE) # ETS(M,Ad,M)

res_ets <- residuals(mod_ets)

# Estacionaridade
residual <- autoplot(res_ets) +
  labs(x = "Ano", y = "Resíduos") +
  theme_bw()

kpss.test(res_ets)

# Independência
ACF_res <- ggAcf(res_ets, lag.max = 4*5) +
  scale_y_continuous(limits = c(-.5, .5)) +
  labs(title = "") + theme_bw() 

PACF_res <- ggPacf(res_ets, lag.max = 4*5) +
  scale_y_continuous(limits = c(-.5, .5)) +
  labs(title = "") + theme_bw() 

Box.test(res_ets, lag = 20, type = "Ljung-Box")

# Normalidade
QQplot_res <- data.frame(res_ets) %>%
  ggplot(aes(sample = res_ets)) +
  stat_qq() +
  stat_qq_line() + labs(x = "Quantis Teóricos", y = "Quantis Amostrais") + theme_bw() 

shapiro.test(res_ets)

plot_grid(residual, QQplot_res,ACF_res,PACF_res, nrow = 2)
ggsave("imagens/residuos_ets.png", width = 178, height = 93, units = "mm")


### modelo ETS Box-Cox  

mod_ets_cox <- ets(treino, model = "AMA", damped = FALSE, restrict = FALSE, lambda = lambada) # ETS(A,M ,A)

res_ets_cox <- residuals(mod_ets_cox)

# Estacionaridade

residual <- autoplot(res_ets_cox) +
  labs(x = "Ano", y = "Resíduos") +
  theme_bw()

kpss.test(res_ets_cox)

# Independência
ACF_res <- ggAcf(res_ets_cox, lag.max = 4*5) +
  scale_y_continuous(limits = c(-.5, .5)) +
  labs(title = "") + theme_bw() 

PACF_res <- ggPacf(res_ets_cox, lag.max = 4*5) +
  scale_y_continuous(limits = c(-.5, .5)) +
  labs(title = "") + theme_bw() 

Box.test(res_ets_cox, lag = 20, type = "Ljung-Box")

# Normalidade
QQplot_res <- data.frame(res_ets_cox) %>%
  ggplot(aes(sample = res_ets_cox)) +
  stat_qq() +
  stat_qq_line() + labs(x = "Quantis Teóricos", y = "Quantis Amostrais") + theme_bw() 

shapiro.test(res_ets_cox)

plot_grid(residual, QQplot_res,ACF_res,PACF_res, nrow = 2)
ggsave("imagens/residuos_ets_cox.png", width = 178, height = 93, units = "mm")




##### Janelas Deslizantes ######

n <- M3[[1936]]$n

#### ARIMA
f_arima <- function(y, h){
  fit = arima(y, order = c(0, 1, 1), seasonal = c(1,0,2))
  forecast(fit, h)
}

CV_arima = tsCV(y=treino, forecastfunction=f_arima, h=5, window = 14)
MAE_arima = CV_arima %>% abs() %>% colMeans(na.rm=T)

f_arima_cox <- function(y, h){
  fit = auto.arima(y,lambda = lambada)
  forecast(fit, h)
}

CV_arima_cox = tsCV(y=treino, forecastfunction=f_arima_cox, h=5, window = 14)
MAE_arima_cox = CV_arima_cox %>% abs() %>% colMeans(na.rm=T)


#### ETS 
f_ets <- function(y, h){
  fit = ets(treino, model = "MAM", damped = TRUE, restrict = FALSE) # ETS(M,Ad,M)
  forecast(fit, h)
}

CV_ets = tsCV(y=treino, forecastfunction=f_ets, h=5, window = 14)
MAE_ets = CV_ets %>% abs() %>% colMeans(na.rm=T)

f_ets_cox <- function(y, h){
  fit =  ets(treino, model = "AMA", damped = FALSE, restrict = FALSE, lambda = lambada)
  forecast(fit, h)
}

CV_ets_cox = tsCV(y=treino, forecastfunction=f_ets_cox, h=5, window = 14)
MAE_ets_cox = CV_ets_cox %>% abs() %>% colMeans(na.rm=T)

H <- c(1,2,3,4,5)

resultados <-  cbind(MAE_ets_cox,MAE_ets, MAE_arima, MAE_arima_cox,H) %>% as.data.frame()

df_plot <- resultados %>% pivot_longer(cols = starts_with("MAE"),
                                       names_to = "Modelos",
                                       names_prefix = "MAE_",
                                       values_to = "MAE",
                                       values_drop_na = T)


df_plot %>%
  ggplot(aes(x=H, y=MAE, group=Modelos, color=Modelos)) +
  geom_line(size=1.2) +
  scale_color_manual(labels=c('ARIMA', "ARIMA BOX-COX", "ETS(M,Ad,M)", "ETS(A,M,A) BOX-COX"),values = c(1,2,3,4,5))+
  xlab('Horizonte de previsão')+
  theme_bw() 

ggsave("imagens/MAE_janelas_deslizantes.png", width = 178, height = 93, units = "mm")


cbind(MAE_ets_cox,MAE_ets, MAE_arima, MAE_arima_cox,H) %>% xtable()

##### Previsões Pontuais e Intervalares ######

arima_preds <- forecast(mod_arima, h = h, level = c(95))
autoplot(treino) + xlab("Ano") + ylab("Valor Observado") +
  theme_bw() +
  autolayer(arima_preds, series="ARIMA") +
  autolayer(teste, series="Observações") +
  scale_colour_manual(values = c("ARIMA" = "#8B0000", "Observações" = "black"), breaks = c("ARIMA", "Observações"), name = "") 
ggsave("imagens/preds_arima.png", width = 158, height = 93, units = "mm")

arima_box_preds <- forecast(mod_box_cox, h = h, level = c(95))
autoplot(treino) + xlab("Ano") + ylab("Valor Observado") +
  theme_bw() +
  autolayer(arima_box_preds, series="ARIMA Box-Cox") +
  autolayer(teste, series="Observações") +
  scale_colour_manual(values = c("ARIMA Box-Cox" = "#8B0000", "Observações" = "black"), breaks = c("ARIMA Box-Cox", "Observações"), name = "") 
ggsave("imagens/preds_arima_cox.png", width = 158, height = 93, units = "mm")


ets_preds <- forecast(mod_ets, h = h, level = c(95))
autoplot(treino) + xlab("Ano") + ylab("Valor Observado") +
  theme_bw() +
  autolayer(ets_preds, series="ETS") +
  autolayer(teste, series="Observações") +
  scale_colour_manual(values = c("ETS" = "#8B0000", "Observações" = "black"), breaks = c("ETS", "Observações"), name = "") 
ggsave("imagens/preds_ets.png", width = 158, height = 93, units = "mm")


ets_cox_preds <- forecast(mod_ets_cox, h = h, level = c(95))
autoplot(treino) + xlab("Ano") + ylab("Valor Observado") +
  theme_bw() +
  autolayer(ets_cox_preds, series="ETS") +
  autolayer(teste, series="Observações") +
  scale_colour_manual(values = c("ETS" = "#8B0000", "Observações" = "black"), breaks = c("ETS", "Observações"), name = "") 
ggsave("imagens/preds_ets_cox.png", width = 158, height = 93, units = "mm")

# Comparação com a auto.arima()

autoarima_preds <- forecast(auto.arima(treino), h = h)
ses_preds <- ses(treino, h = h)
holt_preds <- holt(treino, h = h)
autoets_preds <- forecast(ets(treino), h = h)
stlf_preds <- stlf(treino, h = h)
bats_preds <- forecast(bats(treino), h = h)
tbats_preds <- forecast(tbats(treino), h = h)

lista <- lst(arima_preds,arima_box_preds, ets_preds, ets_cox_preds,autoarima_preds, ses_preds, holt_preds, autoets_preds, stlf_preds, bats_preds, tbats_preds)
mae <- unlist(lapply(lista, function(x) return(mean(abs(teste - x$mean)))))
mape <- unlist(lapply(lista, function(x) return(mean(abs(100*(teste - x$mean)/teste)))))
names(mae) <- sub("_preds", "", names(lista)); names(mape) <- sub("_preds", "", names(lista))
cbind(mae, mape)
xtable(cbind(mae, mape))
