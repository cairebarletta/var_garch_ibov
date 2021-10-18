#carregando as bibliotecas necessárias
library(quantmod)
library(ggplot2)
library(scales)
library(forecast)
library(moments)
library(rugarch)
library(urca)

#inputs iniciais
ticker <- "^BVSP"
from <- "2011-01-01"
to <- "2021-01-01"

#puxando os dados do Yahoo Finance
price <- na.omit(Ad(getSymbols(ticker,
                               from = from,
                               to = to,
                               auto.assign = F)))

#calculando a diferenciação dos log-retornos
log_ret <- na.omit(diff(log(price)))
names(log_ret) <- "ibov"

#plot da pontuação do Ibovespa
g1 <- ggplot() +
  geom_line(aes(y = price, x = index(price)),
            color = "darkblue", size = 0.5) +
  scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
  scale_y_continuous(labels = number_format(big.mark = ".")) +
  labs(title = "Série diária do Ibovespa",
       x = "Data", 
       y = "Pontos", 
       caption = "Fonte: Yahoo Finance.", 
       color = "") +
  theme_light()

#plot da diferenciação dos log-retornos
g2 <- ggplot() +
  geom_line(aes(y = log_ret, x = index(log_ret)),
            color = "darkblue", size = 0.5) +
  scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
  scale_y_continuous(labels = number_format(big.mark = ".")) +
  labs(title = "Série diária do Ibovespa",
       x = "Data", 
       y = "Log-retornos (%)", 
       caption = "Fonte: Yahoo Finance. Elaboração do Autor.") + 
  theme_light()

#teste de Dickey-Fuller para os log-retornos
ur.df(log_ret, type = "none", lags = 0) #ou utilizar o summary

#plot da Função de Autocorrelação (FAC)
g3 <- ggAcf(x = log_ret, lag.max = 10) +
  labs(title = "Autocorrelograma dos log-retornos do Ibovespa",
       subtitle = "Intervalo de confiança de 95%",
       y = "Função de Autocorrelação (FAC)", 
       x = "Defasagens") + 
  theme_light()

#plot da Função de Autocorrelação Parcial (FACP)
g4 <- ggPacf(x = log_ret, lag.max = 10) +
  labs(title = "Autocorrelograma dos log-retornos do Ibovespa",
       subtitle = "Intervalo de confiança de 95%",
       y = "Função de Autocorrelação Parcial (FACP)",
       x = "Defasagens") + 
  theme_light()

#encontrando o modelo que se melhor ajusta
best_fit <- auto.arima(log_ret,
                       max.p = 5,
                       max.q = 7,
                       ic = "aic",
                       approximation = F,
                       trace = T)

#obtendo os coeficientes estimados do modelo
arima_fitted <- fitted(best_fit)
best_fit$coef

#plot dos log-retornos reais vs. estimados pelo modelo
g5 <- ggplot() +
  geom_line(data = log_ret, 
            aes(x = index(log_ret), 
                y = ibov, 
                color = ""), size = 0.5) +
  geom_line(data = arima_fitted, 
            aes(x = index(log_ret), 
                y = arima_fitted,
                color = " "), size = 0.5) + 
  scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
  scale_y_continuous(labels = number_format(big.mark = ".")) +
  scale_color_manual(labels = c("Realizados", "Estimados"), 
                     values = c("darkblue", "gold")) + 
  labs(title = "Log-retornos reais vs. estimados",
       subtitle = "Série diária do Ibovespa",
       x = "Data", 
       y = "Log-retornos (%)", 
       color = "") +
  theme_light() + theme(legend.title = element_blank(), 
                        legend.position = "bottom")

#resíduos do modelo estimado
resid <- best_fit$residuals

#FAC dos resíduos do modelo estimado
g6 <- ggAcf(resid, lag.max = 15) +
  labs(title = "Autocorrelograma dos resíduos do modelo estimado",
       subtitle = "Intervalo de confiança de 95%",
       y = "Função de Autocorrelação (FAC)", 
       x = "Defasagens") + 
  theme_light()

#teste de Dickey-Fuller para os os resíduos do modelo estimado
ur.df(resid, type = "none", lags = 0) #ou utilizar o summary

#histograma dos log-retornos
g7 <- ggplot(log_ret, aes(x = log_ret)) + 
  geom_histogram(aes(y =..density..), colour = "black", fill = "darkblue") +
  geom_density(alpha = 0.35, fill = "gold") +
  scale_x_continuous(labels = number_format(big.mark = ".")) +
  labs(title = "Histograma dos log-retornos",
       subtitle = "Série diária do Ibovespa",
       x = "Log-retornos (%)", 
       y = "Frequência") +
  theme_light()

#momentos da série e estatísticas descritivas
skewness(log_ret) #assimetria negativa: moda > mediana > media
kurtosis(log_ret) #leptocurtica (mais alongada)
summary(log_ret)[c(1, 3, 4, 6), 2]

#elevando os resíduos ao quadrado
resid_quad <- resid^2

#FAC dos resíduos ao quadrado
g8 <- ggAcf(resid_quad, lag.max = 15) +
  labs(title = "Autocorrelograma dos resíduos a quadrado",
       subtitle = "Intervalo de confiança de 95%",
       y = "Função de Autocorrelação (FAC)", 
       x = "Defasagens") + 
  theme_light()

#FACP dos resíduos ao quadrado
g9 <- ggPacf(resid_quad, lag.max = 15) +
  labs(title = "Autocorrelograma dos resíduos a quadrado",
       subtitle = "Intervalo de confiança de 95%",
       y = "Função de Autocorrelação Parcial (FACP)", 
       x = "Defasagens") + 
  theme_light()

#especificando o modelo
t_garch_spec <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1,1)),
  mean.model = list(armaOrder = c(2,2), include.mean = T),
  distribution.model = "std"
)

#encontrando os valores estimados do modelo ARMA(2,2)-GARCH(1,1)
fit_t_garch <- ugarchfit(spec = t_garch_spec,
                         data = log_ret)

#salvando os valores estimados
fitted_t_garch_series <- cbind(log_ret, 
                               fit_t_garch@fit$sigma,
                               fit_t_garch@fit$z)
names(fitted_t_garch_series) <- c("logret", "sigma", "et")

#dez primeiros valores 
head(fitted_t_garch_series, 10)

#dez últimos valores
tail(fitted_t_garch_series, 10)

#observações teste e validação
set <- length(log_ret["2011-01-04/2017-05-17"])

test <- 1:set #observações 1:1575
validation <- (set+1):nrow(log_ret) #observações 1576:2470

#backtesting
roll_garch <- ugarchroll(
  spec = t_garch_spec, #modelo especificado
  data = log_ret,
  n.ahead = 1,
  forecast.length = 1, #a partir dos dados de validação
  n.start = set,
  refit.every = 1, #re-estimando o modelo a cada nova observação
  refit.window = "recursive",
  calculate.VaR = T,
  VaR.alpha = 0.05, #nível de significância de 5%
  keep.coef = T
)

#forma da distribuição da série
shape <- fitdist(distribution = "std", x = log_ret)$pars[3]
t_dist_quantile <- qdist(distribution = "std", shape = shape , p = 0.05)

#valores previstos
validation_VaR <- mean(log_ret[validation]) +
  roll_garch@forecast$density$Sigma * t_dist_quantile

#plot do VaR delta-Normal, VaR GARCH e retornos
g10 <- ggplot() +
  geom_line(aes(y = validation_VaR, 
                x = index(log_ret[validation]), 
                color = ""), size = 0.5) +
  geom_point(aes(y = log_ret[validation],
                 x = index(log_ret[validation]),
                 color = as.factor(log_ret[validation] < validation_VaR)), 
             size = 1.5) +
  geom_hline(aes(yintercept = sd(log_ret) * qnorm(0.05), color = " "),
             size = 0.5) +
  scale_y_continuous(labels = scales::number_format(accuracy = 0.01,
                                                    decimal.mark = '.'),
                     expand = expansion(c(-0.01, 0.01))) +
  scale_color_manual(labels = c("VaR GARCH", "VaR delta-Normal", "Retornos", 
                                "Retornos < VaR GARCH"),
                     values = c("darkcyan", "gold", "gray", 
                                "darkblue")) + 
  labs(title = "VaR GARCH(1,1) vs. VaR delta-Normal",
       y = "%", 
       x = element_blank(), 
       color = "") + 
  theme_light() + theme(legend.position = "bottom")

#resumo de exceções
excecoes_dn <- sum(
  log_ret[validation] < 
    (mean(log_ret) + qnorm(0.05) * sd(log_ret[validation])))
excecoes_garch <- sum(log_ret[validation] < validation_VaR)
melhor_abordagem <- if(
  excecoes_dn < excecoes_garch){"ARMA-GARCH"} else{"delta-Normal"}

cat(" Número de excedências com a abordagem delta-Normal:", excecoes_dn,
    "\n",
    "Número de excedências com a abordagem ARMA-GARCH:", excecoes_garch,
    "\n",
    "A abordagem", melhor_abordagem,
    "consegue captar melhor os valores excedentes!")

#última etapa: previsão
garch_prev <- ugarchforecast(fit_t_garch, n.ahead = 3)

VaR_t1 <- round(
  (t_dist_quantile * garch_prev@forecast$sigmaFor[1]) * 100, 4)
VaR_t2 <- round(
  (t_dist_quantile * garch_prev@forecast$sigmaFor[2]) * 100, 4)
VaR_t3 <- round(
  (t_dist_quantile * garch_prev@forecast$sigmaFor[3]) * 100, 4)
VaR_dn <- round((sd(log_ret) * qnorm(0.05)* 100), 4)

cat(" VaR delta-Normal é: ", VaR_dn, "%.", "\n",
    "VaR-GARCH em t+1 é: ", VaR_t1, "%.", "\n",
    "VaR-GARCH em t+2 é: ", VaR_t2, "%.", "\n",
    "VaR-GARCH em t+3 é: ", VaR_t3, "%.")