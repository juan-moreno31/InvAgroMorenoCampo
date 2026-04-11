# grafica de las series de tiempo del producto agropecuario y del consumo de los hogares
library(ggplot2)
library(readr)
# Cargar los datos
df_producto_agropecuario <- read_csv("Base_100_Anios_Construccion.csv")
df_consumo_final <- read_csv("Base_100_Anios_Consumo.csv")

# Unimos las dos series en un solo data frame para graficar
df <- data.frame(
  Año = df_producto_agropecuario$Año,
  Producto_Agropecuario = df_producto_agropecuario$Valor,
  Consumo_Hogares = df_consumo_final$Valor
)
# pasamos año a formato fecha
df$Año <- as.Date(as.character(df$Año), format="%Y")

library(ggplot2)

# Reformas con nombres y colores
reformas <- data.frame(
  inicio = as.Date(c("1936-01-01", "1961-01-01", "1994-01-01", "2016-01-01")),
  fin    = as.Date(c("1960-12-31", "1993-12-31", "2015-12-31", "2025-12-31")),
  nombre = c("Ley 200 de 1936", "Ley 135 de 1961", "Ley 160 de 1994", "Acuerdo de Paz 2016"),
fill = c("#429cf0ff", "#e7933eff", "#75ea51ff", "#de609bff")
)

# Gráfica
ggplot(df, aes(x = Año, y = Producto_Agropecuario)) +
  
  # Sombras por reforma
  geom_rect(data = reformas,
            aes(xmin = inicio, xmax = fin, ymin = -Inf, ymax = Inf, fill = nombre),
            alpha = 0.3, inherit.aes = FALSE) +
  
  scale_fill_manual(values = setNames(reformas$fill, reformas$nombre)) +
  
  # Serie
  geom_line(size = 1, color = "black") +
  
  labs(
    title = "Producto Agropecuario y Reformas Agrarias en Colombia",
    subtitle = "Áreas sombreadas corresponden a periodos de reforma agraria",
    x = "Año",
    y = "Producto Agropecuario",
    fill = "Reformas",
    caption = "Fuente: Elaboración propia con base en datos del Banco de la República de Colombia.\nNota: Valores expresados en miles de millones."
  ) + 
  
  theme_classic()


# ================================
# MODELO COMPLETO: VENTANAS SIN TRASLAPE
# ================================

library(dplyr)

# --- 1. Definir límites sin traslape ---
lim_1936 <- 1960 - 1936
lim_1961 <- 1993 - 1961
lim_1994 <- 2015 - 1994
lim_2016 <- 2025 - 2016

# --- 2. Grid de duraciones ---
dur_1936 <- seq(5, lim_1936, by = 5)
dur_1961 <- seq(5, lim_1961, by = 5)
dur_1994 <- seq(5, lim_1994, by = 5)
dur_2016 <- seq(3, lim_2016, by = 3)

# --- 3. Inicializar resultados ---
resultados <- data.frame()

# --- 4. Loop principal ---
for(d36 in dur_1936){
for(d61 in dur_1961){
for(d94 in dur_1994){
for(d16 in dur_2016){

  df_temp <- df %>%
    arrange(Año) %>%
    mutate(
      y = log(Producto_Agropecuario),
      dy = y - lag(y),
      
      reforma_1936 = ifelse(
        Año >= as.Date("1936-01-01") &
        Año <= as.Date(paste0(1936 + d36, "-12-31")), 1, 0),
      
      reforma_1961 = ifelse(
        Año >= as.Date("1961-01-01") &
        Año <= as.Date(paste0(1961 + d61, "-12-31")), 1, 0),
      
      reforma_1994 = ifelse(
        Año >= as.Date("1994-01-01") &
        Año <= as.Date(paste0(1994 + d94, "-12-31")), 1, 0),
      
      reforma_2016 = ifelse(
        Año >= as.Date("2016-01-01") &
        Año <= as.Date(paste0(2016 + d16, "-12-31")), 1, 0)
    )
  
  modelo <- lm(
    dy ~ reforma_1936 + reforma_1961 + reforma_1994 + reforma_2016 +
      lag(dy,1) + lag(dy,2),
    data = df_temp
  )
  
  coefs <- summary(modelo)$coefficients
  
  resultados <- rbind(resultados, data.frame(
    d36 = d36,
    d61 = d61,
    d94 = d94,
    d16 = d16,
    
    p_1936 = ifelse("reforma_1936" %in% rownames(coefs), coefs["reforma_1936","Pr(>|t|)"], NA),
    p_1961 = ifelse("reforma_1961" %in% rownames(coefs), coefs["reforma_1961","Pr(>|t|)"], NA),
    p_1994 = ifelse("reforma_1994" %in% rownames(coefs), coefs["reforma_1994","Pr(>|t|)"], NA),
    p_2016 = ifelse("reforma_2016" %in% rownames(coefs), coefs["reforma_2016","Pr(>|t|)"], NA)
  ))

}}}}

# --- 5. Filtrar combinaciones con al menos una significativa ---
resultados_mix <- resultados %>%
  mutate(
    sig_1936 = p_1936 < 0.05,
    sig_1961 = p_1961 < 0.05,
    sig_1994 = p_1994 < 0.05,
    sig_2016 = p_2016 < 0.05
  ) %>%
  filter(
    (sig_1936 | sig_1961 | sig_1994 | sig_2016) &
    !(sig_1936 & sig_1961 & sig_1994 & sig_2016)
  )

# --- 6. Resumen de frecuencia de significancia ---
resumen <- resultados %>%
  summarise(
    p1936 = mean(p_1936 < 0.05, na.rm = TRUE),
    p1961 = mean(p_1961 < 0.05, na.rm = TRUE),
    p1994 = mean(p_1994 < 0.05, na.rm = TRUE),
    p2016 = mean(p_2016 < 0.05, na.rm = TRUE)
  )

# --- 7. Resultados finales ---
resultados_mix
resumen


# ================================ #
# =========ARIMA DE PRUEBA======== #
library(forecast)
# prueba ADF
library(tseries)
adf.test(df$Producto_Agropecuario, alternative = "stationary")
# diff
diff_agro <- diff(df$Producto_Agropecuario)
adf.test(diff_agro, alternative = "stationary")
# es estacionaria la diferencia
# FCF y FCP
acf(diff_agro, main="ACF de la diferencia del producto agropecuario")
pacf(diff_agro, main="PACF de la diferencia del producto agropecuario")
# modelo ARIMA
modelo_arima <- auto.arima(df$Producto_Agropecuario, d = 1)
summary(modelo_arima)

modelo_arima_manual <- auto.arima(df$Producto_Agropecuario, d = 1, max.p = 1, max.q = 1)
summary(modelo_arima_manual)

# comparamos UIC y BIC de los modelos
modelos <- data.frame(
  Modelo = c("ARIMA Automático", "ARIMA Manual"),
  AIC = c(AIC(modelo_arima), AIC(modelo_arima_manual)),
  BIC = c(BIC(modelo_arima), BIC(modelo_arima_manual))
)
modelos
