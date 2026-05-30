

# --- 1. Librerías ---

if (!require("tidyverse")) install.packages("tidyverse")
library(tidyverse)

# --- 2. Carga de Datos y Exploración ---
portafolio <- portafolio_opciones
tasas <- tasas_interes

print("--- Portafolio Head ---")
print(head(portafolio))

print("--- Portafolio Summary ---")
print(summary(portafolio))

print("--- Tasas Head ---")
print(head(tasas))

print("--- Tasas Summary ---")
print(summary(tasas))

# --- 3. Parámetros Iniciales y Preprocesamiento ---
s0 <- 3031.48

# Equivalente interpolacion"
tasa_domestica_interp <- function(dias_target) {
  approx(x = tasas$dias, y = tasas$tasa_dom, xout = dias_target, rule = 2)$y
}

tasa_foranea_interp <- function(dias_target) {
  approx(x = tasas$dias, y = tasas$tasa_for, xout = dias_target, rule = 2)$y
}

# Transformación y tratamientos (Normalizacion) del portafolio
portafolio <- portafolio %>%
  mutate(
    Tiempo_nomr = dias / 365,
    tasa_dom = tasa_domestica_interp(dias),
    tasa_for = tasa_foranea_interp(dias),
    tipo_option_num = if_else(tipo == "CALL", 1, -1)
  )

print("--- Portafolio con Tasas e Interpolación ---")
print(head(portafolio, 7))

# --- 4. Función de Precio de la Opción (Garman-Kohlhagen) ---
option_price <- function(spot, strike, tasa_dom, tasa_for, vola, Tiempo, tipo_option) {
  d1 <- (log(spot / strike) + (tasa_dom - tasa_for + 0.5 * vola^2) * Tiempo) / (vola * sqrt(Tiempo))
  d2 <- d1 - vola * sqrt(Tiempo)
  
  if (tipo_option == 1) {
    precio <- spot * exp(-tasa_for * Tiempo) * pnorm(d1) - strike * exp(-tasa_dom * Tiempo) * pnorm(d2)
  } else if (tipo_option == -1) {
    precio <- strike * exp(-tasa_dom * Tiempo) * pnorm(-d2) - spot * exp(-tasa_for * Tiempo) * pnorm(-d1)
  } else {
    stop("tipo_option debe ser 1 (CALL) o -1 (PUT)")
  }
  return(precio)
}

# --- 5. Funciones para Volatilidad Implícita (Newton-Raphson) ---
option_vega <- function(spot, strike, tasa_dom, tasa_for, vola, Tiempo) {
  d1 <- (log(spot / strike) + (tasa_dom - tasa_for + 0.5 * vola^2) * Tiempo) / (vola * sqrt(Tiempo))
  vega <- spot * exp(-tasa_for * Tiempo) * sqrt(Tiempo) * dnorm(d1)
  return(vega)
}

imp_vol_BS <- function(spot, strike, tasa_dom, tasa_for, vola, Tiempo, tipo_option) {
  sigma <- 0.15 # Búsqueda inicial
  tolerancia <- 1e-6
  max_iteraciones <- 100
  
  for (i in 1:max_iteraciones) {
    p_teorico <- option_price(spot, strike, tasa_dom, tasa_for, sigma, Tiempo, tipo_option)
    vega <- option_vega(spot, strike, tasa_dom, tasa_for, sigma, Tiempo)
    
    if (abs(vega) < 1e-10) {
      break
    }
    
    diferencia <- p_teorico - vola
    nuevo_sigma <- sigma - (diferencia / vega)
    
    if (abs(nuevo_sigma - sigma) < tolerancia) {
      return(nuevo_sigma)
    }
    sigma <- nuevo_sigma
  }
  return(sigma)
}

# --- 6. Cálculo de Volatilidad Implícita en el Portafolio ---

portafolio <- portafolio %>%
  mutate(Volatilidad_Implicita = pmap_dbl(
    list(s0, strike, tasa_dom, tasa_for, precio, Tiempo_nomr, tipo_option_num),
    ~ imp_vol_BS(..1, ..2, ..3, ..4, ..5, ..6, ..7)
  ))

print("--- Portafolio con Volatilidad Implícita ---")
print(head(portafolio))

# --- 7. Función de Letras Griegas ---
option_griega <- function(spot, strike, tasa_dom, tasa_for, vola, Tiempo, tipo_option) {
  d1 <- (log(spot / strike) + (tasa_dom - tasa_for + 0.5 * vola^2) * Tiempo) / (vola * sqrt(Tiempo))
  d2 <- d1 - vola * sqrt(Tiempo)
  nd1 <- dnorm(d1)
  
  gamma <- (exp(-tasa_for * Tiempo) * nd1) / (spot * vola * sqrt(Tiempo))
  vega <- option_vega(spot, strike, tasa_dom, tasa_for, vola, Tiempo) * 0.01
  
  if (tipo_option == 1) {
    delta <- exp(-tasa_for * Tiempo) * pnorm(d1)
    rho <- strike * Tiempo * exp(-tasa_dom * Tiempo) * pnorm(d2) * 0.01
    
    term1 <- -(spot * exp(-tasa_for * Tiempo) * nd1 * vola) / (2 * sqrt(Tiempo))
    term2 <- -tasa_for * spot * exp(-tasa_for * Tiempo) * pnorm(d1)
    term3 <- tasa_dom * strike * exp(-tasa_dom * Tiempo) * pnorm(d2)
    theta <- (term1 + term2 - term3) / 365
  } else {
    delta <- -exp(-tasa_for * Tiempo) * pnorm(-d1)
    rho <- -strike * Tiempo * exp(-tasa_dom * Tiempo) * pnorm(-d2) * 0.01
    
    term1 <- -(spot * exp(-tasa_for * Tiempo) * nd1 * vola) / (2 * sqrt(Tiempo))
    term2 <- tasa_for * spot * exp(-tasa_for * Tiempo) * pnorm(-d1)
    term3 <- -tasa_dom * strike * exp(-tasa_dom * Tiempo) * pnorm(-d2)
    theta <- (term1 + term2 - term3) / 365
  }
  
  # Retornamos una lista con nombres para expandirla fácilmente en el dataframe
  return(list(Delta = delta, Gamma = gamma, Vega = vega, Theta = theta, Rho = rho))
}

# --- 8. Cálculo de Griegas y Exportación Final ---

griegas_df <- pmap_dfr(
  list(s0, portafolio$strike, portafolio$tasa_dom, portafolio$tasa_for, 
       portafolio$Volatilidad_Implicita, portafolio$Tiempo_nomr, portafolio$tipo_option_num),
  ~ option_griega(..1, ..2, ..3, ..4, ..5, ..6, ..7)
)

# Unimos las griegas al portafolio principal
portafolio <- bind_cols(portafolio, griegas_df)

print("--- Portafolio Final con Griegas ---")
print(head(portafolio,10))


# Guardar a CSV (Opcional)
write_csv(portafolio, "Portafolio_resultado.csv")