# Fx-options-pricing-r

## 📌 Propósito del Proyecto
Este repositorio contiene un motor analítico completo desarrollado en **R** para la valoración y gestión de riesgos de portafolios de opciones sobre tasas de cambio (FX). El núcleo del proyecto es construir toda la lógica financiera y algorítmica **desde los cimientos, sin utilizar librerías financieras prefabricadas** (como `fOptions` o `quantmod`), priorizando la eficiencia computacional y el análisis cuantitativo puro.

## 🛠️ Componentes Clave del Desarrollo

1. **Modelo de Garman-Kohlhagen (1983):** Implementación analítica de la extensión de Black-Scholes para divisas, incorporando dinámicamente el diferencial entre la tasa de interés doméstica ($r_d$) y foránea ($r_f$).
2. **Estructura Temporal de Tasas:** Módulo de preprocesamiento que utiliza interpolación lineal para aproximar y alinear los costos de financiación exactos para los días al vencimiento de cada contrato.
3. **Optimización Numérica (Newton-Raphson):** Algoritmo iterativo diseñado para calibrar y extraer la **volatilidad implícita ($\sigma$)** del mercado a partir del precio observado de los derivados, utilizando la sensibilidad Vega como derivada de convergencia cuadrática.
4. **Análisis de Sensibilidad de Riesgo (Griegas):** Cálculo analítico vectorizado de las métricas de sensibilidad de primer y segundo orden:
   * **Delta ($\Delta$):** Sensibilidad ante movimientos del activo subyacente.
   * **Gamma ($\Gamma$):** Curvatura y aceleración de Delta.
   * **Vega ($\nu$):** Exposición ante fluctuaciones en la volatilidad.
   * **Theta ($\Theta$):** Decaimiento temporal diario (*time decay*).
   * **Rho ($\rho$):** Sensibilidad ante variaciones en la tasa de interés doméstica.
