setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()
library(readr)
library(plm)
library(readxl)

Export<- read_excel("Export_PE_CH.xlsx")
str(Export)
View(Export)
country=pdata.frame(Export,index=c("country","year"))

x11()
coplot(cmer ~ country|year, type="l",data=country )


data$year <- as.numeric(data$year)
data$cmer <- as.numeric(data$cmer)
data$tcult <- as.numeric(data$tcult)
data$fbkf <- as.numeric(data$fbkf)
data$terinter <- as.numeric(data$terinter)
data$tipca <- as.numeric(data$tipca)
data$pbi <- as.numeric(data$pbi)
data$fert <- as.numeric(data$fert)


 #gráfica de evoluacion de la cuota de mercado 
x11()

par(mar = c(5, 5, 2, 2))  
coplot(cmer ~ country | year, type = "l", data = country,
       main = "Gráfico Condicional",  # Título del gráfico
       xlab = "País",               # Etiqueta del eje x
       ylab = "C Mer",              # Etiqueta del eje y
       col = "blue",                # Color de las líneas
       pch = 19,                    # Tipo de punto
       cex = 1.2                    # Tamaño de los puntos
)

legend("topright", legend = unique(country$year), col = "blue", pch = 19, title = "Año")
#dev.off()
#---------------------------------------------------------------------------------------

# Heterogeneidad:
install.packages("gplots")
library(gplots)

##Por Paises
x11()
plotmeans(cmer ~ country, main="Heterogeneidad por Region", data=country)
#### MEjorado
x11()
library(RColorBrewer)
colores <- brewer.pal(n = length(unique(country$country)), name = "Set3")
plotmeans(
  cmer ~ country,
  main = "Heterogeneidad por Países",
  data = country,
  col = colores,           
  xlab = "País",           
  ylab = "C Mer",          
  ylim = c(min(country$cmer), max(country$cmer)),  # Rango del eje y
  xaxt = "n"               
)


axis(1, at = 1:length(unique(country$country)), labels = levels(country$country), las = 2)


legend("topright", legend = levels(country$country), fill = colores, title = "País")

title(main = "Heterogeneidad por Países", cex.main = 1.2)
par(cex.lab = 1.0, cex.axis = 0.8)
##dev.off()
#--------------------------------------------------------------
##Heterogeneidad por Años
##Baic
x11()
plotmeans(cmer ~ year, main="Heterogeneidad por Años", data=country) 
##Mjeorado
x11()
colores <- rainbow(length(unique(country$year)))

plotmeans(
  cmer ~ year,
  main = "Heterogeneidad por Años",
  data = country,
  col = colores,           # Colores personalizados para las barras
  xlab = "Año",            # Etiqueta del eje x
  ylab = "C Mer",          # Etiqueta del eje y
  ylim = c(min(country$cmer), max(country$cmer)),  # Rango del eje y
  xaxt = "n"               # Evitar etiquetas automáticas en el eje x
)

axis(1, at = 1:length(unique(country$year)), labels = levels(country$year))

title(main = "Heterogeneidad por Años", cex.main = 1.2)
par(cex.lab = 1.0, cex.axis = 0.8)
#dev.off()
#------------------------------------------------------------

##odelado

model_pool = plm( cmer~  tcult + fbkf + terinter  + pbi + fert, data = country, model = "pooling")
summary(model_pool)


# Modelo de panel con efectos fijos
model_efectos_fijos <- plm(cmer~ tcult + fbkf + terinter +tipca + pbi + fert, data = country, model = "within")

# Resumen del modelo de efectos fijos
summary(model_efectos_fijos)

# Modelo de panel con efectos aleatorios
model_efectos_aleatorios <- plm(cmer~ tcult + fbkf + terinter +tipca + pbi + fert, data = country, model = "random")

# Resumen del modelo de efectos aleatorios
summary(model_efectos_aleatorios)

# Test de Hausman para elegir entre efectos fijos o efectos aleatorios
hausman_test <- phtest(model_efectos_fijos, model_efectos_aleatorios)
print(hausman_test)

# Test de Hausman
# H0: Se prefiere efectos aleatorios
# H1: Se prefiere efectos fijos


##N-------------------NOo correr
###############################################################################
## Cargar las bibliotecas necesarias
library(plm)
library(ggplot2)
library(urca)

# Crear un objeto tibble a partir de los datos proporcionados
data <- tibble::tribble(
  ~country, ~year, ~cmer, ~tcult, ~fbkf, ~terinter, ~tipca, ~pbi, ~fert,
  "Chile", "2010", 0.246, 1.71, 23, 121.3, 510, 217, 3.76e+08,
  "Chile", "2011", 0.289, 1.77, 24.7, 109.5, 484, 251, 5.17e+08,
  "Chile", "2012", 0.236, 1.73, 26.8, 97.8, 487, 267, 5.58e+08,
  "Chile", "2013", 0.25, 1.76, 26.3, 97.2, 495, 277, 5.17e+08,
  "Chile", "2014", 0.248, 1.73, 25.5, 103.4, 570, 259, 4.61e+08,
  "Chile", "2015", 0.253, 1.75, 25.7, 105.8, 569, 246, 4.39e+08,
  "Chile", "2016", 0.256, 1.75, 25.9, 108.7, 579, 244, 4.12e+08,
  "Chile", "2017", 0.263, 1.75, 26.3, 107.9, 575, 250, 3.92e+08,
  "Chile", "2018", 0.262, 1.74, 26.5, 110.2, 564, 255, 3.67e+08,
  "Chile", "2019", 0.272, 1.75, 26.8, 112.7, 557, 247, 3.63e+08,
  "Chile", "2020", 0.276, 1.75, 27.1, 114.9, 560, 224, 3.56e+08,
  "Chile", "2021", 0.284, 1.74, 27.2, 118.3, 563, 226, 3.37e+08,
  "Chile", "2022", 0.289, 1.74, 27.4, 121.6, 575, 229, 3.33e+08,
  "Chile", "2023", 0.295, 1.75, 27.7, 124.9, 580, 234, 3.27e+08,
  "Chile", "2024", 0.298, 1.76, 27.9, 128.1, 583, 238, 3.23e+08,
  "Chile", "2025", 0.302, 1.76, 28.1, 131.3, 586, 242, 3.19e+08,
  "Chile", "2026", 0.305, 1.76, 28.3, 134.4, 589, 246, 3.15e+08,
  "Chile", "2027", 0.309, 1.77, 28.6, 137.5, 592, 250, 3.11e+08,
  "Chile", "2028", 0.312, 1.77, 28.8, 140.6, 596, 255, 3.07e+08,
  "Chile", "2029", 0.316, 1.77, 29.0, 143.7, 600, 259, 3.03e+08,
  "Chile", "2030", 0.320, 1.78, 29.2, 146.7, 604, 264, 2.99e+08
)

# Convertir el año a un formato numérico
data$year <- as.numeric(data$year)

# Crear un objeto de panel
panel_data <- pdata.frame(data, index = c("country", "year"))

# Realizar pruebas de estacionariedad para cada variable en el panel
for (variable in c("cmer", "tcult", "fbkf", "terinter", "tipca", "pbi", "fert")) {
  variable_data <- panel_data[, variable]
  
  # Realizar la prueba de Dickey-Fuller Aumentada (ADF) para estacionariedad
  adf_result <- ur.df(variable_data, lags = 1, type = "trend")
  
  # Imprimir los resultados de la prueba para cada variable
  cat(paste("Variable:", variable, "\n"))
  print(summary(adf_result))
  
  # Crear un gráfico de la serie temporal para cada variable
  ggplot(data = data, aes(x = year, y = .data[[variable]])) +
    geom_line() +
    labs(x = "Año", y = variable) +
    ggtitle(paste("Serie de tiempo para", variable))
}
# Cargar el paquete urca
library(urca)

# ... (código anterior para cargar los datos y crear panel_data) ...

# Realizar pruebas de estacionariedad para cada variable en el panel
for (variable in c("cmer", "tcult", "fbkf", "terinter", "tipca", "pbi", "fert")) {
  variable_data <- panel_data[, variable]
  
  # Realizar la prueba de Dickey-Fuller Aumentada (ADF) para estacionariedad
  adf_result <- ur.df(variable_data, lags = 1, type = "trend")
  
  # Imprimir los resultados de la prueba para cada variable
  cat(paste("Variable:", variable, "\n"))
  print(summary(adf_result))
  
  # Crear un gráfico de la serie temporal para cada variable
  ggplot(data = data, aes(x = year, y = .data[[variable]])) +
    geom_line() +
    labs(x = "Año", y = variable) +
    ggtitle(paste("Serie de tiempo para", variable))
}##**

