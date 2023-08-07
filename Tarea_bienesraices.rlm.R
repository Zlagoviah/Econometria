file <-"https://raw.githubusercontent.com/Zlagoviah/Econometria/main/bienesraices.csv"
df <-read.csv(file,header = TRUE,sep=";")
summary(df)

library(dplyr)
df2 <-
  df %>%
  filter(tiendas  <= 2 & edad <= 10) %>%
  select(edad,distancia,tiendas,latitud,longitud,precio)
head(df2)

# normalizar
# https://www.statology.org/how-to-normalize-data-in-r/

#define Min-Max normalization function
min_max_norm <- function(x) 
  (x - min(x)) / (max(x) - min(x))

#apply Min-Max normalization to first four columns in iris dataset
df_norm <- as.data.frame(lapply(df2[1:5], min_max_norm))

#add back Species column
df_norm$precio <- df2$precio

#view first six rows of normalized iris dataset
head(df_norm)

library(scatterplot3d)
attach(df2)

library(GGally)
ggpairs(df2,lower = list(continuous = "smooth"),
        diag = list(continuous = "barDiag"), axisLabels = "none")
#Matriz de correlaciones
round(cor(x = df, method = "pearson"), 3)

# eliminar fila 136
#  <- df[-136,]

library(gvlma)
mlm <- lm(precio ~ edad+distancia, data=df_norm)
summary(gvlma(mlm))

attributes(mlm)
mlm$coefficients

new_obs <- data.frame(edad=c(6,10,60),
                      distancia=c(500,1000,1500))
predict(mlm,new_obs) %>% round(1)

library(car)

# En la cuarta gráfica, residuales vs valores atípicos,
# sirve para encontrar las observaciones influyentes (leverage).
# Si no existen observaciones influyentes, no debe observarse una curva punteada roja,
# o en caso de verse ligeramente en las esquinas de la gráfica, no deben existir puntos en esa zona.
# En nuestro modelo, claramente la 41, 26 y 3 tienen influencia moderada
# por lo tanto no tenemos problema de valores influyentes.
influencePlot(mlm)

# Exploracion visual
mlm_3d <- scatterplot3d(x=edad, y=distancia, z=precio, pch=16, cex.lab=1,
                        highlight.3d=TRUE, type="h", xlab='edad',
                        ylab='Distancia', zlab='precio')
mlm_3d$plane3d(mlm, lty.box="solid", col='mediumblue')

mlm_3d <- scatterplot3d(x=distancia, y=edad, z=precio, pch=16, cex.lab=1,
                        highlight.3d=TRUE, type="h", xlab='distancia',
                        ylab='edad', zlab='precio')
mlm_3d$plane3d(mlm, lty.box="solid", col='mediumblue')

#Comportamiento de residuales
plot(mlm)
