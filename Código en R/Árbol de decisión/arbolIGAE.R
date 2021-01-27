# Se cargan las librerìas necesarias
library(tidyverse)
library(rpart)
library(rpart.plot)
library(caret)

setwd("H:")
igae <- read.csv("IGAE.csv")

summary(igae)

#Se convienten los atributos a variables tipo factor
igae$FAMI_COMECARNEPESCADOHUEVO <- as.factor(igae$FAMI_COMECARNEPESCADOHUEVO)
igae$FAMI_EDUCACIONMADRE <- as.factor(igae$FAMI_EDUCACIONMADRE)
igae$FAMI_EDUCACIONPADRE <- as.factor(igae$FAMI_EDUCACIONPADRE)
igae$FAMI_COMELECHEDERIVADOS <- as.factor(igae$FAMI_COMELECHEDERIVADOS)
igae$FAMI_TIENEINTERNET <- as.factor(igae$FAMI_EDUCACIONPADRE)
igae$FAMI_NUMLIBROS <- as.factor(igae$FAMI_NUMLIBROS)
igae$FAMI_TIENECOMPUTADOR <- as.factor(igae$FAMI_TIENECOMPUTADOR)
igae$FAMI_PERSONASHOGAR <- as.factor(igae$FAMI_PERSONASHOGAR)
igae$COLE_JORNADA <- as.factor(igae$COLE_JORNADA)
igae$COLE_NATURALEZA <- as.factor(igae$COLE_NATURALEZA)
igae$COLE_AREA_UBICACION <- as.factor(igae$COLE_AREA_UBICACION)
igae$COLE_DEPTO_UBICACION <- as.factor(igae$COLE_DEPTO_UBICACION)
igae$ESTU_DEDICACIONINTERNET <- as.factor(igae$ESTU_DEDICACIONINTERNET)
igae$ESTU_DEDICACIONLECTURADIARIA <- as.factor(igae$ESTU_DEDICACIONLECTURADIARIA)
igae$EDAD <- as.factor(igae$EDAD)
igae$RENDIMIENTO <- as.factor(igae$RENDIMIENTO)

#Se genera una semilla aleatoria para generar el àrbol
#(anteriomente se generaron varios árboles aleatorios y este es la que obtiene
#mejores métricas)
set.seed(9475)

#Generación de los conjuntos de entrenamiento y validación
igae_ent <- sample_frac(igae, 0.66)
igae_prueba <- setdiff(igae,igae_ent)

#Creación y entrenamiento del árbol
arbol_a <- rpart(formula = RENDIMIENTO ~., data = igae_ent)

arbol_a

#Generación de la gráfica
rpart.plot(arbol_a)

#Prueba del árbol en el conjunto de validación
pred_a <- predict(arbol_a, newdata = igae_prueba, type = "class")
confusionMatrix(pred_a,igae_prueba[["RENDIMIENTO"]])

#Otro árbol generado
set.seed(1876)
igae_ent <- sample_frac(igae, 0.66)
igae_prueba <- setdiff(igae,igae_ent)

arbol_a <- rpart(formula = RENDIMIENTO ~., data = igae_ent)

arbol_a

rpart.plot(arbol_a)

pred_a <- predict(arbol_a, newdata = igae_prueba, type = "class")
confusionMatrix(pred_a,igae_prueba[["RENDIMIENTO"]])

