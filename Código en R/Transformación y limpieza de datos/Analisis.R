library(tidyverse)

# Se fija el directorio de trabajo
setwd("C:/Users/jairo.acosta/Documents/TFM Saber 11/Transformaciones de la data/")

# Carga del dataset
datos <- read.csv("C:/Users/jairo.acosta/Documents/TFM Saber 11/Transformaciones de la data/DATASET.csv", header = TRUE, sep = ";")

# Revisar el tipo de datos reconocido por R
sink("tipoVar.txt")
glimpse(datos)
sink()


# Convertir a factor las variables que no deben ser operables
datos$ESTU_NSE_ESTABLECIMIENTO <- as.factor(datos$ESTU_NSE_ESTABLECIMIENTO)

# Existencia de valores ausentes
any(!complete.cases(datos))

# Verificar la cantidad de valores NA
sink("valoresNA.txt")
map_dbl(datos, .f = function(x){sum(is.na(x))})
sink()

# Verificar la cantidad de valores vacios
sink("valoresVacios")
datos %>% map_lgl(.f = function(x){any(!is.na(x) & x == "")})
map_dbl(datos, .f = function(x){sum(!is.na(x) & x=="")})
sink()

# Contar los que tienen la cadena Ninguno
map_dbl(datos, .f = function(x){sum(!is.na(x) & x=="Ninguno")})

#Verificar la existencia de valores vacios
datos %>% map_lgl(.f = function(x){any(!is.na(x) & x== "" | x=="Ninguno")})

# Conversiones de datos e ingreso de valores NA en valores de factores vacios

levels(datos$EDAD)
datos$EDAD <- as.numeric(datos$EDAD)
datos$EDAD[datos$EDAD == ""] <- NA

datos$ESTU_GENERO <- as.factor(datos$ESTU_GENERO)
levels(datos$ESTU_GENERO)
datos$ESTU_GENERO <- as.character(datos$ESTU_GENERO)
datos$ESTU_GENERO[datos$ESTU_GENERO == ""] <- NA
datos$ESTU_GENERO <- as.factor(datos$ESTU_GENERO)
levels(datos$ESTU_GENERO)

levels(datos$ESTU_ETNIA)
datos$ESTU_ETNIA <- as.character(datos$ESTU_ETNIA)
datos$ESTU_ETNIA[datos$ESTU_ETNIA == ""] <- NA
datos$ESTU_ETNIA[datos$ESTU_ETNIA == "Ninguno"] <- NA
datos$ESTU_ETNIA <- as.factor(datos$ESTU_ETNIA)
levels(datos$ESTU_ETNIA)

levels(datos$FAMI_EDUCACIONPADRE)
datos$FAMI_EDUCACIONPADRE <- as.character(datos$FAMI_EDUCACIONPADRE)
datos$FAMI_EDUCACIONPADRE[datos$FAMI_EDUCACIONPADRE == ""] <- NA
#datos$FAMI_EDUCACIONPADRE[datos$FAMI_EDUCACIONPADRE == "Ninguno"] <- NA
#datos$FAMI_EDUCACIONPADRE[datos$FAMI_EDUCACIONPADRE == "No Aplica"] <- NA
#datos$FAMI_EDUCACIONPADRE[datos$FAMI_EDUCACIONPADRE == "No sabe"] <- NA
datos$FAMI_EDUCACIONPADRE <- as.factor(datos$FAMI_EDUCACIONPADRE)
levels(datos$FAMI_EDUCACIONPADRE)

levels(datos$FAMI_EDUCACIONMADRE)
datos$FAMI_EDUCACIONMADRE <- as.character(datos$FAMI_EDUCACIONMADRE)
datos$FAMI_EDUCACIONMADRE[datos$FAMI_EDUCACIONMADRE == ""] <- NA
#datos$FAMI_EDUCACIONMADRE[datos$FAMI_EDUCACIONMADRE == "Ninguno"] <- NA
#datos$FAMI_EDUCACIONMADRE[datos$FAMI_EDUCACIONMADRE == "No Aplica"] <- NA
#datos$FAMI_EDUCACIONMADRE[datos$FAMI_EDUCACIONMADRE == "No sabe"] <- NA
datos$FAMI_EDUCACIONMADRE <- as.factor(datos$FAMI_EDUCACIONMADRE)
levels(datos$FAMI_EDUCACIONMADRE)

levels(datos$FAMI_ESTRATOVIVIENDA)
datos$FAMI_ESTRATOVIVIENDA <- as.character(datos$FAMI_ESTRATOVIVIENDA)
datos$FAMI_ESTRATOVIVIENDA[datos$FAMI_ESTRATOVIVIENDA == ""] <- NA
#datos$FAMI_ESTRATOVIVIENDA[datos$FAMI_ESTRATOVIVIENDA == "Sin Estrato"] <- NA
datos$FAMI_ESTRATOVIVIENDA <- as.factor(datos$FAMI_ESTRATOVIVIENDA)
levels(datos$FAMI_ESTRATOVIVIENDA)

levels(datos$FAMI_PERSONASHOGAR)
datos$FAMI_PERSONASHOGAR <- as.character(datos$FAMI_PERSONASHOGAR)
datos$FAMI_PERSONASHOGAR[datos$FAMI_PERSONASHOGAR == ""] <- NA
datos$FAMI_PERSONASHOGAR <- as.factor(datos$FAMI_PERSONASHOGAR)
levels(datos$FAMI_PERSONASHOGAR)

levels(datos$FAMI_CUARTOSHOGAR)
datos$FAMI_CUARTOSHOGAR <- as.character(datos$FAMI_CUARTOSHOGAR)
datos$FAMI_CUARTOSHOGAR[datos$FAMI_CUARTOSHOGAR == ""] <- NA
datos$FAMI_CUARTOSHOGAR <- as.factor(datos$FAMI_CUARTOSHOGAR)
levels(datos$FAMI_CUARTOSHOGAR)
datos$FAMI_CUARTOSHOGAR <- factor(datos$FAMI_CUARTOSHOGAR, levels(datos$FAMI_CUARTOSHOGAR)[c(6,3,5,2,1,4)])
levels(datos$FAMI_CUARTOSHOGAR)

levels(datos$FAMI_TIENEINTERNET)
datos$FAMI_TIENEINTERNET <- as.character(datos$FAMI_TIENEINTERNET)
datos$FAMI_TIENEINTERNET[datos$FAMI_TIENEINTERNET == ""] <- NA
datos$FAMI_TIENEINTERNET <- as.factor(datos$FAMI_TIENEINTERNET)
levels(datos$FAMI_TIENEINTERNET)
datos$FAMI_TIENEINTERNET <- factor(datos$FAMI_TIENEINTERNET, levels(datos$FAMI_TIENEINTERNET)[c(2,1)])
levels(datos$FAMI_TIENEINTERNET)

levels(datos$FAMI_TIENECOMPUTADOR)
datos$FAMI_TIENECOMPUTADOR <- as.character(datos$FAMI_TIENECOMPUTADOR)
datos$FAMI_TIENECOMPUTADOR[datos$FAMI_TIENECOMPUTADOR == ""] <- NA
datos$FAMI_TIENECOMPUTADOR <- as.factor(datos$FAMI_TIENECOMPUTADOR)
levels(datos$FAMI_TIENECOMPUTADOR)
datos$FAMI_TIENECOMPUTADOR <- factor(datos$FAMI_TIENECOMPUTADOR, levels(datos$FAMI_TIENECOMPUTADOR)[c(2,1)])
levels(datos$FAMI_TIENECOMPUTADOR)

levels(datos$FAMI_TIENELAVADORA)
datos$FAMI_TIENELAVADORA <- as.character(datos$FAMI_TIENELAVADORA)
datos$FAMI_TIENELAVADORA[datos$FAMI_TIENELAVADORA == ""] <- NA
datos$FAMI_TIENELAVADORA <- as.factor(datos$FAMI_TIENELAVADORA)
levels(datos$FAMI_TIENELAVADORA)
datos$FAMI_TIENELAVADORA <- factor(datos$FAMI_TIENELAVADORA, levels(datos$FAMI_TIENELAVADORA)[c(2,1)])
levels(datos$FAMI_TIENELAVADORA)

levels(datos$FAMI_TIENEHORNOMICROOGAS)
datos$FAMI_TIENEHORNOMICROOGAS <- as.character(datos$FAMI_TIENEHORNOMICROOGAS)
datos$FAMI_TIENEHORNOMICROOGAS[datos$FAMI_TIENEHORNOMICROOGAS == ""] <- NA
datos$FAMI_TIENEHORNOMICROOGAS <- as.factor(datos$FAMI_TIENEHORNOMICROOGAS)
levels(datos$FAMI_TIENEHORNOMICROOGAS)
datos$FAMI_TIENEHORNOMICROOGAS <- factor(datos$FAMI_TIENEHORNOMICROOGAS, levels(datos$FAMI_TIENEHORNOMICROOGAS)[c(2,1)])
levels(datos$FAMI_TIENEHORNOMICROOGAS)

levels(datos$FAMI_TIENESERVICIOTV)
datos$FAMI_TIENESERVICIOTV <- as.character(datos$FAMI_TIENESERVICIOTV)
datos$FAMI_TIENESERVICIOTV[datos$FAMI_TIENESERVICIOTV == ""] <- NA
datos$FAMI_TIENESERVICIOTV <- as.factor(datos$FAMI_TIENESERVICIOTV)
levels(datos$FAMI_TIENESERVICIOTV)
datos$FAMI_TIENESERVICIOTV <- factor(datos$FAMI_TIENESERVICIOTV, levels(datos$FAMI_TIENESERVICIOTV)[c(2,1)])
levels(datos$FAMI_TIENESERVICIOTV)

levels(datos$FAMI_TIENEAUTOMOVIL)
datos$FAMI_TIENEAUTOMOVIL <- as.character(datos$FAMI_TIENEAUTOMOVIL)
datos$FAMI_TIENEAUTOMOVIL[datos$FAMI_TIENEAUTOMOVIL == ""] <- NA
datos$FAMI_TIENEAUTOMOVIL <- as.factor(datos$FAMI_TIENEAUTOMOVIL)
levels(datos$FAMI_TIENEAUTOMOVIL)
datos$FAMI_TIENEAUTOMOVIL <- factor(datos$FAMI_TIENEAUTOMOVIL, levels(datos$FAMI_TIENEAUTOMOVIL)[c(2,1)])
levels(datos$FAMI_TIENEAUTOMOVIL)

levels(datos$FAMI_TIENEMOTOCICLETA)
datos$FAMI_TIENEMOTOCICLETA <- as.character(datos$FAMI_TIENEMOTOCICLETA)
datos$FAMI_TIENEMOTOCICLETA[datos$FAMI_TIENEMOTOCICLETA == ""] <- NA
datos$FAMI_TIENEMOTOCICLETA <- as.factor(datos$FAMI_TIENEMOTOCICLETA)
levels(datos$FAMI_TIENEMOTOCICLETA)
datos$FAMI_TIENEMOTOCICLETA <- factor(datos$FAMI_TIENEMOTOCICLETA, levels(datos$FAMI_TIENEMOTOCICLETA)[c(2,1)])
levels(datos$FAMI_TIENEMOTOCICLETA)

levels(datos$FAMI_TIENECONSOLAVIDEOJUEGOS)
datos$FAMI_TIENECONSOLAVIDEOJUEGOS <- as.character(datos$FAMI_TIENECONSOLAVIDEOJUEGOS)
datos$FAMI_TIENECONSOLAVIDEOJUEGOS[datos$FAMI_TIENECONSOLAVIDEOJUEGOS == ""] <- NA
datos$FAMI_TIENECONSOLAVIDEOJUEGOS <- as.factor(datos$FAMI_TIENECONSOLAVIDEOJUEGOS)
levels(datos$FAMI_TIENECONSOLAVIDEOJUEGOS)
datos$FAMI_TIENECONSOLAVIDEOJUEGOS <- factor(datos$FAMI_TIENECONSOLAVIDEOJUEGOS, levels(datos$FAMI_TIENECONSOLAVIDEOJUEGOS)[c(2,1)])
levels(datos$FAMI_TIENECONSOLAVIDEOJUEGOS)

levels(datos$FAMI_NUMLIBROS)
datos$FAMI_NUMLIBROS <- as.character(datos$FAMI_NUMLIBROS)
datos$FAMI_NUMLIBROS[datos$FAMI_NUMLIBROS == ""] <- NA
datos$FAMI_NUMLIBROS <- as.factor(datos$FAMI_NUMLIBROS)
levels(datos$FAMI_NUMLIBROS)

levels(datos$FAMI_COMELECHEDERIVADOS)
datos$FAMI_COMELECHEDERIVADOS <- as.character(datos$FAMI_COMELECHEDERIVADOS)
datos$FAMI_COMELECHEDERIVADOS[datos$FAMI_COMELECHEDERIVADOS == ""] <- NA
datos$FAMI_COMELECHEDERIVADOS <- as.factor(datos$FAMI_COMELECHEDERIVADOS)
levels(datos$FAMI_COMELECHEDERIVADOS)

levels(datos$FAMI_COMECARNEPESCADOHUEVO)
datos$FAMI_COMECARNEPESCADOHUEVO <- as.character(datos$FAMI_COMECARNEPESCADOHUEVO)
datos$FAMI_COMECARNEPESCADOHUEVO[datos$FAMI_COMECARNEPESCADOHUEVO == ""] <- NA
datos$FAMI_COMECARNEPESCADOHUEVO <- as.factor(datos$FAMI_COMECARNEPESCADOHUEVO)
levels(datos$FAMI_COMECARNEPESCADOHUEVO)

levels(datos$FAMI_COMECEREALFRUTOSLEGUMBRE)
datos$FAMI_COMECEREALFRUTOSLEGUMBRE <- as.character(datos$FAMI_COMECEREALFRUTOSLEGUMBRE)
datos$FAMI_COMECEREALFRUTOSLEGUMBRE[datos$FAMI_COMECEREALFRUTOSLEGUMBRE == ""] <- NA
datos$FAMI_COMECEREALFRUTOSLEGUMBRE <- as.factor(datos$FAMI_COMECEREALFRUTOSLEGUMBRE)
levels(datos$FAMI_COMECEREALFRUTOSLEGUMBRE)

levels(datos$FAMI_TRABAJOLABORPADRE)
datos$FAMI_TRABAJOLABORPADRE <- as.character(datos$FAMI_TRABAJOLABORPADRE)
datos$FAMI_TRABAJOLABORPADRE[datos$FAMI_TRABAJOLABORPADRE == ""] <- NA
#datos$FAMI_TRABAJOLABORPADRE[datos$FAMI_TRABAJOLABORPADRE == "No aplica"] <- NA
#datos$FAMI_TRABAJOLABORPADRE[datos$FAMI_TRABAJOLABORPADRE == "No sabe"] <- NA
datos$FAMI_TRABAJOLABORPADRE <- as.factor(datos$FAMI_TRABAJOLABORPADRE)
levels(datos$FAMI_TRABAJOLABORPADRE)

levels(datos$FAMI_TRABAJOLABORMADRE)
datos$FAMI_TRABAJOLABORMADRE <- as.character(datos$FAMI_TRABAJOLABORMADRE)
datos$FAMI_TRABAJOLABORMADRE[datos$FAMI_TRABAJOLABORMADRE == ""] <- NA
#datos$FAMI_TRABAJOLABORMADRE[datos$FAMI_TRABAJOLABORMADRE == "No aplica"] <- NA
#datos$FAMI_TRABAJOLABORMADRE[datos$FAMI_TRABAJOLABORMADRE == "No sabe"] <- NA
datos$FAMI_TRABAJOLABORMADRE <- as.factor(datos$FAMI_TRABAJOLABORMADRE)
levels(datos$FAMI_TRABAJOLABORMADRE)

levels(datos$FAMI_SITUACIONECONOMICA)
datos$FAMI_SITUACIONECONOMICA <- as.character(datos$FAMI_SITUACIONECONOMICA)
datos$FAMI_SITUACIONECONOMICA[datos$FAMI_SITUACIONECONOMICA == ""] <- NA
datos$FAMI_SITUACIONECONOMICA <- as.factor(datos$FAMI_SITUACIONECONOMICA)
levels(datos$FAMI_SITUACIONECONOMICA)

levels(datos$ESTU_DEDICACIONLECTURADIARIA)
datos$ESTU_DEDICACIONLECTURADIARIA <- as.character(datos$ESTU_DEDICACIONLECTURADIARIA)
datos$ESTU_DEDICACIONLECTURADIARIA[datos$ESTU_DEDICACIONLECTURADIARIA == ""] <- NA
datos$ESTU_DEDICACIONLECTURADIARIA <- as.factor(datos$ESTU_DEDICACIONLECTURADIARIA)
levels(datos$ESTU_DEDICACIONLECTURADIARIA)
datos$ESTU_DEDICACIONLECTURADIARIA <- factor(datos$ESTU_DEDICACIONLECTURADIARIA,
                                             levels(datos$ESTU_DEDICACIONLECTURADIARIA)[c(5,1,3,2,4)])
levels(datos$ESTU_DEDICACIONLECTURADIARIA)

levels(datos$ESTU_DEDICACIONINTERNET)
datos$ESTU_DEDICACIONINTERNET <- as.character(datos$ESTU_DEDICACIONINTERNET)
datos$ESTU_DEDICACIONINTERNET[datos$ESTU_DEDICACIONINTERNET == ""] <- NA
datos$ESTU_DEDICACIONINTERNET <- as.factor(datos$ESTU_DEDICACIONINTERNET)
levels(datos$ESTU_DEDICACIONINTERNET)
datos$ESTU_DEDICACIONINTERNET <- factor(datos$ESTU_DEDICACIONINTERNET,
                                             levels(datos$ESTU_DEDICACIONINTERNET)[c(5,1,3,2,4)])
levels(datos$ESTU_DEDICACIONINTERNET)

levels(datos$ESTU_HORASSEMANATRABAJA)
datos$ESTU_HORASSEMANATRABAJA <- as.character(datos$ESTU_HORASSEMANATRABAJA)
datos$ESTU_HORASSEMANATRABAJA[datos$ESTU_HORASSEMANATRABAJA == ""] <- NA
datos$ESTU_HORASSEMANATRABAJA <- as.factor(datos$ESTU_HORASSEMANATRABAJA)
levels(datos$ESTU_HORASSEMANATRABAJA)
datos$ESTU_HORASSEMANATRABAJA <- factor(datos$ESTU_HORASSEMANATRABAJA,
                                        levels(datos$ESTU_HORASSEMANATRABAJA)[c(1,5,2,3,4)])
levels(datos$ESTU_HORASSEMANATRABAJA)

levels(datos$ESTU_TIPOREMUNERACION)
datos$ESTU_TIPOREMUNERACION <- as.character(datos$ESTU_TIPOREMUNERACION)
datos$ESTU_TIPOREMUNERACION[datos$ESTU_TIPOREMUNERACION == ""] <- NA
datos$ESTU_TIPOREMUNERACION <- as.factor(datos$ESTU_TIPOREMUNERACION)
levels(datos$ESTU_TIPOREMUNERACION)
datos$ESTU_TIPOREMUNERACION <- factor(datos$ESTU_TIPOREMUNERACION,
                                        levels(datos$ESTU_TIPOREMUNERACION)[c(1,2,4,3)])
levels(datos$ESTU_TIPOREMUNERACION)

levels(datos$COLE_BILINGUE)
datos$COLE_BILINGUE <- as.character(datos$COLE_BILINGUE)
datos$COLE_BILINGUE[datos$COLE_BILINGUE == ""] <- NA
datos$COLE_BILINGUE <- as.factor(datos$COLE_BILINGUE)
levels(datos$COLE_BILINGUE)

levels(datos$COLE_CARACTER)
datos$COLE_CARACTER <- as.character(datos$COLE_CARACTER)
datos$COLE_CARACTER[datos$COLE_CARACTER == ""] <- NA
datos$COLE_CARACTER <- as.factor(datos$COLE_CARACTER)
levels(datos$COLE_CARACTER)
datos$COLE_CARACTER <- factor(datos$COLE_CARACTER,
                                      levels(datos$COLE_CARACTER)[c(1,3,4,2)])
levels(datos$COLE_CARACTER)

levels(datos$COLE_JORNADA)
datos$COLE_CARACTER <- as.character(datos$COLE_CARACTER)
datos$COLE_CARACTER[datos$COLE_CARACTER == ""] <- NA
datos$COLE_CARACTER <- as.factor(datos$COLE_CARACTER)
levels(datos$COLE_CARACTER)

levels(datos$ESTU_NSE_INDIVIDUAL)
datos$ESTU_NSE_INDIVIDUAL <- as.character(datos$ESTU_NSE_INDIVIDUAL)
datos$ESTU_NSE_INDIVIDUAL[datos$ESTU_NSE_INDIVIDUAL == ""] <- NA
datos$ESTU_NSE_INDIVIDUAL[datos$ESTU_NSE_INDIVIDUAL == "1"] <- "NSE1"
datos$ESTU_NSE_INDIVIDUAL[datos$ESTU_NSE_INDIVIDUAL == "2"] <- "NSE2"
datos$ESTU_NSE_INDIVIDUAL[datos$ESTU_NSE_INDIVIDUAL == "3"] <- "NSE3"
datos$ESTU_NSE_INDIVIDUAL[datos$ESTU_NSE_INDIVIDUAL == "4"] <- "NSE4"
datos$ESTU_NSE_INDIVIDUAL <- as.factor(datos$ESTU_NSE_INDIVIDUAL)
levels(datos$ESTU_NSE_INDIVIDUAL)

# Resumen de los datos
sink("Resumen.txt")
summary(datos)
sink()

# Desviaciones estandar
sd(datos$PERCENTIL_LECTURA_CRITICA)
sd(datos$PERCENTIL_MATEMATICAS)
sd(datos$PERCENTIL_C_NATURALES)
sd(datos$PERCENTIL_SOCIALES_CIUDADANAS)
sd(datos$PERCENTIL_INGLES)
global <- na.omit(datos$PERCENTIL_GLOBAL)
sd(global)

install.packages("modeest") # Libreria para calcular la moda
library(modeest)
mlv(datos$PERCENTIL_LECTURA_CRITICA, method = "mfv")
mlv(datos$PERCENTIL_MATEMATICAS, method = "mfv")
mlv(datos$PERCENTIL_C_NATURALES, method = "mfv")
mlv(datos$PERCENTIL_SOCIALES_CIUDADANAS, method = "mfv")
mlv(datos$PERCENTIL_INGLES, method = "mfv")
mlv(datos$PERCENTIL_GLOBAL, method = "mfv")

# Histogramas de las variables numericas de interes
hist(datos$PERCENTIL_LECTURA_CRITICA)
hist(datos$PERCENTIL_C_NATURALES)
hist(datos$PERCENTIL_SOCIALES_CIUDADANAS)
hist(datos$PERCENTIL_MATEMATICAS)
hist(datos$PERCENTIL_INGLES)
hist(datos$PERCENTIL_GLOBAL)
hist(datos$EDAD, breaks = 20, col = "blue")

# BoxPlot de las variables numericas
        # Graficos de violin
library(vioplot)
vioplot(datos$EDAD ~ datos$COLE_DEPTO_UBICACION,
        col = c("blue", "lightblue"), xlab = "Departamento", ylab = "Edad")
par(mfrow = c(2,1))
vioplot(datos$PERCENTIL_LECTURA_CRITICA ~ datos$COLE_DEPTO_UBICACION,
        col = c("blue", "lightblue"), xlab = "Departamento", ylab = "Lectura Crítica")
vioplot(datos$PERCENTIL_MATEMATICAS ~ datos$COLE_DEPTO_UBICACION,
        col = c("blue", "lightblue"), xlab = "Departamento", ylab = "Matemáticas")
vioplot(datos$PERCENTIL_C_NATURALES ~ datos$COLE_DEPTO_UBICACION,
        col = c("blue", "lightblue"), xlab = "Departamento", ylab = "Ciencias Naturales")
vioplot(datos$PERCENTIL_SOCIALES_CIUDADANAS ~ datos$COLE_DEPTO_UBICACION,
        col = c("blue", "lightblue"), xlab = "Departamento", ylab = "Ciencias Sociales y Ciudadanas")
vioplot(datos$PERCENTIL_INGLES ~ datos$COLE_DEPTO_UBICACION,
        col = c("blue", "lightblue"), xlab = "Departamento", ylab = "Inglés")
vioplot(datos$PERCENTIL_GLOBAL ~ datos$COLE_DEPTO_UBICACION,
        col = c("blue", "lightblue"), xlab = "Departamento", ylab = "Global")
        
        #Boxplot
boxplot(datos$EDAD ~ datos$COLE_DEPTO_UBICACION, data = datos, xlab = "Departamento",
        ylab = "Edad", col = rep(c("blue", "lightblue"),4))
boxplot(datos$PERCENTIL_LECTURA_CRITICA ~ datos$COLE_DEPTO_UBICACION, data = datos, xlab = "Departamento",
        ylab = "Percentil Lectura Critica", col = rep(c("blue", "lightblue"),4))
boxplot(datos$PERCENTIL_MATEMATICAS ~ datos$COLE_DEPTO_UBICACION, data = datos, xlab = "Departamento",
        ylab = "Matemáticas", col = rep(c("blue", "lightblue"),4))
boxplot(datos$PERCENTIL_C_NATURALES ~ datos$COLE_DEPTO_UBICACION, data = datos, xlab = "Departamento",
        ylab = "Ciencias Naturales", col = rep(c("blue", "lightblue"),4))
boxplot(datos$PERCENTIL_SOCIALES_CIUDADANAS ~ datos$COLE_DEPTO_UBICACION, data = datos, xlab = "Departamento",
        ylab = "Percentil Ciencias Sociales y Ciudadanas", col = rep(c("blue", "lightblue"),4))
boxplot(datos$PERCENTIL_INGLES ~ datos$COLE_DEPTO_UBICACION, data = datos, xlab = "Departamento",
        ylab = "Percentil Inglés", col = rep(c("blue", "lightblue"),4))
boxplot(datos$PERCENTIL_GLOBAL ~ datos$COLE_DEPTO_UBICACION, data = datos, xlab = "Departamento",
        ylab = "Percentil Global", col = rep(c("blue", "lightblue"),4))

# Borrado de columnas que no haran parte del modelado y creacion de la data final

borrar <- c("ESTU_TIPODOCUMENTO", "ESTU_NACIONALIDAD", "ESTU_FECHANACIMIENTO",
            "ESTU_ESTUDIANTE", "ESTU_PAIS_RESIDE", "ESTU_TIENEETNIA", "ESTU_ETNIA",
            "ESTU_DEPTO_RESIDE", "COLE_MCPIO_UBICACION", "ESTU_MCPIO_RESIDE",
            "ESTU_HORASSEMANATRABAJA", "ESTU_TIPOREMUNERACION", "COLE_NOMBRE_ESTABLECIMIENTO",
            "COLE_NOMBRE_SEDE", "COLE_SEDE_PRINCIPAL", "COLE_MCPIO_UBICACION",
            "ESTU_PRIVADO_LIBERTAD", "ESTU_MCPIO_PRESENTACION", "ESTU_DEPTO_PRESENTACION",
            "PUNT_LECTURA_CRITICA", "PERCENTIL_LECTURA_CRITICA", "DESEMP_LECTURA_CRITICA",
            "PUNT_MATEMATICAS", "PERCENTIL_MATEMATICAS", "DESEMP_MATEMATICAS",
            "PUNT_C_NATURALES", "PERCENTIL_C_NATURALES", "DESEMP_C_NATURALES",
            "PUNT_SOCIALES_CIUDADANAS", "PERCENTIL_SOCIALES_CIUDADANAS", "DESEMP_SOCIALES_CIUDADANAS",
            "PUNT_INGLES", "PERCENTIL_INGLES", "DESEMP_INGLES", "PUNT_GLOBAL", "ESTU_INSE_INDIVIDUAL",
            "ESTU_NSE_INDIVIDUAL", "ESTU_NSE_ESTABLECIMIENTO", "ESTU_ESTADOINVESTIGACION",
            "ESTU_PILOPAGA", "ESTU_GENERACION.E")

        # Eliminación de las columnas e instancias con valores NA
dataf <- datos[,!(names(datos) %in% borrar)]
map_dbl(dataf, .f = function(x){sum(is.na(x))})
dataTotalSinNA <- na.omit(datos)
write.csv(dataTotalSinNA, file = "dataTotalSinNA")

        # La clase de categoriza en dos valores de acuerdo al rendimiento
typeof(dataf$PERCENTIL_GLOBAL)
dataf$PERCENTIL_GLOBAL <- as.character(dataf$PERCENTIL_GLOBAL)
dataf$PERCENTIL_GLOBAL[dataf$PERCENTIL_GLOBAL %in% c(0:68)] <- "Mínimo o insuficiente"
dataf$PERCENTIL_GLOBAL[dataf$PERCENTIL_GLOBAL %in% c(69:100)] <- "Satisfactorio o avanzado"
        
        # En caso de desarrollar un modelo predictivo con las cuatro clases de rendimiento
#dataf$PERCENTIL_GLOBAL[dataf$PERCENTIL_GLOBAL %in% c(0:40)] <- "Insuficiente"
#dataf$PERCENTIL_GLOBAL[dataf$PERCENTIL_GLOBAL %in% c(41:68)] <- "Minimo"
#dataf$PERCENTIL_GLOBAL[dataf$PERCENTIL_GLOBAL %in% c(69:80)] <- "Satisfactorio"
#dataf$PERCENTIL_GLOBAL[dataf$PERCENTIL_GLOBAL %in% c(81:100)] <- "Avanzado"

write.csv(dataf, file = "dataConNA.csv")
datafSinNA <- na.omit(dataf)
write.csv(datafSinNA, file = "dataSinNA.csv")

# Creacion del conjunto de testeo
datatest <- datafSinNA[datafSinNA$PERIODO == "2019-03-30",]
write.csv(datatest, file="test.csv")

# Creacion del conjunto de entrenamiento
dataFinal <- datafSinNA[datafSinNA$PERIODO != "2019-03-30",]
write.csv(dataFinal, file = "dataFinal.csv")

set.seed(123)
library(caret)
indice <- createDataPartition(y = dataFinal$PERCENTIL_GLOBAL, p = .7, list = FALSE, times = 1)
entrenamiento <- dataFinal[indice,]
write.csv(entrenamiento, file = "entrenamiento.csv") # Escritura a csv

# Creacion de conjunto de validacion
validacion <- dataFinal[-indice,]
write.csv(validacion, file = "validacion.csv")
