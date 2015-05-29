##-------------------------------------------------##
##-----------      Modelos Linelaes     -----------##
##-----------        Trabajo 01         -----------##
##-- Nombre:Ana Julia Escobar


# 2.1 Leer el archivo de datos data.txt, y analizar de que estructura de datos se trata.
# Utilice la función read.table()
data <- read.table("data.txt", header = TRUE, dec=",", sep="\t")
str(data)
names(data)
# 2.2 Calcular el mínimo, la media, el máximo de la variable Edad.
# Utilice las funciones min(), mean(), max(), de ser necesario utilice 
# el parámetro na.rm = TRUE

edad <- data[,1]
edad
menor_edad<-min(edad,na.rm=T)
media_edad<-mean(edad,na.rm=T)
mayor_edad<-max(edad,na.rm=T)
media_edad
menor_edad
mayor_edad
# 2.3 Para la variable Genero, contar cuantos sujetos son de Genero: Femenino.
# Utilice la función table()

data_genero <- subset(data,subset=data[,"Genero"]=="Femenino")
table(data_genero[,3])

# 2.4 Encontrar la Edad mínima, media, máxima de los sujetos que Si son dependientes.

data_dependientes<-subset(data, subset=data[,"Dependiente"]=="Si")
table(data_dependientes[,"Dependiente"])
edad_dependientes<-data_dependientes[,"Edad"]
minimo_dependientes<-min(edad_dependientes,na.rm =TRUE )
maximo_dependientes<-max(edad_dependientes, na.rm =TRUE)
media_dependientes<-mean(edad_dependientes,na.rm = TRUE)
minimo_dependientes
maximo_dependientes
media_dependientes

# 2.5 Identificar el tipo de elementos que contiene cada variable.
# Utilice la función typeof()

tipo_elemento <- numeric(ncol(data))
for (i in 1:ncol(data)){
  tipo_elemento[i] <- typeof(data[,i])
}
tipo_elemento

# 2.6 Identificar la clase de cada variable (columna).
# Utilice la función class()

clase_variable <- numeric(ncol(data))
for (i in 1:ncol(data)){
  clase_variable[i] <- class(data[,i])
}
clase_variable

# 2.7 Calcular la media de todas las variables numéricas (double, integer).
# Recordar que para un factor no es posible obtener la media debido a que 
# éstos representan variables

media_numericas<- numeric(ncol(data))
for (i in 1:ncol(data)){
  media_numericas[i] <- mean(data[,i],na.rm =TRUE)
}
media_numericas

# 2.8 Calcular el porcentaje de valores perdidos que contiene cada variable.
# Utilice la función is.na()

perdidos <- numeric(ncol(data))
for (i in 1:ncol(data)){
  perdidos[i] <- sum(is.na(data[,i]))
}
perdidos
prop.table(perdidos)

# 3. Selecionando sujetos mediante un determinado criterio:
# 3.1 Seleccione los sujetos con una Edad mayor a 40 años.
# Utilice la función subset()

data_edadmayor <- subset(data,subset=data[,"Edad"]>40)
table(data_edadmayor[,"Edad"])

# 3.2 Seleccione los sujetos que tienen Vivienda Propia.
data_vivienda<- subset(data,subset=data[,"Vivienda"]=="Propia")
table(data_vivienda[,"Vivienda"])

# 3.3 Seleccione los sujetos que tienen más ($>$) de dos cargas familiatres.

data_cargasfamiliares<- subset(data,subset=data[,"Cargas"]>2)
table(data_cargasfamiliares[,"Cargas"])

# 3.4 Seleccione los sujetos con una Deuda superior o igual a 500 dólares
# y más ($>$) de 8 Dias_Atraso.

data_deuda<- subset(data,(subset=data[,"Deuda"]>=500)&(subset=data[,"Dias_Atraso"]>8))
table(data_deuda[,"Deuda"],data_deuda[,"Dias_Atraso"])

# 3.5 Seleccione los sujetos con un Score mayor o igual a 900 puntos, una Edad menor
# o igual a 35 años y con más ($>$) de 3 tarjetas de crédito (Numero_TC).

data_score <- subset(data,(subset=data[,"Score"]>=900)&(subset=data[,"Edad"]<=35)&(subset=data[,"Numero_TC"]>3))
table(data_score[,"Score"],data_score[,"Edad"],data_score[,"Numero_TC"])

# 4. Gráficos:
# 4.1 Realice un histograma de la variable Edad, utilice como color de relleno: red

hist(edad,col = "red",main="Edad")

# 4.2 Realice un diagrama de cajas de la variable Edad, utilice como color de relleno: green
# Utilice la función boxplot()

boxplot(edad,col = "green",main="Edad")


