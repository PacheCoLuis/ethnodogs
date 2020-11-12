# LEE Y AJUSTA BASE DE DATOS DE HUNT ALLOCATION (HA), SANTA CRUZ, TOLEDO, BELICE.

# ENTREVISTA DISENIADA Y REALIZADA POR LUIS PACHECO-COBOS, AFILIADO AL
# PROYECTO HUMAN AND SOCIAL DYNAMICS COLLABORATIVE RESEARCH [NSF GRANT
# 0827277] "DEVELOPMENT AND RESILIENCE OF COMPLEX SOCIOECONOMIC SYSTEMS:
# A THEORETICAL MODEL AND CASE STUDY FROM THE MAYA LOWLANDS" BAJO LA
# DIRECCION DE BRUCE WINTERHALDER (UC-DAVIS) DURANTE JULIO 2011 Y ENERO-
# DICIEMBRE 2012. DATOS CODIFICADOS Y ANALIZADOS POR JORGE ALEJANDRO
# VALENCIA GARCIA, MAESTRIA EN CIENCIAS BIOLOGICAS, (FB-X, UV) 2019.


############################ CARGANDO DATOS
#setwd("")
HA <- read.csv("HA_fecha_ed.CSV")
# Nombres de los cazadores 'name.hun' omitidos en esta
# version para proteger la identidad de los informantes

############################ AJUSTES PRELIMINARES
Sys.setenv(TZ="America/Mexico_City")
# Asignando el formato de fecha y hora a las variables correspondientes #,tz="America/Belize"
HA$date.hunt <- as.POSIXct(HA$date.hunt,tz="America/Belize",format="%d/%m/%Y",usetz=TRUE)
HA$date.surv <- as.POSIXct(HA$date.surv,tz="America/Belize",format="%d/%m/%Y",usetz=TRUE)
horai <- as.POSIXct(paste0(HA$date.hunt," ",HA$hunt.start),tz="America/Belize",format="%Y-%m-%d %H:%M",usetz=TRUE)
horaf <- as.POSIXct(paste0(HA$date.hunt," ",HA$hunt.end),tz="America/Belize",format="%Y-%m-%d %H:%M",usetz=TRUE)


# Convirtiendo de libras a kilogramos 'pweight1' a 'pweight4'
HA$pweight1<-HA$pweight1*0.454
HA$pweight2<-HA$pweight2*0.454
HA$pweight3<-HA$pweight3*0.454
HA$pweight4<-HA$pweight4*0.454


# Calculando la duracion de cada salida...
#     para las ocasiones en que la duracion fue < cero, i.e. casos en que la caceria termino al
#     siguiente dia del que comenzo: sumamos a 'horaf ' el total de segundos correspondientes
#     a un dia para que la duracion resultante sea > 0
which(horaf-horai < 0) # 8 18 19 30 62
horaf[8] <- horaf[8]+(3600*24)    #HA n08
horaf[18] <- horaf[18]+(3600*24)  #HA n18
horaf[19] <- horaf[19]+(3600*24)  #HA n19
horaf[30] <- horaf[30]+(3600*24)  #HA n32
horaf[62] <- horaf[62]+(3600*24)  #HA n60ii
which(horaf-horai < 0)
#     creamos la variable 'HA$dura' para incluir dentro del objeto 'HA'
#     la duracion de cada salida en el formato de fecha y hora que R maneja
HA$dura <- horaf-horai
range(HA$dura,na.rm=TRUE) # nota: rango de duraciones sin casos NA
rm(horaf,horai)


# Omitiendo casos (pesca y trampas) que no fueron de caceria
# substraccion por tecnologia '$weapon' y presa '$prey1'...

# ...revisando los niveles
#levels(HA$weapon)
unique(HA$weapon) # estan las cuatro
#levels(HA$prey1)
unique(HA$prey1) # caso HA n60b: ajustado con is.na(HA$prey1) y 'HA$other.res' == 'JUTE'
#levels(HA$tripurp)
unique(HA$tripurp) # "HUNT AND FISH" debe ser "HUNT"

# ...revisando los casos NA
which(is.na(HA$weapon))
which(is.na(HA$prey1)) # cinco casos: filas 21 33 34 47 55

# Casos a omitir (3er intento)
otros <- c(which(HA$weapon=="STRIKEGUN" | HA$weapon=="TRAP" | HA$prey1=="FISH"))


# Asignacion de marco de datos HAsub
HAsub <- HA[-otros,]
dim(HAsub) # 84 filas 90 variables
#HAsub$weapon <- droplevels(HAsub$weapon)
#HAsub$prey1 <- droplevels(HAsub$prey1)
#levels(HAsub$weapon)
#levels(HAsub$prey1)
#levels(HAsub$dogs.used)
which(HAsub$weapon=="STRIKEGUN") # integer(0)
which(HAsub$weapon=="TRAP") # integer(0)
which(HAsub$prey1=="FISH") # integer(0)
rm(otros)

# variable que indica el total de presas cazadas por salida, independiente de la especie
HAsub$pnkillt<-c()
for (i in 1:length(HAsub$pnkill1)){
  HAsub$pnkillt[i]<-sum(HAsub$pnkill1[i],HAsub$pnkill2[i],HAsub$pnkill3[i],na.rm = TRUE)
  }
HAsub$pnkillt

# variable que indica el total de presas avistadas por salida, independiente de la especie
HAsub$pnumt<-c()
for (i in 1:length(HAsub$pnum1)){
  HAsub$pnumt[i]<-sum(HAsub$pnum1[i],HAsub$pnum2[i],HAsub$pnum3[i],na.rm = TRUE)
}
HAsub$pnumt

HAsub$pweightt<-c()
for (i in 1:length(HAsub$pweight1)){
  HAsub$pweightt[i]<-sum(HAsub$pweight1[i],HAsub$pweight2[i],HAsub$pweight3[i],na.rm = TRUE)
}
HAsub$pweightt


dim(HAsub) # 84 filas 87 variables


# removiendo objetos que no se usaran en el analisis
ls()
rm(HA,i)

############################ FIN