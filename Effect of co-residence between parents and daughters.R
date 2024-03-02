##Effect of co-residence between parents and daughters##
##Cedeplar: 2022
##Ali M. Arrieta-Arrieta



##Db: EDER 2017:  https://www.inegi.org.mx/programas/eder/2017/#microdatos##


rm(list=ls())
gc()
setwd("C:/Users/Alí/Documents/PDh_Cedeplar/Primer_year/Primer_Semestre/Estadistica/Trabajo Final/EDER_2017/eder2017_bases_sav")

#install.packages("ggbreak")
#install.packages("ggbreak")
install.packages("tidyverse")
library(tidyverse)
library(car) 
library(fdth)
library(data.table)
library(ggplot2)
# library(ggbreak) 
# library(patchwork)

###################################################################################
#I. PARTE: PRIMERA UNION  
HV <- read.csv("historiavida.csv", header=TRUE)
Individuos <- distinct(HV_unidas, key)
rm(Individuos)
##23,831 individuos##

HV <- filter(HV, sexo>=2)
write.csv(HV, file="C:/Users/Alí/Documents/PDh_Cedeplar/Primer_year/Primer_Semestre/Estadistica/Trabajo Final/Base_Mujeres.csv") 
HV_unidas <- read.csv("C:/Users/Alí/Documents/PDh_Cedeplar/Primer_year/Primer_Semestre/Estadistica/Trabajo Final/Base_Mujeres.csv", header=TRUE)

##Mujeres con una primera union##

HV_unidas <- filter(HV_unidas, matrimonio==1) #se mantienen las mujeres con una unión#
HV_unidas <- filter(HV_unidas, edo_civil1>=1) #se eliminan las mujeres que manifiestan tener solo una union pero manifiestan estan solteras#
HV_unidas <- unite(HV_unidas, key, c("folioviv", "foliohog", "id_pobla"), sep = "", remove = T)
Mujeres <- distinct(HV_unidas, key) ##9,036 mujeres##
rm(Mujeres)


HV_unidas <- HV_unidas %>% group_by(anio_retro) %>% arrange(key) 
HV_unidas <- filter(HV_unidas, nom_cony1>0) ##Se eliminan las observaciones que no tienen información de la primera union#

HV_unidas <-  HV_unidas  %>% 
                 mutate( 
                        Aux1= case_when(
                        edo_civil1==6|edo_civil1==7|edo_civil1==17~0.5,
                        edo_civil1==18|edo_civil1==60|edo_civil1==70|edo_civil1==80~0,
                        TRUE ~ 1),
                        Aux2=case_when(
                        cor_union1==0| cor_union1==4 ~ 0,
                        cor_union1==1|cor_union1==3 ~ 0.5,
                        TRUE ~ 1)
                        )

table(HV_unidas$Aux2) #Años en los que la mujer corresidio con el primer conyugue##
table(HV_unidas$cor_union1) #la suma de las categorias de la segunda variable tienen que coincidir con la primera#


###################################################################################
#II. PARTE: AÑOS PERSONA UNIDOS
##Se crean los APU [Años personas unidos] de la primera unión
APU <- HV_unidas %>% 
                 group_by(key) %>% 
                 summarise(APU = sum(Aux2))

mean(APU$APU) ##El calculo correcto es con la variable de corresidencia porque no sobrestima los periodos##
sd.default(APU$APU)
median(APU$APU)


##Proceso para pegarle los APU de cada mujer##

HV_unidas <- merge(x=HV_unidas,y=APU, by = c("key"), all.x=TRUE)

###################################################################################
#III. PARTE: TIPO O NATURALEZA DE LA PRIMERA UNION
##  NATURALEZA DE LA PRIMERA UNION##
##Codigos: 1: union libre; 2:matrimonio civil; 3: matrimonio civil y religioso##

A <- table(HV_unidas$edo_civil1)
B <- round((prop.table(A)),3)*100 ##Para conocer la frecuencia de los grupos##

Tip_union <- HV_unidas %>% 
             group_by(key) %>% 
             filter(row_number()==1) ##Me quedo solo con la primera fila de cuando comenzo la primera union para conocer la naturaleza o tipo de union##

Tip_union <-  Tip_union  %>% 
              mutate(Edad_one_union = edad_retro,
              naturaleza= case_when( 
              edo_civil1==1|edo_civil1==12|edo_civil1==13|edo_civil1==14|edo_civil1==17|edo_civil1==18 ~ 1,
              edo_civil1==2|edo_civil1==26|edo_civil1==27|edo_civil1==28 ~ 2,
              edo_civil1==3|edo_civil1==4|edo_civil1==37|edo_civil1==46|edo_civil1==47|edo_civil1==48~3),
              naturaleza=factor(naturaleza,levels = c("1","2", "3"), labels = c("UL", "MC", "MCR")))

table(Tip_union$naturaleza) ##Para conocer la distribución de los tres grupos de interes##

##Promedios de APU por tipo de primera union y general
Tip_union %>%  group_by(naturaleza) %>%                        
  summarise_at(vars(APU),
               list(name = mean))
mean(APU$APU)


##COHORTES ANALIZADAS##

table(Tip_union$anio_nac)
##Nota: se eliminan las mujeres que nacieron después de 1989, porque estas mujeres tienen muy poco tiempo para experimentar el evento de interes, 
##es decir, una primera unión y los APU son muy pocos. Por tanto se trabaja con 3 cohortes##

Tip_union <-  Tip_union  %>% 
  mutate(Cohorte_nto=case_when(
    anio_nac>=1962 & anio_nac<=1969~1,
    anio_nac>=1970 & anio_nac<=1979~2,
    anio_nac>=1980 & anio_nac<=1989~3),
    Cohorte_nto=factor(Cohorte_nto,levels = c("1","2","3"), labels = c("Coh.1962-69", "Coh.1970-79", "Coh.1980-89")))
table(Tip_union$Cohorte_nto)

###################################################################################
#IV.PARTE: NO CORRESIDENCIA CON LOS PADRES EN LA ADOLESCENCIA

##NO Corresidecia con los padres durante la adolescencia##

##Tipo de NO corresidencia##
##1: no corresidencia paterna; 2: no corresidencia materna; 3: ambas

Adolescencia <- read.csv("C:/Users/Alí/Documents/PDh_Cedeplar/Primer_year/Primer_Semestre/Estadistica/Trabajo Final/Base_Mujeres.csv", header=TRUE)
Adolescencia <- filter(Adolescencia, edad_retro>=12 & edad_retro<=19) ##Me quedo con el rago de edad de 12 a 19 años como rango de adolescencia##
Adolescencia <- unite(Adolescencia, key, c("folioviv", "foliohog", "id_pobla"), sep = "", remove = F)

table(Adolescencia$padre_cor)
table(Adolescencia$madre_cor)
table(Adolescencia$edad_retro)

Prueba <- Adolescencia %>% 
           count(key, sort = TRUE)

Prueba %>% summarise_at(vars(n), ##Prueba para corroborar que se selecciono correctamente el rango de adolescencia##
            list(name = mean))
rm(Prueba)

Adolescencia <-  Adolescencia  %>% 
                 mutate( 
                 Aux1=case_when(
                 padre_cor==1| padre_cor==2| padre_cor==91| padre_cor==92 ~ 0,
                 TRUE ~ 1),
                 Aux2=case_when(
                 madre_cor==1| madre_cor==2| madre_cor==91| madre_cor==92 ~ 0,
                 TRUE ~ 1),
                 Tipo_NC=case_when(
                 Aux1==0 & Aux2==0~0,
                 Aux1==1 & Aux2==0~1,
                 Aux1==0 & Aux2==1~2,
                 Aux1==1 & Aux2==1~3,
                 TRUE ~0),
                 Tipo_NC= factor(Tipo_NC, levels = c("0", "1", "2", "3"), 
                                 labels = c("Corresid", "Paterna", "Materna", "Ambas")))

table(Adolescencia$Tipo_NC) 

Tipo_NC <- Adolescencia %>% 
                group_by(key) %>% 
                slice(which.max(Tipo_NC)) %>%
                arrange(key)

table(Tipo_NC$Tipo_NC) ##Tabla por tipos de NO corresidencia; 0 significa que corresidio con ambos padres en la adolescencia. Ambas; significa que por lo menos un año no corresidio con ninguno de los dos padres##

Tipo_NC <- select(Tipo_NC, key, Tipo_NC)

##Nivel de la NO corresidencia##

##1: temprana (12-14 años);  

Temprana <- filter(Adolescencia, edad_retro>=12 & edad_retro<=14)

Temprana <-  Temprana  %>% 
             mutate( 
             NC_temp=case_when(
             Aux1==0 & Aux2==0~0,
             Aux1==1 | Aux2==1~1))

Temprana <- Temprana %>% group_by(key) %>% slice(which.max(NC_temp)) ##Si por lo menos un año de la adolescencia temprana no corresidio con los padres##
table(Temprana$NC_temp) ##Tabla; 1: por lo menos un año de la etapa temprana no corresidio##
Temprana <- select(Temprana,key,NC_temp)

## 2: tardía (15-19 años);
Tardia <- filter(Adolescencia, edad_retro>=15 & edad_retro<=19)

Tardia <-    Tardia  %>% 
             mutate( 
             NC_tardia=case_when(
             Aux1==0 & Aux2==0~0,
             Aux1==1 | Aux2==1~1))

Tardia <-  Tardia %>%  group_by(key) %>% slice(which.max(NC_tardia))

Tardia <- select(Tardia, key,NC_tardia)
table(Tardia$NC_tardia) ##Tabla; 1: por lo menos un año de la etapa tardia no corresidio##

##Union de ficheros##
##1: temprana; 2: tardia ; 3: toda la adolescencia

Nivel_NC <-  merge(x=Temprana, y=Tardia, by = c("key"), all.x=TRUE)

Nivel_NC <- Nivel_NC %>% 
                mutate(
                Nivel_NC= case_when(
                   NC_temp==0 & NC_tardia==0 ~ 1,
                   NC_temp==0 & NC_tardia==1 ~ 2,
                   NC_temp==1 & NC_tardia==0 ~ 2,
                   NC_temp==1 & NC_tardia==1 ~ 3),
                Nivel_NC=factor(Nivel_NC, levels = c("1", "2", "3"), labels = c("Correside", "Tardia", "Tempra-Tardia")) 
                   )
                 
table(Nivel_NC$Nivel_NC)                 
##Nota: las observaciones que experimentaron solo temprana fueron 23 casos, porque la mayoría de las que informan NO corresidencia temprana tambien lo hacen en la etapa tardia##


##Intensidad de la NO corresidencia##

Adolescencia  <- Adolescencia  %>% 
                    mutate( 
                    Corresidencia=case_when(
                    Aux1==0 & Aux2==0~0,
                    Aux1==1 | Aux2==1~1))

table(Adolescencia$Corresidencia) #Tabla: 1 significa que no corresidio por lo menos un año de la adolescencia con los padres#


APNC  <- Adolescencia %>% 
         group_by(key) %>% 
         summarise(APNC = sum(Corresidencia))

APNC <- APNC %>% mutate(
                 APNC_cat= case_when(
                 APNC==0 ~1,
                 APNC>=1 & APNC<=4~2,
                 APNC>=5 & APNC<=8~3),
                 APNC_cat=factor(APNC_cat, levels = c("1", "2", "3"), labels = c("Correside", "1-4APNC", "4-8APNC")))

table(APNC$APNC_cat) ##Tabla; correside significa que corresidio toda la adolescencia con los padres#


##Merge de todas las bases: Tipo, nivel e intensidad de la NO corresidencia##

Corresidencia <- merge(x=Tipo_NC, y=Nivel_NC, by = c("key"), all.x=TRUE)
Corresidencia <- merge(x=Corresidencia, y=APNC, by = c("key"), all.x=TRUE)


###################################################################################
#V.PARTE: VARIABLES INDEPENDIENTES##


##Se construye una variable de origen socioeconomico  de variables de antescentes referentes a las condiciones del hogar y vivienda##
##Para construir el indicador de estrato social de origen se usa el metodo de componentes principales

##ANALISIS DE COMPONENTES PRINCIPALES##

Antecedentes <- read.csv("C:/Users/Temporal/EDER_2017/antecedentes.csv", header=TRUE)
Antecedentes <- unite(Antecedentes, key, c("folioviv", "foliohog", "id_pobla"), sep = "", remove = FALSE)
rownames(Antecedentes) <- Antecedentes$key

##Variables para la construcción de el estrato socioeconomico de origen: 
##Television, computadora, telefono
##Estufa, refrigerador, lavadora, automovil
##Agua potable, baño interno, calle pavimentada
table(Antecedentes$television)
table(Antecedentes$compu)
table(Antecedentes$telefono)

table(Antecedentes$estufa)
table(Antecedentes$refrigera)
table(Antecedentes$lavadora_v)
table(Antecedentes$automovil)

table(Antecedentes$agua_entub)
table(Antecedentes$banio_int)
table(Antecedentes$calle_pav)

##Matriz de correlacion entre las variables##
MatrizCP <- select(Antecedentes, 
                   television,telefono,compu, estufa, refrigera, lavadora_v,
                   agua_entub, banio_int, calle_pav, automovil)

str(MatrizCP)
summary(MatrizCP)
MatrizCP <- na.omit(MatrizCP)
corr <- cor(MatrizCP) #Matriz de correlacion#
write.csv(corr, file="C:/Users/Alí/Documents/PDh_Cedeplar/Primer_year/Primer_Semestre/Estadistica/Trabajo Final/matriz_correlacion.csv") 

###Analisis de los componentes##

Resultados_CP <- prcomp(MatrizCP, center=TRUE,  scale=TRUE)
names(Resultados_CP)
summary(Resultados_CP) ##El primer componente CP1 explica más del 50% de la varianza

x <- data.frame(Resultados_CP$x)
MatrizCP$CP1 <- x$PC1
MatrizCP$CP2 <- x$PC2
cor_CP <- cor(MatrizCP) ##El nivel de correlacion entre las variables con el CP1 es alto. Superio al 60%, solo un caso es de 50%.

##Grafico de codo-elección del número de componente
e <- eigen(corr)$val
e <- round(e,4)
plot(e,type = "b", pch=20)
abline(h=1, lwd=2, col='#8c8c8c')

##Decision: bajo este criterio, estaría indicando que las dos primeras CP son las necesarias para el analisis.
##En este caso como nos interesa tener sólo un indice, procedi a elegir sólo el primer componente,
##basandome en que el CP1 explica en más del 50% de la varianza del grupo de variables analizadas.


##Gráfico de correlacion de las variables con los CP1 y CP2
biplot(x=Resultados_CP, scale = 0, cex=0.8, col=c('#8c8c8c', "#2f2c79"))
abline(h=0,v=0)
       
##Participacion de las variables por componentes##
corrvar_cp <- Resultados_CP$rotation %*% diag(Resultados_CP$sdev)
corrvar_cp <- corrvar_cp[, 1:2]
barplot(t(corrvar_cp), beside = TRUE, ylim = c(-1,1), col=c('#8c8c8c', "#2f2c79"))
legend(x="topright",  fill = c('#8c8c8c', "#2f2c79"), legend = c("Comp1", "Comp2"), lty=1:2, cex=0.2)


##Cuartiles del CP1##

quantile(MatrizCP$CP1, prob=c(0.25,0.5,0.75,1))
quantile(MatrizCP$CP1, prob=c(0,0.20,0.40,0.60,0.80,1))

MatrizCP <- MatrizCP %>% 
             mutate(
               Cuartiles_CP1= case_when(
                 CP1<=-1.6814964~4,
                 CP1>-1.681496 & CP1<=-0.2099101~3,
                 CP1>-0.2099101 & CP1<=1.9406580~2,
                 CP1>1.9406580 & CP1<=3.3211676~1),
               Cuartiles_CP1=factor(Cuartiles_CP1, levels = c("1", "2", "3", "4") , labels = c("Cuartil 1", "Cuartil 2", "Cuartil 3", "Cuartil 4")),
               Quintiles_CP1= case_when(
                 CP1<=-2.2653498~5,
                 CP1>-2.2653498 & CP1<=-0.8932803~4,
                 CP1>-0.8932803 & CP1<=0.5073069~3,
                 CP1>0.5073069 & CP1<=2.5928509~2,
                 CP1>2.5928509 & CP1<=3.3211676~1))

MatrizCP <- select(MatrizCP, CP1, CP2, Cuartiles_CP1, Quintiles_CP1) 
MatrizCP$key <- rownames(MatrizCP)
table(MatrizCP$Cuartiles_CP1)
table(MatrizCP$Quintiles_CP1)

Antecedentes <-  merge(x=Antecedentes, y=MatrizCP, by=c("key"), all.x = TRUE)

##Otras variables de interes de contexto en la adolescencia##

#escolaridad del padre
table(Antecedentes$nivel_p)

#escolaridad de la madre
table(Antecedentes$nivel_m)

#Estatus socioeconómico de la ocupación del padre
table(Antecedentes$sinco_p)
Antecedentes$sector = substr(Antecedentes$sinco_p,1,1)
table(Antecedentes$sector)

#Hijos de la madre
table(Antecedentes$hijos_m)

#Hijos nacidos antes del individuo
table(Antecedentes$hijos_nac)


##Lengua indigena por los padres
table(Antecedentes$lengua_p)
table(Antecedentes$lengua_m)

Antecedentes <- Antecedentes %>% 
mutate( 
  Num_hijos=case_when(
  hijos_m<=2~1, 
  hijos_m==3~2,
  hijos_m==4~3,
  hijos_m==5|hijos_m==6~4,
  hijos_m>=7~5),
Num_hijos_antes=case_when(
  hijos_nac==0~1, 
  hijos_nac==1~2,
  hijos_nac==2~3,
  hijos_nac==3~4,
  hijos_nac==4|hijos_nac==5~5,
  hijos_nac>=6~6),
Indigena_ma= case_when(
  lengua_m==1~1,
  lengua_m==2~0),
Indigena_pa= case_when(
  lengua_p==1~1,
  lengua_p==2~0),
Escolaridad_pa=case_when(
  nivel_p==0~1,
  nivel_p==1~2,
  nivel_p==2~2,
  nivel_p==3~3,
  nivel_p>=4 & nivel_p<=9~4,
  nivel_p>=10 & nivel_p<99~5),
Escolaridad_pa=factor(Escolaridad_pa, levels = c("1", "2", "3", "4", "5"), labels = c("Ninguno", "Primaria", "Secundaria", "Preparatoria", "Lic-maestr-doc")),
Escolaridad_ma=case_when(
  nivel_m==0~1,
  nivel_m==1~2,
  nivel_m==2~2,
  nivel_m==3~3,
  nivel_m>=4 & nivel_m<=9~4,
  nivel_m>=10 & nivel_m<99~5),
Escolaridad_ma=factor(Escolaridad_ma, levels = c("1", "2", "3", "4", "5"), labels = c("Ninguno", "Primaria", "Secundaria", "Preparatoria", "Lic-maestr-doc")),
Ocupacion_pa=case_when(
  sector<=3~1,
  sector==4| sector==5~2,
  sector==6~3,
  sector==7~4,
  sector==8| sector==9~5),
Ocupacion_pa=factor(Ocupacion_pa, levels = c("1", "2", "3", "4", "5"), labels = c("Profes.", "Comerc.", "Agro.", "Artesal.", "Otros")),
edad_primera= case_when(
    Edad_one_union <=17~1,
    Edad_one_union >=18 &  Edad_one_union <=19 ~2,
    Edad_one_union >=20 &  Edad_one_union <=24 ~3,
    Edad_one_union >=25~4),
    edad_primera=factor(edad_primera, levels = c("1", "2", "3", "4"), labels = c("<=17anos", "18-19anos", "20-24anos", ">=25anos") ))



table(DB$edad_primera)
table(Antecedentes$Num_hijos)
table(Antecedentes$Num_hijos_antes)
table(Antecedentes$Escolaridad_pa)
table(Antecedentes$Escolaridad_ma)
table(Antecedentes$Ocupacion_pa)

Antecedentes <- select(Antecedentes, key, CP1, CP2, Cuartiles_CP1, Quintiles_CP1,
                       Num_hijos, Num_hijos_antes, Escolaridad_pa,Escolaridad_ma, sector,Ocupacion_pa,
                       Indigena_ma, Indigena_pa)


##Variables de contexto de Adolescencia##
##Las variables de escolaridad y trabajo se construyen con base en la edad de 15 años##

year15 <- read.csv("C:/Users/Alí/Documents/PDh_Cedeplar/Primer_year/Primer_Semestre/Estadistica/Trabajo Final/Base_Mujeres.csv", header=TRUE)
year15 <- filter(year15, edad_retro==15)
year15 <- unite(year15, key, c("folioviv", "foliohog", "id_pobla"), sep = "", remove = FALSE)

table(year15$nivel_esc)
table(year15$trabajo)
table(year15$pos_tra)


year15 <- year15 %>%
          mutate(Estudiar=case_when(
          nivel_esc=="00"|nivel_esc=="901"|nivel_esc=="902"|nivel_esc=="903"|nivel_esc=="904"|nivel_esc=="905"|nivel_esc=="906"|nivel_esc=="907"|nivel_esc=="913" ~0,
          nivel_esc=="01"|nivel_esc=="02"|nivel_esc=="03"|nivel_esc=="04"|nivel_esc=="05"|nivel_esc=="06"|nivel_esc=="07"|nivel_esc=="08"|nivel_esc=="13"~1),
          Trabajar=case_when(
            pos_tra==0~0,
            pos_tra>0~1))

table(year15$Estudiar)
table(year15$Trabajar)

##Localidad##

Viviendas <- read.csv("vivienda.csv", header=TRUE)
Hogares <- read.csv("hogar.csv", header=TRUE)
Persona <- read.csv("persona.csv", header=TRUE)

DBAux <- merge(x=Hogares, y=Viviendas, by=c("folioviv"), all.x = TRUE)
DBAux <- merge(x=Persona, y=DBAux, by=c("folioviv","foliohog"), all.x = TRUE)

year15 <- merge(x=year15, y=DBAux, by=c("folioviv","foliohog","id_pobla"), all.x = TRUE)
year15 <- unite(year15, key, c("folioviv", "foliohog", "id_pobla"), sep = "", remove = FALSE)

table(year15$tam_loc)
table(year15$geo_eder)


year15$pais = substr(year15$geo_eder,1,3)
year15$estado = substr(year15$geo_eder, 4, 5)

table(year15$pais)
table(year15$estado)

##Region
year15 <- year15 %>%
          mutate(region=case_when(
          estado== "02"|estado== "03"|estado== "08"|estado== "25"|estado== "26"~1,
          estado== "05"|estado== "10"|estado== "19"|estado== "24"|estado== "28"~2,
          estado== "01"|estado== "06"|estado== "11"|estado== "14"|estado== "16"|estado== "18"|estado== "22"|estado== "32"~3,
          estado== "09"|estado== "15"|estado== "12"|estado== "13"|estado== "17"|estado== "21"|estado== "29"~4,
          estado== "04"|estado== "07"|estado== "20"|estado== "23"|estado== "27"|estado== "30"|estado== "31"~5),
          region=factor(region, levels = c("1","2","3","4","5"),  labels = c("Noroeste", "Noreste", "Occidente", "Centro", "Sureste") )
          )
          
table(year15$region)

year15 <- select(year15, key, Estudiar,Trabajar,tam_loc,geo_eder,pais,estado,region)


###################################################################################
#VI.PARTE: UNION DE TODOS LOS FICHEROS##
##UNIR TODAS LAS VARIABLES Y UNIFICAR LAS BASE FINAL DEL ESTUDIO##

DB <- merge(x=Tip_union, y=Corresidencia, by=c("key"), all.x = TRUE)
DB <- merge(x=DB, y=Antecedentes, by=c("key"), all.x = TRUE)
DB <- merge(x=DB, y=year15, by=c("key"), all.x = TRUE)
rownames(DB) <- DB$key

DB <- DB[,200:230]
write.csv(DB, file="C:/Users/Alí/Documents/PDh_Cedeplar/Primer_year/Primer_Semestre/Estadistica/Trabajo Final/Base_Master.csv") 




###################################################################################
#VII.PARTE: Estadisticas descriptivas####

DB <- read.csv("C:/Users/Alí/Documents/PDh_Cedeplar/Primer_year/Primer_Semestre/Estadistica/Trabajo Final/Base_Master.csv", header = TRUE) 

DB %>%  group_by(naturaleza) %>%                        
  summarise_at(vars(APU),
               list(name = mean))
mean(DB$APU)


A <- ftable(DB[, c("Cohorte_nto","naturaleza" )])
A1 <- round(prop.table(A,1),2)


DB %>%  group_by(Cohorte_nto) %>%                        
  summarise_at(vars(APU),
               list(name = mean))


##Por corresidencia
B <- table(DB$naturaleza, DB$APNC_cat)
B1 <- round(prop.table(B,1),2)

C <- table(DB$Cohorte_nto ,DB$naturaleza, DB$APNC_cat)
C1 <- round(prop.table(C,1),2)



D <- DB %>%  group_by(APNC_cat, naturaleza) %>%                        
             summarise_at(vars(APU),
               list(name = mean))

E <- DB %>%  group_by(Cohorte_nto, naturaleza) %>%                        
             summarise_at(vars(Edad_one_union),
               list(name = mean))
E <- na.omit(E)

F <- DB %>%  group_by(Cohorte_nto, APNC_cat, naturaleza) %>%                        
             summarise_at(vars(Edad_one_union),
               list(name = mean))
F <- na.omit(F)

grap10 <-  ggplot(data=F, aes(x=APNC_cat, y=name, fill=naturaleza)) + 
           geom_bar(stat="identity", position="dodge") +
           facet_wrap(~ Cohorte_nto)

grap10 +labs(title="", x="", y = "Edad promedio de la 1° Unión", 
            caption = "Fuente: elaboración propia con base en EDER-2017 
                               https://www.inegi.org.mx/programas/eder/2017/") +
scale_fill_manual(values=c("#132f49","#56b86f", '#8c8c8c')) 
  


G <- table(DB$naturaleza, DB$Tipo_NC)
G.1 <- round(prop.table(G,1),2)

H <- table(DB$naturaleza, DB$Nivel_NC)
H.1 <- round(prop.table(H,1),2)



##Analisis con el resto de variables independientes##

I   <- table(DB$naturaleza , DB$Cuartiles_CP1)
I.1 <- round(prop.table(I,1),2)


J <-  table(DB$naturaleza , DB$Escolaridad_pa)
J.1 <- round(prop.table(J,1),2)

H <-  table(DB$naturaleza , DB$Escolaridad_ma)
H.1 <- round(prop.table(H,1),2)

K <-  table(DB$naturaleza , DB$Ocupacion_pa )
K.1 <- round(prop.table(K,1),2)

L <-  table(DB$naturaleza , DB$tam_loc)
L.1 <- round(prop.table(L,1),2)


##GRAFICAS##

##Graph 1##

C <- data.frame(table(DB$Edad_one_union, DB$naturaleza))
C$Naturaleza <- C$Var2

grap1 <- ggplot(C, aes(x=Var1, y=Freq, group=Naturaleza, color=Naturaleza)) + 
  geom_line(aes(linetype=Naturaleza), size=0.8)
grap1 +labs(title="", x="Edad de la primera unión", y = "Frecuencia (N° Mujeres)",
            caption = "Fuente: elaboración propia con base en EDER-2017 
                               https://www.inegi.org.mx/programas/eder/2017/")+
  theme_classic() +
  scale_x_discrete( breaks = seq(0, 55, by = 5))+
  scale_color_manual(values=c("#efb810","#2f2c79",'#8c8c8c'))
ggsave( filename  =  'C:/Users/Alí/Documents/PDh_Cedeplar/Primer_year/Primer_Semestre/Estadistica/Trabajo Final/Grafica_1.jpg', width = 6, height = 4, dpi = 600)


##Graph 2##

DB <-  DB  %>% 
  mutate(Cohorte_nto1=case_when(
    anio_nac>=1962 & anio_nac<=1969~1,
    anio_nac>=1970 & anio_nac<=1979~2,
    anio_nac>=1980 & anio_nac<=1989~3),
    Cohorte_nto1=factor(Cohorte_nto1,levels = c("1","2","3"), labels = c("Coh.1962-69", "Coh.1970-79", "Coh.1980-89")))

D <- data.frame(table(DB$APU, DB$Cohorte_nto1))
D$Cohortes <- D$Var2
D$Freq[D$Freq == "0"] <- NA

grap2 <- ggplot(D, aes(x=Var1, y=Freq, group=Cohortes, color=Cohortes)) + 
  geom_point(aes(shape=Cohortes), stat="identity", size=1.5)
grap2 +labs(title="", x="Años persona unidos (Duración de la 1° Unión)", y = "N° Mujeres",
            caption = "Fuente: elaboración propia con base en EDER-2017 
                               https://www.inegi.org.mx/programas/eder/2017/")+
  theme_classic()+
  scale_x_discrete( breaks = seq(0, 55, by = 5))+
  scale_color_manual(values=c("#000020","#2f2c79",'#8c8c8c'))
ggsave( filename  =  'C:/Users/Alí/Documents/PDh_Cedeplar/Primer_year/Primer_Semestre/Estadistica/Trabajo Final/Grafica_2.jpg', width = 6, height = 4, dpi = 600)



##Graph 3##

E <- data.frame(table( DB$APU, DB$naturaleza))

grap3 <- ggplot(E, aes(x=Var1, y=Freq, group=Var2, color=Var2)) + 
  geom_point(aes(shape=Var2), stat="identity", size=1.5)
grap3 +labs(title="", x="Años persona unidos (Duración de la 1° Unión)", y = " Frecuencia (N°Mujeres)", 
            caption = "Fuente: elaboración propia con base en EDER-2017 
                               https://www.inegi.org.mx/programas/eder/2017/") +
  theme_classic() +
  scale_x_discrete( breaks = seq(0, 43, by = 5))+
  scale_color_manual(values=c("#efb810","#2f2c79",'#8c8c8c'))
ggsave( filename  =  'C:/Users/Alí/Documents/PDh_Cedeplar/Primer_year/Primer_Semestre/Estadistica/Trabajo Final/Grafica_3.jpg', width = 6, height = 4, dpi = 600)


##Graph 4##
O <- data.frame(table(DB$Cohorte_nto1, DB$APU, DB$naturaleza))
O$Tipo_Unión <- O$Var3
O$Freq[O$Freq == "0"] <- NA

grap4 <- ggplot(O, aes(x=Var2, y=Freq, group=Tipo_Unión, color=Tipo_Unión)) + 
  geom_point(aes(), stat="identity", size=1.3) +
  scale_x_discrete( breaks = seq(0, 43, by = 5))+
  facet_wrap(~ Var1)
grap4 +labs(title="", x="Años persona unidos (Duración de la 1° Unión)", y = " Frecuencia (N°Mujeres)", 
            caption = "Fuente: elaboración propia con base en EDER-2017 
                               https://www.inegi.org.mx/programas/eder/2017/") +
   theme_minimal() +
  scale_color_manual(values=c("#efb810","#2f2c79",'#8c8c8c'))
ggsave( filename  =  'C:/Users/Alí/Documents/PDh_Cedeplar/Primer_year/Primer_Semestre/Estadistica/Trabajo Final/Grafica_4.jpg', width = 6.9, height = 4.72, dpi = 600)


##Graph 5##

P <- data.frame(table(DB$Cohorte_nto1, DB$APU, DB$APNC_cat))
P$APNC <- P$Var3
P$Freq[P$Freq == "0"] <- NA

grap5 <- ggplot(P, aes(x=Var2, y=Freq, group=APNC, color=APNC)) + 
  scale_x_discrete( breaks = seq(0, 45, by = 5))+
  geom_point(aes(), stat="identity", size=1.5) +
  scale_x_discrete( breaks = seq(0, 43, by = 5))+
  facet_wrap(~ Var1)
grap5 +labs(title="", x="Años persona unidos (Duración de la 1° Unión)", y = " Frecuencia (N°Mujeres)", 
            caption = "Fuente: elaboración propia con base en EDER-2017 
                               https://www.inegi.org.mx/programas/eder/2017/") +
  theme_minimal() +
  scale_color_manual(values=c("#8db600",'#8c8c8c',"#2f2c79"))
ggsave( filename  =  'C:/Users/Alí/Documents/PDh_Cedeplar/Primer_year/Primer_Semestre/Estadistica/Trabajo Final/Grafica_5.jpg', width = 6.9, height = 4.72, dpi = 600)



##MODELOS: LOGISTICO MULTINOMIAL##

if(!require(nnet)){install.packages('nnet');library(nnet)}
if(!require(foreign)){install.packages('foreign');library(foreign)}
if(!require(zoo)){install.packages('zoo');library(zoo)}
if(!require(lmtest)){install.packages('lmtest');library(lmtest)}


DB <- read.csv("C:/Users/Ali/Documents/MNLG/Base_Master.csv", header = TRUE) 

# Verificando si la variable dependente esta como fator
class(DB$naturaleza) # Ok!
table(DB$naturaleza, useNA="always") #La ditribución entre los grupos es homogenea##


DB <- DB %>% mutate(
Escolaridad_pa=factor(Escolaridad_pa, levels = c("1", "2", "3", "4", "5"), labels = c("Ninguno", "Primaria", "Secundaria", "Preparatoria", "Lic-maestr-doc")),
Escolaridad_ma=factor(Escolaridad_ma, levels = c("1", "2", "3", "4", "5"), labels = c("Ninguno", "Primaria", "Secundaria", "Preparatoria", "Lic-maestr-doc")),
Ocupacion_pa=factor(Ocupacion_pa, levels = c("1", "2", "3", "4", "5"), labels = c("Profes.", "Comerc.", "Agro.", "Artesal.", "Otros")),
edad_primera= case_when(
  Edad_one_union <=17~1,
  Edad_one_union >=18 &  Edad_one_union <=19 ~2,
  Edad_one_union >=20 &  Edad_one_union <=24 ~3,
  Edad_one_union >=25~4),
edad_primera=factor(edad_primera, levels = c("1", "2", "3", "4"), labels = c("<=17anos", "18-19anos", "20-24anos", ">=25anos") ))

table(DB$Escolaridad_ma, useNA = "always")
table(DB$Ocupacion_pa, useNA = "always")
table(DB$Cohorte_nto, useNA = "always")

table(DB$APNC_cat, useNA="always") #La ditribución de la variable independiente##

#cambio de la categoria de referencia#

DB <- DB %>% mutate(APNC_cate=case_when(
                    APNC_cat=="Correside"~1,
                    APNC_cat=="1-4APNC"~2,
                    APNC_cat=="4-8APNC"~3),
                    APNC_cate=factor(APNC_cate, levels = c("1", "2", "3"), labels = c("Correside", "1-4APNC", "4-8APNC")))

table(DB$APNC_cate, useNA="always") #La ditribución de la variable independiente##


##UNIVERSO DE ESTUDIO##

DB <- DB %>% filter(Cohorte_nto %in% c("Coh.1962-69", "Coh.1970-79", "Coh.1980-89"))
DB <- select(DB, naturaleza, Cohorte_nto, APNC_cat, APNC_cate, edad_primera, Cuartiles_CP1, Escolaridad_ma, Ocupacion_pa)

#Variables independientes#
# APNC_cat <<<<- Anos persona no corresidos con los padres
#Cohorte_nto  <<<- cohorte de nacimiento
#edad_primera <<<- edad de la primera union
#Cuartiles_CP1 <<<- cuartiles de Estrato economico retrospectivo
#Escolaridad_pa <<<- escolaridad del padre
#Escolaridad_ma <<<- escolaridad de la madre
#Ocupacion_pa <<<- sector de ocupacion del padre
#tam_loc <<<- tamano de localidad
DB <- na.omit(DB)



#Prueba de independencia de las categorias
# Estimando los contrastes binarios
#Dejando como categoria de referencia UL: union libre
#install.packages("mlogit")
library(mlogit)

mm0 <- mlogit(naturaleza ~1| Cohorte_nto + APNC_cate + edad_primera + Cuartiles_CP1 + Escolaridad_ma + Ocupacion_pa, 
                              data=DB, shape="wide", reflevel = "UL")
summary(mm0)


mm1 <- mlogit(naturaleza ~1| Cohorte_nto +  APNC_cate + edad_primera + Cuartiles_CP1 + Escolaridad_ma + Ocupacion_pa, 
              data=DB, shape="wide", reflevel = "UL",
              alt.subset = c("UL", "MC")) #comparando UL vs MC#
summary(mm1)


mm2 <- mlogit(naturaleza ~1| Cohorte_nto + APNC_cate + edad_primera + Cuartiles_CP1 + Escolaridad_ma + Ocupacion_pa, 
              data=DB, shape="wide", reflevel = "UL",
              alt.subset = c("UL", "MCR")) #comparando UL vs MCR#
summary(mm2)

# Comparando  el primer contraste con los dos ultimos contrastes#
#Hipotesis nula: hay independencia de alternativas irrelevantes
#Hipotesis alternativa: NO hay independencia de alternativas irrelevantes

mlogit::hmftest(mm0, mm1)
mlogit::hmftest(mm0, mm2)

#Criterio: dado que p-valor en ambos casos es mayor a 0.05, significa que no hay independencia de alternativas




#Indentificar problemas de multicolinealidad#
##Test de VIF
##se crea una variable cuantitativa como dependiente

DB <- DB %>% mutate(Tipunion=case_when(
             naturaleza=="UL"~1,
             naturaleza=="MC"~2,
             naturaleza=="MCR"~3))

mrl <- lm(Tipunion ~ Cohorte_nto + edad_primera + Cuartiles_CP1 + Escolaridad_ma + Ocupacion_pa, data=DB)
car::vif(mrl) #se evidencia que el vif es considerablemente bajo en todas las varibales independientes, por tanto, se asume que no hay problemas graves de multicolinealdidad#



# Estimando el modelo multinomial considerando las 3 categorias simultaneamente
modmmial1 <- multinom(naturaleza ~ Cohorte_nto + APNC_cate + edad_primera + Cuartiles_CP1 + Escolaridad_ma + Ocupacion_pa, data=DB)
summary(modmmial1) # interpretacion en logito
round(exp(coef(modmmial1)), digits=3) # interpretacao en odds-ratio

z <- summary(modmmial1)$coefficients/summary(modmmial1)$standard.errors 
pvalues <- (1-pnorm(abs(z),0,1))*2


#Cambiando la categoria de referencia#

DB <- DB %>% mutate(Tipunion=factor(Tipunion, levels=c("1","2","3"), 
                                              labels=c("UL","MC","MCR")))
levels(DB$Tipunion)

modmmial2 <- multinom(Tipunion ~ Cohorte_nto + APNC_cate + edad_primera + Cuartiles_CP1 + Escolaridad_ma + Ocupacion_pa, data=DB)
summary(modmmial2) # interpretacion en logito
coef <- data.frame(summary(modmmial2)$coefficients) 
odds_ratios <- data.frame(round(exp(coef(modmmial2)), digits=3)) # interpretacao en odds-ratio
sterror <- data.frame(summary(modmmial2)$standard.errors) 
IC95 <- data.frame(exp(confint(modmmial2)))



##AJUSTES DEL MODELO##

modmmial0 <- multinom(Tipunion ~1, data =na.omit(DB[ , all.vars(formula(modmmial2))]))
anova(modmmial0, modmmial2)
#criterio: el modelo con las variables dependientes, es diferente al modelo nulo.


##Efectos generales
car::Anova(modmmial2,type="II",test="Wald")
#se puede observar que la mayoria de las variables tienen un efecto significativo, solo la escolaridad de la madre no lo es.


# Teste de Razao de Verosimilianza para variables explicativas
z <- summary(modmmial2)$coefficients/summary(modmmial2)$standard.errors 
pvalues <- round((1-pnorm(abs(z),0,1))*2,5)
pvalues <- data.frame(pvalues)


# Teste de wald para la variable corresidencia#
linearHypothesis(modmmial2,c("MC:APNC_cat4-8APNC","MCR:APNC_cat4-8APNC")) # Teste de wald
linearHypothesis(modmmial2,c("MC:APNC_catCorreside","MCR:APNC_catCorreside"))
#La ultima hipotesis establece que APNC_catCorreside es no significativa entre MC y MCR, es decir no tiene un efecto diferenciador entre estas dos categorias de primera union


mod.f <- multinom(Tipunion ~ Cohorte_nto + edad_primera + Cuartiles_CP1 + Escolaridad_ma + Ocupacion_pa, data=DB)
mod.r <- multinom(Tipunion ~ Cohorte_nto + APNC_cate + edad_primera + Cuartiles_CP1 + Escolaridad_ma + Ocupacion_pa, data=DB)
if(!require(lmtest)){install.packages("lmtest"); require(lmtest)}
lrtest(mod.f,mod.r) # LR -> rejeicao de que la corresidencia nao tem efeito conjunto
linearHypothesis(modmmial2,c("MC:APNC_cat4-8APNC"="MCR:APNC_cat4-8APNC")) 
linearHypothesis(modmmial2,c("MC:APNC_catCorreside"="MCR:APNC_catCorreside")) 
#Ambas hipotesis establecen que APNC_cat4-8APN y APNC_catCorreside tienen un efecto diferente de cero.



##Hipotesis principal: la no corresidencia tiene un efecto diferente en el tipo de primera union
# Teste a hipotese de que esse efeito e significativo

b1 <- coef(modmmial2)[2,4]-coef(modmmial2)[1,4]
b2 <- coef(modmmial2)[2,5]-coef(modmmial2)[1,5]

covmod1 <- data.frame(round(vcov(modmmial2),3)) #Matrix de covarianzas#

ep1 <- vcov(modmmial2)[23,23] + vcov(modmmial2)[4,4]-2*vcov(modmmial2)[23,4]
ep2 <- vcov(modmmial2)[24,24] + vcov(modmmial2)[5,5]-2*vcov(modmmial2)[24,5]

pvalor1 <- (1-pnorm(b1/ep1))*2 # pvalue (two-tailed test)
pvalor2 <- (1-pnorm(b2/ep2))*2

#conclusion: #


#Tabla con los resultados finales#
#install.packages("gtsummary")
library(gtsummary)
gtsummary::tbl_regression(modmmial2, exponentiate=FALSE)
ggsave( filename  = 'C:/Users/Ali/Documents/MNLG/Tabla_modelos_logito.jpg', width = 6.9, height = 4.72, dpi = 600)

gtsummary::tbl_regression(modmmial2, exponentiate=TRUE)
ggsave( filename  = 'C:/Users/Ali/Documents/MNLG/Tabla_modelos_Odds.jpg', width = 6.9, height = 4.72, dpi = 600)


#Tabla de clasificacion del modelo

Tabc <- table(Observado=DB$Tipunion, Predict=predict(modmmial2))
propTabc <- round(prop.table(Tabc)*100,2)
clasificacion <-round(sum(diag(Tabc))/sum(Tabc), 3)*100

#install.packages("caret")
library(caret)
caret::confusionMatrix(predict(modmmial2),DB$Tipunion )

# Plotting de los margins#

predict <- cbind(DB[2:7], predict(modmmial2, type="probs", se=TRUE))

predict <- reshape2::melt(predict,
                          id.vars=c("Cohorte_nto", "APNC_cate", "edad_primera", "Cuartiles_CP1", "Escolaridad_ma", "Ocupacion_pa"),
                          value.name = "Probabilidad",
                          variable.name= "TipoUnion")



grap6 <- ggplot(predict) + 
         geom_bar(aes(x=APNC_cat, y = Probabilidad, fill = ( TipoUnion)),
           position=position_dodge(), stat="identity", width = 0.55) +
           scale_y_continuous(limits = c(0,0.8), breaks = seq(0,0.8,.2)) +
            theme_minimal() +
             facet_grid(Cohorte_nto  ~.)

grap6 + labs(title="", x="Años-persona de NO corresidencia con los padres", y = " Probabilidad", 
            caption = "Fuente: elaboración propia con base en EDER-2017 
                               https://www.inegi.org.mx/programas/eder/2017/") + theme_minimal() + 
scale_fill_manual(values=c("#003366",'#8c8c8c', "#8db600"))
ggsave( filename  =  'C:/Users/Ali/Documents/MNLG/Grafica_3.jpg', width = 6.9, height = 4.72, dpi = 600)


  
  















