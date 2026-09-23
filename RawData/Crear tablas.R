#crear tablas para apendice


#1) Tabla de especies; estatus; categoria pol; grupo taxonomico; pajaros.

dat <- read.csv("RawData/plantasxparque_completo.csv")
head(dat)
colnames(dat)[1] <- "id"
colnames(dat)[11] <- "Origen"
colnames(dat)[12] <- "Importancia_polinizadores"
colnames(dat)[13] <- "Principal_grupo"
colnames(dat)[14] <- "Importancia_frugivoros"

#na's
summary(dat)
unique(dat$Importancia_polinizadores)
unique(dat$Origen)
unique(dat$Porte)
unique(dat$Principal_grupo)
dat$Principal_grupo <- as.factor(dat$Principal_grupo)
levels(dat$Principal_grupo)[1] <- c("varios")
levels(dat$Principal_grupo)[2] <- c("abeja_melifera")
unique(dat$Importancia_frugivoros)
dat$Importancia_frugivoros <- as.factor(dat$Importancia_frugivoros)
levels(dat$Importancia_frugivoros)[1] <- c("nula")
levels(dat$Importancia_frugivoros)[2] <- c("alta")

#Resumen estatus; categoria pol; grupo taxonomico; pajaros.
library(reshape2)

table1 <- dcast(dat, Especie + Porte + Origen + 
                  Importancia_polinizadores + Principal_grupo + 
                  Importancia_frugivoros ~ . , fun.aggregate = length)
head(table1)
colnames(table1)[8] <- "abundancia"
write.csv(table1, "data/Especies.csv")

#2) Tabla de parques; numero especies nativas; exoticas; Especies buenas para pol; y pajaros.

head(dat)
tabla2 <- dcast(dat, Parque + Tipologia + 
                  Barrio + Distrito ~ Origen, fun.aggregate = length)
temp2 <- dcast(dat, Parque + Tipologia + 
                 Barrio + Distrito ~ Importancia_polinizadores, fun.aggregate = length)
temp3 <- dcast(dat, Parque + Tipologia + 
                 Barrio + Distrito ~ Importancia_frugivoros, fun.aggregate = length)

tabla2$pol_alta <- temp2$alta
tabla2$pol_media <- temp2$media
tabla2$pol_baja <- temp2$baja
tabla2$pol_nula <- temp2$nula
tabla2$paj_alta <- temp3$alta
tabla2$paj_nula <- temp3$nula

head(tabla2)
write.csv(tabla2, "data/Parques.csv")


#create metadata.
library(dataspice)
#create basic .csv metadata files and folders.
create_spice()

#Add creators
edit_creators()

#Add how to access the data
prep_access()
edit_access()

#Add metadata
edit_biblio()

#Describe variables
prep_attributes()
edit_attributes()

#create a json file
write_spice()

#look at the json:
jsonlite::read_json(here::here("data", "metadata", "dataspice.json")) %>% listviewer::jsonedit()

#build a webpage!
build_site()




