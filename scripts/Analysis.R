#script to determine plant species good for pollinators and where they occur.
#We need to 
#1) Get native/exotic status.
#2) Get pollinator attractiveness (qualitative or quantitative)
#3) Get % of pollinators covered per park or any measure easy to transmit
#4) Give recomendations (per park)

#Notes:
#Bees prefer natives (reference to Moron-Lopez TFM)
#Complementary highly attractive plants for different pollinator groups should be planted 

  
#read data:

arboles <- read.csv(file = "RawData/arboles.csv")
head(arboles)
arboles <- arboles[,c("ESPECIE.ARBOLES.ZONAS.VERDES", "UG", 
                      "TIPOLOGÍA", "BARRIO", "DISTRITO", "ELEMENTO", "X", "Y")]
arboles$porte <- "arbol"
colnames(arboles) <- c("Especie", "Parque",                          
                       "Tipologia", "Barrio",                      
                       "Distrito", "Elemento",                    
                       "X", "Y",                           
                       "Porte")
arbustos <- read.csv(file = "RawData/arbustos.csv")
head(arbustos)
arbustos <- arbustos[,c("ESPECIE.ARBUSTO", "UG", 
                      "TIPOLOGÍA", "BARRIO", "DISTRITO", "ELEMENTO", "X", "Y")]
arbustos$porte <- "arbusto"
colnames(arbustos) <- c("Especie", "Parque",                          
                       "Tipologia", "Barrio",                      
                       "Distrito", "Elemento",                    
                       "X", "Y",                           
                       "Porte")
dat <- rbind(arboles, arbustos)
head(dat)
sort(unique(dat$Especie))
dat <- subset(dat, !Especie %in% c("ESPECIE ARBOLES ZONAS VERDES", "", "Tocón Tocón",
                                  "Marra o Alcorque vacío", "Marra o Alcorque\nvacío", 
                                  "ESPECIE ARBUSTO", "Arbustos varios"))
dat$Especie <- gsub(pattern = "\n", replacement = " ", x = dat$Especie, fixed = TRUE)
unique(dat$Parque)
dat <- subset(dat, !Parque %in% c("UG", ""))
dat$Parque <- gsub(pattern = "\n", replacement = " ", x = dat$Parque, fixed = TRUE)
unique(dat$Barrio)
dat$Barrio <- gsub(pattern = "\n", replacement = " ", x = dat$Barrio, fixed = TRUE)
dim(dat) #25000 entradas!
head(dat)
#fix  ARCO IRIS, JARD═N PLAZA "═" -> "Í"  
dat$Parque <- gsub(x = dat$Parque, pattern = "=", replacement = "Í", fixed = TRUE)
dat$Parque <- gsub(x = dat$Parque, pattern = "Ú", replacement = "e", fixed = TRUE)
dat$Parque <- gsub(x = dat$Parque, pattern = "±", replacement = "ñ", fixed = TRUE)
dat$Parque <- gsub(x = dat$Parque, pattern = "╔", replacement = "É", fixed = TRUE)
dat$Parque <- gsub(x = dat$Parque, pattern = "¬", replacement = "a", fixed = TRUE)
dat$Parque <- gsub(x = dat$Parque, pattern = "┴", replacement = "Á", fixed = TRUE)
sort(unique(dat$Parque))

#Cuales son nativas?
source(file = "Originr_patch.R")
#obj <- flora_europaea(sp = "Quercus ilex")
sp <- unique(dat$Especie)
length(sp)
native <- c()
for(i in sp){
  obj <- flora_europaea(sp = i)
  if(is.null(obj)){
    native[i] <- "not found"}
  else{
  native[i] <- any(obj$native %in% "Spain")
    }
  }
natives <- data.frame(sp, native) 
head(natives, 20)
length(which(natives == "not found")) #262
#needs a quick manual check for sp's, ...
#write.csv(natives, file = "plantas_sevilla_.csv") DO NOT RUN (_ added to prevent overwriting)

#I manually added origin and importance for pollinators.
plants <- read.csv(file = "plantas_sevilla.csv", row.names = 1)
head(plants)
plants$native <- ifelse(plants$native == TRUE, "nativa", "exotica") 
plants$polinizadores <- as.factor(plants$polinizadores)
levels(plants$polinizadores) <- c("alta", "baja", "media", "nula")
colnames(plants) <- c("sp", "origen", "importancia_polinizadores", "notas")
which(rownames(plants) != plants$sp)
plants[175,] <- c("Gaura Lindheimeri", "exotica", "baja", "") #to fix.a correction in excel

which(rownames(plants) == "Cistus crispus")

table(plants$origen)
table(plants$origen, plants$importancia_polinizadores)

top <- plants$sp[which(plants$origen == "nativa" & plants$importancia_polinizadores == "alta")]

#unir
dats <- merge(dat, plants, by.x = "Especie", by.y = "sp")
dim(dat); dim(dats)
sort(unique(dats$Especie))
sort(table(dats$Especie))
head(dats)
sort(unique(dats$Barrio))
#fix stuff
dats <- dats[which(!dats$Especie == "Especie sin determinar"),] 
dats$Especie <- ifelse(dats$Especie == "Gaura Lindheimeri", "Gaura lindheimeri", dats$Especie)

write.csv(dats, file = "plantasxparque.csv")

#summaries
library(reshape2)
data <- dcast(dats, 
              Especie + Parque + Tipologia + Barrio + Distrito +
                Elemento + Porte + origen + importancia_polinizadores ~ "Numero", fun.aggregate = function(x){length(x)}, 
              value.var = "Especie")
dim(data) #2600 combinaciones de especie-parque
head(data)

#some stats per park
#species per park and type
dcast(data, 
      Distrito #Parque + Tipologia + Barrio + Distrito + Elemento
      ~ Porte, fun.aggregate = function(x){length(x)}, 
      value.var = "Especie")
#park and origin
#data[which(is.na(data$origen)),]
temp <- dcast(data, 
      Parque #Parque + Tipologia + Barrio + Distrito + Elemento
      ~ origen, fun.aggregate = function(x){length(x)}, 
      value.var = "Especie")
length(which(temp$exotica > 0))
length(which(temp$nativa > 0))

#park and pollinator importance
dcast(data, 
      Distrito #Parque + Tipologia + Barrio + Distrito + Elemento
      ~ importancia_polinizadores, fun.aggregate = function(x){length(x)}, 
      value.var = "Especie")
temp <- dcast(data, 
      Parque #Parque + Tipologia + Barrio + Distrito + Elemento
      ~ importancia_polinizadores, fun.aggregate = function(x){length(x)}, 
      value.var = "Especie")
length(which(temp$alta == 0))
length(which(temp$alta > -1))

#abundance per park and type
dcast(data, 
      Parque #+ Tipologia + Barrio + Distrito + Elemento
      ~ Porte, fun.aggregate = sum, 
      value.var = "Numero")



#Data from TRILLO
#read pollinator data
pol <- read.csv(file = "RawData/polinizadores.csv")
head(pol)
rownames(pol) <- pol$pollinator_species
pol[order(pol$n_parks, decreasing = TRUE),] #has to be abundance...
nrow(pol)
#do rarefaction?
library(vegan)
estimateR(pol$n_parks)
table(pol$order)

plant <- read.csv(file = "RawData/plantas_visitadas.csv")
head(plant)
plant[order(plant$Censuses, decreasing = TRUE),] 
table(plant$native_nonnative)

pp <- read.csv(file = "RawData/Plant_pollinators.csv")
head(pp)
pp[order(pp$total_visits, decreasing = TRUE),c(1,4,5,6)] 
table(pp$bearing)
table(pp$native_range.N.O.)
