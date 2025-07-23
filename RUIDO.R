#carpeta de trabajo
#setwd("C:/PERSONAL_MAPU/UNIVERSIDADES/NACIONAL/DISEÑO_DE_EXPERIMENTOS/PROYECTO")
rm(list = ls())
############################### tamaño estimado ################################
#### power anova test ####
library(pwr)      # pwr.anova.test
# k: n??mero de tratamientos, n: n??mero de replicas, f=phi/sqrt(n)
# con esta funci??n se averigua el n para una potencia dada 
pot <- pwr.anova.test(k = 6, f = 0.25, sig.level = 0.05, power = 0.8)

#### Simulando datos ####
library(lme4)
library(simr)

# Diseño: 6 tratamientos, 3 bloques, 2 niveles de experiencia
ruido <- c("sin_musica", "videojuegos", "clasica")
dificultad <- c("facil", "medio")
facultad <- c("ciencias", "ingenieria", "humanas")
experiencia <- c("si", "no")

# Número inicial de réplicas (por celda tratamiento x bloque x experiencia)
n_rep <- 5  # puedes empezar con 5 y ajustar

# Crear diseño completo
d <- expand.grid(ruido=ruido,
                 dificultad=dificultad,
                 facultad=facultad,
                 experiencia=experiencia,
                 rep=1:n_rep)

# Supuestos del modelo
intercepto <- 10
efecto_ruido <- c(sin_musica=4, videojuegos=-8, clasica=-10)
efecto_dificultad <- c(facil=-3, medio=3)
efecto_experiencia <- c(si=-8, no=4)
desv_bloque <- 5
desv_residual <- 7

# Simular datos
set.seed(123)
d$tiempo <- intercepto +
  efecto_ruido[d$ruido] +
  efecto_dificultad[d$dificultad] +
  efecto_experiencia[d$experiencia] +
  rnorm(length(d$facultad), 0, desv_bloque)[as.numeric(d$facultad)] +
  rnorm(nrow(d), 0, desv_residual)

# Convertir a factores
d$facultad <- factor(d$facultad)
d$ruido <- factor(d$ruido)
d$dificultad <- factor(d$dificultad)
d$experiencia <- factor(d$experiencia)

# Modelo mixto
modelo <- lm(tiempo ~ ruido*dificultad + experiencia + facultad, data=d)

# Convertir a objeto simr
modelo_sim <- extend(modelo, along="rep", n=n_rep)

# Potencia para la interacción ruido*dificultad
powerSim(modelo_sim, fixed("ruido:dificultad", "anova"), nsim=100)

################################ estudio piloto ################################ 
library(readxl)
piloto <- read_excel("RUIDO_(respuestas piloto).xlsx")
View(piloto)
piloto <- piloto[,-2]
colnames(piloto) <- c("Fecha", "Correo", "Edad","Sexo","Conocimiento","veces",
                     "Facultad","Ruido","Dificultad","Tiempo")
library(dplyr)
library(tidyr)
piloto <- piloto %>%
  separate(Tiempo, into = c("min", "seg", "centesimas"), sep = "\\.", 
           remove = FALSE) %>%
  mutate(across(c(min, seg, centesimas), as.numeric),
         tiempo_minutos = min + seg/60 + centesimas/6000)
modelo.piloto <- lm(tiempo_minutos~Facultad+Ruido+Dificultad+Ruido*Dificultad,
                    data=piloto)
library(car)
Anova(modelo.piloto,type=3)
f.piloto <- sqrt(((15.332+5.052+41.905)/5)/(139.499/5))
library(pwr)
pwr.anova.test(k = 6, 
               f = f.piloto, 
               sig.level = 0.05, 
               power = 0.8)



################################### estudio ####################################
#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#
### librerías ###
library(readxl)
library(dplyr)
library(car)
library(ggplot2)
library(pwr)
#------------------------------------------------------------------------------#
### Datos ###
RUIDO<- read_excel("RUIDO_PARA_LA_CONCENTRACIÓN_(respuestas).xlsx")
#View(RUIDO)
ruido <- RUIDO[,-c(1,2)]
colnames(ruido) <- c("Edad","Sexo","Conocimiento","veces",
                      "Facultad","Ruido","Dificultad","min","seg",
                     "centesimas")
ruido <- ruido %>%
  mutate(Tiempo = min + seg/60 + centesimas/6000)
#------------------------------------------------------------------------------#
### Descriptiva
## Edad-Sexo-Facultad
ggplot(data = ruido,aes(x = Sexo,y = Edad,colour = Sexo))+
  geom_boxplot()+
  facet_wrap("Facultad")+
  theme_minimal()
## Edad-Facultad
ggplot(data = ruido,aes(y = Edad,colour = Facultad))+
  geom_boxplot()+
  facet_wrap("Facultad")+
  theme_minimal()
## Edad-sexo
ggplot(data = ruido,aes(y = Edad,colour = Sexo))+
  geom_boxplot()+
  facet_wrap("Sexo")+
  theme_minimal()
## Tiempo-Sexo-Facultad
ggplot(data = ruido,aes(x = Sexo,y = Tiempo,colour = Sexo))+
  geom_boxplot()+
  facet_wrap("Facultad")+
  theme_minimal()
## Tiempo-Facultad
ggplot(data = ruido,aes(y = Tiempo,colour = Facultad))+
  geom_boxplot()+
  facet_wrap("Facultad")+
  theme_minimal()
## Tiempo-sexo
ggplot(data = ruido,aes(y = Tiempo,colour = Sexo))+
  geom_boxplot()+
  facet_wrap("Sexo")+
  theme_minimal()
ruido$tratamiento <- interaction(ruido$Ruido,ruido$Dificultad)
## Tiempo-tratamiento
ggplot(data = ruido,aes(y = Tiempo,colour = tratamiento))+
  geom_boxplot()+
  facet_wrap("tratamiento")+
  theme_minimal()
#------------------------------------------------------------------------------#
### Modelo ###

lm.ruido <- lm(Tiempo~Ruido*Dificultad+Facultad+Edad+Sexo+Conocimiento+veces,
               data = ruido,contrasts = list(Ruido="contr.sum",
                                             Dificultad="contr.sum",
                                             Facultad="contr.sum"))
summary(lm.ruido)
Anova(lm.ruido)
#------------------------------------------------------------------------------#
### Potencia ###
f.proyecto <- sqrt(((2.659+206.122+11.431)/5)/(310.950/24))
pwr.anova.test(k = 6, 
               f = f.proyecto, 
               sig.level = 0.05,
               n = 6)
