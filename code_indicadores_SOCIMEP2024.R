# Analisis indicadores de SOCIMEP
# Fecha: 30/10/2023
# Elaborado por: Frank Zela-Coila

# Paquetes
library(tidyverse)
library(openxlsx)
library(compareGroups)
library(Hmisc)
library(ggcorrplot)
library("ggpubr")


df_indicadores <- read.xlsx("Indicadores_SOCIMEP_journal/BD_indicadores_autores.xlsx",
                       sheet = "BD_analisis")

df_indicadores

# Sondeo
hist(df_indicadores$sigaps)
hist(df_indicadores$socimep)
hist(df_indicadores$sumatoria)
hist(df_indicadores$cantidad)


# Solo A * B
mod1 <- ggplot(df_indicadores,
               aes(x = sigaps,
                   y = cantidad))+
  geom_point()+
  geom_smooth(method = "lm")

mod1

res1 <- cor.test(df_indicadores$sigaps, 
                 df_indicadores$cantidad, 
                 method = "spearman")
res1

# socimepcion
mod2 <- ggplot(df_indicadores,
               aes(x = socimep,
                   y = cantidad))+
  geom_point()+
  geom_smooth(method = "lm")

mod2

res2 <- cor.test(df_indicadores$socimep, 
                 df_indicadores$cantidad, 
                 method = "spearman")
res2

# sumatoria
mod3 <- ggplot(df_indicadores,
               aes(x = sumatoria,
                   y = cantidad))+
  geom_point()+
  geom_smooth(method = "lm")

mod3

res3 <- cor.test(df_indicadores$sumatoria, 
                 df_indicadores$cantidad, 
                 method = "spearman")
res3

df_modelos1 <- df_indicadores %>% 
  select(sigaps, socimep, sumatoria, cantidad)

######################################################
## GRAFICO FINAL FINALISIMO ##########################
######################################################
summary(df_modelos1$sigaps)
summary(df_modelos1$socimep)
summary(df_modelos1$sumatoria)
summary(df_modelos1$cantidad)


ggpairs(df_modelos1, 
        columnLabels = nuevos_titulos,
        diag = list(continuous = "barDiag"), 
        upper = list(continuous = wrap("cor", method = "spearman")),
        lower = list(continuous = wrap("smooth", method = "lm", 
                                       se = FALSE, color = "#616a6b"))
) +
  labs(caption = "Método: Spearman") + 
  theme_bw()

