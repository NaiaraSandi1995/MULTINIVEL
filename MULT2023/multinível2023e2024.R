#########LAPOP2023

library(readstata13)
library(tidyverse)
library(survey)
library(Rmisc)
library(broom)
library(foreign)

#Observação e organização das bases de dados

BRA_2023_LAPOP_AmericasBarometer_v1_0_w
str(BRA_2023_LAPOP_AmericasBarometer_v1_0_w)
colnames(BRA_2023_LAPOP_AmericasBarometer_v1_0_w)

Lapop <- BRA_2023_LAPOP_AmericasBarometer_v1_0_w

table(Lapop$m1)
# 1   2   3   4   5 
# 165 506 523 107 213 

#Labels:
#   value                      label
# 1                  Muito bom
# 2                        Bom
# 3 Nem bom, nem mau (regular)
# 4                        Mau
# 5        Muito mau (péssimo)
# NA(a)                   Não sabe
# NA(b)               Não responde

# br21 <- read.dta13("BRA_2021_LAPOP_AmericasBarometer_v1.2_w.dta", #readstata13
#                    convert.factors = FALSE)
# BRA_2023_LAPOP_AmericasBarometer_v1_0_w

lapop2 <- read.dta13("BRA_2023_LAPOP_AmericasBarometer_v1.0_w.dta",
                     convert.factors = TRUE)

BASECJLAPOP2023 <- bind_cols(Lapop, lapop2)

save(BASECJLAPOP2023, file = "BASECJLAPOP2023.RAMERLATINA")

table(BASECJLAPOP2023$m1...256) #Avaliação Lula 2023
# 
# Muito bom                        Bom Nem bom, nem mau (regular) 
# 165                        506                        523 
# Mau        Muito mau (péssimo)                   Não sabe 
# 107                        213                          0 
# Não responde 
# 0 



#salvando
library(writexl)

writexl::write_xlsx(BASECJLAPOP2023, path = "BASECJLAPOP2023.xlsx")

#Variáveis que precisamos
# c("pais", "TolerHomo", 
#   "Denom", "AtRelig",
#   "IntRelig", "Dem",
#   "ConfInt", "Ed_sup",
#   "Idade", "Sexo",
#   "Países", "TNaH",
#   "TolerHomoP" ,
#   "Denom_Desc", "Desemprego",
#   "Dir_minorias","PaisD",
#   "Denom1",
#   "Liberdade",
#   "Desp_Ed",
#   "IDE" ,
#   "idnum")]


#2023####
#1ARGENTINA####

#País
ARG$pais
# Labels:
#   value     label
# 17 Argentina

#Tolerância
ARG$d5
# Labels:
#   value                 label
# 1 Desaprueba firmemente
# 10    Aprueba firmemente
# NA(a)               No sabe
# NA(b)           No responde

table (ARG$d5)
#   1   2   3   4   5   6   7   8   9  10 
# 164  51  51  50 119 104 122 125 110 612 

#Denominação religiosa 
ARG$q3cn
# 
# Labels:
#   value                                                                  label
# 1                                                               Católico
# 2       Protestante, Protestante Tradicional o Protestante no Evangélico
# 3                                    Religiones Orientales no Cristianas
# 4 Ninguna (Cree en un Ser Superior pero no pertenece a ninguna religión)
# 5                                               Evangélica y Pentecostal
# 7                                               Religiones Tradicionales
# 11                                     Agnóstico o ateo (no cree en Dios)
# 77                                                                   Otro
# NA(a)                                                                No sabe
# NA(b)                                                            No responde

#Ativismo religioso
ARG$q5a #Não tem


#Intensidade Religiosa 
ARG$q5b
# Labels:
#   value           label
# 1  Muy importante
# 2 Algo importante
# 3 Poco importante
# 4 Nada importante
# NA(a)         No sabe
# NA(b)     No responde


#Condicionantes#
#Adesão a democracia (churCOSTARICAliana)
ARG$ing4
# 
# Labels:
#   value             label
# 1 Muy en desacuerdo
# 7    Muy de acuerdo
# NA(a)           No sabe
# NA(b)       No responde

#Confiança
ARG$it1
# Labels:
#   value          label
# 1  Muy confiable
# 2 Algo confiable
# 3 Poco confiable
# 4 Nada confiable
# NA(a)        No sabe
# NA(b)    No responde

#Escolaridade
ARG$edre
# Labels:
#   value                                label
# 0                              Ninguna
# 1                  Primaria incompleta
# 2                    Primaria completa
# 3                Secundaria incompleta
# 4                  Secundaria completa
# 5 Terciaria o universitaria incompleta
# 6   Terciaria o universitaria completa
# NA(a)                              No sabe
# NA(b)                          No responde

#Idade
ARG$q2
#[1] "Edad"

#Sexo
ARG$q1tc_r
# Labels:
#   value                                      label
# 1                           Hombre/masculino
# 2                             Mujer/femenino
# 3 No se identifica como hombre ni como mujer
# NA(a)                                    No sabe
# NA(b)                                No responde

#
ARG2023N <- ARG[ ,c("pais", "d5", "q3cn", "q5b",
                     "ing4", "it1", "edre","q2", "q1tc_r")]

#____________
###Abrindo  a base de dados em dta e convertendo em 
#labels para poder juntar sem precisar ficar recategorizando
#no final
ARG2023 <- read.dta13("ARG.dta",
                     convert.factors = TRUE)

ARG2023C <- ARG2023[ ,c("pais", "d5", "q3cn", "q5b",
                    "ing4", "it1", "edre","q2", "q1tc_r")]

ARG2023C$Países <- ARG2023C$pais
ARG2023C$TolerHomo <- ARG2023C$d5
ARG2023C$Denom_Desc <- ARG2023C$q3cn
ARG2023C$IntReligC <- ARG2023C$q5b
ARG2023C$ConfIntC <- ARG2023C$it1
ARG2023C$EscolC <- ARG2023C$edre
ARG2023C$Idade <- ARG2023C$q2
ARG2023C$Dem <- ARG2023C$ing4


table(ARG2023C$q1tc_r)

ARG2023C <- ARG2023C %>% 
  mutate(Sexo = case_when(
    q1tc_r == "Hombre/masculino" ~ "Homem",
    q1tc_r == "Mujer/femenino" ~ "Mulher",
    q1tc_r == "No se identifica como hombre ni como mujer"
    ~ "Não se identifica"
  ))
table(ARG2023C$Sexo)

ARG2023F <- ARG2023C[ ,c("Países", "TolerHomo", "Denom_Desc",
                        "IntReligC", "Dem", "ConfIntC",
                        "EscolC", "Idade",
                        "Sexo")]


ARG2023F <- bind_cols(ARG2023N, ARG2023F)

#2BOLÍVIA####

#País
BOL$pais
# Labels:
#   value     label
# 17 BOLentina

#Tolerância
BOL$d5
# Labels:
#   value                 label
# 1 Desaprueba firmemente
# 10    Aprueba firmemente
# NA(a)               No sabe
# NA(b)           No responde

table (BOL$d5)
#   1   2   3   4   5   6   7   8   9  10 
# 164  51  51  50 119 104 122 125 110 612 

#Denominação religiosa 
BOL$q3cn
# 
# Labels:
#   value                                                                  label
# 1                                                               Católico
# 2       Protestante, Protestante Tradicional o Protestante no Evangélico
# 3                                    Religiones Orientales no Cristianas
# 4 Ninguna (Cree en un Ser Superior pero no pertenece a ninguna religión)
# 5                                               Evangélica y Pentecostal
# 7                                               Religiones Tradicionales
# 11                                     Agnóstico o ateo (no cree en Dios)
# 77                                                                   Otro
# NA(a)                                                                No sabe
# NA(b)                                                            No responde

#Ativismo religioso
BOL$q5a
#não tem 

#Intensidade Religiosa 
BOL$q5b
# Labels:
#   value           label
# 1  Muy importante
# 2 Algo importante
# 3 Poco importante
# 4 Nada importante
# NA(a)         No sabe
# NA(b)     No responde


#Condicionantes
#Adesão a democracia (churCOSTARICAliana)
BOL$ing4
# 
# Labels:
#   value             label
# 1 Muy en desacuerdo
# 7    Muy de acuerdo
# NA(a)           No sabe
# NA(b)       No responde

#Confiança
BOL$it1
# Labels:
#   value          label
# 1  Muy confiable
# 2 Algo confiable
# 3 Poco confiable
# 4 Nada confiable
# NA(a)        No sabe
# NA(b)    No responde

#Escolaridade
BOL$edre
# Labels:
#   value                                label
# 0                              Ninguna
# 1                  Primaria incompleta
# 2                    Primaria completa
# 3                Secundaria incompleta
# 4                  Secundaria completa
# 5 Terciaria o universitaria incompleta
# 6   Terciaria o universitaria completa
# NA(a)                              No sabe
# NA(b)                          No responde

#Idade
BOL$q2
#[1] "Edad"

#Sexo
BOL$q1tc_r
# Labels:
#   value                                      label
# 1                           Hombre/masculino
# 2                             Mujer/femenino
# 3 No se identifica como hombre ni como mujer
# NA(a)                                    No sabe
# NA(b)                                No responde

BOL2023N <- BOL[ ,c("pais", "d5", "q3cn", "q5b",
                   "ing4", "it1", "edre","q2", "q1tc_r")]


#____________
###Abrindo  a base de dados em dta e convertendo em 
#labels para poder juntar sem precisar ficar recategorizando
#no final
BOL2023 <- read.dta13("BOL.dta",
                      convert.factors = TRUE)

BOL2023C <- BOL2023[ ,c("pais", "d5", "q3cn", "q5b",
                        "ing4", "it1", "edre","q2", "q1tc_r")]

BOL2023C$Países <- BOL2023C$pais
BOL2023C$TolerHomo <- BOL2023C$d5
BOL2023C$Denom_Desc <- BOL2023C$q3cn
BOL2023C$IntReligC <- BOL2023C$q5b
BOL2023C$ConfIntC <- BOL2023C$it1
BOL2023C$EscolC <- BOL2023C$edre
BOL2023C$Idade <- BOL2023C$q2
BOL2023C$Dem <- BOL2023C$ing4


table(BOL2023C$q1tc_r)

BOL2023C <- BOL2023C %>% 
  mutate(Sexo = case_when(
    q1tc_r == "Hombre/masculino" ~ "Homem",
    q1tc_r == "Mujer/femenino" ~ "Mulher",
    q1tc_r == "No se identifica como hombre ni como mujer"
    ~ "Não se identifica"
  ))
table(BOL2023C$Sexo)

BOL2023F <- BOL2023C[ ,c("Países", "TolerHomo", "Denom_Desc",
                         "IntReligC", "Dem", "ConfIntC",
                         "EscolC", "Idade",
                         "Sexo")]


BOL2023F <- bind_cols(BOL2023N, BOL2023F)

#3BRASIL####

#País
BRA$pais
# Labels:
#   value     label
#  15 Brasil

#Tolerância
BRA$d5
# Labels:
#   value                 label
# 1 Desaprueba firmemente
# 10    Aprueba firmemente
# NA(a)               No sabe
# NA(b)           No responde

table (BRA$d5)
#   1   2   3   4   5   6   7   8   9  10 
# 176  52  49  54 127  94 101 136 116 588

#Denominação religiosa 
BRA$q3cn
# 
# Labels:
#   value                                                                  label
# 1                                                               Católico
# 2       Protestante, Protestante Tradicional o Protestante no Evangélico
# 3                                    Religiones Orientales no Cristianas
# 4 Ninguna (Cree en un Ser Superior pero no pertenece a ninguna religión)
# 5                                               Evangélica y Pentecostal
# 7                                               Religiones Tradicionales
# 11                                     Agnóstico o ateo (no cree en Dios)
# 77                                                                   Otro
# NA(a)                                                                No sabe
# NA(b)                                                            No responde

#Ativismo religioso
BRA$Q5a
#não tem 

#Intensidade Religiosa 
BRA$q5b
# Labels:
#   value           label
# 1  Muy importante
# 2 Algo importante
# 3 Poco importante
# 4 Nada importante
# NA(a)         No sabe
# NA(b)     No responde


#Condicionantes#
#Adesão a democracia (churCOSTARICAliana)
BRA$ing4
# 
# Labels:
#   value             label
# 1 Muy en desacuerdo
# 7    Muy de acuerdo
# NA(a)           No sabe
# NA(b)       No responde

#Confiança
BRA$it1
# Labels:
#   value          label
# 1  Muy confiable
# 2 Algo confiable
# 3 Poco confiable
# 4 Nada confiable
# NA(a)        No sabe
# NA(b)    No responde

#Escolaridade
BRA$edre
# Labels:
#   value                                              label
# 0                                             Nenhum
# 1           Primário/Ensino Fundamental (incompleto)
# 2             Primário/Ensino Fundamental (completo)
# 3      Secundário/Ensino Médio/colegial (incompleto)
# 4        Secundário/Ensino Médio/colegial (completo)
# 5 Ensino Superior/Bacharelado/Faculdade (incompleto)
# 6   Ensino Superior/Bacharelado/Faculdade (completo)
# NA(a)                                           Não sabe
# NA(b)                                       Não responde

#Idade
BRA$q2
#[1] "Idade"

#Sexo
BRA$q1tc_r
# value                                             label
# 1                                   Homem/masculino
# 2                                   Mulher/feminino
# 3 Não se identifica nem como homem, nem como mulher
# NA(a)                                          Não sabe
# NA(b)                                      Não responde

BRA2023N <- BRA[ ,c("pais", "d5", "q3cn", "q5b",
                   "ing4", "it1", "edre","q2", "q1tc_r")]


#____________
###Abrindo  a base de dados em dta e convertendo em 
#labels para poder juntar sem precisar ficar recategorizando
#no final
BRA2023 <- read.dta13("BRA.dta",
                      convert.factors = TRUE)

BRA2023C <- BRA2023[ ,c("pais", "d5", "q3cn", "q5b",
                        "ing4", "it1", "edre","q2", "q1tc_r")]

BRA2023C$Países <- BRA2023C$pais
BRA2023C$TolerHomo <- BRA2023C$d5
BRA2023C$Denom_Desc <- BRA2023C$q3cn
BRA2023C$IntReligC <- BRA2023C$q5b
BRA2023C$ConfIntC <- BRA2023C$it1
BRA2023C$EscolC <- BRA2023C$edre
BRA2023C$Idade <- BRA2023C$q2
BRA2023C$Dem <- BRA2023C$ing4


table(BRA2023C$q1tc_r)

BRA2023C <- BRA2023C %>% 
  mutate(Sexo = case_when(
    q1tc_r == "Homem/masculino" ~ "Homem",
    q1tc_r == "Mulher/feminino" ~ "Mulher",
    q1tc_r == "Não se identifica nem como homem, nem como mulher"
    ~ "Não se identifica"
  ))
table(BRA2023C$Sexo)

BRA2023F <- BRA2023C[ ,c("Países", "TolerHomo", "Denom_Desc",
                         "IntReligC", "Dem", "ConfIntC",
                         "EscolC", "Idade",
                         "Sexo")]


BRA2023F <- bind_cols(BRA2023N, BRA2023F)

#4CHILE####



#País
CHILE$pais
# Labels:
#   value     label
#  13 CHILEle

#Tolerância
CHILE$d5
# Labels:
#   value                 label
# 1 Desaprueba firmemente
# 10    Aprueba firmemente
# NA(a)               No sabe
# NA(b)           No responde

table (CHILE$d5)
#   1   2   3   4   5   6   7   8   9  10 
# 165  39  56  60 181 127 141 105  62 691 

#Denominação religiosa 
CHILE$q3cn
# 
# Labels:
#   value                                                                  label
# value                                                                  label
# 1                                                               Católico
# 2       Protestante, Protestante Tradicional o Protestante no Evangélico
# 3                                    Religiones Orientales no Cristianas
# 4 Ninguna (Cree en un Ser Superior pero no pertenece a ninguna religión)
# 5                                               Evangélica y Pentecostal
# 7                                               Religiones Tradicionales
# 11                                     Agnóstico o ateo (no cree en Dios)
# 77                                                                   Otro
# NA(a)                                                                No sabe
# NA(b)                                                            No responde

#Ativismo religioso
CHILE$Q5a
#não tem 

#Intensidade Religiosa 
CHILE$q5b
# Labels:
#   value           label
# 1  Muy importante
# 2 Algo importante
# 3 Poco importante
# 4 Nada importante
# NA(a)         No sabe
# NA(b)     No responde


#Condicionantes
#Adesão a democracia (churCHILEliana)
CHILE$ing4
# 
# Labels:
#   value             label
# 1 Muy en desacuerdo
# 7    Muy de acuerdo
# NA(a)           No sabe
# NA(b)       No responde

#Confiança
CHILE$it1
# Labels:
#   value          label
# 1  Muy confiable
# 2 Algo confiable
# 3 Poco confiable
# 4 Nada confiable
# NA(a)        No sabe
# NA(b)    No responde

#Escolaridade
CHILE$edre
# Labels:
#   value                                              label
# 0                                             Nenhum
# 1           Primário/Ensino Fundamental (incompleto)
# 2             Primário/Ensino Fundamental (completo)
# 3      Secundário/Ensino Médio/colegial (incompleto)
# 4        Secundário/Ensino Médio/colegial (completo)
# 5 Ensino Superior/Bacharelado/Faculdade (incompleto)
# 6   Ensino Superior/Bacharelado/Faculdade (completo)
# NA(a)                                           Não sabe
# NA(b)                                       Não responde

#Idade
CHILE$q2
#[1] "Idade"

#Sexo
CHILE$q1tc_r
# value                                             label
# 1                                   Homem/masculino
# 2                                   Mulher/feminino
# 3 Não se identifica nem como homem, nem como mulher
# NA(a)                                          Não sabe
# NA(b)                                      Não responde


CHILE2023N <- CHILE[ ,c("pais", "d5", "q3cn", "q5b",
                   "ing4", "it1", "edre","q2", "q1tc_r")]



#____________
###Abrindo  a base de dados em dta e convertendo em 
#labels para poder juntar sem precisar ficar recategorizando
#no final
CHILE2023 <- read.dta13("CHILE.dta",
                      convert.factors = TRUE)

CHILE2023C <- CHILE2023[ ,c("pais", "d5", "q3cn", "q5b",
                        "ing4", "it1", "edre","q2", "q1tc_r")]

CHILE2023C$Países <- CHILE2023C$pais
CHILE2023C$TolerHomo <- CHILE2023C$d5
CHILE2023C$Denom_Desc <- CHILE2023C$q3cn
CHILE2023C$IntReligC <- CHILE2023C$q5b
CHILE2023C$ConfIntC <- CHILE2023C$it1
CHILE2023C$EscolC <- CHILE2023C$edre
CHILE2023C$Idade <- CHILE2023C$q2
CHILE2023C$Dem <- CHILE2023C$ing4


table(CHILE2023C$q1tc_r)

CHILE2023C <- CHILE2023C %>% 
  mutate(Sexo = case_when(
    q1tc_r == "Hombre/masculino" ~ "Homem",
    q1tc_r == "Mujer/femenino" ~ "Mulher",
    q1tc_r == "No se identifica como hombre ni como mujer"
    ~ "Não se identifica"
  ))
table(CHILE2023C$Sexo)

CHILE2023F <- CHILE2023C[ ,c("Países", "TolerHomo", "Denom_Desc",
                         "IntReligC", "Dem", "ConfIntC",
                         "EscolC", "Idade",
                         "Sexo")]


CHILE2023F <- bind_cols(CHILE2023N, CHILE2023F)

#5COSTA RICA####

COSTARICA

#País
COSTARICA$pais
# Labels:
#   value     label
#  6 Costa Rica

#Tolerância
COSTARICA$d5
# Labels:
#   value                 label
# 1 Desaprueba firmemente
# 10    Aprueba firmemente
# NA(a)               No sabe
# NA(b)           No responde

table (COSTARICA$d5)
#   1   2   3   4   5   6   7   8   9  10 
#273  66  65  69 154  89 116 143 112 414    

#Denominação religiosa 
COSTARICA$q3cn
# 
# Labels:
#   value                                                                  label
# value                                                                  label
# 1                                                               Católico
# 2       Protestante, Protestante Tradicional o Protestante no Evangélico
# 3                                    Religiones Orientales no Cristianas
# 4 Ninguna (Cree en un Ser Superior pero no pertenece a ninguna religión)
# 5                                               Evangélica y Pentecostal
# 7                                               Religiones Tradicionales
# 11                                     Agnóstico o ateo (no cree en Dios)
# 77                                                                   Otro
# NA(a)                                                                No sabe
# NA(b)                                                            No responde

#Ativismo religioso
COSTARICA$Q5a
#não tem 

#Intensidade Religiosa 
COSTARICA$q5b
# Labels:
#   value           label
# 1  Muy importante
# 2 Algo importante
# 3 Poco importante
# 4 Nada importante
# NA(a)         No sabe
# NA(b)     No responde


#Condicionantes
#Adesão a democracia (churCOSTARICAliana)
COSTARICA$ing4
# 
# Labels:
#   value             label
# 1 Muy en desacuerdo
# 7    Muy de acuerdo
# NA(a)           No sabe
# NA(b)       No responde

#Confiança
COSTARICA$it1
# Labels:
#   value          label
# 1  Muy confiable
# 2 Algo confiable
# 3 Poco confiable
# 4 Nada confiable
# NA(a)        No sabe
# NA(b)    No responde

#Escolaridade
COSTARICA$edre
# Labels:
#   value                                              label
# 0                                             Nenhum
# 1           Primário/Ensino Fundamental (incompleto)
# 2             Primário/Ensino Fundamental (completo)
# 3      Secundário/Ensino Médio/colegial (incompleto)
# 4        Secundário/Ensino Médio/colegial (completo)
# 5 Ensino Superior/Bacharelado/Faculdade (incompleto)
# 6   Ensino Superior/Bacharelado/Faculdade (completo)
# NA(a)                                           Não sabe
# NA(b)                                       Não responde

#Idade
COSTARICA$q2
#[1] "Idade"

#Sexo
COSTARICA$q1tc_r
# value                                             label
# 1                                   Homem/masculino
# 2                                   Mulher/feminino
# 3 Não se identifica nem como homem, nem como mulher
# NA(a)                                          Não sabe
# NA(b)                                      Não responde

COSTARICA2023N <- COSTARICA[ ,c("pais", "d5", "q3cn", "q5b",
                       "ing4", "it1", "edre","q2", "q1tc_r")]



#____________
###Abrindo  a base de dados em dta e convertendo em 
#labels para poder juntar sem precisar ficar recategorizando
#no final
COSTARICA2023 <- read.dta13("COSTARICA.dta",
                            convert.factors = TRUE)

COSTARICA2023C <- COSTARICA2023[ ,c("pais", "d5", "q3cn", "q5b",
                                    "ing4", "it1", "edre","q2", "q1tc_r")]

COSTARICA2023C$Países <- COSTARICA2023C$pais
COSTARICA2023C$TolerHomo <- COSTARICA2023C$d5
COSTARICA2023C$Denom_Desc <- COSTARICA2023C$q3cn
COSTARICA2023C$IntReligC <- COSTARICA2023C$q5b
COSTARICA2023C$ConfIntC <- COSTARICA2023C$it1
COSTARICA2023C$EscolC <- COSTARICA2023C$edre
COSTARICA2023C$Idade <- COSTARICA2023C$q2
COSTARICA2023C$Dem <- COSTARICA2023C$ing4


table(COSTARICA2023C$q1tc_r)

COSTARICA2023C <- COSTARICA2023C %>% 
  mutate(Sexo = case_when(
    q1tc_r == "Hombre/masculino" ~ "Homem",
    q1tc_r == "Mujer/femenino" ~ "Mulher",
    q1tc_r == "No se identifica como hombre ni como mujer"
    ~ "Não se identifica"
  ))
table(COSTARICA2023C$Sexo)

COSTARICA2023F <- COSTARICA2023C[ ,c("Países", "TolerHomo", "Denom_Desc",
                                     "IntReligC", "Dem", "ConfIntC",
                                     "EscolC", "Idade",
                                     "Sexo")]


COSTARICA2023F <- bind_cols(COSTARICA2023N, COSTARICA2023F)


#6ELSALVADOR####

ELSAL

#País
ELSAL$pais
# Labels:
#   value     label
# 3 El Salvador

#Tolerância
ELSAL$d5
# Labels:
#   value                 label
# 1 Desaprueba firmemente
# 10    Aprueba firmemente
# NA(a)               No sabe
# NA(b)           No responde

table (ELSAL$d5)
#   1   2   3   4   5   6   7   8   9  10 
#432 133 123 110 183 117  93 114  65 130    

#Denominação religiosa 
ELSAL$q3cn
# 
# Labels:
#   value                                                                  label
# value                                                                  label
# 1                                                               Católico
# 2       Protestante, Protestante Tradicional o Protestante no Evangélico
# 3                                    Religiones Orientales no Cristianas
# 4 Ninguna (Cree en un Ser Superior pero no pertenece a ninguna religión)
# 5                                               Evangélica y Pentecostal
# 7                                               Religiones Tradicionales
# 11                                     Agnóstico o ateo (no cree en Dios)
# 77                                                                   Otro
# NA(a)                                                                No sabe
# NA(b)                                                            No responde

#Ativismo religioso
ELSAL$Q5a
#não tem 

#Intensidade Religiosa 
ELSAL$q5b
# Labels:
#   value           label
# 1  Muy importante
# 2 Algo importante
# 3 Poco importante
# 4 Nada importante
# NA(a)         No sabe
# NA(b)     No responde


#Condicionantes
#Adesão a democracia (churELSALliana)
ELSAL$ing4
# 
# Labels:
#   value             label
# 1 Muy en desacuerdo
# 7    Muy de acuerdo
# NA(a)           No sabe
# NA(b)       No responde

#Confiança
ELSAL$it1
# Labels:
#   value          label
# 1  Muy confiable
# 2 Algo confiable
# 3 Poco confiable
# 4 Nada confiable
# NA(a)        No sabe
# NA(b)    No responde

#Escolaridade
ELSAL$edre
# Labels:
#   value                                              label
# 0                                             Nenhum
# 1           Primário/Ensino Fundamental (incompleto)
# 2             Primário/Ensino Fundamental (completo)
# 3      Secundário/Ensino Médio/colegial (incompleto)
# 4        Secundário/Ensino Médio/colegial (completo)
# 5 Ensino Superior/Bacharelado/Faculdade (incompleto)
# 6   Ensino Superior/Bacharelado/Faculdade (completo)
# NA(a)                                           Não sabe
# NA(b)                                       Não responde

#Idade
ELSAL$q2
#[1] "Idade"

#Sexo
ELSAL$q1tc_r
# value                                             label
# 1                                   Homem/masculino
# 2                                   Mulher/feminino
# 3 Não se identifica nem como homem, nem como mulher
# NA(a)                                          Não sabe
# NA(b)                                      Não responde


ELSAL2023N <- ELSAL[ ,c("pais", "d5", "q3cn", "q5b",
                               "ing4", "it1", "edre","q2", "q1tc_r")]



#____________
###Abrindo  a base de dados em dta e convertendo em 
#labels para poder juntar sem precisar ficar recategorizando
#no final
ELSAL2023 <- read.dta13("ELSAL.dta",
                        convert.factors = TRUE)

ELSAL2023C <- ELSAL2023[ ,c("pais", "d5", "q3cn", "q5b",
                            "ing4", "it1", "edre","q2", "q1tc_r")]

ELSAL2023C$Países <- ELSAL2023C$pais
ELSAL2023C$TolerHomo <- ELSAL2023C$d5
ELSAL2023C$Denom_Desc <- ELSAL2023C$q3cn
ELSAL2023C$IntReligC <- ELSAL2023C$q5b
ELSAL2023C$ConfIntC <- ELSAL2023C$it1
ELSAL2023C$EscolC <- ELSAL2023C$edre
ELSAL2023C$Idade <- ELSAL2023C$q2
ELSAL2023C$Dem <- ELSAL2023C$ing4


table(ELSAL2023C$q1tc_r)

ELSAL2023C <- ELSAL2023C %>% 
  mutate(Sexo = case_when(
    q1tc_r == "Hombre/masculino" ~ "Homem",
    q1tc_r == "Mujer/femenino" ~ "Mulher",
    q1tc_r == "No se identifica como hombre ni como mujer"
    ~ "Não se identifica"
  ))
table(ELSAL2023C$Sexo)

ELSAL2023F <- ELSAL2023C[ ,c("Países", "TolerHomo", "Denom_Desc",
                             "IntReligC", "Dem", "ConfIntC",
                             "EscolC", "Idade",
                             "Sexo")]


ELSAL2023F <- bind_cols(ELSAL2023N, ELSAL2023F)



#7EQUADOR####

ECUA

#País
ECUA$pais
# Labels:
#   value     label
# 9 Ecuador

#Tolerância
ECUA$d5
# Labels:
#   value                 label
# 1 Desaprueba firmemente
# 10    Aprueba firmemente
# NA(a)               No sabe
# NA(b)           No responde

table (ECUA$d5)
#   1   2   3   4   5   6   7   8   9  10 
#372 101  93 109 181 116 118 144  89 249   

#Denominação religiosa 
ECUA$q3cn
# 
# Labels:
#   value                                                                  label
# value                                                                  label
# 1                                                               Católico
# 2       Protestante, Protestante Tradicional o Protestante no Evangélico
# 3                                    Religiones Orientales no Cristianas
# 4 Ninguna (Cree en un Ser Superior pero no pertenece a ninguna religión)
# 5                                               Evangélica y Pentecostal
# 7                                               Religiones Tradicionales
# 11                                     Agnóstico o ateo (no cree en Dios)
# 77                                                                   Otro
# NA(a)                                                                No sabe
# NA(b)                                                            No responde

#Ativismo religioso
ECUA$Q5a
#não tem 

#Intensidade Religiosa 
ECUA$q5b
# Labels:
#   value           label
# 1  Muy importante
# 2 Algo importante
# 3 Poco importante
# 4 Nada importante
# NA(a)         No sabe
# NA(b)     No responde


#Condicionantes
#Adesão a democracia (churECUAliana)
ECUA$ing4
# 
# Labels:
#   value             label
# 1 Muy en desacuerdo
# 7    Muy de acuerdo
# NA(a)           No sabe
# NA(b)       No responde

#Confiança
ECUA$it1
# Labels:
#   value          label
# 1  Muy confiable
# 2 Algo confiable
# 3 Poco confiable
# 4 Nada confiable
# NA(a)        No sabe
# NA(b)    No responde

#Escolaridade
ECUA$edre
# Labels:
#   value                                              label
# 0                                             Nenhum
# 1           Primário/Ensino Fundamental (incompleto)
# 2             Primário/Ensino Fundamental (completo)
# 3      Secundário/Ensino Médio/colegial (incompleto)
# 4        Secundário/Ensino Médio/colegial (completo)
# 5 Ensino Superior/Bacharelado/Faculdade (incompleto)
# 6   Ensino Superior/Bacharelado/Faculdade (completo)
# NA(a)                                           Não sabe
# NA(b)                                       Não responde

#Idade
ECUA$q2
#[1] "Idade"

#Sexo
ECUA$q1tc_r
# value                                             label
# 1                                   Homem/masculino
# 2                                   Mulher/feminino
# 3 Não se identifica nem como homem, nem como mulher
# NA(a)                                          Não sabe
# NA(b)                                      Não responde


ECUA2023N <- ECUA[ ,c("pais", "d5", "q3cn", "q5b",
                       "ing4", "it1", "edre","q2", "q1tc_r")]

#____________
###Abrindo  a base de dados em dta e convertendo em 
#labels para poder juntar sem precisar ficar recategorizando
#no final
ECUA2023 <- read.dta13("ECUA.dta",
                       convert.factors = TRUE)

ECUA2023C <- ECUA2023[ ,c("pais", "d5", "q3cn", "q5b",
                          "ing4", "it1", "edre","q2", "q1tc_r")]

ECUA2023C$Países <- ECUA2023C$pais
ECUA2023C$TolerHomo <- ECUA2023C$d5
ECUA2023C$Denom_Desc <- ECUA2023C$q3cn
ECUA2023C$IntReligC <- ECUA2023C$q5b
ECUA2023C$ConfIntC <- ECUA2023C$it1
ECUA2023C$EscolC <- ECUA2023C$edre
ECUA2023C$Idade <- ECUA2023C$q2
ECUA2023C$Dem <- ECUA2023C$ing4


table(ECUA2023C$q1tc_r)

ECUA2023C <- ECUA2023C %>% 
  mutate(Sexo = case_when(
    q1tc_r == "Hombre/masculino" ~ "Homem",
    q1tc_r == "Mujer/femenino" ~ "Mulher",
    q1tc_r == "No se identifica como hombre ni como mujer"
    ~ "Não se identifica"
  ))
table(ECUA2023C$Sexo)

ECUA2023F <- ECUA2023C[ ,c("Países", "TolerHomo", "Denom_Desc",
                           "IntReligC", "Dem", "ConfIntC",
                           "EscolC", "Idade",
                           "Sexo")]


ECUA2023F <- bind_cols(ECUA2023N, ECUA2023F)





#8GUATEMALA####

GUAT

#País
GUAT$pais
# Labels:
#   value     label
# 2 Guatemala

#Tolerância
GUAT$d5
# Labels:
#   value                 label
# 1 Desaprueba firmemente
# 10    Aprueba firmemente
# NA(a)               No sabe
# NA(b)           No responde

table (GUAT$d5)
#   1   2   3   4   5   6   7   8   9  10 
#473 175 130 136 199  90  88  83  42 103 

#Denominação religiosa 
GUAT$q3cn
# 
# Labels:
#   value                                                                  label
# value                                                                  label
# 1                                                               Católico
# 2       Protestante, Protestante Tradicional o Protestante no Evangélico
# 3                                    Religiones Orientales no Cristianas
# 4 Ninguna (Cree en un Ser Superior pero no pertenece a ninguna religión)
# 5                                               Evangélica y Pentecostal
# 7                                               Religiones Tradicionales
# 11                                     Agnóstico o ateo (no cree en Dios)
# 77                                                                   Otro
# NA(a)                                                                No sabe
# NA(b)                                                            No responde

#Ativismo religioso
GUAT$Q5a
#não tem 

#Intensidade Religiosa 
GUAT$q5b
# Labels:
#   value           label
# 1  Muy importante
# 2 Algo importante
# 3 Poco importante
# 4 Nada importante
# NA(a)         No sabe
# NA(b)     No responde


#Condicionantes
#Adesão a democracia (churGUATliana)
GUAT$ing4
# 
# Labels:
#   value             label
# 1 Muy en desacuerdo
# 7    Muy de acuerdo
# NA(a)           No sabe
# NA(b)       No responde

#Confiança
GUAT$it1
# Labels:
#   value          label
# 1  Muy confiable
# 2 Algo confiable
# 3 Poco confiable
# 4 Nada confiable
# NA(a)        No sabe
# NA(b)    No responde

#Escolaridade
GUAT$edre
# Labels:
#   value                                              label
# 0                                             Nenhum
# 1           Primário/Ensino Fundamental (incompleto)
# 2             Primário/Ensino Fundamental (completo)
# 3      Secundário/Ensino Médio/colegial (incompleto)
# 4        Secundário/Ensino Médio/colegial (completo)
# 5 Ensino Superior/Bacharelado/Faculdade (incompleto)
# 6   Ensino Superior/Bacharelado/Faculdade (completo)
# NA(a)                                           Não sabe
# NA(b)                                       Não responde

#Idade
GUAT$q2
#[1] "Idade"

#Sexo
GUAT$q1tc_r
# value                                             label
# 1                                   Homem/masculino
# 2                                   Mulher/feminino
# 3 Não se identifica nem como homem, nem como mulher
# NA(a)                                          Não sabe
# NA(b)                                      Não responde


GUAT2023N <- GUAT[ ,c("pais", "d5", "q3cn", "q5b",
                     "ing4", "it1", "edre","q2", "q1tc_r")]


#____________
###Abrindo  a base de dados em dta e convertendo em 
#labels para poder juntar sem precisar ficar recategorizando
#no final
GUAT2023 <- read.dta13("GUAT.dta",
                       convert.factors = TRUE)

GUAT2023C <- GUAT2023[ ,c("pais", "d5", "q3cn", "q5b",
                          "ing4", "it1", "edre","q2", "q1tc_r")]

GUAT2023C$Países <- GUAT2023C$pais
GUAT2023C$TolerHomo <- GUAT2023C$d5
GUAT2023C$Denom_Desc <- GUAT2023C$q3cn
GUAT2023C$IntReligC <- GUAT2023C$q5b
GUAT2023C$ConfIntC <- GUAT2023C$it1
GUAT2023C$EscolC <- GUAT2023C$edre
GUAT2023C$Idade <- GUAT2023C$q2
GUAT2023C$Dem <- GUAT2023C$ing4


table(GUAT2023C$q1tc_r)

GUAT2023C <- GUAT2023C %>% 
  mutate(Sexo = case_when(
    q1tc_r == "Hombre/masculino" ~ "Homem",
    q1tc_r == "Mujer/femenino" ~ "Mulher",
    q1tc_r == "No se identifica como hombre ni como mujer"
    ~ "Não se identifica"
  ))
table(GUAT2023C$Sexo)

GUAT2023F <- GUAT2023C[ ,c("Países", "TolerHomo", "Denom_Desc",
                           "IntReligC", "Dem", "ConfIntC",
                           "EscolC", "Idade",
                           "Sexo")]


GUAT2023F <- bind_cols(GUAT2023N, GUAT2023F)



#9HAITI NÃO#### 

#NÃO VAI DAR PRA TRABALHAR COM O HAITI PORQUE RETIRARAM 
#AS PRINCIPAIS VARIÁVEIS DEPENDENTE E INDEPENDE
HAITI

#País
HAITI$pais
# Labels:
#   value     label
# 22 Haiti

#Tolerância
HAITI$d5
# Labels:
#   value                 label
# 1 Desaprueba firmemente
# 10    Aprueba firmemente
# NA(a)               No sabe
# NA(b)           No responde

table(HAITI$d5)
#   1   2   3   4   5   6   7   8   9  10 
#473 175 130 136 199  90  88  83  42 103 

#Denominação religiosa 
HAITI$q3cn
# 
# Labels:
#   value                                                                  label
# value                                                                  label
# 1                                                               Católico
# 2       Protestante, Protestante Tradicional o Protestante no Evangélico
# 3                                    Religiones Orientales no Cristianas
# 4 Ninguna (Cree en un Ser Superior pero no pertenece a ninguna religión)
# 5                                               Evangélica y Pentecostal
# 7                                               Religiones Tradicionales
# 11                                     Agnóstico o ateo (no cree en Dios)
# 77                                                                   Otro
# NA(a)                                                                No sabe
# NA(b)                                                            No responde

#Ativismo religioso
HAITI$Q5a
#não tem 

#Intensidade Religiosa 
HAITI$q5b
# Labels:
#   value           label
# 1  Muy importante
# 2 Algo importante
# 3 Poco importante
# 4 Nada importante
# NA(a)         No sabe
# NA(b)     No responde


#Condicionantes
#Adesão a democracia (churHAITIliana)
HAITI$ing4
# 
# Labels:
#   value             label
# 1 Muy en desacuerdo
# 7    Muy de acuerdo
# NA(a)           No sabe
# NA(b)       No responde

#Confiança
HAITI$it1
# Labels:
#   value          label
# 1  Muy confiable
# 2 Algo confiable
# 3 Poco confiable
# 4 Nada confiable
# NA(a)        No sabe
# NA(b)    No responde

#Escolaridade
HAITI$edre
# Labels:
#   value                                              label
# 0                                             Nenhum
# 1           Primário/Ensino Fundamental (incompleto)
# 2             Primário/Ensino Fundamental (completo)
# 3      Secundário/Ensino Médio/colegial (incompleto)
# 4        Secundário/Ensino Médio/colegial (completo)
# 5 Ensino Superior/Bacharelado/Faculdade (incompleto)
# 6   Ensino Superior/Bacharelado/Faculdade (completo)
# NA(a)                                           Não sabe
# NA(b)                                       Não responde

#Idade
HAITI$q2
#[1] "Idade"

#Sexo
HAITI$q1tc_r
# value                                             label
# 1                                   Homem/masculino
# 2                                   Mulher/feminino
# 3 Não se identifica nem como homem, nem como mulher
# NA(a)                                          Não sabe
# NA(b)                                      Não responde

#9HAITI2016####

HAITI2016

#País

HAITI2016$pais
# Labels:
#   value     label
#22 Haiti

#Tolerância
# HAITI2016$d5
# value               label
# 1 Strongly disapprove
# 10    Strongly approve
# NA(a)          Don't Know
#  NA(b)         No Response

table (HAITI2016$d5)
#   1   2   3   4   5   6   7   8   9  10 
#1710  113   43   44   58   29   25   26   13   83

#Denominação religiosa 
HAITI2016$q3c

# 
# label
# Catholic
# Protestant, Mainline Protestant or Protestant non-Evangelical
# Non-Christian Eastern Religion
# None (Believes in a Supreme Entity but does not belong to any religion)
# Evangelical and Protestant
# Church of Latter Day Saints (Mormon)
# Traditional or Native Religion
# Jewish (Orthodox, Conservative, Reform)
# Agnostic or Atheist (Does not believe in God)
# Jehovah's Witness
#                                                                    Other
#                                                               Don't Know
# No Response                                                        No responde

#Ativismo religioso
HAITI2016$Q5a
#não tem 

#Intensidade Religiosa 
HAITI2016$q5b
# Labels:
#   value                label
# 1       Very Important
# 2   Somewhat Important
# 3   Not Very Important
# 4 Not Important at All
# NA(a)           Don't Know
#  NA(b)          No Response


#Condicionantes
#Adesão a democracia (churHAITI2016liana)
HAITI2016$ing4
# 
# Labels:
# Labels:
#   value             label
# 1 Strongly Disagree
# 7    Strongly Agree
# NA(a)        Don't Know
#  NA(b)       No Response

#Confiança
HAITI2016$it1
# Labels:
# value                label
# 1     Very Trustworthy
# 2 Somewhat Trustworthy
# 3 Not Very Trustworthy
# 4        Untrustworthy
# NA(a)           Don't Know
#  NA(b)          No Response

#Escolaridade
HAITI2016$ed2
# Labels:
#   value       label
# 0        None
# 18         18+
#   NA(a)  Don't Know
#  NA(b) No Response


#Labels:
#   value                                        label
# 0                                         None
# 1                           Primary incomplete
# 2                             Primary complete
# 3                         Secondary incomplete
# 4                           Secondary complete
# 5 Technical school/Associate degree incomplete
# 6   Technical school/Associate degree complete
# 7                        University incomplete
# 8                          University complete
# NA(a)                                   Don't Know
#  NA(b)                                  No Response

#Idade
HAITI2016$q2
#[1] "Idade"

#Sexo
HAITI2016$q1
# Labels:
#   value  label
# 1   Male
# 2 Female

HAITI2016.2N <- HAITI2016[ ,c("pais", "d5", "q3c", "q5b",
                              "ing4", "it1", "ed","q2", "q1")]

#____________
###Abrindo  a base de dados em dta e convertendo em 
#labels para poder juntar sem precisar ficar recategorizando
#no final
HAITI20162023 <- read.dta13("HAITI2016.dta",
                            convert.factors = TRUE)

HAITI20162023C <- HAITI20162023[ ,c("pais", "d5", "q3c", "q5b",
                                    "ing4", "it1", "ed","q2", "q1")]

# URUG2023C <- URUG2023[ ,c("pais", "d5", "q3cn", "q5b",
#                           "ing4", "it1", "edre","q2", "q1tc_r")]

HAITI20162023C$Países <- HAITI20162023C$pais
HAITI20162023C$TolerHomo <- HAITI20162023C$d5
HAITI20162023C$Denom_Desc <- HAITI20162023C$q3c
HAITI20162023C$IntReligC <- HAITI20162023C$q5b
HAITI20162023C$ConfIntC <- HAITI20162023C$it1
HAITI20162023C$EscolC <- HAITI20162023C$ed
HAITI20162023C$Idade <- HAITI20162023C$q2
HAITI20162023C$Dem <- HAITI20162023C$ing4
HAITI20162023C$edre <- HAITI20162023C$ed

table(HAITI20162023C$q1)



HAITI20162023C <- HAITI20162023C %>% 
  mutate(Sexo = case_when(
    q1 == "Male" ~ "Homem",
    q1 == "Female" ~ "Mulher"
  ))
table(HAITI20162023C$Sexo)

HAITI20162023F <- HAITI20162023C[ ,c("Países", "TolerHomo", "Denom_Desc",
                                     "IntReligC", "Dem", "ConfIntC",
                                     "EscolC", "Idade",
                                     "Sexo", "edre" )]

table(HAITI2016$q3c)


HAITI20162023F$EscolC <- as.factor(HAITI20162023F$EscolC)

HAITI20162023F <- bind_cols(HAITI2016.2N, HAITI20162023F)

# Definir a recategorização
HAITI20162023F <- HAITI20162023F %>%
  mutate(Denom_Desc = case_when(
    Denom_Desc == "Catholic" ~ "Católico",
    Denom_Desc == "Protestant, Mainline Protestant or Protestant non-Evangelical" ~ "Protestante, Protestante Tradicional o Protestante no Evangélico",
    Denom_Desc == "Non-Christian Eastern Religion" ~ "Religiones Orientales no Cristianas",
    Denom_Desc == "None (Believes in a Supreme Entity but does not belong to any religion)" ~ "Ninguna (Cree en un Ser Superior pero no pertenece a ninguna religión)",
    Denom_Desc == "Evangelical and Protestant" ~ "Evangélica y Pentecostal",
    Denom_Desc == "Church of Latter Day Saints (Mormon)" | Denom_Desc == "Jehovah's Witness" ~ "Outra",
    Denom_Desc == "Traditional or Native Religion" ~ "Religiones Tradicionales",
    Denom_Desc == "Agnostic or Atheist (Does not believe in God)" ~ "Agnóstico o ateo (no cree en Dios)",
    Denom_Desc == "Other" ~ "Otro",
    TRUE ~ Denom_Desc  # Manter os valores não especificados
  ))


table(HAITI20162023F$IntReligC)
table(AMERLATINA$IntReligC)

HAITI20162023F <- HAITI20162023F %>%
  mutate(IntReligC = case_when(
    IntReligC == "Very Important" ~ "Muy importante",
    IntReligC == "Somewhat Important" ~ "Algo importante",
    IntReligC == "Not Very Important" ~ "Poco importante",
    IntReligC == "Not Important at All" ~ "Nada importante",
    IntReligC == "Don't Know" | IntReligC == "No Response" ~ "No sabe",
    TRUE ~ IntReligC  # Manter os valores não especificados
  ))


table(HAITI20162023F$ConfIntC)
table(AMERLATINA$ConfIntC)



HAITI20162023F <- HAITI20162023F %>%
  mutate(ConfIntC = case_when(
    ConfIntC == "Very Trustworthy" ~ "Muy confiable",
    ConfIntC == "Somewhat Trustworthy" ~ "Algo confiable",
    ConfIntC == "Not Very Trustworthy" ~ "Poco confiable",
    ConfIntC == "Untrustworthy" ~ "Nada confiable",
    ConfIntC == "Don't Know" | ConfIntC == "No Response" ~ "No sabe",
    TRUE ~ ConfIntC  # Manter os valores não especificados
  ))
#by= c("IDFuncionario" = "IDDepartamento"))

colnames(AMERLATINA)

AMERLATINA2 <- full_join(AMERLATINA, 
                         HAITI20162023F, 
                         by= c("pais"="pais", 
                               "d5"="d5",
                               "q3cn"="q3c",
                               "ing4"="ing4",
                               "it1"="it1",
                               "edre"="edre",
                               "q2"="q2",
                               "q1tc_r"="q1",
                               "Países"="Países",
                               "TolerHomo"="TolerHomo",
                               "Denom_Desc"="Denom_Desc",
                               "IntReligC"="IntReligC",
                               "Dem"="Dem",
                               "ConfIntC"="ConfIntC",
                               "EscolC"="EscolC",
                               "Idade"="Idade",
                               "Sexo"="Sexo")) 

#Retirar as variáveis que não me interessam 

#10HONDURAS####

HOND

#País
HOND$pais
# Labels:
#   value     label
# 4 Honduras

#Tolerância
HOND$d5
# Labels:
#   value                 label
# 1 Desaprueba firmemente
# 10    Aprueba firmemente
# NA(a)               No sabe
# NA(b)           No responde

table (HOND$d5)
#   1   2   3   4   5   6   7   8   9  10 
#473 175 130 136 199  90  88  83  42 103 

#Denominação religiosa 
HOND$q3cn
# 
# Labels:
#   value                                                                  label
# value                                                                  label
# 1                                                               Católico
# 2       Protestante, Protestante Tradicional o Protestante no Evangélico
# 3                                    Religiones Orientales no Cristianas
# 4 Ninguna (Cree en un Ser Superior pero no pertenece a ninguna religión)
# 5                                               Evangélica y Pentecostal
# 7                                               Religiones Tradicionales
# 11                                     Agnóstico o ateo (no cree en Dios)
# 77                                                                   Otro
# NA(a)                                                                No sabe
# NA(b)                                                            No responde

#Ativismo religioso
HOND$Q5a
#não tem 

#Intensidade Religiosa 
HOND$q5b
# Labels:
#   value           label
# 1  Muy importante
# 2 Algo importante
# 3 Poco importante
# 4 Nada importante
# NA(a)         No sabe
# NA(b)     No responde


#Condicionantes
#Adesão a democracia (churHONDliana)
HOND$ing4
# 
# Labels:
#   value             label
# 1 Muy en desacuerdo
# 7    Muy de acuerdo
# NA(a)           No sabe
# NA(b)       No responde

#Confiança
HOND$it1
# Labels:
#   value          label
# 1  Muy confiable
# 2 Algo confiable
# 3 Poco confiable
# 4 Nada confiable
# NA(a)        No sabe
# NA(b)    No responde

#Escolaridade
HOND$edre
# Labels:
#   value                                              label
# 0                                             Nenhum
# 1           Primário/Ensino Fundamental (incompleto)
# 2             Primário/Ensino Fundamental (completo)
# 3      Secundário/Ensino Médio/colegial (incompleto)
# 4        Secundário/Ensino Médio/colegial (completo)
# 5 Ensino Superior/Bacharelado/Faculdade (incompleto)
# 6   Ensino Superior/Bacharelado/Faculdade (completo)
# NA(a)                                           Não sabe
# NA(b)                                       Não responde

#Idade
HOND$q2
#[1] "Idade"

#Sexo
HOND$q1tc_r
# value                                             label
# 1                                   Homem/masculino
# 2                                   Mulher/feminino
# 3 Não se identifica nem como homem, nem como mulher
# NA(a)                                          Não sabe
# NA(b)                                      Não responde

HOND2023N <- HOND[ ,c("pais", "d5", "q3cn", "q5b",
                     "ing4", "it1", "edre","q2", "q1tc_r")]



#____________
###Abrindo  a base de dados em dta e convertendo em 
#labels para poder juntar sem precisar ficar recategorizando
#no final
HOND2023 <- read.dta13("HOND.dta",
                       convert.factors = TRUE)

HOND2023C <- HOND2023[ ,c("pais", "d5", "q3cn", "q5b",
                          "ing4", "it1", "edre","q2", "q1tc_r")]

HOND2023C$Países <- HOND2023C$pais
HOND2023C$TolerHomo <- HOND2023C$d5
HOND2023C$Denom_Desc <- HOND2023C$q3cn
HOND2023C$IntReligC <- HOND2023C$q5b
HOND2023C$ConfIntC <- HOND2023C$it1
HOND2023C$EscolC <- HOND2023C$edre
HOND2023C$Idade <- HOND2023C$q2
HOND2023C$Dem <- HOND2023C$ing4


table(HOND2023C$q1tc_r)

HOND2023C <- HOND2023C %>% 
  mutate(Sexo = case_when(
    q1tc_r == "Hombre/masculino" ~ "Homem",
    q1tc_r == "Mujer/femenino" ~ "Mulher",
    q1tc_r == "No se identifica como hombre ni como mujer"
    ~ "Não se identifica"
  ))
table(HOND2023C$Sexo)

HOND2023F <- HOND2023C[ ,c("Países", "TolerHomo", "Denom_Desc",
                           "IntReligC", "Dem", "ConfIntC",
                           "EscolC", "Idade",
                           "Sexo")]


HOND2023F <- bind_cols(HOND2023N, HOND2023F)


#11MÉXICO####

MEX

#País
MEX$pais
# Labels:
#   value     label
# 1 México

#Tolerância
MEX$d5
# Labels:
#   value                 label
# 1 Desaprueba firmemente
# 10    Aprueba firmemente
# NA(a)               No sabe
# NA(b)           No responde

table (MEX$d5)
#   1   2   3   4   5   6   7   8   9  10 
#240  70  80  69 171 107 152 247 123 335 

#Denominação religiosa 
MEX$q3cn
# 
# Labels:
#   value                                                                  label
# value                                                                  label
# 1                                                               Católico
# 2       Protestante, Protestante Tradicional o Protestante no Evangélico
# 3                                    Religiones Orientales no Cristianas
# 4 Ninguna (Cree en un Ser Superior pero no pertenece a ninguna religión)
# 5                                               Evangélica y Pentecostal
# 7                                               Religiones Tradicionales
# 11                                     Agnóstico o ateo (no cree en Dios)
# 77                                                                   Otro
# NA(a)                                                                No sabe
# NA(b)                                                            No responde

#Ativismo religioso
MEX$Q5a
#não tem 

#Intensidade Religiosa 
MEX$q5b
# Labels:
#   value           label
# 1  Muy importante
# 2 Algo importante
# 3 Poco importante
# 4 Nada importante
# NA(a)         No sabe
# NA(b)     No responde


#Condicionantes
#Adesão a democracia (churMEXliana)
MEX$ing4
# 
# Labels:
#   value             label
# 1 Muy en desacuerdo
# 7    Muy de acuerdo
# NA(a)           No sabe
# NA(b)       No responde

#Confiança
MEX$it1
# Labels:
#   value          label
# 1  Muy confiable
# 2 Algo confiable
# 3 Poco confiable
# 4 Nada confiable
# NA(a)        No sabe
# NA(b)    No responde

#Escolaridade
MEX$edre
# Labels:
#   value                                              label
# 0                                             Nenhum
# 1           Primário/Ensino Fundamental (incompleto)
# 2             Primário/Ensino Fundamental (completo)
# 3      Secundário/Ensino Médio/colegial (incompleto)
# 4        Secundário/Ensino Médio/colegial (completo)
# 5 Ensino Superior/Bacharelado/Faculdade (incompleto)
# 6   Ensino Superior/Bacharelado/Faculdade (completo)
# NA(a)                                           Não sabe
# NA(b)                                       Não responde

#Idade
MEX$q2
#[1] "Idade"

#Sexo
MEX$q1tc_r
# value                                             label
# 1                                   Homem/masculino
# 2                                   Mulher/feminino
# 3 Não se identifica nem como homem, nem como mulher
# NA(a)                                          Não sabe
# NA(b)                                      Não responde


MEX2023N <- MEX[ ,c("pais", "d5", "q3cn", "q5b",
                     "ing4", "it1", "edre","q2", "q1tc_r")]


#____________
###Abrindo  a base de dados em dta e convertendo em 
#labels para poder juntar sem precisar ficar recategorizando
#no final
MEX2023 <- read.dta13("MEX.dta",
                       convert.factors = TRUE)

MEX2023C <- MEX2023[ ,c("pais", "d5", "q3cn", "q5b",
                          "ing4", "it1", "edre","q2", "q1tc_r")]

MEX2023C$Países <- MEX2023C$pais
MEX2023C$TolerHomo <- MEX2023C$d5
MEX2023C$Denom_Desc <- MEX2023C$q3cn
MEX2023C$IntReligC <- MEX2023C$q5b
MEX2023C$ConfIntC <- MEX2023C$it1
MEX2023C$EscolC <- MEX2023C$edre
MEX2023C$Idade <- MEX2023C$q2
MEX2023C$Dem <- MEX2023C$ing4


table(MEX2023C$q1tc_r)

MEX2023C <- MEX2023C %>% 
  mutate(Sexo = case_when(
    q1tc_r == "Hombre/masculino" ~ "Homem",
    q1tc_r == "Mujer/femenino" ~ "Mulher",
    q1tc_r == "No se identifica como hombre ni como mujer"
    ~ "Não se identifica"
  ))
table(MEX2023C$Sexo)

MEX2023F <- MEX2023C[ ,c("Países", "TolerHomo", "Denom_Desc",
                           "IntReligC", "Dem", "ConfIntC",
                           "EscolC", "Idade",
                           "Sexo")]


MEX2023F <- bind_cols(MEX2023N, MEX2023F)




#12NICARÁGUAnão####
#Não tem importantes variáveis religiosas, talvez usar
#rodada anterior

NIC

#País
NIC$pais
# Labels:
#   value     label
# 5 Nicaragua

#Tolerância
NIC$d5
# Labels:
#   value                 label
# 1 Desaprueba firmemente
# 10    Aprueba firmemente
# NA(a)               No sabe
# NA(b)           No responde

table (NIC$d5)
#   1   2   3   4   5   6   7   8   9  10 
#367  46  57  50 125  49  74 124  45 431 

#Denominação religiosa 
NIC$q3

NIC$q
  
# 
# Labels:
#   value                                                                  label
# value                                                                  label
# 1                                                               Católico
# 2       Protestante, Protestante Tradicional o Protestante no Evangélico
# 3                                    Religiones Orientales no Cristianas
# 4 Ninguna (Cree en un Ser Superior pero no pertenece a ninguna religión)
# 5                                               Evangélica y Pentecostal
# 7                                               Religiones Tradicionales
# 11                                     Agnóstico o ateo (no cree en Dios)
# 77                                                                   Otro
# NA(a)                                                                No sabe
# NA(b)                                                            No responde

#Ativismo religioso
NIC$Q5a
#não tem 

#Intensidade Religiosa 
NIC$q5b
#Não tem


#Condicionantes
#Adesão a democracia (churNICliana)
NIC$ing4
# 
# Labels:
#   value             label
# 1 Muy en desacuerdo
# 7    Muy de acuerdo
# NA(a)           No sabe
# NA(b)       No responde

#Confiança
NIC$it1
# Labels:
#   value          label
# 1  Muy confiable
# 2 Algo confiable
# 3 Poco confiable
# 4 Nada confiable
# NA(a)        No sabe
# NA(b)    No responde

#Escolaridade
NIC$edre
# Labels:
#   value                                              label
# 0                                             Nenhum
# 1           Primário/Ensino Fundamental (incompleto)
# 2             Primário/Ensino Fundamental (completo)
# 3      Secundário/Ensino Médio/colegial (incompleto)
# 4        Secundário/Ensino Médio/colegial (completo)
# 5 Ensino Superior/Bacharelado/Faculdade (incompleto)
# 6   Ensino Superior/Bacharelado/Faculdade (completo)
# NA(a)                                           Não sabe
# NA(b)                                       Não responde

#Idade
NIC$q2
#[1] "Idade"

#Sexo
NIC$q1tc_r
# value                                             label
# 1                                   Homem/masculino
# 2                                   Mulher/feminino
# 3 Não se identifica nem como homem, nem como mulher
# NA(a)                                          Não sabe
# NA(b)                                      Não responde


NIC2023 <- NIC[ ,c("pais", "d5", "q3cn", "q5b",
                   "ing4", "it1", "edre","q2", "q1tc_r")]




#13PANAMÁ####


#País
PANAM$pais
# Labels:
#   value     label
#  7 Panamá

#Tolerância
PANAM$d5
# Labels:
#   value                 label
# 1 Desaprueba firmemente
# 10    Aprueba firmemente
# NA(a)               No sabe
# NA(b)           No responde

table (PANAM$d5)
#   1   2   3   4   5   6   7   8   9  10 
#581  97  82  63 164  77  93  96  53 203

#Denominação religiosa 
PANAM$q3cn


# 
# Labels:
#   value                                                                  label
# value                                                                  label
# 1                                                               Católico
# 2       Protestante, Protestante Tradicional o Protestante no Evangélico
# 3                                    Religiones Orientales no Cristianas
# 4 Ninguna (Cree en un Ser Superior pero no pertenece a ninguna religión)
# 5                                               Evangélica y Pentecostal
# 7                                               Religiones Tradicionales
# 11                                     Agnóstico o ateo (no cree en Dios)
# 77                                                                   Otro
# NA(a)                                                                No sabe
# NA(b)                                                            No responde

#Ativismo religioso
PANAM$Q5a
#não tem 

#Intensidade Religiosa 
PANAM$q5b
# Labels:
#   value           label
# 1  Muy importante
# 2 Algo importante
# 3 Poco importante
# 4 Nada importante
# NA(a)         No sabe
# NA(b)     No responde


#Condicionantes
#Adesão a democracia (churPANAMliana)
PANAM$ing4
# 
# Labels:
#   value             label
# 1 Muy en desacuerdo
# 7    Muy de acuerdo
# NA(a)           No sabe
# NA(b)       No responde

#Confiança
PANAM$it1
# Labels:
#   value          label
# 1  Muy confiable
# 2 Algo confiable
# 3 Poco confiable
# 4 Nada confiable
# NA(a)        No sabe
# NA(b)    No responde

#Escolaridade
PANAM$edre
# Labels:
#   value                                              label
# 0                                             Nenhum
# 1           Primário/Ensino Fundamental (incompleto)
# 2             Primário/Ensino Fundamental (completo)
# 3      Secundário/Ensino Médio/colegial (incompleto)
# 4        Secundário/Ensino Médio/colegial (completo)
# 5 Ensino Superior/Bacharelado/Faculdade (incompleto)
# 6   Ensino Superior/Bacharelado/Faculdade (completo)
# NA(a)                                           Não sabe
# NA(b)                                       Não responde

#Idade
PANAM$q2
#[1] "Idade"

#Sexo
PANAM$q1tc_r
# value                                             label
# 1                                   Homem/masculino
# 2                                   Mulher/feminino
# 3 Não se identifica nem como homem, nem como mulher
# NA(a)                                          Não sabe
# NA(b)                                      Não responde


PANAM2023N <- PANAM[ ,c("pais", "d5", "q3cn", "q5b",
                   "ing4", "it1", "edre","q2", "q1tc_r")]


#____________
###Abrindo  a base de dados em dta e convertendo em 
#labels para poder juntar sem precisar ficar recategorizando
#no final
PANAM2023 <- read.dta13("PANAM.dta",
                        convert.factors = TRUE)

PANAM2023C <- PANAM2023[ ,c("pais", "d5", "q3cn", "q5b",
                            "ing4", "it1", "edre","q2", "q1tc_r")]

PANAM2023C$Países <- PANAM2023C$pais
PANAM2023C$TolerHomo <- PANAM2023C$d5
PANAM2023C$Denom_Desc <- PANAM2023C$q3cn
PANAM2023C$IntReligC <- PANAM2023C$q5b
PANAM2023C$ConfIntC <- PANAM2023C$it1
PANAM2023C$EscolC <- PANAM2023C$edre
PANAM2023C$Idade <- PANAM2023C$q2
PANAM2023C$Dem <- PANAM2023C$ing4


table(PANAM2023C$q1tc_r)

PANAM2023C <- PANAM2023C %>% 
  mutate(Sexo = case_when(
    q1tc_r == "Hombre/masculino" ~ "Homem",
    q1tc_r == "Mujer/femenino" ~ "Mulher",
    q1tc_r == "No se identifica como hombre ni como mujer"
    ~ "Não se identifica"
  ))
table(PANAM2023C$Sexo)

PANAM2023F <- PANAM2023C[ ,c("Países", "TolerHomo", "Denom_Desc",
                             "IntReligC", "Dem", "ConfIntC",
                             "EscolC", "Idade",
                             "Sexo")]


PANAM2023F <- bind_cols(PANAM2023N, PANAM2023F)

#14PARAGUAI####

PARAG

#País
PARAG$pais
# Labels:
#   value     label
#  12 Paraguay

#Tolerância
PARAG$d5
# Labels:
#   value                 label
# 1 Desaprueba firmemente
# 10    Aprueba firmemente
# NA(a)               No sabe
# NA(b)           No responde

table (PARAG$d5)
#   1   2   3   4   5   6   7   8   9  10 
#535 115  96  84 164  83  92  87  56 153 

#Denominação religiosa 
PARAG$q3cn


# 
# Labels:
#   value                                                                  label
# value                                                                  label
# 1                                                               Católico
# 2       Protestante, Protestante Tradicional o Protestante no Evangélico
# 3                                    Religiones Orientales no Cristianas
# 4 Ninguna (Cree en un Ser Superior pero no pertenece a ninguna religión)
# 5                                               Evangélica y Pentecostal
# 7                                               Religiones Tradicionales
# 11                                     Agnóstico o ateo (no cree en Dios)
# 77                                                                   Otro
# NA(a)                                                                No sabe
# NA(b)                                                            No responde

#Ativismo religioso
PARAG$Q5a
#não tem 

#Intensidade Religiosa 
PARAG$q5b
# Labels:
#   value           label
# 1  Muy importante
# 2 Algo importante
# 3 Poco importante
# 4 Nada importante
# NA(a)         No sabe
# NA(b)     No responde


#Condicionantes
#Adesão a democracia (churPARAGliana)
PARAG$ing4
# 
# Labels:
#   value             label
# 1 Muy en desacuerdo
# 7    Muy de acuerdo
# NA(a)           No sabe
# NA(b)       No responde

#Confiança
PARAG$it1
# Labels:
#   value          label
# 1  Muy confiable
# 2 Algo confiable
# 3 Poco confiable
# 4 Nada confiable
# NA(a)        No sabe
# NA(b)    No responde

#Escolaridade
PARAG$edre
# Labels:
#   value                                              label
# 0                                             Nenhum
# 1           Primário/Ensino Fundamental (incompleto)
# 2             Primário/Ensino Fundamental (completo)
# 3      Secundário/Ensino Médio/colegial (incompleto)
# 4        Secundário/Ensino Médio/colegial (completo)
# 5 Ensino Superior/Bacharelado/Faculdade (incompleto)
# 6   Ensino Superior/Bacharelado/Faculdade (completo)
# NA(a)                                           Não sabe
# NA(b)                                       Não responde

#Idade
PARAG$q2
#[1] "Idade"

#Sexo
PARAG$q1tc_r
# value                                             label
# 1                                   Homem/masculino
# 2                                   Mulher/feminino
# 3 Não se identifica nem como homem, nem como mulher
# NA(a)                                          Não sabe
# NA(b)                                      Não responde

PARAG2023N <- PARAG[ ,c("pais", "d5", "q3cn", "q5b",
                       "ing4", "it1", "edre","q2", "q1tc_r")]

#____________
###Abrindo  a base de dados em dta e convertendo em 
#labels para poder juntar sem precisar ficar recategorizando
#no final
PARAG2023 <- read.dta13("PARAG.dta",
                        convert.factors = TRUE)

PARAG2023C <- PARAG2023[ ,c("pais", "d5", "q3cn", "q5b",
                            "ing4", "it1", "edre","q2", "q1tc_r")]

PARAG2023C$Países <- PARAG2023C$pais
PARAG2023C$TolerHomo <- PARAG2023C$d5
PARAG2023C$Denom_Desc <- PARAG2023C$q3cn
PARAG2023C$IntReligC <- PARAG2023C$q5b
PARAG2023C$ConfIntC <- PARAG2023C$it1
PARAG2023C$EscolC <- PARAG2023C$edre
PARAG2023C$Idade <- PARAG2023C$q2
PARAG2023C$Dem <- PARAG2023C$ing4


table(PARAG2023C$q1tc_r)

PARAG2023C <- PARAG2023C %>% 
  mutate(Sexo = case_when(
    q1tc_r == "Hombre/masculino" ~ "Homem",
    q1tc_r == "Mujer/femenino" ~ "Mulher",
    q1tc_r == "No se identifica como hombre ni como mujer"
    ~ "Não se identifica"
  ))
table(PARAG2023C$Sexo)

PARAG2023F <- PARAG2023C[ ,c("Países", "TolerHomo", "Denom_Desc",
                             "IntReligC", "Dem", "ConfIntC",
                             "EscolC", "Idade",
                             "Sexo")]


PARAG2023F <- bind_cols(PARAG2023N, PARAG2023F)


#15PERU####

PERU

#País

PERU$pais
# Labels:
#   value     label
# 11  Perú

#Tolerância
PERU$d5
# Labels:
#   value                 label
# 1 Desaprueba firmemente
# 10    Aprueba firmemente
# NA(a)               No sabe
# NA(b)           No responde

table (PERU$d5)
#   1   2   3   4   5   6   7   8   9  10 
#348 109  96 119 208 106 133 135  64 202 

#Denominação religiosa 
PERU$q3cn


# 
# Labels:
#   value                                                                  label
# value                                                                  label
# 1                                                               Católico
# 2       Protestante, Protestante Tradicional o Protestante no Evangélico
# 3                                    Religiones Orientales no Cristianas
# 4 Ninguna (Cree en un Ser Superior pero no pertenece a ninguna religión)
# 5                                               Evangélica y Pentecostal
# 7                                               Religiones Tradicionales
# 11                                     Agnóstico o ateo (no cree en Dios)
# 77                                                                   Otro
# NA(a)                                                                No sabe
# NA(b)                                                            No responde

#Ativismo religioso
PERU$Q5a
#não tem 

#Intensidade Religiosa 
PERU$q5b
# Labels:
#   value           label
# 1  Muy importante
# 2 Algo importante
# 3 Poco importante
# 4 Nada importante
# NA(a)         No sabe
# NA(b)     No responde


#Condicionantes
#Adesão a democracia (churPERUliana)
PERU$ing4
# 
# Labels:
#   value             label
# 1 Muy en desacuerdo
# 7    Muy de acuerdo
# NA(a)           No sabe
# NA(b)       No responde

#Confiança
PERU$it1
# Labels:
#   value          label
# 1  Muy confiable
# 2 Algo confiable
# 3 Poco confiable
# 4 Nada confiable
# NA(a)        No sabe
# NA(b)    No responde

#Escolaridade
PERU$edre
# Labels:
#   value                                              label
# 0                                             Nenhum
# 1           Primário/Ensino Fundamental (incompleto)
# 2             Primário/Ensino Fundamental (completo)
# 3      Secundário/Ensino Médio/colegial (incompleto)
# 4        Secundário/Ensino Médio/colegial (completo)
# 5 Ensino Superior/Bacharelado/Faculdade (incompleto)
# 6   Ensino Superior/Bacharelado/Faculdade (completo)
# NA(a)                                           Não sabe
# NA(b)                                       Não responde

#Idade
PERU$q2
#[1] "Idade"

#Sexo
PERU$q1tc_r
# value                                             label
# 1                                   Homem/masculino
# 2                                   Mulher/feminino
# 3 Não se identifica nem como homem, nem como mulher
# NA(a)                                          Não sabe
# NA(b)                                      Não responde


PERU2023N <- PERU[ ,c("pais", "d5", "q3cn", "q5b",
                       "ing4", "it1", "edre","q2", "q1tc_r")]

#____________
###Abrindo  a base de dados em dta e convertendo em 
#labels para poder juntar sem precisar ficar recategorizando
#no final
PERU2023 <- read.dta13("PERU.dta",
                       convert.factors = TRUE)

PERU2023C <- PERU2023[ ,c("pais", "d5", "q3cn", "q5b",
                          "ing4", "it1", "edre","q2", "q1tc_r")]

PERU2023C$Países <- PERU2023C$pais
PERU2023C$TolerHomo <- PERU2023C$d5
PERU2023C$Denom_Desc <- PERU2023C$q3cn
PERU2023C$IntReligC <- PERU2023C$q5b
PERU2023C$ConfIntC <- PERU2023C$it1
PERU2023C$EscolC <- PERU2023C$edre
PERU2023C$Idade <- PERU2023C$q2
PERU2023C$Dem <- PERU2023C$ing4


table(PERU2023C$q1tc_r)

PERU2023C <- PERU2023C %>% 
  mutate(Sexo = case_when(
    q1tc_r == "Hombre/masculino" ~ "Homem",
    q1tc_r == "Mujer/femenino" ~ "Mulher",
    q1tc_r == "No se identifica como hombre ni como mujer"
    ~ "Não se identifica"
  ))
table(PERU2023C$Sexo)

PERU2023F <- PERU2023C[ ,c("Países", "TolerHomo", "Denom_Desc",
                           "IntReligC", "Dem", "ConfIntC",
                           "EscolC", "Idade",
                           "Sexo")]


PERU2023F <- bind_cols(PERU2023N, PERU2023F)

#16REPÚBLICA DOMINICANA####

REPDOM

#País

REPDOM$pais
# Labels:
#   value     label
#21 República Dominicana

#Tolerância
REPDOM$d5
# Labels:
#   value                 label
# 1 Desaprueba firmemente
# 10    Aprueba firmemente
# NA(a)               No sabe
# NA(b)           No responde

table (REPDOM$d5)
#   1   2   3   4   5   6   7   8   9  10 
#682 100  72  58 161  66  72  66  58 232 

#Denominação religiosa 
REPDOM$q3cn


# 
# Labels:
#   value                                                                  label
# value                                                                  label
# 1                                                               Católico
# 2       Protestante, Protestante Tradicional o Protestante no Evangélico
# 3                                    Religiones Orientales no Cristianas
# 4 Ninguna (Cree en un Ser Superior pero no pertenece a ninguna religión)
# 5                                               Evangélica y Pentecostal
# 7                                               Religiones Tradicionales
# 11                                     Agnóstico o ateo (no cree en Dios)
# 77                                                                   Otro
# NA(a)                                                                No sabe
# NA(b)                                                            No responde

#Ativismo religioso
REPDOM$Q5a
#não tem 

#Intensidade Religiosa 
REPDOM$q5b
# Labels:
#   value           label
# 1  Muy importante
# 2 Algo importante
# 3 Poco importante
# 4 Nada importante
# NA(a)         No sabe
# NA(b)     No responde


#Condicionantes
#Adesão a democracia (churREPDOMliana)
REPDOM$ing4
# 
# Labels:
#   value             label
# 1 Muy en desacuerdo
# 7    Muy de acuerdo
# NA(a)           No sabe
# NA(b)       No responde

#Confiança
REPDOM$it1
# Labels:
#   value          label
# 1  Muy confiable
# 2 Algo confiable
# 3 Poco confiable
# 4 Nada confiable
# NA(a)        No sabe
# NA(b)    No responde

#Escolaridade
REPDOM$edre
# Labels:
#   value                                              label
# 0                                             Nenhum
# 1           Primário/Ensino Fundamental (incompleto)
# 2             Primário/Ensino Fundamental (completo)
# 3      Secundário/Ensino Médio/colegial (incompleto)
# 4        Secundário/Ensino Médio/colegial (completo)
# 5 Ensino Superior/Bacharelado/Faculdade (incompleto)
# 6   Ensino Superior/Bacharelado/Faculdade (completo)
# NA(a)                                           Não sabe
# NA(b)                                       Não responde

#Idade
REPDOM$q2
#[1] "Idade"

#Sexo
REPDOM$q1tc_r
# value                                             label
# 1                                   Homem/masculino
# 2                                   Mulher/feminino
# 3 Não se identifica nem como homem, nem como mulher
# NA(a)                                          Não sabe
# NA(b)                                      Não responde

REPDOM2023N <- REPDOM[ ,c("pais", "d5", "q3cn", "q5b",
                     "ing4", "it1", "edre","q2", "q1tc_r")]


#____________
###Abrindo  a base de dados em dta e convertendo em 
#labels para poder juntar sem precisar ficar recategorizando
#no final
REPDOM2023 <- read.dta13("REPDOM.dta",
                         convert.factors = TRUE)

REPDOM2023C <- REPDOM2023[ ,c("pais", "d5", "q3cn", "q5b",
                              "ing4", "it1", "edre","q2", "q1tc_r")]

REPDOM2023C$Países <- REPDOM2023C$pais
REPDOM2023C$TolerHomo <- REPDOM2023C$d5
REPDOM2023C$Denom_Desc <- REPDOM2023C$q3cn
REPDOM2023C$IntReligC <- REPDOM2023C$q5b
REPDOM2023C$ConfIntC <- REPDOM2023C$it1
REPDOM2023C$EscolC <- REPDOM2023C$edre
REPDOM2023C$Idade <- REPDOM2023C$q2
REPDOM2023C$Dem <- REPDOM2023C$ing4


table(REPDOM2023C$q1tc_r)

REPDOM2023C <- REPDOM2023C %>% 
  mutate(Sexo = case_when(
    q1tc_r == "Hombre/masculino" ~ "Homem",
    q1tc_r == "Mujer/femenino" ~ "Mulher",
    q1tc_r == "No se identifica como hombre ni como mujer"
    ~ "Não se identifica"
  ))
table(REPDOM2023C$Sexo)

REPDOM2023F <- REPDOM2023C[ ,c("Países", "TolerHomo", "Denom_Desc",
                               "IntReligC", "Dem", "ConfIntC",
                               "EscolC", "Idade",
                               "Sexo")]


REPDOM2023F <- bind_cols(REPDOM2023N, REPDOM2023F)

#17URUGUAI####

URUG

#País

URUG$pais
# Labels:
#   value     label
#14 Uruguay

#Tolerância
URUG$d5
# Labels:
#   value                 label
# 1 Desaprueba firmemente
# 10    Aprueba firmemente
# NA(a)               No sabe
# NA(b)           No responde

table (URUG$d5)
#   1   2   3   4   5   6   7   8   9  10 
#105  32  38  27  99  83 104 128 134 748

#Denominação religiosa 
URUG$q3cn


# 
# Labels:
#   value                                                                  label
# value                                                                  label
# 1                                                               Católico
# 2       Protestante, Protestante Tradicional o Protestante no Evangélico
# 3                                    Religiones Orientales no Cristianas
# 4 Ninguna (Cree en un Ser Superior pero no pertenece a ninguna religión)
# 5                                               Evangélica y Pentecostal
# 7                                               Religiones Tradicionales
# 11                                     Agnóstico o ateo (no cree en Dios)
# 77                                                                   Otro
# NA(a)                                                                No sabe
# NA(b)                                                            No responde

#Ativismo religioso
URUG$Q5a
#não tem 

#Intensidade Religiosa 
URUG$q5b
# Labels:
#   value           label
# 1  Muy importante
# 2 Algo importante
# 3 Poco importante
# 4 Nada importante
# NA(a)         No sabe
# NA(b)     No responde


#Condicionantes
#Adesão a democracia (churURUGliana)
URUG$ing4
# 
# Labels:
#   value             label
# 1 Muy en desacuerdo
# 7    Muy de acuerdo
# NA(a)           No sabe
# NA(b)       No responde

#Confiança
URUG$it1
# Labels:
#   value          label
# 1  Muy confiable
# 2 Algo confiable
# 3 Poco confiable
# 4 Nada confiable
# NA(a)        No sabe
# NA(b)    No responde

#Escolaridade
URUG$edre
# Labels:
#   value                                              label
# 0                                             Nenhum
# 1           Primário/Ensino Fundamental (incompleto)
# 2             Primário/Ensino Fundamental (completo)
# 3      Secundário/Ensino Médio/colegial (incompleto)
# 4        Secundário/Ensino Médio/colegial (completo)
# 5 Ensino Superior/Bacharelado/Faculdade (incompleto)
# 6   Ensino Superior/Bacharelado/Faculdade (completo)
# NA(a)                                           Não sabe
# NA(b)                                       Não responde

#Idade
URUG$q2
#[1] "Idade"

#Sexo
URUG$q1tc_r
# value                                             label
# 1                                   Homem/masculino
# 2                                   Mulher/feminino
# 3 Não se identifica nem como homem, nem como mulher
# NA(a)                                          Não sabe
# NA(b)                                      Não responde

URUG2023N <- URUG[ ,c("pais", "d5", "q3cn", "q5b",
                         "ing4", "it1", "edre","q2", "q1tc_r")]

#____________
###Abrindo  a base de dados em dta e convertendo em 
#labels para poder juntar sem precisar ficar recategorizando
#no final
URUG2023 <- read.dta13("URUG.dta",
                       convert.factors = TRUE)

URUG2023C <- URUG2023[ ,c("pais", "d5", "q3cn", "q5b",
                          "ing4", "it1", "edre","q2", "q1tc_r")]

URUG2023C$Países <- URUG2023C$pais
URUG2023C$TolerHomo <- URUG2023C$d5
URUG2023C$Denom_Desc <- URUG2023C$q3cn
URUG2023C$IntReligC <- URUG2023C$q5b
URUG2023C$ConfIntC <- URUG2023C$it1
URUG2023C$EscolC <- URUG2023C$edre
URUG2023C$Idade <- URUG2023C$q2
URUG2023C$Dem <- URUG2023C$ing4


table(URUG2023C$q1tc_r)

URUG2023C <- URUG2023C %>% 
  mutate(Sexo = case_when(
    q1tc_r == "Hombre/masculino" ~ "Homem",
    q1tc_r == "Mujer/femenino" ~ "Mulher",
    q1tc_r == "No se identifica como hombre ni como mujer"
    ~ "Não se identifica"
  ))
table(URUG2023C$Sexo)

URUG2023F <- URUG2023C[ ,c("Países", "TolerHomo", "Denom_Desc",
                           "IntReligC", "Dem", "ConfIntC",
                           "EscolC", "Idade",
                           "Sexo")]


URUG2023F <- bind_cols(URUG2023N, URUG2023F)

#18VENEZUELA####

VEN16

#País

VEN16$pais
# Labels:
#   value     label
#16 Venezuela

#Tolerância
VEN16$d5
# Labels:
#   value                 label
# 1 Desaprueba firmemente
# 10    Aprueba firmemente
# NA(a)               No sabe
# NA(b)           No responde

table (VEN16$d5)
#   1   2   3   4   5   6   7   8   9  10 
#413  80  58  70 154  97 109  96  55 378

#Denominação religiosa ##nÃO TEM
VEN16$q3c


# 
# Labels:
#   value                                                                  label
# value                                                                  label
# 1                                                               Católico
# 2       Protestante, Protestante Tradicional o Protestante no Evangélico
# 3                                    Religiones Orientales no Cristianas
# 4 Ninguna (Cree en un Ser Superior pero no pertenece a ninguna religión)
# 5                                               Evangélica y Pentecostal
# 7                                               Religiones Tradicionales
# 11                                     Agnóstico o ateo (no cree en Dios)
# 77                                                                   Otro
# NA(a)                                                                No sabe
# NA(b)                                                            No responde

#Ativismo religioso
VEN16$Q5a
#não tem 

#Intensidade Religiosa 
VEN16$q5b
# Labels:
#   value           label
# 1  Muy importante
# 2 Algo importante
# 3 Poco importante
# 4 Nada importante
# NA(a)         No sabe
# NA(b)     No responde


#Condicionantes
#Adesão a democracia (churVEN16liana)
VEN16$ing4
# 
# Labels:
#   value             label
# 1 Muy en desacuerdo
# 7    Muy de acuerdo
# NA(a)           No sabe
# NA(b)       No responde

#Confiança
VEN16$it1
# Labels:
#   value          label
# 1  Muy confiable
# 2 Algo confiable
# 3 Poco confiable
# 4 Nada confiable
# NA(a)        No sabe
# NA(b)    No responde

#Escolaridade
VEN16$ed
# Labels:
#   value                                              label
# 0                                             Nenhum
# 1           Primário/Ensino Fundamental (incompleto)
# 2             Primário/Ensino Fundamental (completo)
# 3      Secundário/Ensino Médio/colegial (incompleto)
# 4        Secundário/Ensino Médio/colegial (completo)
# 5 Ensino Superior/Bacharelado/Faculdade (incompleto)
# 6   Ensino Superior/Bacharelado/Faculdade (completo)
# NA(a)                                           Não sabe
# NA(b)                                       Não responde

#Idade
VEN16$q2
#[1] "Idade"

#Sexo
VEN16$q1
# value                                             label
# 1                                   Homem/masculino
# 2                                   Mulher/feminino
# 3 Não se identifica nem como homem, nem como mulher
# NA(a)                                          Não sabe
# NA(b)                                      Não responde


VEN16.2N <- VEN16[ ,c("pais", "d5", "q3c", "q5b",
                     "ing4", "it1", "ed","q2", "q1")]

#____________
###Abrindo  a base de dados em dta e convertendo em 
#labels para poder juntar sem precisar ficar recategorizando
#no final
VEN162023 <- read.dta13("VEN16.dta",
                        convert.factors = TRUE)

VEN162023C <- VEN162023[ ,c("pais", "d5", "q3c", "q5b",
                            "ing4", "it1", "ed","q2", "q1")]

# URUG2023C <- URUG2023[ ,c("pais", "d5", "q3cn", "q5b",
#                           "ing4", "it1", "edre","q2", "q1tc_r")]

VEN162023C$Países <- VEN162023C$pais
VEN162023C$TolerHomo <- VEN162023C$d5
VEN162023C$Denom_Desc <- VEN162023C$q3c
VEN162023C$IntReligC <- VEN162023C$q5b
VEN162023C$ConfIntC <- VEN162023C$it1
VEN162023C$EscolC <- VEN162023C$ed
VEN162023C$Idade <- VEN162023C$q2
VEN162023C$Dem <- VEN162023C$ing4
VEN162023C$edre <- VEN162023C$ed

table(VEN162023C$q1)



VEN162023C <- VEN162023C %>% 
  mutate(Sexo = case_when(
    q1 == "Male" ~ "Homem",
    q1 == "Female" ~ "Mulher"
  ))
table(VEN162023C$Sexo)

VEN162023F <- VEN162023C[ ,c("Países", "TolerHomo", "Denom_Desc",
                             "IntReligC", "Dem", "ConfIntC",
                             "EscolC", "Idade",
                             "Sexo", "edre" )]

table(VEN162023F$q3cn)
table(URUG2023F$q3cn)
table(URUG2023F$Denom_Desc)

VEN162023F$EscolC <- as.factor(VEN162023F$EscolC)

VEN162023F <- bind_cols(VEN16.2N, VEN162023F)

summary(AMERLATINA$Denom_Desc)
summary(AMERLATINA$EscolC)
summary(AMERLATINA$edre)

table(AMERLATINA$q1tc_r)
summary(AMERLATINA$q1tc_r)


#MERGE####
library(dplyr)
#Só tenho que resolver a questão do haiti e da 
#Nicaragua #acho que usarei a versão anterior mesmo
#Fazer o q 

AMERLATINA <- bind_rows (ARG2023F, BOL2023F, BRA2023F,
                         CHILE2023F, COSTARICA2023F, 
                         ELSAL2023F, ECUA2023F, 
                         GUAT2023F, HOND2023F, MEX2023F,
                         PANAM2023F, PARAG2023F, PERU2023F,
                         REPDOM2023F, URUG2023F)

save(AMERLATINA, file = "AMERLATINA.RData")

AMERLATINA1 <- bind_rows (ARG2023N, BOL2023N, BRA2023N,
                         CHILE2023N, COSTARICA2023N, 
                         ELSAL2023N, ECUA2023N, 
                         GUAT2023N, HOND2023N, MEX2023N,
                         PANAM2023N, PARAG2023N, PERU2023N,
                         REPDOM2023N, URUG2023N,
                         VEN16.2N)

save(AMERLATINA1, file = "AMERLATINA1.RData")



colnames(AMERLATINA)
# [1] "pais"ok     "d5"ok     "q3cn"ok       "q5b"ok        "ing4"ok       "it1"ok       
# [7] "edre"ok     "q2"ok         "q1tc_r"ok     "Países"ok     "TolerHomo"ok  "Denom_Desc"ok
# [13] "IntReligC"ok  "Dem"ok     ConfIntC"ok   "EscolC"ok     "Idade"ok      "Sexo"ok  

colnames(VEN162023F)
# [1] "pais"       "d5"         "q3c"        "q5b"        "ing4"       "it1"        
#"ed"        # [8] "q2"         "q1"         "Países"     "TolerHomo"  "Denom_Desc" 
#"IntReligC"  "Dem"     # [15] "ConfIntC"   "EscolC"     "Idade"      "Sexo"       "edre"      

table(VEN162023F$Denom_Desc)
table(AMERLATINA$Denom_Desc)

# Definir a recategorização
VEN162023F <- VEN162023F %>%
  mutate(Denom_Desc = case_when(
    Denom_Desc == "Catholic" ~ "Católico",
    Denom_Desc == "Protestant, Mainline Protestant or Protestant non-Evangelical" ~ "Protestante, Protestante Tradicional o Protestante no Evangélico",
    Denom_Desc == "Non-Christian Eastern Religion" ~ "Religiones Orientales no Cristianas",
    Denom_Desc == "None (Believes in a Supreme Entity but does not belong to any religion)" ~ "Ninguna (Cree en un Ser Superior pero no pertenece a ninguna religión)",
    Denom_Desc == "Evangelical and Protestant" ~ "Evangélica y Pentecostal",
    Denom_Desc == "Church of Latter Day Saints (Mormon)" | Denom_Desc == "Jehovah's Witness" ~ "Outra",
    Denom_Desc == "Traditional or Native Religion" ~ "Religiones Tradicionales",
    Denom_Desc == "Agnostic or Atheist (Does not believe in God)" ~ "Agnóstico o ateo (no cree en Dios)",
    Denom_Desc == "Other" ~ "Otro",
    TRUE ~ Denom_Desc  # Manter os valores não especificados
  ))


table(VEN162023F$IntReligC)
table(AMERLATINA$IntReligC)

VEN162023F <- VEN162023F %>%
  mutate(IntReligC = case_when(
    IntReligC == "Very Important" ~ "Muy importante",
    IntReligC == "Somewhat Important" ~ "Algo importante",
    IntReligC == "Not Very Important" ~ "Poco importante",
    IntReligC == "Not Important at All" ~ "Nada importante",
    IntReligC == "Don't Know" | IntReligC == "No Response" ~ "No sabe",
    TRUE ~ IntReligC  # Manter os valores não especificados
  ))


table(VEN162023F$ConfIntC)
table(AMERLATINA$ConfIntC)



VEN162023F <- VEN162023F %>%
  mutate(ConfIntC = case_when(
    ConfIntC == "Very Trustworthy" ~ "Muy confiable",
    ConfIntC == "Somewhat Trustworthy" ~ "Algo confiable",
    ConfIntC == "Not Very Trustworthy" ~ "Poco confiable",
    ConfIntC == "Untrustworthy" ~ "Nada confiable",
    ConfIntC == "Don't Know" | ConfIntC == "No Response" ~ "No sabe",
    TRUE ~ ConfIntC  # Manter os valores não especificados
  ))
#by= c("IDFuncionario" = "IDDepartamento"))

colnames(AMERLATINA)

AMERLATINA2 <- full_join(AMERLATINA, 
                        VEN162023F, 
                        by= c("pais"="pais", 
                              "d5"="d5",
                              "q3cn"="q3c",
                              "ing4"="ing4",
                              "it1"="it1",
                              "edre"="edre",
                              "q2"="q2",
                              "q1tc_r"="q1",
                              "Países"="Países",
                              "TolerHomo"="TolerHomo",
                              "Denom_Desc"="Denom_Desc",
                              "IntReligC"="IntReligC",
                              "Dem"="Dem",
                              "ConfIntC"="ConfIntC",
                              "EscolC"="EscolC",
                              "Idade"="Idade",
                              "Sexo"="Sexo")) 

#Retirar as variáveis que não me interessam 

AMERLATINA <- AMERLATINA2[ , -c(18,19)]


save(AMERLATINA, file = "AMERLATINA.RData")

#RECATEGORIZAÇÕES####

colnames(AMERLATINA)
# [1] "pais"       "d5"         "q3cn"       "ing4"       "it1"       
# [6] "edre"       "q2"         "q1tc_r"     "Países"     "TolerHomo" 
# [11] "Denom_Desc" "IntReligC"  "Dem"        "ConfIntC"   "EscolC"    
# [16] "Idade"      "Sexo"   

table(AMERLATINA$pais)
# 1    2    3    4    6    7    9   10   11   12   13   14   15   16 
# 1622 1556 1516 1602 1527 1532 1604 1706 1535 1524 1653 1517 1526 1558 
# 17   21   22 
# 1540 1596 2221 
table(AMERLATINA$Países)
# 
# Argentina              Bolivia               Brasil 
# 1540                 1706                 1526 
# Chile           Costa Rica          El Salvador 
# 1653                 1527                 1516 
# Ecuador            Guatemala             Honduras 
# 1604                 1556                 1602 
# México               Panamá             Paraguay 
# 1622                 1532                 1524 
# Perú República Dominicana              Uruguay 
# 1535                 1596                 1517 
# Venezuela                Haiti 
# 1558                 2221 

#Tolerhomo####
table(AMERLATINA$TolerHomo)
# 1    2    3    4    5    6    7    8    9   10 
# 7662 1589 1331 1345 2717 1599 1751 1943 1295 5471 


summary(AMERLATINA$TolerHomo)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#   1.000   1.000   5.000   5.151   9.000  10.000     632 


summary(AMERLATINA$TolerHomo)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#   1.000   1.000   5.000   5.151   9.000  10.000     632

table(AMERLATINA$Denom_Desc)

#Denominação####

# Recodificar a variável Denom_Desc
AMERLATINA <- AMERLATINA %>% 
  mutate(Denom_Desc = mapeamento_religioes[Denom_Desc])

# Visualizar as novas contagens
table(AMERLATINA$Denom_Desc)


# Criar dicionário de mapeamento com as novas categorias em português
mapeamento_religioes <- c(
  "Agnostic or Atheist" = "Agnóstico ou Ateu",
  "Catholic" = "Católico",
  "Evangelical Pentecostal" = "Evangélico Pentecostal",
  "Evangelical Protestant" = "Evangélico Protestante",
  "Jehovah's Witness" = "Testemunha de Jeová",
  "Jewish" = "Judaísmo",
  "Latter Day Saints" = "Igreja de Jesus Cristo dos Santos dos Últimos Dias",
  "Mainline Protestant" = "Protestante Tradicional",
  "Non-Christian Eastern" = "Não Cristão Oriental",
  "None" = "Nenhum",
  "Other" = "Outro",
  "Spiritist" = "Espírita",
  "Traditional or Native" = "Tradicional ou Nativo",
  "Traditional or Non-Pentecostal Protestant" = "Protestante Tradicional ou Não Pentecostal"
)

# Recodificar a variável Denom_Desc
AMERLATINA <- AMERLATINA %>% 
  mutate(Denom_Desc = mapeamento_religioes[Denom_Desc])

# Visualizar as novas contagens
table(AMERLATINA$Denom_Desc)


mapeamento_religioes <- c(
  "Agnóstico ou Ateu" = "Agnóstico/Ateu/Nenhum",
  "Nenhum" = "Agnóstico/Ateu/Nenhum",
  "Igreja de Jesus Cristo dos Santos dos Últimos Dias" = "Outras denominações",
  "Judaísmo" = "Outras denominações",
  "Não Cristão Oriental" = "Outras denominações",
  "Testemunha de Jeová" = "Outras denominações",
  "Tradicional ou Nativo" = "Outras denominações",
  "Outro" = "Outras denominações"
)

# Recodificar a variável Denom_Desc
AMERLATINA <- AMERLATINA %>% 
  mutate(Denom_Desc = ifelse(Denom_Desc %in% names(mapeamento_religioes), 
                             mapeamento_religioes[Denom_Desc], 
                             Denom_Desc))

# Visualizar as novas contagens
table(AMERLATINA$Denom_Desc)




# Criar dicionário de mapeamento
mapeamento_religioes <- c(
  "Protestante Tradicional" = "Protestante Tradicional",
  "Protestante Tradicional ou Não Pentecostal" = "Protestante Tradicional",
  "Espírita" = "Outras denominações"
)

# Recodificar a variável Denom_Desc
AMERLATINA <- AMERLATINA %>% 
  mutate(Denom_Desc = ifelse(Denom_Desc %in% names(mapeamento_religioes), 
                             mapeamento_religioes[Denom_Desc], 
                             Denom_Desc))

# Visualizar as novas contagens
table(AMERLATINA$Denom_Desc)

# Criar dicionário de mapeamento
mapeamento_religioes <- c(
  "Evangélico Protestante" = "Protestante Tradicional",
  "Protestante Tradicional" = "Protestante Tradicional",
  "Protestante Tradicional ou Não Pentecostal" = "Protestante Tradicional"
)

# Recodificar a variável Denom_Desc
AMERLATINA <- AMERLATINA %>% 
  mutate(Denom_Desc = ifelse(Denom_Desc %in% names(mapeamento_religioes), 
                             mapeamento_religioes[Denom_Desc], 
                             Denom_Desc))

# Visualizar as novas contagens
table(AMERLATINA$Denom_Desc)

# Contar quantas observações NA existem em toda a base de dados
na_total <- sum(is.na(AMERLATINA$Denom_Desc))

# Exibir o número total de observações NA
print(na_total)

# Agnóstico/Ateu/Nenhum                Católico  Evangélico Pentecostal     Outras denominações 
# 4491                   14232                    4389                    1563 
# Protestante Tradicional 
# 1903 

# Reorganizar os níveis da variável Denom_Desc
AMERLATINA$Denom_Desc <- factor(AMERLATINA$Denom_Desc, 
                                levels = c("Evangélico Pentecostal", 
                                           "Protestante Tradicional", 
                                           "Católico", 
                                           "Outras denominações", 
                                           "Agnóstico/Ateu/Nenhum"))

# Verificar a nova ordem dos níveis
levels(AMERLATINA$Denom_Desc)



#Intesidade religiosa####


# Recodificar a variável
AMERLATINA <- AMERLATINA %>%
  mutate(InteRelig = case_when(
    InteRelig == "Algo importante" ~ "Um Pouco Importante",
    InteRelig == "Mais ou menos importante" ~ "Não Muito Importante",
    InteRelig == "Muito importante" ~ "Muito Importante",
    InteRelig == "Muy importante" ~ "Muito Importante",
    InteRelig == "Nada importante" ~ "Nada Importante",
    InteRelig == "Not Important at All" ~ "Nada Importante",
    InteRelig == "Not Very Important" ~ "Não Muito Importante",
    InteRelig == "Poco importante" ~ "Não Muito Importante",
    InteRelig == "Pouco importante" ~ "Não Muito Importante",
    InteRelig == "Somewhat Important" ~ "Um Pouco Importante",
    InteRelig == "Very Important" ~ "Muito Importante",
    TRUE ~ NA_character_  # Se nenhuma das condições acima for atendida
  ))

# Verificar a distribuição da variável recodificada
table(AMERLATINA$InteRelig)


# Reorganizar os níveis da variável Denom_Desc
AMERLATINA$InteRelig <- factor(AMERLATINA$InteRelig, 
                                levels = c("Muito Importante", 
                                           "Um Pouco Importante", 
                                           "Não Muito Importante",
                                           "Nada Importante"))

# Criar um vetor de contagens das categorias originais
counts <- c(
  "Nada Importante" = 1,
  "Não Muito Importante" = 2,
  "Um Pouco Importante" = 3,
  "Muito Importante" = 4
)

# Criar uma nova variável numérica baseada nas contagens das categorias originais
AMERLATINA <- AMERLATINA %>%
  mutate(
    InteRelig_Num = match(AMERLATINA$InteRelig, names(counts))
  )

# Verificar a nova variável numérica
table(AMERLATINA$InteRelig_Num)


colnames(AMERLATINA)
# [1] "pais"ok     "d5"ok     "q3cn"ok       "q5b"ok        "ing4"ok       "it1"ok       
# [7] "edre"ok     "q2"ok         "q1tc_r"ok     "Países"ok     "TolerHomo"ok  "Denom_Desc"ok
# [13] "IntReligC"ok  "Dem"ok     ConfIntC"ok   "EscolC"ok     "Idade"ok      "Sexo"ok  

#Democracia####
table(AMERLATINA$Dem)
# 1    2    3    4    5    6    7 
# 1735 1398 2709 4915 5657 3921 6184 

#Confiança interpessoal####
table(AMERLATINA$ConfIntC)


# Criar dicionário de mapeamento
mapeamento_confianca <- c(
  "Algo confiable" = "Mais ou menos confiável",
  "Algo confiable (ejerovia)" = "Mais ou menos confiável",
  "Mais ou menos confiáveis" = "Mais ou menos confiável",
  "Muito confiáveis" = "Muito confiável",
  "Muy confiable" = "Muito confiável",
  "Muy confiable (ejeroviaiterei)" = "Muito confiável",
  "Nada confiable" = "Nada confiável",
  "Nada confiable (ndaipori ejeroviarã)" = "Nada confiável",
  "Not Very Trustworthy" = "Nada confiável",
  "Poco confiable" = "Pouco confiável",
  "Poco confiable (saʼi ejerovia)" = "Pouco confiável",
  "Pouco confiáveis" = "Pouco confiável",
  "Somewhat Trustworthy" = "Mais ou menos confiável",
  "Untrustworthy" = "Nada confiável",
  "Very Trustworthy" = "Muito confiável"
)

# Recodificar a variável de confiança
AMERLATINA <- AMERLATINA %>% 
  mutate(ConfIntC = factor(mapeamento_confianca[ConfIntC],
                                  levels = c("Muito confiável",
                                             "Mais ou menos confiável",
                                             "Pouco confiável",
                                             "Nada confiável"),
                                  ordered = TRUE))

# Visualizar as novas contagens
table(AMERLATINA$ConfIntC)


#Idade#####
summary(AMERLATINA$Idade)

# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#   16.00   27.00   38.00   40.36   51.00   98.00      18 

#Sexo####
table(AMERLATINA$Sexo)

# Homem            Mulher Não se identifica 
# 13426             13523                32 



#Escolaridade em anos####
summary(AMERLATINA$edre)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#   0.000   3.000   4.000   4.529   5.000  18.000     158 

AMERLATINA$EscolN <- AMERLATINA$edre


table(AMERLATINA$EscolC)




#Escolaridade Cat####
# Converter a coluna para fator
AMERLATINA <- AMERLATINA %>%
  mutate(EscolC = as.factor(EscolC)) %>%
  mutate(EscolC_Categoria = case_when(
    # NS/NR
    EscolC %in% c("Não sabe", "No sabe", "No responde") ~ "NS/NR",
    # Nenhuma
    EscolC %in% c("Nenhum", "Ninguna", "0") ~ "Nenhuma",
    # Ensino Fundamental
    EscolC %in% c("Primaria completa", "Primaria incompleta", 
                  "Primário/Ensino Fundamental (completo)", 
                  "Primário/Ensino Fundamental (incompleto)", "1", "2", "3", "4", "5", "6", "7", "8") ~ "Ensino Fundamental",
    # Ensino Médio
    EscolC %in% c("Secundaria completa", "Secundaria incompleta",
                  "Secundário/Ensino Médio/colegial (completo)",
                  "Secundário/Ensino Médio/colegial (incompleto)", 
                  "Primaria o educación básica incompleta", 
                  "Primaria o educación básica completa",
                  "Secundaria o educación media científica-humanista o técnica incompleta", 
                  "Secundaria o educación media científica-humanista o técnica completa",
                  "Secundaria, ciclo básico o ciclo diversificado incompleta", 
                  "Secundaria, ciclo básico o ciclo diversificado completa", 
                  "Secundaria incompleta (EEB 7° a 9°/E. Media 1° a 2°)",
                  "Secundaria completa (EEB 7° a 9°/E. Media 1° a 3°)","9", "10", "11", "12", "13") ~ "Ensino Médio",
    # Ensino Superior
    EscolC %in% c("Ensino Superior/Bacharelado/Faculdade (completo)", 
                  "Ensino Superior/Bacharelado/Faculdade (incompleto)", 
                  "Terciaria o universitaria completa", "Terciaria o universitaria incompleta", 
                  "Universitaria o superior no universitaria o educación técnica completa", 
                  "Universitaria o superior no universitaria o educación técnica incompleta",
                  "Terciaria, universitaria o técnico superior incompleta",
                  "Terciaria, universitaria o técnico superior completa",
                  "Universitaria o superior o superior no universitaria/técnico incompleta",
                  "Universitaria o superior o superior no universitaria/técnico completa",
                  "Universitaria o superior no universitaria - técnico incompleta", 
                  "Universitaria o superior no universitaria - técnico completa",
                  "Universitaria, superior no universitaria o técnico universitario incompleta",
                  "Universitaria, superior no universitaria o técnico universitario completa", 
                  "Universitaria o técnica superior incompleta", 
                  "Universitaria o técnica superior completa", 
                  "Primaria incompleta (EEB 1° a 5° grado)",
                  "Primaria completa (EEB 1° a 6° grado)", 
                  "Secundaria incompleta (EEB 7° a 9°/E. Media 1° a 2°)",
                  "Secundaria completa (EEB 7° a 9°/E. Media 1° a 3°)", "14", "15", "16", "17", "18") ~ "Ensino Superior",
    # Caso padrão
    TRUE ~ "Outro"
  ))

# Visualizar a tabela resultante
table(AMERLATINA$EscolC_Categoria)



total_respondentes <- sum(complete.cases(AMERLATINA$EscolC))
total_respondentes


#Save####
save(AMERLATINA, file = "AMERLATINA.RData")

##########
###########
#MULTINÍVEL#######



#Base de dados######
AMERLATINA

save(AMERLATINA, file = "AMERLATINABC.RData")

#Pacotes#######
library(sjPlot)
library(multilevel)#ativa o pacote para execução do modelo ANOVA
library(lme4)

#ANOVA############
#TABELA 4
#ModeloNULO
#TolerHomo
# Remover os rótulos das colunas de um dataframe
library(labelled)
# Remover os rótulos das variáveis numéricas e de caractere
attr(AMERLATINA, "labelled_vars") <- NULL

AMERLATINA$pais <- haven::zap_labels(AMERLATINA$pais)

AMERLATINA <- haven::zap_labels(AMERLATINA)

# Verificar se os rótulos foram removidos
str(AMERLATINA)


AMERLATINA$TolerHomo <- as.numeric(AMERLATINA$TolerHomo)

#MOdelo nulo####
Null.Model <- lme(TolerHomo~1, random = ~1|pais, data = AMERLATINA,
                  control = list(opt="optim"), na.action=na.omit)

Null.Model
# Linear mixed-effects model fit by REML
# Data: AMERLATINA 
# Log-restricted-likelihood: -68392.76
# Fixed: TolerHomo ~ 1 
# (Intercept) 
# 5.225549 
# 
# Random effects:
#   Formula: ~1 | pais
# (Intercept) Residual
# StdDev:    1.548364  3.12809
# 
# Number of Observations: 26703
# Number of Groups: 17 



summary(Null.Model)
# Linear mixed-effects model fit by REML
# Data: AMERLATINA 
# AIC      BIC    logLik
# 136791.5 136816.1 -68392.76
# 
# Random effects:
#   Formula: ~1 | pais
# (Intercept) Residual
# StdDev:    1.548364  3.12809
# 
# Fixed effects:  TolerHomo ~ 1 
# Value Std.Error    DF  t-value p-value
# (Intercept) 5.225549 0.3760246 26686 13.89683       0
# 
# Standardized Within-Group Residuals:
#   Min         Q1        Med         Q3        Max 
# -2.2008995 -0.9399520 -0.0504781  0.9006751  2.5931644 
# 
# Number of Observations: 26703
# Number of Groups: 17 


##ICC####

VarCorr(Null.Model)
# 
# > VarCorr(Null.Model)
# pais = pdLogChol(1) 
# Variance StdDev  
# (Intercept) 2.397432 1.548364
# Residual    9.784945 3.128090
AMERLATINA$pais <- as.factor(AMERLATINA$pais)

memisc::Table(AMERLATINA$pais)

desc()


2.397432/(2.397432   +9.784945 ) 
#[1] 0.1967951

##MODELO SemVariação
#####Estamos criando um modelo sem variação de nível 2
Null.gls<-gls(TolerHomo ~1,data=AMERLATINA, control=list(opt="optim"),
              na.action=na.omit)
logLik(Null.gls)*-2

#'log Lik.' 142643.6 (df=2)

logLik(Null.Model)*-2

#'log Lik.' 136785.5 (df=3)

y= 142643.6 - 136785.5 
y
y = 5858.1
#5858.1 é a diferença entre os dois valores, que é significativamente em uma distribuição de 
#qui<-quadrado com 1 grau de liberdade, ou seja, indica significativa variação do intercepto.
anova(Null.gls, Null.Model)

#             Model df      AIC      BIC    logLik   Test  L.Ratio p-value
# Null.gls       1  2 142647.6 142664.0 -71321.80                        
# Null.Model     2  3 136791.5 136816.1 -68392.76 1 vs 2 5858.078  <.0001

##TABELA 5*####
#Modelo de nível 1 
Model1.Tol <- lme(TolerHomo~ Denom_Desc +  InteRelig_Num + Dem + it1 + 
                    EscolN + Idade + Sexo,
                  random = ~1|pais, data = AMERLATINA, na.action=na.omit,
                  control = list(opt="optim"))

colnames(AMERLATINA$InteRelig_Num)
summary(AMERLATINA$it1)



Model1.Tol <- lm(TolerHomo~ Denom_Desc +  InteRelig_Num + Dem + it1 + 
                   EscolN + Idade + Sexo,
                 random = ~1|pais, data = AMERLATINA, na.action=na.omit,
                 control = list(opt="optim"))

tab_model(Model1.Tol, show.ci = F, auto.label =T, 
          show.se = F, collapse.se = F, wrap.labels = 60,
          p.style ="stars")

##rendimento do teste
C <- 1.240673
N <- 1.465626
x= C/N
x
x <- 0.846514
y= 1 - x
y
y = 0.153486

#MERGE COM A BASE MACRO####
AMERLATINA <- merge(AMERLATINA, BancoMacro, by= "pais")
save(AMERLATINA, file = "AMERLATINA.RData")



##TABELA6*####
#modelo completo (Irei utilizar as duas variáveis mais importantes)
Model.Comp1 <-lme(TolerHomo~ Denom_Desc +  InteRelig_Num + Dem + it1 + 
                    EscolN + Idade + Sexo + TNaH + Desemprego, random=~1|pais, 
                  data=AMERLATINA, na.action=na.omit, control=list(opt="optim"))

tab_model(Model.Comp1, show.ci = F, auto.label =T, 
          show.se = F, collapse.se = F, wrap.labels = 60, 
          p.style ="stars")


#AJUSTE DO MODELO####################
#TABELA7####
##DENOMINAÇÃO
Model.1.Aleatorio.Denom<-lme(TolerHomo~ Denom_Desc +  InteRelig_Num + Dem + it1 + 
                               EscolN + Idade + Sexo, random=~Denom_Desc|pais, 
                             data=AMERLATINA, na.action=na.omit, control=list(opt="optim"))

Model.1.Aleatorio.DenomA <-update(Model.1.Aleatorio.Denom,
                                  random=~1|pais)

anova(Model.1.Aleatorio.Denom,Model.1.Aleatorio.DenomA)

#                           Model df      AIC      BIC    logLik   Test  L.Ratio p-value
# Model.1.Aleatorio.Denom      1 28 122000.3 122227.1 -60972.15                        
# Model.1.Aleatorio.DenomA     2 14 122031.3 122144.7 -61001.65 1 vs 2 58.99905  <.0001

##INTENSIDADE RELIGIOSA
Model.3.Aleatorio.IntRelig<-lme(TolerHomo~ Denom_Desc +  InteRelig_Num + Dem + it1 + 
                                  EscolN + Idade + Sexo, random=~InteRelig|pais, 
                                data=AMERLATINA, na.action=na.omit, control=list(opt="optim"))

Model.3.Aleatorio.IntReligA <-update(Model.3.Aleatorio.IntRelig,
                                     random=~1|pais)

anova(Model.3.Aleatorio.IntReligA,Model.3.Aleatorio.IntRelig)

#                             Model df      AIC      BIC    logLik   Test  L.Ratio p-value
# Model.3.Aleatorio.IntReligA     1 14 122031.3 122144.7 -61001.65                        
# Model.3.Aleatorio.IntRelig      2 23 121985.5 122171.8 -60969.76 1 vs 2 63.78838  <.0001

##TABELA8*####
#Interação Denom e TNaH
ModelTol.Denom.TNaH <-lme(TolerHomo~ Denom_Desc + Denom_Desc:TNaH +  
                            InteRelig + Dem + it1 
                          + EscolN + Idade + Sexo + Desemprego,random=~1|pais, 
                          data=AMERLATINA, na.action=na.omit, control=list(opt="optim"))

tab_model(ModelTol.Denom.TNaH, show.ci = F, auto.label =T, 
          show.se = F, collapse.se = F, wrap.labels = 60, 
          p.style ="stars")


#VALORES PREDITOS####
#DATA FREME COM APENAS 4 CASOS,
#Deixamos constante todas as variáveis
#Menos a que estamos a estamos interagindo.

#Interação entre denominação e tolerância nacional aos homossexuais 
PRED.Denom.TNaH <- data.frame(Denom=c("1", "1", "2", "2", "3", "3", "4", "4"),
                              AtRelig=c(0,0,2,2,2,2,2,2),
                              IntRelig=c(0,0,2,2,2,2,2,2),
                              Dem=c(4, 4, 4, 4, 4, 4, 4, 4),
                              ConfInt=c(2, 2, 2, 2, 2, 2, 2, 2),
                              Ed_sup=c(0, 0, 0, 0, 0, 0, 0, 0),
                              Idade=c(40, 40, 40, 40, 40, 40, 40, 40),
                              Sexo=c(0,0,0,0,0,0,0,0),
                              TNaH=c(0, 2,0, 2,0, 2,0, 2),
                              Desemprego= c(11,11,11,11,11,11,11,11))
predict(Model.Comp1,PRED.Denom.TNaH,level=0)

#Gráfico dos valores preditos ####
PRED.Denom.TNaH$TolerHomo <-predict(Model.Comp1,PRED.Denom.TNaH,level=0)

with(PRED.Denom.TNaH,
     interaction.plot(Denom,TNaH,TolerHomo,
                      xlab="Denominação", 
                      ylab="Média de tolerância política", 
                      legend=T, col = "steelblue",
                      main="Interação entre Denominações religiosas 
e Direitos dos Homossexuais (TNaH)")) 
#Linha tracejada é sem direitos, e a sólida é a média de tolerância 
#para todas as religiões nos contextos com direitos 

##TABELA9*####

#medida de tolerância
AMERLATINA$MedToler <- as.numeric(AMERLATINA$MedToler)
AMERLATINA$TolerHomo <- as.numeric(AMERLATINA$TolerHomo)

Model.Comp1 <-lme(TolerHomo~ Denom_Desc  + InteRelig + Dem + it1 +
                    EscolN + Idade + Sexo + TNaH + MedToler +
                    Desemprego, random=~1|pais, 
                  data=AMERLATINA, na.action=na.omit, control=list(opt="optim"))


AMERLATINA$IVE2012 <- as.numeric(AMERLATINA$IVE2012)

#índice de valores emancipatórios
Model.Comp2 <-lme(TolerHomo~ Denom_Desc  + InteRelig + Dem + it1 
                  + EscolN + Idade + Sexo + TNaH + IVE2012 +
                    Desemprego, random=~1|pais, 
                  data=AMERLATINA, na.action=na.omit, control=list(opt="optim"))


#Para visualizar a saída do modelo, com o sjPlot
tab_model(Model.Comp1, Model.Comp2, show.ci = F, auto.label =T, 
          show.se = F, collapse.se = F, wrap.labels = 60, 
          p.style ="stars")

table(AMERLATINA$Denom)
#TABELA10####
#Interação Denom e TNaH
ModelTol.Denom.TNaH <-lme(TolerHomo~ Denom_Desc + Denom_Desc:TNaH  + InteRelig + 
                            Dem + it1  + EscolN + Idade + Sexo + 
                            MedToler + Desemprego,random=~1|pais, 
                          data=AMERLATINA, na.action=na.omit, control=list(opt="optim"))


ModelTol.At.TNaH <-lme(TolerHomo~ Denom_Desc  + TNaH + InteRelig + Dem + it1 
                       + EscolN + Idade + Sexo + Desemprego + MedToler,random=~1|pais, 
                       data=AMERLATINA, na.action=na.omit, control=list(opt="optim"))

ModelTol.Int.TNaH <-lme(TolerHomo~ Denom_Desc  + InteRelig + 
                          InteRelig:TNaH + Dem + it1 
                        + EscolN + Idade + Sexo + Desemprego + 
                          TNaH,random=~1|pais, 
                        data=AMERLATINA, na.action=na.omit, 
                        control=list(opt="optim"))


tab_model(ModelTol.Denom.TNaH, ModelTol.At.TNaH,
          ModelTol.Int.TNaH,
          show.ci = F, auto.label =T, 
          show.se = F, collapse.se = F, wrap.labels = 60, 
          p.style ="stars")



#TABELA11####
#Interação Denom e MedToler
ModelTol.Denom.MedToler <-lme(TolerHomo~ Denom_Desc + Denom_Desc:MedToler  + InteRelig + 
                                Dem + it1  + EscolN + Idade + Sexo + 
                                MedToler + Desemprego,random=~1|pais, 
                              data=AMERLATINA, na.action=na.omit, control=list(opt="optim"))

ModelTol.At.MedToler <-lme(TolerHomo~ Denom_Desc  + MedToler + InteRelig + Dem + it1 
                           + EscolN + Idade + Sexo + Desemprego + MedToler,random=~1|pais, 
                           data=AMERLATINA, na.action=na.omit, control=list(opt="optim"))

ModelTol.Int.MedToler <-lme(TolerHomo~ Denom_Desc  + InteRelig + 
                              InteRelig:MedToler + Dem + it1 
                            + EscolN + Idade + Sexo + Desemprego + 
                              MedToler,random=~1|pais, 
                            data=AMERLATINA, na.action=na.omit, 
                            control=list(opt="optim"))

tab_model(ModelTol.Denom.MedToler,
          ModelTol.At.MedToler,
          ModelTol.Int.MedToler, show.ci = F, auto.label =T, 
          show.se = F, collapse.se = F, wrap.labels = 60, 
          p.style ="stars")


