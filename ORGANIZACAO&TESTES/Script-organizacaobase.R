##ESCRIPT DE ORGANIZAÇÃO DA BASE DE DADOS###

BaseLapopMenor <- BancoLAPOPCompleto[ ,c("pais", "TolerHomo", 
                                         "Denom", "AtRelig",
                                         "IntRelig", "Dem",
                                         "ConfInt", "Ed_sup",
                                         "Idade", "Sexo",
                                         "Países", "TNaH",
                                         "TolerHomoP" ,
                                         "Denom_Desc", "Desemprego",
                                         "Dir_minorias","PaisD",
                                         "Denom1",
                                         "Liberdade",
                                         "Desp_Ed",
                                         "IDE" ,
                                         "idnum")]


save(BaseLapopMenor, file = "BaseLapopMenor.RData")

table(BaseLapopMenor$Denom_Desc)

BaseLapopMenor <- merge(BaseLapopMenor, BancoMacro)

BaseLapopMenor <- BaseLapopMenor[ ,-c(11:12)]
                                  
BaseLapopMenor$TNaH <- BaseLapopMenor$TNaH.x

table(BaseLapopMenor$Denom_Desc)                 

BaseLapopMenor$Denom_Desc <- recode(BaseLapopMenor$Denom, "Ateus" <- 1, 
                            "Protestante" <- 2, "Católicos" <- 3, 
                            "Outras" <- 4)

library(memisc) #Para utilizar o string Table
tapply(BaseLapopMenor$TNaH, BaseLapopMenor$pais, Table, percent = T)
#Com essa função nós verificamos a variação da variável Índice de Tolerância Nacional
#a homossexuais, sendo que essa variável está organizada de 0 a 2
#Sendo 0 nenhum direito, 1 direito ao casamento e 2 direito a casamento e adoção.

