
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

MENORARGENTINA <- BaseLapopMenor %>%
  filter(Países == "Argentina") 

#
Bancomenor2 <- BancoFinal[ ,c("pais", "TolerHomo", 
                              "Denom", "AtRelig",
                              "IntRelig", "Dem",
                              "ConfInt", "Ed_sup",
                              "Idade", "Sexo",
                              "Países","TNaH",
                              "Desemprego",
                              "Dir_minorias",
                              "Liberdade",
                              "Desp_Ed",
                              "IDE" ,
                              "idnum")]



MAIORARGENTINA <- Bancomenor2 %>%
  filter(Países == "Argentina") 

library(writexl)
writexl::write_xlsx(MAIORARGENTINA, path = "MAIORARGENTINA.xlsx")
writexl::write_xlsx(MENORARGENTINA, path = "MENORARGENTINA.xlsx")
