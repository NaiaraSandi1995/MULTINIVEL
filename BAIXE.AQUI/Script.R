#Análise multinível#######

#Base de dados######
load("BaseLapopMenor.RData")

#Pacotes#######
library(sjPlot)
library(multilevel)#ativa o pacote para execução do modelo ANOVA
library(lme4)

#ANOVA############
#TABELA 5
#ModeloNULO
#TolerHomo

BaseLapopMenor$TolerHomo <- as.numeric(BaseLapopMenor$TolerHomo)

Null.Model <- lme(TolerHomo~1, random = ~1|pais, data = BaseLapopMenor,
                  control = list(opt="optim"), na.action=na.omit)
Null.Model

summary(Null.Model)

##ICC####

VarCorr(Null.Model)

2.142135/(2.142135+9.775700) 

##MODELO SemVariação
#####Estamos criando um modelo sem variação de nível 2
Null.gls<-gls(TolerHomo ~1,data=BaseLapopMenor, control=list(opt="optim"),
              na.action=na.omit)
logLik(Null.gls)*-2

logLik(Null.Model)*-2

y= 141990.4 - 136666.2 
y
y = 5324.2
#5324.2 é a diferença entre os dois valores, que é significativamente em uma distribuição de 
#qui<-quadrado com 1 grau de liberdade, ou seja, indica significativa variação do intercepto.
anova(Null.gls, Null.Model)

##TABELA 5*####
#Modelo de nível 1 
# Model1.Tol <- lme(TolerHomo~ Denom + AtRelig + IntRelig + Dem + ConfInt +
#                     Ed_sup + Idade + Sexo,
#                   random = ~1|pais, data = BaseLapopMenor, na.action=na.omit,
#                   control = list(opt="optim"))

Model1.Tol <- lm(TolerHomo~ Denom + AtRelig + IntRelig + Dem + ConfInt + 
                   Ed_sup + Idade + Sexo,
                 random = ~1|pais, data = BaseLapopMenor, na.action=na.omit,
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


##TABELA6*####
#modelo completo (Irei utilizar as duas variáveis mais importantes)
Model.Comp1 <-lme(TolerHomo~ Denom + AtRelig + IntRelig + Dem + ConfInt 
                  + Ed_sup + Idade + Sexo + TNaH + Desemprego, random=~1|pais, 
                  data=BaseLapopMenor, na.action=na.omit, control=list(opt="optim"))

tab_model(Model.Comp1, show.ci = F, auto.label =T, 
          show.se = F, collapse.se = F, wrap.labels = 60, 
          p.style ="stars")


#AJUSTE DO MODELO####################
#TABELA7####
##DENOMINAÇÃO
 
Model.1.Aleatorio.Denom<-lme(TolerHomo~ Denom + AtRelig + IntRelig + Dem + ConfInt 
                             + Ed_sup + Idade + Sexo, random=~Denom|pais, 
                             data=BaseLapopMenor, na.action=na.omit, control=list(opt="optim"))

Model.1.Aleatorio.DenomA <-update(Model.1.Aleatorio.Denom,
                                  random=~1|pais)

anova(Model.1.Aleatorio.Denom,Model.1.Aleatorio.DenomA)

##ATIVISMO RELIGIOSO
Model.2.Aleatorio.AtRelig<-lme(TolerHomo~ Denom + AtRelig + IntRelig + Dem + ConfInt 
                               + Ed_sup + Idade + Sexo, random=~AtRelig|pais, 
                               data=BaseLapopMenor, na.action=na.omit,
                               control=list(opt="optim"))


Model.2.Aleatorio.AtReligA <-update(Model.2.Aleatorio.AtRelig,
                                    random=~1|pais)

anova(Model.2.Aleatorio.AtReligA,Model.2.Aleatorio.AtRelig)

##INTENSIDADE RELIGIOSA

Model.3.Aleatorio.IntRelig<-lme(TolerHomo~ Denom + AtRelig + IntRelig + Dem + ConfInt 
                                + Ed_sup + Idade + Sexo, random=~IntRelig|pais, 
                                data=BaseLapopMenor, na.action=na.omit, control=list(opt="optim"))

Model.3.Aleatorio.IntReligA <-update(Model.3.Aleatorio.IntRelig,
                                     random=~1|pais)

anova(Model.3.Aleatorio.IntReligA,Model.3.Aleatorio.IntRelig)



##TABELA8*####
#Interação Denom e TNaH
ModelTol.Denom.TNaH <-lme(TolerHomo~ Denom + Denom:TNaH + AtRelig + IntRelig + Dem + ConfInt 
                          + Ed_sup + Idade + Sexo + Desemprego,random=~1|pais, 
                          data=BaseLapopMenor, na.action=na.omit, control=list(opt="optim"))

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
BaseLapopMenor$MedToler <- as.numeric(BaseLapopMenor$MedToler)
BaseLapopMenor$TolerHomo <- as.numeric(BaseLapopMenor$TolerHomo)

Model.Comp1 <-lme(TolerHomo~ Denom + AtRelig + IntRelig + Dem + ConfInt +
                    Ed_sup + Idade + Sexo + TNaH + MedToler +
                    Desemprego, random=~1|pais, 
                  data=BaseLapopMenor, na.action=na.omit, control=list(opt="optim"))


BaseLapopMenor$IVE2012 <- as.numeric(BaseLapopMenor$IVE2012)

#índice de valores emancipatórios
Model.Comp2 <-lme(TolerHomo~ Denom + AtRelig + IntRelig + Dem + ConfInt 
                  + Ed_sup + Idade + Sexo + TNaH + IVE2012 +
                    Desemprego, random=~1|pais, 
                  data=BaseLapopMenor, na.action=na.omit, control=list(opt="optim"))


#Para visualizar a saída do modelo, com o sjPlot
tab_model(Model.Comp1, Model.Comp2, show.ci = F, auto.label =T, 
          show.se = F, collapse.se = F, wrap.labels = 60, 
          p.style ="stars")

table(BaseLapopMenor$Denom)
#TABELA10####
#Interação Denom e TNaH
ModelTol.Denom.TNaH <-lme(TolerHomo~ Denom + Denom:TNaH + AtRelig + IntRelig + 
                            Dem + ConfInt  + Ed_sup + Idade + Sexo + 
                            MedToler + Desemprego,random=~1|pais, 
                          data=BaseLapopMenor, na.action=na.omit, control=list(opt="optim"))


ModelTol.At.TNaH <-lme(TolerHomo~ Denom  + AtRelig + AtRelig:TNaH + IntRelig + Dem + ConfInt 
                       + Ed_sup + Idade + Sexo + Desemprego + MedToler,random=~1|pais, 
                       data=BaseLapopMenor, na.action=na.omit, control=list(opt="optim"))

ModelTol.Int.TNaH <-lme(TolerHomo~ Denom  + AtRelig  + IntRelig + 
                          IntRelig:TNaH + Dem + ConfInt 
                        + Ed_sup + Idade + Sexo + Desemprego + 
                          TNaH,random=~1|pais, 
                        data=BaseLapopMenor, na.action=na.omit, 
                        control=list(opt="optim"))


tab_model(ModelTol.Denom.TNaH, ModelTol.At.TNaH,
          ModelTol.Int.TNaH,
          show.ci = F, auto.label =T, 
          show.se = F, collapse.se = F, wrap.labels = 60, 
          p.style ="stars")



#TABELA11####
#Interação Denom e MedToler
ModelTol.Denom.MedToler <-lme(TolerHomo~ Denom + Denom:MedToler + AtRelig + IntRelig + 
                                Dem + ConfInt  + Ed_sup + Idade + Sexo + 
                                MedToler + Desemprego,random=~1|pais, 
                              data=BaseLapopMenor, na.action=na.omit, control=list(opt="optim"))

ModelTol.At.MedToler <-lme(TolerHomo~ Denom  + AtRelig + AtRelig:MedToler + IntRelig + Dem + ConfInt 
                           + Ed_sup + Idade + Sexo + Desemprego + MedToler,random=~1|pais, 
                           data=BaseLapopMenor, na.action=na.omit, control=list(opt="optim"))

ModelTol.Int.MedToler <-lme(TolerHomo~ Denom  + AtRelig  + IntRelig + 
                              IntRelig:MedToler + Dem + ConfInt 
                            + Ed_sup + Idade + Sexo + Desemprego + 
                              MedToler,random=~1|pais, 
                            data=BaseLapopMenor, na.action=na.omit, 
                            control=list(opt="optim"))

tab_model(ModelTol.Denom.MedToler,
          ModelTol.At.MedToler,
          ModelTol.Int.MedToler, show.ci = F, auto.label =T, 
          show.se = F, collapse.se = F, wrap.labels = 60, 
          p.style ="stars")


