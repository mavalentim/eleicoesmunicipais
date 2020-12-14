#carregando bibliotecas
library(readxl)
library(lubridate)
library(stringr)
library(dplyr)
library(ggplot2)
library(plm)
library(tidyr)
library(reshape2)
library(geobr)
library(xlsx)
library(psych)
library(readr)

#ANTES DAS PARTES, TEM-SE ALGUMAS PREPARACOES
ufcod2 <- read_excel("C:/Users/Matheus/Desktop/tcc/excel/ufcod.xlsx")

ufcod2 %>%
  mutate(uf = as.numeric(uf))


mundsei2 <- read_excel("C:/Users/Matheus/Desktop/pedidosai/respostapedidopopulacao/Recurso Demografico 2010-2019.xlsx", 
                      sheet = "MUNICIPIO", col_types = c("text","text", "text"))


mundsei2 <- mundsei2 %>%
  rename(ibge = CO_MUNICIPIO_IBGE, dsei = DSEI_GESTAO)%>%
  mutate(teste = 1)%>%
  group_by(ibge)%>%
  summarize(teste = sum(teste))%>%
  select(-teste)%>%
  mutate(cidadededsei = 1)




#PARTE 1) CRIANDO AS VARIÁVEIS PARA PODER COLAPSAR POR MUNICÍPIO AS CARACTERÍSTICAS RELEVANTES

candi_2016 <- read.csv("C:/Users/Matheus/Desktop/tcc/excel/politica/candidatostudo2016/consulta_cand_2016_BRASIL.csv", sep=";")


politica2016 <- candi_2016 %>%
  mutate(candidato =1)%>%
  
  #SE É INDIGENA
  mutate(indigena = if_else(DS_COR_RACA == 'INDÍGENA',1,0))%>%
  
  # SE FOI ELEITO
  mutate(eleito = if_else(DS_SIT_TOT_TURNO == "ELEITO" | DS_SIT_TOT_TURNO == "ELEITO POR QP" |
                            DS_SIT_TOT_TURNO == "ELEITO POR MÉDIA",1,0))%>%
  # SE FOI IND?GENA E ELEITO
  mutate(ind_eleito = case_when(indigena == 1 & eleito == 1 ~1))%>%
 
   #SE É INDÍNGENA VINCULADO COM A ESQUERDA
  mutate(cand_ind_esq = case_when(SG_PARTIDO == "PT" & indigena == 1| SG_PARTIDO == 'PC do B' & indigena==1 | 
                                    SG_PARTIDO == "PSOL" & indigena ==1 |SG_PARTIDO == "PSL" & indigena ==1  ~ 1))%>%
  #SE É INDÍGENA ELEITO POR UM PARTIDO DE ESQUERDA
  mutate(ind_eleito_esq = case_when(cand_ind_esq == 1 & eleito == 1 ~ 1)) %>%
 
   #SE UM CANDIDATO CONCORREU PARA PREFEITO OU PARA VEREADOR
  mutate(prefeito = if_else(DS_CARGO == 'PREFEITO' | DS_CARGO == 'VICE-PREFEITO',1,0), 
         vereador = if_else(DS_CARGO == 'VEREADOR',1,0))%>%
  
  # SE UM INDÍGENA CONCORREU PARA PREFEITO OU PARA VEREADOR
  mutate(prefeito_ind = case_when(prefeito == 1 & indigena == 1 ~ 1), 
         vereador_ind = case_when( vereador == 1 & indigena == 1 ~ 1 ))%>%
  
  #SE UM INDÍGENA FOI ELEITO CONCORRENDO PARA PREFEITO OU VEREADOR
  mutate(prefeito_ind_el = case_when(prefeito == 1 & ind_eleito == 1 ~ 1), 
         vereador_ind_el = case_when( vereador == 1 & ind_eleito == 1 ~ 1))%>%
  
  replace_na(list(cand_ind_esq = 0, ind_eleito_esq = 0,ind_eleito=0, prefeito_ind = 0, vereador_ind =0,
                  prefeito_ind_el = 0, vereador_ind_el = 0))%>%
  
  select(CD_COR_RACA,DS_COR_RACA, DS_SIT_TOT_TURNO, NR_PARTIDO, SG_PARTIDO, NM_UE, DS_CARGO, SG_UE, SG_UF,
         NM_MUNICIPIO_NASCIMENTO,SG_UF_NASCIMENTO,
         candidato, indigena, eleito, ind_eleito, cand_ind_esq, ind_eleito_esq, prefeito_ind, vereador_ind, 
         prefeito, vereador, prefeito_ind_el, vereador_ind_el)







#PARTE 2) COLOCANDO CODIGO IBGE PARA MERGIR COM AS OUTRAS BASES

codigos_IBGE <- read_excel("C:/Users/Matheus/Desktop/tcc/excel/Codigos IBGE.xlsx")

codigos_IBGE <- codigos_IBGE%>%
  mutate(Municipio = str_to_upper(Municipio))%>%
  mutate(Municipio = str_replace_all(Municipio, "Á", "A" ))%>%
  mutate(Municipio = str_replace_all(Municipio, "É", "E" ))%>%
  mutate(Municipio = str_replace_all(Municipio, "Í", "I" ))%>%
  mutate(Municipio = str_replace_all(Municipio, "Ó", "O" ))%>%
  mutate(Municipio = str_replace_all(Municipio, "Ú", "U" ))%>%
  mutate(Municipio = str_replace_all(Municipio, "Â", "A" ))%>%
  mutate(Municipio = str_replace_all(Municipio, "Ê", "E" ))%>%
  mutate(Municipio = str_replace_all(Municipio, "Î", "I" ))%>%
  mutate(Municipio = str_replace_all(Municipio, "Ô", "O" ))%>%
  mutate(Municipio = str_replace_all(Municipio, "Û", "U" ))%>%
  mutate(Municipio = str_replace_all(Municipio, "Ã", "A" ))%>%
  mutate(Municipio = str_replace_all(Municipio, "Ã", "O" ))%>%
  mutate(Municipio = str_replace_all(Municipio, "Ç", "C" ))%>%
  mutate(mun = str_c(UF,Municipio))



politica2016cibge <- politica2016%>%
  rename(Municipio = NM_UE)%>%
  rename(UF = SG_UF)%>%
  mutate(Municipio = str_to_upper(Municipio))%>%
  mutate(Municipio = str_replace_all(Municipio, "Á", "A" ))%>%
  mutate(Municipio = str_replace_all(Municipio, "É", "E" ))%>%
  mutate(Municipio = str_replace_all(Municipio, "Í", "I" ))%>%
  mutate(Municipio = str_replace_all(Municipio, "Ó", "O" ))%>%
  mutate(Municipio = str_replace_all(Municipio, "Ú", "U" ))%>%
  mutate(Municipio = str_replace_all(Municipio, "Â", "A" ))%>%
  mutate(Municipio = str_replace_all(Municipio, "Ê", "E" ))%>%
  mutate(Municipio = str_replace_all(Municipio, "Î", "I" ))%>%
  mutate(Municipio = str_replace_all(Municipio, "Ô", "O" ))%>%
  mutate(Municipio = str_replace_all(Municipio, "Û", "U" ))%>%
  mutate(Municipio = str_replace_all(Municipio, "Â", "A" ))%>%
  mutate(Municipio = str_replace_all(Municipio, "Ô", "O" ))%>%
  mutate(Municipio = str_replace_all(Municipio, "Ç", "C" ))%>%
  mutate(mun = str_c(UF,Municipio))%>%
  left_join(codigos_IBGE, by = 'mun')%>%
  select(-Municipio.x ,  -Digito)

#PARTE EXTRA: CRIANDO UMA BASE PARA IDENTIFICAR CANDIDATOS QUE VIERAM DE REGI?ES DE ?BITO IND?GENA

politica2016munnasc<- politica2016cibge%>%
  filter(indigena == 1)%>%
  mutate(cind =1)%>%
  select(NM_MUNICIPIO_NASCIMENTO,SG_UF_NASCIMENTO, Codigo2, cind)%>%
  rename(codigo_concorreu =  Codigo2)%>%
  rename(Municipio2 = NM_MUNICIPIO_NASCIMENTO)%>%
  rename(UF2 = SG_UF_NASCIMENTO)%>%
  mutate(Municipio2 = str_to_upper(Municipio2))%>%
  mutate(Municipio2 = str_replace_all(Municipio2, "Á", "A" ))%>%
  mutate(Municipio2 = str_replace_all(Municipio2, "É", "E" ))%>%
  mutate(Municipio2 = str_replace_all(Municipio2, "Í", "I" ))%>%
  mutate(Municipio2 = str_replace_all(Municipio2, "Ó", "O" ))%>%
  mutate(Municipio2 = str_replace_all(Municipio2, "Ú", "U" ))%>%
  mutate(Municipio2 = str_replace_all(Municipio2, "Â", "A" ))%>%
  mutate(Municipio2 = str_replace_all(Municipio2, "Ê", "E" ))%>%
  mutate(Municipio2 = str_replace_all(Municipio2, "Î", "I" ))%>%
  mutate(Municipio2 = str_replace_all(Municipio2, "Ô", "O" ))%>%
  mutate(Municipio2 = str_replace_all(Municipio2, "Û", "U" ))%>%
  mutate(Municipio2 = str_replace_all(Municipio2, "Ã", "A" ))%>%
  mutate(Municipio2 = str_replace_all(Municipio2, "Õ", "O" ))%>%
  mutate(Municipio2 = str_replace_all(Municipio2, "Ç", "C" ))%>%
  mutate(mun2 = str_c(UF2,Municipio2))%>%
  left_join(codigos_IBGE, by = c('mun2' = 'mun'))%>%
  select(  -Digito)%>%
  rename(codigo_nascimento = Codigo2)%>%
  mutate(codigo_nascimento = as.character(codigo_nascimento))%>%
  #fazendo join com a base da sa?de  vendo se o municipio de nascimento ? de DSEI
  left_join(mundsei, by = c('codigo_nascimento' = 'ibge'))%>%
  replace_na(list(cidadededsei =0))%>%
  #sumarizando por municipio para ver quantos candidatos daquele municipio vieram de "DSEI"
  group_by(codigo_concorreu)%>%
  summarize(candidatosdedsei = sum(cidadededsei),
            candidatosindigena = sum(cind))%>%
  mutate(porcent_vindo_dsei = candidatosdedsei/ candidatosindigena,
         codigo_concorreu= as.character(codigo_concorreu))%>%
  select(-candidatosindigena, - candidatosdedsei)


#colapsando a base
poli16 <- politica2016cibge %>%
  group_by(Codigo2)%>%
  summarize(cand_ind = sum(indigena), 
            cand_total = sum(candidato), 
            cand_ind_esq = sum(cand_ind_esq), 
            ind_eleito = sum(ind_eleito), 
            ind_eleito_esq = sum(ind_eleito_esq), 
            eleitos = sum(eleito),
            vereador_ind = sum(vereador_ind), 
            vereador_ind_el = sum(vereador_ind_el), 
            vereador = sum(vereador))%>%
  ungroup()%>%
  #CRIANDO PORCENTAGENS RELEVANTES PARA OS DADOS 
  mutate(prop_cand_ind = (cand_ind/cand_total)*100, 
         prop_ind_esq = (cand_ind_esq/cand_ind)*100,
         prop_ind_eleito = (ind_eleito/eleitos)*100, 
         aproveitamento_ind = (ind_eleito/cand_ind)*100,
         prop_vereador_ind = (vereador_ind/vereador)*100)%>%
  mutate(ano = as.character(2016), ibge = as.character(Codigo2))%>%
  select( -Codigo2, -ano)%>%
  left_join(politica2016munnasc, by = c('ibge' = 'codigo_concorreu'))



#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
#GRÁFICOS E EXPLORACOES NOS DADOS DE 2016
##############################################

candidatosind <- candi_2016%>%
  filter(DS_COR_RACA == 'INDÍGENA')

#criando diferentes bases para analisar diferentes caracteristicas
base1 <- candidatosind%>%
  select(DS_OCUPACAO, DS_SIT_TOT_TURNO, DS_GENERO, ST_REELEICAO, NM_MUNICIPIO_NASCIMENTO, DS_GRAU_INSTRUCAO)

base2<- base1%>%
  count(DS_OCUPACAO)%>%
  arrange(desc(n))
 
base3 <-  base1%>%
  count(DS_GENERO)%>%
  arrange(desc(n))

base4<-  base1%>%
  count(DS_GRAU_INSTRUCAO)%>%
  arrange(desc(n))

base5<- base1%>%
  count(DS_OCUPACAO)%>%
  arrange(desc(n))


base6<- politica2016munnasc%>%
  filter(porcent_vindo_dsei ==0)
#PARA 2020

candidatosind2 <- candi_2020%>%
  filter(DS_COR_RACA == 'INDÍGENA')


base1 <- candidatosind2%>%
  select(DS_OCUPACAO, DS_SIT_TOT_TURNO, DS_GENERO, ST_REELEICAO, NM_MUNICIPIO_NASCIMENTO, DS_GRAU_INSTRUCAO)

base2<- base1%>%
  count(DS_OCUPACAO)%>%
  arrange(desc(n))

base3 <-  base1%>%
  count(DS_GENERO)%>%
  arrange(desc(n))

base4<-  base1%>%
  count(DS_GRAU_INSTRUCAO)%>%
  arrange(desc(n))

base5<- base1%>%
  count(DS_OCUPACAO)%>%
  arrange(desc(n))






estatpoli<-poli16%>%
  summarize(pcand = mean(prop_cand_ind, na.rm = T),
            pcandeleito = mean(prop_ind_eleito, na.rm = T),
            meanacand = median(prop_cand_ind, na.rm = T),
            meanaeleito = median(prop_ind_eleito, na.rm = T),
            sdcand= sd(prop_cand_ind, na.rm = T),
            sdeleito = sd(prop_ind_eleito, na.rm = T))
           
describeBy(poli16) 

mapapolitics<- poli16%>%
  select(ibge, prop_cand_ind, prop_ind_eleito, aproveitamento_ind )

write.xlsx(mapapolitics, file = "C:/Users/Matheus/Desktop/mapapolitics.xlsx")


###############


#%%%%%%%%%%%%%%%%%%%%%%%%%
#GGPLOTS DA PARTICIPACAO INDIGENA POR PARTIDO
#################
dadospolitica16 <- politica2016%>%
  group_by(SG_PARTIDO)%>%
  summarize(totalcandidatos = sum(candidato),
            totalindigenas = sum(indigena))%>%
  mutate(proporcao_indigena16 = (totalindigenas/totalcandidatos)*100,
         ano = '2016')%>%
  select(SG_PARTIDO,proporcao_indigena16)
#alterando o nome do patriota
dadospolitica16[7,1] = "PATRIOTA"




dadospolitica20<- politica2020%>%
  group_by(SG_PARTIDO)%>%
  summarize(totalcandidatos = sum(candidato),
            totalindigenas = sum(indigena))%>%
  mutate(proporcao_indigena20 = (totalindigenas/totalcandidatos)*100,
         ano='2020')%>%
  select(SG_PARTIDO,proporcao_indigena20)


partido<- dadospolitica16%>%
  full_join(dadospolitica20, by='SG_PARTIDO')%>%
  replace_na(list( proporcao_indigena16=0, proporcao_indigena20=0))%>%
  mutate(variacao = proporcao_indigena20 - proporcao_indigena16,
         media= (proporcao_indigena20 + proporcao_indigena16)/2)%>%
  mutate(positiva = if_else(variacao>0,"1","0"))%>%
  arrange(positiva)



ggplot()   +geom_col(data =partido, aes(x=reorder(SG_PARTIDO, variacao), y=variacao, fill=positiva))+
  coord_flip() + 
  labs(x = "Partido", y = "Variação da Proporção de Candidatos Ind?genas (em %)", 
       title = "Varia??o da Proporção de Candidatos Indígenas entre as Eleições de 2016 e 2020, por partido") +
  theme_classic()+ theme(legend.position = "none") +theme(axis.text.x = element_text(angle = 50, hjust = 1))


ggplot()   +geom_col(data =partido, aes(x=reorder(SG_PARTIDO, media), y=media, fill=media))+
  labs(x = "Partido", y = "Média da Proporção de Candidatos Indígenas (em %)", 
       title = "Média da Proporção de Candidatos Indígenas por partido") +
  theme_classic()+theme(axis.text.x = element_text(angle = 50, hjust = 1))




ggplot(dadospolitica16, aes(x=SG_PARTIDO,y=proporcao_indigena, group=1)) + geom_col()  +
  labs(x = "Partido", y = "Proporção de Candidatos Indígenas (em %)", 
       title = "Proporção de Candidatos Indígenas por cada partido - Eleição de 2016") +
  theme_classic()+ theme(axis.text.x = element_text(angle = 50, hjust = 1))

ggplot(dadospolitica20, aes(x=SG_PARTIDO,y=proporcao_indigena, group=1)) + geom_col()  +
  labs(x = "Partido", y = "Proporção de Candidatos Indígenas (em %)", 
       title = "Proporção de Candidatos Indígenas por cada partido - Eleição de 2020") +
  theme_classic()+ theme(axis.text.x = element_text(angle = 50, hjust = 1))



#######
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#BASE PARA O MAPA











#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
#2014

candi2014 <- read.csv(
  "C:/Users/Matheus/Desktop/tcc/excel/politica/candidatostudo2014/consulta_cand_2014_BRASIL.csv", sep=";")

politica14 <- candi2014%>%
  mutate(candidato =1)%>%
  
  mutate(indigena = if_else(DS_COR_RACA == 'INDÍGENA',1,0))%>%
  
  mutate(eleito = if_else(DS_SIT_TOT_TURNO == "ELEITO" | DS_SIT_TOT_TURNO == "ELEITO POR QP" |
                            DS_SIT_TOT_TURNO == "ELEITO POR MÉDIA",1,0))%>%
  
  mutate(ind_eleito = case_when(indigena == 1 & eleito == 1 ~1))%>%
  
  mutate(cand_ind_esq = case_when(SG_PARTIDO == "PT" & indigena == 1| SG_PARTIDO == 'PC do B' & indigena==1 | 
                                    SG_PARTIDO == "PSOL" & indigena ==1 |SG_PARTIDO == "PSL" & indigena ==1  ~ 1))%>%
  
  mutate(ind_eleito_esq = case_when(cand_ind_esq == 1 & eleito == 1 ~ 1)) %>%
  
  replace_na(list(cand_ind_esq = 0, ind_eleito_esq = 0,ind_eleito=0 ))%>%
  
  select(CD_COR_RACA,DS_COR_RACA, DS_SIT_TOT_TURNO, NR_PARTIDO, SG_PARTIDO, NM_UE, ST_REELEICAO,
         candidato, indigena, eleito, ind_eleito, cand_ind_esq, ind_eleito_esq)


#quais partidos teve mais candidato
dadospolitica14 <- politica14%>%
  filter(indigena == 1)%>%
  count(SG_PARTIDO)









poli14 <- politica14 %>%
  group_by(NM_UE)%>%
  summarize(cand_ind = sum(indigena), cand_total = sum(candidato), cand_ind_esq = sum(cand_ind_esq), 
            ind_eleito = sum(ind_eleito), ind_eleito_esq = sum(ind_eleito_esq), eleitos = sum(eleito) )%>%
  mutate(proporcao_cand_ind = cand_ind/cand_total, proporcao_ind_esq = cand_ind_esq/cand_ind,
         proporcao_ind_eleito = ind_eleito/eleitos )%>%
  mutate(ano = as.character(2014))%>%
  inner_join(ufcod2, by = c('NM_UE' = 'nomedauf2'))



#2018

candi_2018 <- read.csv("C:/Users/Matheus/Desktop/tcc/excel/politica/candidatostudo2018/consulta_cand_2018_BRASIL.csv", sep=";")

politica_2018 <- candi_2018 %>%
  mutate(candidato =1)%>%
  
  mutate(indigena = if_else(DS_COR_RACA == 'INDÍGENA',1,0))%>%
  
  mutate(eleito = if_else(DS_SIT_TOT_TURNO == "ELEITO" | DS_SIT_TOT_TURNO == "ELEITO POR QP" |
                            DS_SIT_TOT_TURNO == "ELEITO POR MÉDIA",1,0))%>%
  
  mutate(ind_eleito = case_when(indigena == 1 & eleito == 1 ~1))%>%
  
  mutate(cand_ind_esq = case_when(SG_PARTIDO == "PT" & indigena == 1| SG_PARTIDO == 'PC do B' & indigena==1 | 
                                    SG_PARTIDO == "PSOL" & indigena ==1 |SG_PARTIDO == "PSL" & indigena ==1  ~ 1))%>%
  
  mutate(ind_eleito_esq = case_when(cand_ind_esq == 1 & eleito == 1 ~ 1)) %>%
  
  replace_na(list(cand_ind_esq = 0, ind_eleito_esq = 0,ind_eleito=0 ))%>%
  
  select(CD_COR_RACA,DS_COR_RACA, DS_SIT_TOT_TURNO, NR_PARTIDO, SG_PARTIDO, NM_UE, ST_REELEICAO,
         candidato, indigena, eleito, ind_eleito, cand_ind_esq, ind_eleito_esq)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#ELABORANDO GRAFICOS SOBRE A ELEICAO
#quais partidos teve mais candidato
dadospolitica18 <- politica_2018%>%
  filter(indigena == 1)%>%
  count(SG_PARTIDO)



ggplot(dadospolitica18, aes(x=SG_PARTIDO,y=n, group=1)) + geom_col()  +
  labs(x = "Partido", y = "N?mero de Candidatos", 
       title = "Candidatos inscritos por cada partido - Elei??o de 2018") +
  theme_classic()+ theme(axis.text.x = element_text(angle = 50, hjust = 1))







#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

poli18 <- politica_2018 %>%
  group_by(NM_UE)%>%
  summarize(cand_ind = sum(indigena), cand_total = sum(candidato), cand_ind_esq = sum(cand_ind_esq), 
            ind_eleito = sum(ind_eleito), ind_eleito_esq = sum(ind_eleito_esq), eleitos = sum(eleito) )%>%
  mutate(proporcao_cand_ind = cand_ind/cand_total, proporcao_ind_esq = cand_ind_esq/cand_ind,
         proporcao_ind_eleito = ind_eleito/eleitos )%>%
  mutate(ano = as.character(2014))%>%
  inner_join(ufcod2, by = c('NM_UE' = 'nomedauf2'))





polifed <- poli14 %>%
  add_row(poli18)


#$$$$$$$$$$$$$$$$$$$$$$
#$ PERFIL DO ELEITORADO?









#bases originais - os primeiros dados que temos sao realmente de 2014 - obtidas manualmente e não tão uteis
##################################################abrindo a base 2014


# candidatos_ind_2014 <- read_excel("C:/Users/Matheus/Desktop/tcc/excel/candidatos_ind_2014.xlsx")
# 
# cand2014 <- candidatos_ind_2014 %>%
#   transmute(uf = UF, partido = Partido, nome = `Nome do candidato`, cargo = Cargo,
#             candidatura = `Situa??o da candidatura`, detalhe = `Detalhe situa??o candidatura`,
#             eleito= `Situa??o de totaliza??o`)
#   
# 
# #abrindo a base 2016
# 
# candidatos_ind_2016 <- read_excel("C:/Users/Matheus/Desktop/tcc/excel/candidatos_ind_2016.xlsx")
# 
# cand2016<- candidatos_ind_2016 %>%
#   transmute(uf = UF, partido = Partido, nome = `Nome do candidato`, cargo = Cargo,
#             candidatura = `Situa??o da candidatura`, detalhe = `Detalhe situa??o candidatura`,
#             eleito= `Situa??o de totaliza??o`)
# 
# 
# #abrindo a base de 2018
# 
# candidatos_ind_2018 <- read_excel("C:/Users/Matheus/Desktop/tcc/excel/candidatos_ind_2018.xlsx")
# 
# cand2018<- candidatos_ind_2018 %>%
#   transmute(uf = UF, partido = Partido, nome = `Nome do candidato`, cargo = Cargo,
#             candidatura = `Situa??o da candidatura`, detalhe = `Detalhe situa??o candidatura`,
#             eleito= `Situa??o de totaliza??o`)
# 
# 
# 
# 
# 
# 
# ##############################################parte 1 - de analise s? de partidos
# 
# 
# #2014
# partidos14 <- cand2014%>%
#   group_by(partido)%>%
#   count(partido)%>%
#   rename(numdecandidatos = n)%>%
#   mutate(ano = 2014)%>%
#   arrange(desc(numdecandidatos))%>%
#   ungroup()
# 
# #2016
# 
# partidos16 <- cand2016%>%
#   group_by(partido)%>%
#   count(partido)%>%
#   rename(numdecandidatos = n)%>%
#   mutate(ano = 2016)%>%
#   arrange(desc(numdecandidatos))%>%
#   ungroup()
# 
# #2018
# partidos18 <- cand2018%>%
#   group_by(partido)%>%
#   count(partido)%>%
#   rename(numdecandidatos = n)%>%
#   mutate(ano = 2018)%>%
#   arrange(desc(numdecandidatos))%>%
#   ungroup()
# 
# 
# 
# 
# partidos_eleicaoestadual <- partidos14%>%
#   add_row(partidos18)
#   
# 
# ##graficos
# #candidatos eleicao 14
# ggplot(partidos14, aes(x=partido ,y=numdecandidatos, group=1)) + geom_col()  +
#   labs(x = "Partido", y = "N?mero de Candidatos", 
#        title = "Candidatos inscritos por cada partido - Elei??o de 2014")
# 
# 
# #candidatos eleicao 18
# ggplot(partidos18, aes(x=partido ,y=numdecandidatos, group=1)) + geom_col()  +
#   labs(x = "Partido", y = "N?mero de Candidatos", 
#        title = "Candidatos inscritos por cada partido - Elei??o de 2018")
# 
# 
# 













