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

#abrindo a base de dados municipais

candi_2020 <- read.csv("C:/Users/Matheus/Desktop/consulta_cand_2020_BRASIL.csv", sep=";")

#selecionando características dentro da base de dados municipais através de dummies
politica2020 <- candi_2020 %>%
  mutate(candidato =1)%>%
  
  #SE É INDÍGENA
  mutate(indigena = if_else(DS_COR_RACA == 'INDÍGENA',1,0))%>%
  
  #SE FOI ELEITO
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
  mutate(prefeito = if_else(DS_CARGO == 'PREFEITO',1,0), 
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







# COLOCANDO CÓDIGO IBGE PARA MERGIR COM AS OUTRAS BASES

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
  mutate(Municipio = str_replace_all(Municipio, "Õ", "O" ))%>%
  mutate(Municipio = str_replace_all(Municipio, "Ç", "C" ))%>%
  mutate(mun = str_c(UF,Municipio))



politica2020cibge <- politica2020%>%
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
  mutate(Municipio = str_replace_all(Municipio, "Ã", "A" ))%>%
  mutate(Municipio = str_replace_all(Municipio, "Õ", "O" ))%>%
  mutate(Municipio = str_replace_all(Municipio, "Ç", "C" ))%>%
  mutate(mun = str_c(UF,Municipio))%>%
  inner_join(codigos_IBGE, by = 'mun')%>%
  select(-Municipio.x ,  -Digito)

#PARTE EXTRA: CRIANDO UMA BASE PARA IDENTIFICAR CANDIDATOS QUE VIERAM DE REGI?ES DE ?BITO IND?GENA

politica2020munnasc<- politica2020cibge%>%
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
  left_join(mundsei2, by = c('codigo_nascimento' = 'ibge'))%>%
  replace_na(list(cidadededsei =0))%>%
  #sumarizando por municipio para ver quantos candidatos daquele municipio vieram de "DSEI"
  group_by(codigo_concorreu)%>%
  summarize(candidatosdedsei = sum(cidadededsei),
            candidatosindigena = sum(cind))%>%
  mutate(porcent_vindo_dsei = candidatosdedsei/ candidatosindigena,
         codigo_concorreu= as.character(codigo_concorreu))%>%
  select(-candidatosindigena, - candidatosdedsei)%>%
  mutate(tevecandnasc = if_else(porcent_vindo_dsei>0,1,0))

#escrevendo um dta para o stata
write.dta(politica2020munnasc, "C:/Users/Matheus/Desktop/stata/origens/politicacomorigens20.dta")

politica2020munnasc%>%
  count(tevecandnasc)



#colapsando a base
poli202 <- politica2020cibge %>%
  group_by(Codigo2)%>%
  summarize(cand_ind = sum(indigena), 
            cand_total = sum(candidato), 
            cand_ind_esq = sum(cand_ind_esq), 
            ind_eleito = sum(ind_eleito), 
            ind_eleito_esq = sum(ind_eleito_esq), 
            eleitos = sum(eleito),
            vereador_ind = sum(vereador_ind), 
            vereador_ind_el = sum(vereador_ind_el), 
            vereador = sum(vereador),
            prefeito_ind_el = sum(prefeito_ind_el),
            prefeito_ind = sum(prefeito_ind))%>%
  ungroup()%>%
  #CRIANDO PORCENTAGENS RELEVANTES PARA OS DADOS 
  mutate(prop_cand_ind = (cand_ind/cand_total)*100, 
         prop_ind_esq = (cand_ind_esq/cand_ind)*100,
         prop_ind_eleito = (ind_eleito/eleitos)*100, 
         aproveitamento_ind = (ind_eleito/cand_ind)*100,
         prop_vereador_ind = (vereador_ind/vereador)*100)%>%
  mutate(ano = as.character(2016), ibge = as.character(Codigo2),
         muntevecand = if_else(cand_ind > 0,1,0),
         munelegeucand = if_else(ind_eleito>0,1,0),
         percentil_eleito = ind_eleito/cand_ind)%>%
  select( -Codigo2, -ano)
# inner_join(politica2016munnasc, by = c('ibge' = 'codigo_concorreu'))




write_dta(poli202, "C:/Users/Matheus/Desktop/politica2020.dta")



