
## Tópicos adicionais em tabulação de dados

###Utilizando o pacote 'dplyr' para tabulação de dados
###Podemos gerar filtros e agrupamentos em tabelas sem necessáriamente
###"criar" novas variáveis ou objetos, mantendo assim menor alocação na 
###memória do R.
#install.packages("dplyr")
library(dplyr)


## as.factor() e levels() para manipulação de variáveis categóricas
### Muitas vezes, ao trabalhar com variáveis categóricas (fatores) no R,
### é necessário alterar a ordem dos níveis ou renomeá-los para facilitar
### a análise e a visualização dos dados. As funções as.factor() e levels()
### são ferramentas úteis para essa tarefa.


## Criação de funções
UF_order=function(vet){
  #Norte
  levels(vet)[levels(vet)=="Rondônia"]="1.1_Rondônia"
  levels(vet)[levels(vet)=="Acre"]="1.2_Acre"
  levels(vet)[levels(vet)=="Amazonas"]="1.3_Amazonas"
  levels(vet)[levels(vet)=="Roraima"]="1.4_Roraima"
  levels(vet)[levels(vet)=="Pará"]="1.5_Pará"
  levels(vet)[levels(vet)=="Amapá"]="1.6_Amapá"
  levels(vet)[levels(vet)=="Tocantins"]="1.7_Tocantins"
  #Nordeste
  levels(vet)[levels(vet)=="Maranhão"]="2.1_Maranhão"
  levels(vet)[levels(vet)=="Piauí"]="2.2_Piauí"
  levels(vet)[levels(vet)=="Ceará"]="2.3_Ceará"
  levels(vet)[levels(vet)=="Rio Grande do Norte"]="2.4_Rio Grande do Norte"
  levels(vet)[levels(vet)=="Paraíba"]="2.5_Paraíba"
  levels(vet)[levels(vet)=="Pernambuco"]="2.6_Pernambuco"
  levels(vet)[levels(vet)=="Alagoas"]="2.7_Alagoas"
  levels(vet)[levels(vet)=="Sergipe"]="2.8_Sergipe"
  levels(vet)[levels(vet)=="Bahia"]="2.9_Bahia"
  #Sudeste
  levels(vet)[levels(vet)=="Minas Gerais"]="3.1_Minas Gerais"
  levels(vet)[levels(vet)=="Espírito Santo"]="3.2_Espírito Santo"
  levels(vet)[levels(vet)=="Rio de Janeiro"]="3.3_Rio de Janeiro"
  levels(vet)[levels(vet)=="São Paulo"]="3.4_São Paulo"
  #Sul
  levels(vet)[levels(vet)=="Paraná"]="4.1_Paraná"
  levels(vet)[levels(vet)=="Santa Catarina"]="4.2_Santa Catarina"
  levels(vet)[levels(vet)=="Rio Grande do Sul"]="4.3_Rio Grande do Sul"
  #Centro-Oeste
  levels(vet)[levels(vet)=="Mato Grosso do Sul"]="5.1_Mato Grosso do Sul"
  levels(vet)[levels(vet)=="Mato Grosso"]="5.2_Mato Grosso"
  levels(vet)[levels(vet)=="Goiás"]="5.3_Goiás"
  levels(vet)[levels(vet)=="Distrito Federal"]="5.4_Distrito Federal"
  
  return(vet)
}


uf_regiao=function(vet){
  #Norte
  levels(vet)[levels(vet)=="Rondônia"]="1_Norte"
  levels(vet)[levels(vet)=="Acre"]="1_Norte"
  levels(vet)[levels(vet)=="Amazonas"]="1_Norte"
  levels(vet)[levels(vet)=="Roraima"]="1_Norte"
  levels(vet)[levels(vet)=="Pará"]="1_Norte"
  levels(vet)[levels(vet)=="Amapá"]="1_Norte"
  levels(vet)[levels(vet)=="Tocantins"]="1_Norte"
  #Nordeste
  levels(vet)[levels(vet)=="Maranhão"]="2_Nordeste"
  levels(vet)[levels(vet)=="Piauí"]="2_Nordeste"
  levels(vet)[levels(vet)=="Ceará"]="2_Nordeste"
  levels(vet)[levels(vet)=="Rio Grande do Norte"]="2_Nordeste"
  levels(vet)[levels(vet)=="Paraíba"]="2_Nordeste"
  levels(vet)[levels(vet)=="Pernambuco"]="2_Nordeste"
  levels(vet)[levels(vet)=="Alagoas"]="2_Nordeste"
  levels(vet)[levels(vet)=="Sergipe"]="2_Nordeste"
  levels(vet)[levels(vet)=="Bahia"]="2_Nordeste"
  #Sudeste
  levels(vet)[levels(vet)=="Minas Gerais"]="3_Sudeste"
  levels(vet)[levels(vet)=="Espírito Santo"]="3_Sudeste"
  levels(vet)[levels(vet)=="Rio de Janeiro"]="3_Sudeste"
  levels(vet)[levels(vet)=="São Paulo"]="3_Sudeste"
  #Sul
  levels(vet)[levels(vet)=="Paraná"]="4_Sul"
  levels(vet)[levels(vet)=="Santa Catarina"]="4_Sul"
  levels(vet)[levels(vet)=="Rio Grande do Sul"]="4_Sul"
  #Centro-Oeste
  levels(vet)[levels(vet)=="Mato Grosso do Sul"]="5_Centro-Oeste"
  levels(vet)[levels(vet)=="Mato Grosso"]="5_Centro-Oeste"
  levels(vet)[levels(vet)=="Goiás"]="5_Centro-Oeste"
  levels(vet)[levels(vet)=="Distrito Federal"]="5_Centro-Oeste"
  
  return(vet)
}

dados_final2$uf_estrangeiro=as.factor(dados_final2$uf_estrangeiro)
#dados_final2$UF_ordenado = UF_order(dados_final2$uf_estrangeiro)
#dados_final2$regiao=uf_regiao(dados_final2$uf_estrangeiro)
levels(dados_final2$regiao)
# cgil$uf_estrangeiro[cgil$regiao==""]
# levels(cgil$regiao)[levels(cgil$regiao)==""]="Não Informado"

d.tab <- dados_final2 %>% filter(((mes%in%c(8,9) & ano==2025)
                               |(ano==2024 & mes==9))&
                                andamento=="DEFERIDO" & modalidade != "CNIg") %>%
  group_by(uf_regiao(uf_estrangeiro),UF_order(uf_estrangeiro), ano_mes) %>% 
  summarise(n = n())
write.csv2(d.tab, file="tabela_uf.csv", na="", fileEncoding ="latin1")
