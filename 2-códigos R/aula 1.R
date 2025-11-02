###-------------------------------------------------
### Aula 1
###-------------------------------------------------

# Utilizaremos '#' para fazer comentários no código

#Site com microdados utilizados no curso
#https://portaldeimigracao.mj.gov.br/pt/base-de-dados

#Usando R como uma calculadora
2 + 2
5 * 3
10 / 2
4^2


#Atribuindo valores a objetos
a <- 10
b <- 5
c <- a + b
c
#Obs: tambem e possivel utilizar '=' para atribuicao
c2 = a * b
c2

#help: pesquise por palavra chave ou o nome de alguma funcao
help(sum)


###------------------------------------------
# pre-leitura dos microdados (formato .csv)
###------------------------------------------
#identificar o diretorio de trabalho
getwd()
#setar o diretório de trabalho
setwd("G:/Meu Drive/Universidade/2025/Administrativo/Extensão/minicurso R/1-dados")
#ver quais arquivos e pastas estao no diretorio
dir()
#pre-leitura dos microdados
readLines("CGIL_CNIg_2024.csv",10)
#Dica: verifique possivel acentuação no arquivo
#lendo o arquivo com a função read.csv2 (ponto e vírgula como separador)
dados <- read.csv2("CGIL_CNIg_2024.csv", fileEncoding = "UTF-8")
#Outra possibilidade de Encoding: "latin1"

#verificando as primeiras linhas do banco de dados
head(dados)

###------------------------------------------
# selecionando linhas e colunas específicas
###------------------------------------------
names(dados) #ver nomes das variaveis
dim(dados)   #ver dimensões do banco de dados (linhas e colunas)

table(dados$andamento) #tabela de frequencias da variavel andamento

#selecionando apenas linhas com andamento 'DEFERIDO'
dados_deferidos <- dados[dados$andamento == "DEFERIDO", ]

#selecionando apenas as colunas 'ano' e 'andamento'
dados_ano_andamento <- dados[, c("ano", "andamento")]


###------------------------------------------
# Objetivos: Tabelas de frequências
###------------------------------------------
# https://portaldeimigracao.mj.gov.br/pt/dados/2-sem-categoria/401967-ano-6-numero-09-setembro-2025

#Filtros que serao aplicados:
## andamento - DEFERIDO
## mes - 09
## modalidade - CGIL 
## ano - 2024

# Variaveis de interesse: tipo de visto, pais de origem, faixa etaria,
##sexo, amparo, escolaridade, ocupacao, estado destino, valor do investimento


#filtrando os dados conforme os critérios acima
##usar o padrao dados[linhas, colunas]
## & representa o operador "E" logico
## | representa o operador "OU" logico
dados_filtrados <- dados[
  dados$andamento == "DEFERIDO" &
    dados$mes == 9 &
    dados$modalidade == "CGIL" &
    dados$ano == "2024", 
  c("tipo_visto", "andamento", "amparo_legal", "modalidade",
    "mes", "ano", "valor_investimento_ajustado", "uf_estrangeiro",
    "data_nascimento", "genero", "escolaridade", "pais",
    "codigo_cbo")                 
]


#Para o relatorio tabular, desejamos agregar as informacoes de 2025
#será necessario utilizar uma agregacao entre os dados de 2024 e de 2025
dados25 <- read.csv2("CGIL_CNIg_jan-set2025.csv", fileEncoding = "UTF-8")
dados25_filtrados <- dados25[
  dados25$andamento == "DEFERIDO" &
    dados25$mes %in% c(8,9) &
    dados25$modalidade == "CGIL" &
    dados25$ano == "2025",
  c("tipo_visto", "andamento", "amparo_legal", "modalidade",
    "mes", "ano", "valor_investimento_ajustado", "uf_estrangeiro",
    "data_nascimento", "genero", "escolaridade", "pais",
    "codigo_cbo")  
]


#verificando se as colunas estao iguais
names(dados_filtrados)
names(dados25_filtrados)

names(dados_filtrados)==names(dados25_filtrados)#teste logico de igualdade

## juntando dados de 2024 e 2025
dados_final <- rbind.data.frame(dados_filtrados, dados25_filtrados)

###------------------------------------------
# Tabelas de frequências e Manupulacao das variaveis de interesse
###------------------------------------------
# criando variavel 'ano_mes' a partir das variaveis 'ano' e 'mes'
dados_final$ano_mes <- paste(dados_final$ano, dados_final$mes, sep="_")
table(dados_final$ano_mes)

#sexo
table(dados_final$genero)
dados_final$genero <- factor(dados_final$genero,
                                    levels = c("M", "F"),
                                    labels = c("Masculino", "Feminino"))
table(dados_final$genero)
table(dados_final$genero, dados_final$ano_mes)
#add o total das linhas
addmargins(table(dados_final$genero, dados_final$ano_mes),1)
#atribuir a um objeto e exportar para o diretorio
tab=addmargins(table(dados_final$genero, dados_final$ano_mes),1)
write.csv2(tab, "tabela1.csv", row.names = TRUE)

#tipo de visto
table(dados_final$tipo_visto)
(tab2=addmargins(table(dados_final$tipo_visto, dados_final$ano_mes),1))
#write.csv2(tab2, "tabela2.csv", row.names = TRUE)

#paises
table(dados_final$pais)
(tab3=addmargins(table(dados_final$pais, dados_final$ano_mes),1))
#write.csv2(tab3, "tabela3.csv", row.names = TRUE)

#escolaridade
table(dados_final$escolaridade)
levels(as.factor(dados_final$escolaridade))
#transformar e padronizar os niveis da variavel escolaridade
dados_final$escolaridade[dados_final$escolaridade=="Doutorado"]
dados_final$escolaridade[dados_final$escolaridade=="Doutorado"]="8_Doutorado"
dados_final$escolaridade[dados_final$escolaridade=="Mestrado"]="7_Mestrado" 
dados_final$escolaridade[dados_final$escolaridade=="Especialização"]="6_Pós-Graduação"
dados_final$escolaridade[dados_final$escolaridade=="Fundamental Completo"]="3_Fundamental"
dados_final$escolaridade[dados_final$escolaridade=="Fundamental Incompleto"]="2_Fundamental Incompleto"
dados_final$escolaridade[dados_final$escolaridade=="Médio Completo"]="4_Médio"
dados_final$escolaridade[dados_final$escolaridade=="Médio Incompleto"]="3_Fundamental"
dados_final$escolaridade[dados_final$escolaridade=="Pós-doutorado"]="8_Doutorado"
dados_final$escolaridade[dados_final$escolaridade=="Superior Completo"]="5_Superior"
dados_final$escolaridade[dados_final$escolaridade=="Superior Incompleto"]="4_Médio"
table(dados_final$escolaridade)
(tab4=addmargins(table(dados_final$escolaridade, dados_final$ano_mes),1))
#write.csv2(tab4, "tabela4.csv", row.names = TRUE)

#Unidade da Federacao
table(dados_final$uf_estrangeiro)
(tab5=addmargins(table(dados_final$uf_estrangeiro, dados_final$ano_mes),1))
#write.csv2(tab5, "tabela5.csv", row.names = TRUE)
                       
#faixa etaria
#primeiro, calcular a idade a partir da data de nascimento
dados_final$data_nascimento <- as.Date(dados_final$data_nascimento)
#utilizar uma data de referencia para o calculo da idade
#para fins de exemplificacao, utilizaremos a data do mesmo mes em que foi concedida a autorizacao
dados_final$ano
dados_final$mes
paste(dados_final$ano, dados_final$mes, "01", sep = "-")
data_ref <- as.Date(paste(dados_final$ano, dados_final$mes, "01", sep = "-"))

#dias entre o nascimento e a data de referencia
difftime(data_ref,dados_final$data_nascimento,unit="days")
#conversao para anos, levando em consideracao anos bisestos
difftime(data_ref,dados_final$data_nascimento,unit="days")/365.25
#escolher apenas a parte inteira do numero
floor(difftime(data_ref,dados_final$data_nascimento,unit="days")/365.25)
#atribuir a variavel idade
dados_final$idade <- floor(difftime(data_ref,dados_final$data_nascimento,unit="days")/365.25)

#fazer uma conferencia da variavel idade
dados_final$idade[1:10]#10 primeiras idades
dados_final$data_nascimento[1:10]#10 primeiras datas de nascimento
data_ref[1:10]#10 primeiras datas de referencia
table(dados_final$idade)
#temos um problema em idade = -1, precisamos investigar essa idade
dados_final[dados_final$idade==-1,]

#criando faixas de idade
#dados_final$faixa_etaria <- NA
#para fins de exemplo didatico, vamos adotar que idades menores que 15 serao consideradas NA
dados_final$faixa_etaria[dados_final$idade < 15 ] <- "Não informado"
dados_final$faixa_etaria[dados_final$idade >= 15 & dados_final$idade < 20 ] <- "Menor que 20"
dados_final$faixa_etaria[dados_final$idade >= 20 & dados_final$idade <= 34 ] <- "20-34"
dados_final$faixa_etaria[dados_final$idade >= 35 & dados_final$idade <= 49 ] <- "35-49"
dados_final$faixa_etaria[dados_final$idade >= 50 & dados_final$idade <= 64 ] <- "50-64"
dados_final$faixa_etaria[dados_final$idade >= 65] <-"65 ou mais"

table(dados_final$faixa_etaria)
(tab6=addmargins(table(dados_final$faixa_etaria, dados_final$ano_mes),1))
#write.csv2(tab6, "tabela6.csv", row.names = TRUE)

#Resolucao Normativa
table(dados_final$amparo_legal)
#transformar dos dados em factor
levels(as.factor(dados_final$amparo_legal))
#ler a base de RNs
dir()
readLines("RNs_geral_17052025.csv",10)
rns <- read.csv2("RNs_geral_17052025.csv", fileEncoding = "UTF-8")

#verificar se todos os amparos estao na base de RNs
dados_final$amparo_legal%in%rns$amparo_legal
dados_final$amparo_legal[!dados_final$amparo_legal%in%rns$amparo_legal]

#juntar a base de dados com as informacoes de RNs por meio de um 'merge'
dados_final2 <- merge.data.frame(dados_final, rns,
                          by.x = "amparo_legal",
                          by.y = "amparo_legal",
                          all.x = TRUE)
table(dados_final2$RN)
(tab7=addmargins(table(dados_final2$RN, dados_final2$ano_mes),1))
#write.csv2(tab7, "tabela7.csv", row.names = TRUE)