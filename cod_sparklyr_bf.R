# Pacotes necessarios 
#install.packages("sparklyr")
#spark_install()
require(sparklyr)
require(tidyverse)

#Defina o local do arquivo .csv do BF
setwd("D://portal_transparencia//")

# Criando uma conexao com o Spark (sc = spark connection)
sc <- spark_connect(master = "local")

# Verificando  conexao 
spark_connection_is_open(sc)

#Defina mês e ano (sintaxe de variáveis mudou em maio de 2018)
mes=01
ano=2020

#aux meses
meses=c("jan","fev","mar","abr","mai","jun","jul","ago","set",
        "out","nov","dez")


#verificando existencia do parquet
id.parquet <- sum(list.files()==paste0("BF_",ano,sprintf("%02d", mes),"_parquet"))

if (id.parquet==0) {
#csv para parquet
dados_csv <- spark_read_csv(sc,   
                            "BF",  
                            paste0(ano,sprintf("%02d", mes),"_BolsaFamilia_Pagamentos.csv"),   #caminho do arquivo csv
                            delim = ";", dec=".",
                            memory = FALSE)  

# Escrevendo os dados em parquet
spark_write_parquet(dados_csv,   #DataFrame do Spark
                    paste0("BF_",ano,sprintf("%02d", mes),"_parquet"))   #caminho para salvar o arquivo parquet gerado
cat("parquet_created \n")
}
# Lendo o arquivo em parquet
BF <- spark_read_parquet(sc, 
                         paste0("BF_",meses[mes],"_",ano,"_p"),  #referencia no Spark
                         paste0("BF_",ano,sprintf("%02d", mes),"_parquet"), #nome do arquivo no R
                         memory = FALSE) #para nao ler na memoria RAM

#verificando base
BF %>% head()
BF %>% count()

#nome das variáveis
BF %>% colnames()

verificar <- (ano>=2019) || (mes>=5 && ano >=2018) || (mes==12 && ano==2013)
# Selecionando as variáveis de interesse
if (verificar) {
  BF_tbl <- BF %>% 
    select(NIS_FAVORECIDO,UF, CiDIGO_MUNICiPIO_SIAFI, NOME_MUNICiPIO, VALOR_PARCELA)
} else {
  BF_tbl <- BF %>% 
    select(NIS_Beneficiirio,UF, Cidigo_Municipio_SIAFI, Nome_Municipio_SIAFI, Valor_Beneficio)
}

BF_tbl %>% sdf_dim() 

# Trocando o separador de decimal de virgula para ponto
if (verificar) {
  BF_tbl <- BF_tbl %>% mutate(VALOR_PARCELA = regexp_replace(VALOR_PARCELA, ",", "."))
  } else {
  BF_tbl <- BF_tbl %>% mutate(Valor_Beneficio = regexp_replace(Valor_Beneficio, ",", "."))
}


# Mudando o tipo de variavel
if (verificar) {
  BF_tbl <- BF_tbl %>% mutate(VALOR_PARCELA = as.numeric(VALOR_PARCELA))
} else {
  BF_tbl <- BF_tbl %>% mutate(Valor_Beneficio = as.numeric(Valor_Beneficio))
}


#Agregando dados por municipio

if (verificar) {
  ben_munic <- BF_tbl %>%
    group_by(UF, NOME_MUNICiPIO,CiDIGO_MUNICiPIO_SIAFI) %>%
    summarize(receb = sum(VALOR_PARCELA, na.rm = TRUE),
              n=n_distinct(NIS_FAVORECIDO)) %>%
    collect()
} else {
  ben_munic <- BF_tbl %>%
    group_by(UF, Nome_Municipio_SIAFI,Cidigo_Municipio_SIAFI) %>%
    summarize(receb = sum(Valor_Beneficio, na.rm = TRUE),
              n=n_distinct(NIS_Beneficiirio)) %>%
    collect()
}


#View(ben_munic)

# Renomeando
colnames(ben_munic) <- c("UF", "NOME_MUNIC","CODIGO_SIAFI","VALOR_RECEBIDO","QTD_BENEF")

# Escrevendo em CSV
write_csv(ben_munic, paste0("BF_",sprintf("%02d", mes),"_",ano,"_ben_munic.csv"))

spark_disconnect(sc)

