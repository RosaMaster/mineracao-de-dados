###############################################################################################################
#         Ciência dos Dados -  #naPrática - Marketing Analytics em R - Segmentação Hierárquica de Clientes
###############################################################################################################


###############################################################################################################
#---------------------- Parte 1 - Coleta de Dados de Marketing e Análise Exploratória
###############################################################################################################


# Configurando o diretório de trabalho
setwd("")
getwd()


# Carregando os dados
# Dataset: 51.243 observações e 3 variáveis
data = read.delim(file = 'compras.txt', header = FALSE, sep = '\t', dec = '.')

# Resumo dos dados
head(data)
summary(data)

# Adicionando cabeçalho e adicionando uma coluna extra com o ano da compra
colnames(data) = c('cliente_id', 'valor_compra', 'data_compra')
data$data_compra = as.Date(data$data_compra, "%Y-%m-%d")
data$ano_compra = as.numeric(format(data$data_compra, "%Y"))

# Resumo dos dados após modificação
head(data)
summary(data)
str(data)

# Explorando os dados de forma simples com linguagem SQL
install.packages("sqldf")
library(sqldf)

# Número de compras por ano
sqlqry = sqldf("SELECT ano_compra, 
               COUNT(ano_compra) AS 'compras_por_ano' 
               FROM data 
               GROUP BY 1 ORDER BY 1")

barplot(sqlqry$compras_por_ano, names.arg = sqlqry$ano_compra)

# Valor médio das compras por ano
sqlqry = sqldf("SELECT ano_compra, AVG(valor_compra) AS 'media_compra'
               FROM data GROUP BY 1 ORDER BY 1")

barplot(sqlqry$media_compra, names.arg = sqlqry$ano_compra)

# Total de compras por ano
sqlqry = sqldf("SELECT ano_compra, 
                SUM(valor_compra) AS 'total_compras' 
                FROM data GROUP BY 1 ORDER BY 1")
barplot(sqlqry$total_compras, names.arg = sqlqry$ano_compra)

# Resumo 
sqlqry = sqldf("SELECT ano_compra,
                  COUNT(ano_compra) AS 'compras_por_ano',
                  AVG(valor_compra) AS 'media_compra',
                  SUM(valor_compra) AS 'total_compras'
           FROM data GROUP BY 1 ORDER BY 1")

print(sqlqry)

###############################################################################################################
# -----------------------Parte 2 - Segmentação Estatística e Hierárquica de Clientes
###############################################################################################################


# Adicionando cabeçalho e adicionando uma coluna extra com o ano da compra
data$data_compra = as.Date(data$data_compra, "%Y-%m-%d")
data$days_since  = as.numeric(difftime(time1 = "2020-01-01", time2 = data$data_compra, units = "days"))
data$ano_compra = as.numeric(format(data$data_compra, "%Y"))

# Resumo dos dados após modificação
head(data)
summary(data)

# Computando indicadores chave de Marketing com SQL- frequência, média de compras e days-since
clientes = sqldf("SELECT cliente_id,
                          MIN(days_since) AS 'days_since',
                          COUNT(*) AS 'frequencia',
                          AVG(valor_compra) AS 'media_compra'
                   FROM data GROUP BY 1")

# Explorando os dados
head(clientes)
summary(clientes)
hist(clientes$days_since)
hist(clientes$frequencia)
hist(clientes$media_compra)
hist(clientes$media_compra, breaks = 120)


#####---------------- Preparando e transformando os dados --------------------

# Copiando os dados em um novo dataframe
new_data = clientes

# Remove o cliente_id como variável e armazena como título para as linhas
head(new_data)
row.names(new_data) = new_data$cliente_id
new_data$cliente_id = NULL
head(new_data)

# Aplicando transformação baseada em log para o valor médio das compras
# Isso vai converter os dados para uma distribuição normal
new_data$media_compra = log(new_data$media_compra)
hist(new_data$media_compra)

# Padronização das variáveis
new_data = scale(new_data)
head(new_data)


#####---------------- Segmentação Hierárquica --------------------

# Obtendo uma amostra de 10%
sample = seq(1, 18417, by = 10)
head(sample)
clientes_sample = clientes[sample, ]
new_data_sample  = new_data[sample, ]

# Computação das métricas de distância nos dados padronizados
?dist
d = dist(new_data_sample)

# Clustering Hierárquico nas métricas de distância
?hclust
c = hclust(d, method = "ward.D2")

# Plot do Dendograma
plot(c)

# Reduz para 9 segmentos
?cutree
members = cutree(c, k = 9)
members

# Tabela de frequência de 30 clientes
members[1:30]
table(members)

# Perfil de cada segmento
aggregate(clientes_sample[, 2:4], by = list(members), mean)



###############################################################################################################
#                                           Obrigado!
###############################################################################################################
