#Cálculo de diferença entre médias dos casos de covid no Brasil

#carregando bibliotecas
library("tidyverse")


#Leitura dos dados
# A leitura foi realizada pelo método click and point de import dataset histórico
#Ano de 2020
df_parte1 <- read.csv("HIST_PAINEL_COVIDBR_2020_Parte1_30jan2022.csv", sep = ";")
df_parte2 <- read.csv("HIST_PAINEL_COVIDBR_2020_Parte2_30jan2022.csv", sep = ";")

#Ano de 2021
df_parte1_2021 <- read.csv("HIST_PAINEL_COVIDBR_2021_Parte1_30jan2022.csv", sep = ";")
df_parte2_2021 <-read.csv("HIST_PAINEL_COVIDBR_2021_Parte2_30jan2022.csv", sep = ";")

#Ano de 2022
df_parte1_2022 <-read.csv("HIST_PAINEL_COVIDBR_2022_Parte1_30jan2022.csv", sep = ";")


#Selecionando apenas as variáveis de interesse
#Os argumentos utilizados escolhem a label = Brasil na coluna referente a regiao e depois as variáveis elegidas
df_parte1_sel <- df_parte1[df_parte1$regiao == "Brasil",c("regiao","data","casosNovos", "obitosNovos")] 
df_parte2_sel <- df_parte2[df_parte2$regiao == "Brasil",c("regiao","data","casosNovos", "obitosNovos")]

df_parte1_sel_2021 <- df_parte1_2021[df_parte1_2021$regiao == "Brasil",c("regiao","data","casosNovos", "obitosNovos")] 
df_parte2_sel_2021 <- df_parte2_2021[df_parte2_2021$regiao == "Brasil",c("regiao","data","casosNovos", "obitosNovos")]

df_parte1_sel_2022 <- df_parte1_2022[df_parte1$regiao == "Brasil",c("regiao","data","casosNovos", "obitosNovos")] 

#select das variáveis de interesse, exluindo a label Brasil
df_parte1_sel <- df_parte1_sel %>% select(data, casosNovos, obitosNovos)
df_parte2_sel <- df_parte2_sel %>% select(data, casosNovos, obitosNovos)

df_parte1_sel_2021 <- df_parte1_sel_2021 %>% select(data, casosNovos, obitosNovos)
df_parte2_sel_2021 <- df_parte2_sel_2021 %>% select(data, casosNovos, obitosNovos)

df_parte1_sel_2022 <- df_parte1_sel_2022 %>% select(data, casosNovos, obitosNovos)


#Empilhando as bases
df <- bind_rows(df_parte1_sel, df_parte2_sel, df_parte1_sel_2021, df_parte2_sel_2021, df_parte1_sel_2022 )
head(df, n=30)

#Resumo estatístico da base de dados agrupada

descritivas_df <- summarise(df,
                            observações=n(),
                            média=mean(obitosNovos, ),
                            mediana=median(obitosNovos),
                            desv_pad=sd(obitosNovos),
                            mínimo=min(obitosNovos),
                            máximo=max(obitosNovos))        

print(descritivas_df)

#Testes de normalidade com a biblioteca moments
library("moments")
library(zoo)

#histogramas para visualização
hist(df$obitosNovos)
hist(df$casosNovos)

#Assimetria. 
#Para banco de dados com até 100 obs espera-se que a medida esteja entre -1 e 1; com mais de 100 obs, admite-se valores mais largos, entre -2 e 2
skewness(df$obitosNovos, na.rm=T)

#Teste de Shapiro Wilk apresenta um p-valor para medir quão próxima da dist normal estão os dados
#quando p-valor no teste de Shapiro-Wilk está ACIMA de 0,05 então avalia-se que os dados estão próximos da normalidade
#Conforme o número de obs aumenta há tendência que o p-valor fique abaixo de 0,05
shapiro.test(df$obitosNovos)


# #Calculando das médias móveis
# mm <- c(rollmean(df$obitosNovos, 7, align = "right"))
# teste_tabela <- cbind(df$data, c(rep(NA, 6), mm))
# teste_tabela <- teste_tabela[22:834,]
# 
# #criando um dataframe da tabela de médias móveis
# df_mm <- teste_tabela %>% as.data.frame(teste_tabela) %>% 
#                           rename(dias = V1, mm_obitos = V2)  
# 
# 
# #convertendo as colunas
# media_movel_obitos <- as.numeric(df_mm$mm_obitos)
# 
# 
# str(df_mm)
# 
# hist(media_movel_obitos)

#Separando os grupos para comparação entre as médias

#definem os marcos. Início da vacinação e data  em que houve cerca de 50% das pessoas vacinadas com uma dose
inicio_vax <- as.Date("18/01/2021",format = '%d/%m/%Y')
half_vax <- as.Date("18/08/2021",format = '%d/%m/%Y') 

#Filtros para separação dos grupos e comparação de médias
filtro_iniciovax1 <- filter(df, data <= inicio_vax)
filtro_iniciovax2 <- filter(df, data > inicio_vax)

#Filtros dividindo os períodos antes e depois da metade da pop vacinada
filtro_halfvax2 <- filter(df, data >= half_vax)
filtro_halfvax1 <- filter(df, data < half_vax)



descritivas_filtro1 <- summarise(filtro_halfvax1,
                            observações=n(),
                            média=mean(obitosNovos, ),
                            mediana=median(obitosNovos),
                            desv_pad=sd(obitosNovos),
                            mínimo=min(obitosNovos),
                            máximo=max(obitosNovos))        

print(descritivas_df)

descritivas_filtro2 <- summarise(filtro_halfvax2,
                                 observações=n(),
                                 média=mean(obitosNovos, ),
                                 mediana=median(obitosNovos),
                                 desv_pad=sd(obitosNovos),
                                 mínimo=min(obitosNovos),
                                 máximo=max(obitosNovos))        

print(descritivas_filtro2)




#teste de Wilcox considerado que independencia entre os grupos
wilcox.test(filtro_iniciovax2$obitosNovos, filtro_iniciovax1$obitosNovos, paired=F)
wilcox.test(filtro_halfvax2$obitosNovos, filtro_iniciovax1$obitosNovos, paired=F)
