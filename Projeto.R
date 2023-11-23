#Arrumar df
df_dados2022=read.csv("/cloud/project/esus-vepi.LeitoOcupacao_2022.csv")
df_dados2022<- subset(df_dados2022, select = -c( ocupacaoSuspeitoUti,ocupacaoSuspeitoCli, ocupacaoConfirmadoCli, ocupacaoConfirmadoUti))
df_dados2022$dataNotificacao<-as.POSIXct(df_dados2022$dataNotificacao, origin="1970-01-01")
#limpando a base
library(dplyr)
df_dados2022<-df_dados2022 %>% filter(ocupacaoHospitalarUti  >-1)
df_dados2022<-df_dados2022 %>% filter(ocupacaoHospitalarCli  >-1)
df_dados2022<-df_dados2022 %>% filter(saidaSuspeitaObitos  >-1)
df_dados2022<-df_dados2022 %>% filter(saidaSuspeitaAltas  >-1)
df_dados2022<-df_dados2022 %>% filter(saidaConfirmadaObitos >-1)
df_dados2022<-df_dados2022 %>% filter(saidaConfirmadaAltas >-1)
df_dados2022<-df_dados2022 %>% filter(ocupacaoCovidCli >-1)
df_dados2022<-df_dados2022 %>% filter(ocupacaoCovidCli >-1)
df_dados2022<-df_dados2022 %>% filter(ocupacaoCovidCli <20000)
df_dados2022<-df_dados2022 %>% filter(saidaConfirmadaAltas  <1000)
df_dados2022<-df_dados2022 %>% filter(saidaSuspeitaAltas   <2000)
df_dados2022<-df_dados2022 %>% filter(ocupacaoHospitalarUti   <2000)

df_dados2022<-df_dados2022 %>% filter(saidaConfirmadaObitos   <2000)

summary(df_dados2022)

save(df_dados2022,file="dados2022")

contagem<-df_dados2022 %>% group_by(estado) %>% 
  summarise(somaOcupa=sum(ocupacaoCovidUti),
            .groups = 'drop')


contagem_top5 <- contagem %>% 
  arrange(desc(somaOcupa)) %>% 
  head(5)

ggplot(contagem_top5, aes(x = estado, y = somaOcupa)) +
  geom_bar(stat = "identity", fill = "blue") +  # You can choose a different fill color if needed
  labs(title = "Bar Plot of Estado vs. Contagem",
      x = "Estado",
       y = "Contagem")


contagem_top2 <- contagem %>% 
  arrange(desc(somaOcupa)) %>% 
  head(2)

ggplot(contagem_top2, aes(x = estado, y = somaOcupa)) +
  geom_bar(stat = "identity", fill = "blue") +  # You can choose a different fill color if needed
  labs(title = "Bar Plot of Estado vs. Contagem",
       x = "Estado",
       y = "Contagem")

contagem_Obitos<-df_dados2022 %>% group_by(dataNotificacao) %>% 
  summarise(somaOcupa=sum(saidaConfirmadaObitos),
            .groups = 'drop')
plot(contagem_Obitos$dataNotificacao, y = contagem_Obitos$somaOcupa, type = "l", lty = 1, ylab = "Soma casos",xlab = "Data")

#Quais estados e períodos houve maior número de internações por COVID?
df_dados2022$mes<- format(as.Date(df$dataNotificacao, format="%d/%m/%Y"),"%m")
contagemUTI<-df_dados2022 %>% group_by(mes,estado) %>% 
  summarise(somaOcupa=count(ocupacaoHospitalarUti),
            .groups = 'drop')
plot(contagem_Obitos$dataNotificacao, y = contagem_Obitos$somaOcupa, type = "l", lty = 1, ylab = "Soma casos",xlab = "Data")


column_name <- "saidaConfirmadaObitos"


#  Qual município foi mais atingido (óbitos)?
contagemMortes<-df_dados2022 %>% group_by(municipio) %>% 
  summarise(somaOcupa=sum(saidaConfirmadaObitos),
            .groups = 'drop')


contagem_top6Mortes <- contagemMortes %>% 
  arrange(desc(somaOcupa)) %>% 
  head(6)

ggplot(contagem_top6Mortes, aes(x = municipio, y = somaOcupa)) +
  geom_bar(stat = "identity", fill = "blue") +  
  labs(title = "Bar Plot of Cidade vs. Deaths",
       x = "Cidade",
       y = "Deaths")
nome_da_variavel_x <- "municipio"
nome_da_variavel_y <- "saidaSuspeitaObitos "





#  É possível afirmar que aumentaram óbitos de 2020 para 2021?
df_dados2020=read.csv("/cloud/project/esus-vepi.LeitoOcupacao_2020.csv")
df_dados2021=read.csv("/cloud/project/esus-vepi.LeitoOcupacao_2021.csv")

summary(df_dados2020)
summary(df_dados2021$saidaConfirmadaObitos)

#Criando o boxplot
xp<-list(df_dados2020$saidaConfirmadaObitos,df_dados2021$saidaConfirmadaObitos)
boxplot(xp,pch="-", col="lightblue", border="black", boxwex=0.3, names=c("2020","2021"))






#Teste F para comparar as variâncias
r_0 = 1
var.test(x = df_dados2020$saidaConfirmadaObitos, 
         y = df_dados2021$saidaConfirmadaObitos, 
         ratio = 1, 
         alternative = "two.sided", 
         conf.level = .95)
#diferentes pois o p é 0.00000000000000022, que é menor que 0.05


#Teste t 
t.test(x = df_dados2020$saidaConfirmadaObitos, 
       y = df_dados2021$saidaConfirmadaObitos, 
       alternative = "less", 
       mu = 0, 
       conf.level = 0.95,
       var.equal = FALSE)


#diferentes pois o p é 0.00000000000000022, que é menor que 0.05




