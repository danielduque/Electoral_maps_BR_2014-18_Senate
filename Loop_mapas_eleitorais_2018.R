rm(list = ls())

# Os 15 primeiros pacotes são os que sempre dou "library", só para garantir
# Os últimos 6 são necessários para o loop
library(bit64)
library(data.table)
library(descr)
library(readr)
library(survey)
library(checkmate)
library(lme4)
library(oaxaca)
library(dplyr)
library(ggplot2)
library(gganimate)
library(gifski)
library(png)
library(transformr)
library(zoo)
library(foreach)
library(readxl)
library(reshape2)
library(geobr)
library(scales)
library(maptools)
library(RColorBrewer)
library(stringi)

# É preciso definir qualquer endereço, para salvar os mapas
setwd("C:/Users/DaniellaBritto/Desktop/R/CLP/Eleicoes")

# Planilha auxiliar
UFs <- read_excel("C:/Users/DaniellaBritto/Desktop/R/CLP/UFs.xlsx")

estados <- list.files(pattern = ".csv", full.names = TRUE)


for (x in 1:nrow(UFs)){
  
# Baixar dados municipais usando o ElectionsBR
Vot <- vote_mun_zone_fed(2018, uf=UFs$Arquivo[x])

Vot <- Vot %>%  select("SIGLA_UF", "SIGLA_PARTIDO", "DESCRICAO_CARGO", "CODIGO_MUNICIPIO", "TOTAL_VOTOS","NOME_MUNICIPIO", "NOME_URNA_CANDIDATO", "NUM_TURNO","NUMERO_ZONA")

Vot <- Vot %>%  mutate(NOME_MUNICIPIO = stri_trans_general(NOME_MUNICIPIO, "Latin-ASCII"),
                       NOME_MUNICIPIO=as.character(NOME_MUNICIPIO)) 


Senadores <- Vot %>% 
  filter(DESCRICAO_CARGO == "SENADOR") %>%
  arrange(NOME_URNA_CANDIDATO,NOME_MUNICIPIO) 

SenadoresDef1 <- Senadores %>% 
  group_by(NOME_MUNICIPIO) %>% 
  summarise(SUMMun = sum(TOTAL_VOTOS))

SenadoresDef2 <- Senadores %>% 
  group_by(SIGLA_UF,NOME_MUNICIPIO,NOME_URNA_CANDIDATO,SIGLA_PARTIDO) %>% 
  summarise(SUMCand = sum(TOTAL_VOTOS))

  Senadores1 <- SenadoresDef2 %>% 
    filter(NOME_URNA_CANDIDATO==UFs$`Senador 1`[x]) 
  
  Senadores1 <- merge(Senadores1,SenadoresDef1,by=c("NOME_MUNICIPIO"))
  
  Senadores1 <- Senadores1 %>% mutate(Percentual=SUMCand/SUMMun*100)
  
  Senadores2 <- SenadoresDef2 %>% 
    filter(NOME_URNA_CANDIDATO==UFs$`Senador 2`[x]) 
  
  Senadores2 <- merge(Senadores2,SenadoresDef1,by=c("NOME_MUNICIPIO"))
  
  Senadores2 <- Senadores2 %>% mutate(Percentual=SUMCand/SUMMun*100)
  
  
  # Usar pacote geobr para pegar mapa dos Municípios por Estado  
  UF <- read_municipality(code_muni=UFs$Codigo_UF[x], year=2010)
  
  UF <- UF %>% mutate(NOME_MUNICIPIO = toupper(name_muni))
  
  SPSen1 <- left_join(UF,Senadores1,by="NOME_MUNICIPIO")
  
  SPSen2 <- left_join(UF,Senadores2,by="NOME_MUNICIPIO")
  
plot1 <-   ggplot() + geom_sf(data=SPSen1, aes(fill= Percentual),
                     color = "grey", size = 0.01) +
    scale_fill_distiller(palette = "RdBu", 
                         breaks = pretty_breaks(n = 10))+
    guides(fill = guide_legend(reverse = TRUE))+
  labs(title=paste(UFs$`Senador 1`[x]," ",SPSen1$SIGLA_PARTIDO[1],"/",SPSen1$SIGLA_UF[1],sep=""))
  
  ggsave(plot1,file=paste(UFs$`Senador 1`[x],".png",sep=""))
  
  plot2 <-   ggplot() + geom_sf(data=SPSen2, aes(fill= Percentual),
                                color = "grey", size = 0.01) +
    scale_fill_distiller(palette = "RdBu", 
                         breaks = pretty_breaks(n = 10))+
    guides(fill = guide_legend(reverse = TRUE))+
    labs(title=paste(UFs$`Senador 2`[x]," ",SPSen2$SIGLA_PARTIDO[1],"/",SPSen2$SIGLA_UF[1],sep=""))
  
  ggsave(plot2,file=paste(UFs$`Senador 2`[x],".png",sep=""))
  
}
