# Banco: Série histórica de estatísticas municipais no Brasil
# Cientista de Dados: Marco Antonio Faganello - marcofaga@gmail.com - http://github.com/marcofaga
# Codificação: Banco e Script em ISO-8859-1 ou latin1 ou Europeu Ocidental (ISO)
# Descrição do Script: construção do banco principal

# BASE ======================================================================
setwd("C:\\Users\\Marco Faganello\\Dropbox\\Estatisticas Essenciais\\Estatísticas Municipais")
load("histmun.RData")
options(stringsAsFactors = F)

# libraries==================================================================
# install.packages('xlsx')
library(xlsx)
#install.packages("stringr")
library(stringr)
# install.packages('dplyr')
library(dplyr)

# functions==================================================================
source("functions/insere_index.R")
source("functions/padroniza_texto.R")
source("functions/sumario_catcon.R")
source("functions/clean_apag.R")
source("functions/latex_tabela.R")
options(stringsAsFactors = F)

stop()
# Início do script============================================================

#inserindo estatística contagem da população (Fonte: IBGE)

apag <- list.files("dados brutos/ibge/contagempop", full.names = T)
apag2 <- unzip(apag[1], exdir = "dados brutos/ibge/contagempop")

apag3 <- data.frame()

for(ap1 in apag2) {

  ap2 <- loadWorkbook(ap1)
  ap3 <- getSheets(ap2)

  if(length(ap3) != 1) {

    ap3 <- names(ap3)
    ap3 <- which(grepl("mun", ap3, ignore.case = T))


  } else {

    ap3 <- 1

  }

  ap2 <- read.xlsx(ap1, ap3)
  ap4 <- which(ap2[,1] == "DF")
  ap5 <- min(which(ap2[,1] == "RO"))

  ap2 <- ap2[c(ap5:ap4), c(1:5)]
  ap2$ano <- ap1
  names(ap2) <- c(1:6)

  apag3 <- rbind(apag3, ap2)

}

file.remove(apag2)
names(apag3) <- c("uf", "cod_uf", "cod_ibge", "mun", "pop", "ano")
apag3$ano <- gsub("dados brutos/ibge/contagempop/", "", apag3$ano)
apag3$ano <- gsub(".xls", "", apag3$ano)
apag3$ano <- as.numeric(apag3$ano)

apag3$cod_ibge <- ifelse(apag3$ano >= 2011, paste(apag3$cod_uf, apag3$cod_ibge, sep=""), apag3$cod_ibge)
apag3$cod_ibge <- ifelse(apag3$ano == 2009, str_pad(apag3$cod_ibge, 5, "left", "0"), apag3$cod_ibge)
apag3$cod_ibge <- ifelse(apag3$ano >= 2007 & apag3$ano <= 2009, paste(apag3$cod_uf, apag3$cod_ibge, sep=""), apag3$cod_ibge)
apag3$cod_ibge <- ifelse(apag3$ano <= 1999, substr(apag3$cod_ibge, 2, 5), apag3$cod_ibge)
apag3$cod_ibge <- ifelse(apag3$ano <= 2006 & apag3$ano >= 2000, str_pad(apag3$cod_ibge, 4, "left", "0"), apag3$cod_ibge)
apag3$cod_ibge <- ifelse(apag3$ano <= 2006, paste(apag3$cod_uf, apag3$cod_ibge, sep=""), apag3$cod_ibge)
apag3$cod_ibge <- ifelse(apag3$ano >= 2007, substr(apag3$cod_ibge, 1, 6), apag3$cod_ibge)

apag <- read.xlsx("dados brutos/Base_MUNIC_2015.xls", 2)
apag$cod <- substr(apag$A1, 1, 6)

apag3$nome_mun <- apag$Nome[match(apag3$cod_ibge, apag$cod)]
apag3$cod_ibgedv <- apag$A1[match(apag3$cod_ibge, apag$cod)]
apag3 <- apag3[, c(6, 1, 2, 3, 8, 7, 5)]

apag3$cod_ibgedv <- as.character(apag3$cod_ibgedv)

#dados de 1996 e 2010
apag <- list.files("dados brutos/ibge/contagempop/1996", full.names = T)
apag2 <- data.frame()

for(ap1 in apag) {

  unzip(ap1, exdir = "dados brutos/ibge/contagempop/1996")
  ap2 <- grep(".xls", list.files("dados brutos/ibge/contagempop/1996", full.names = T), value = T, ignore.case = T)
  ap3 <- read.xls(ap2, 1)
  ap3 <- ap3[-which(ap3$MUNICÍPIOS == "Total"),]
  ap3[,] <- lapply(ap3[,], function(x) as.character(x))
  names(ap3) <- letters[1:7]
  file.remove(ap2)
  apag2 <- rbind(apag2, ap3)

}

apag2$a <- substr(apag2$a, 1, 6)
apag2$uf <- apag3$uf[match(apag2$a, apag3$cod_ibge)]

names(apag2) <- c("cod_ibge",
                  "nome_mun",
                  "pop_abs",
                  "homens_abs",
                  "mulheres_abs",
                  "areamun_km2",
                  "dens_pop",
                  "uf"
)


apag2[,c(3:7)] <- lapply(apag2[,c(3:7)], function(x) gsub(",", "", x))
apag2$cod_uf <- substr(apag2$cod_ibge, 1, 2)
apag2$ano <- 1996
apag2$cod_ibgedv <- apag3$cod_ibgedv[match(apag2$cod_ibge, apag3$cod_ibge)]
apag2$nome_mun <- apag3$nome_mun[match(apag2$cod_ibge, apag3$cod_ibge)]

apag4 <- apag2[, c(10, 8, 9, 1, 11, 2, 3)]
names(apag4)[7] <- "pop"

apag3 <- rbind(apag3, apag4)

apag <- read.csv("dados brutos/ibge/contagempop/censo_2010.csv", sep = ";", colClasses = rep("character", 2))
apag$X1100015 <- trimws(apag$X1100015)
apag$uf <- apag3$uf[match(apag$X1100015, apag3$cod_ibgedv)]
apag$cod_uf <- substr(apag$X1100015, 1, 2)
apag$mun <- apag3$nome_mun[match(apag$X1100015, apag3$cod_ibgedv)]
apag$ano <- 2010
apag$cod_ibge <- substr(apag$X1100015, 1, 6)
apag <- apag[,c(3, 4, 1, 5, 2, 6)]
apag <- apag[, c(6, 3, 4, 7, 1, 5, 2)]
names(apag) <- names(apag3)

apag3 <- rbind(apag3, apag)

apag3$pop <- gsub("\\.", "", apag3$pop)
apag3$pop <- gsub("\\(\\d*\\)", "", apag3$pop)
apag3$pop <- gsub("\\(\\*\\)", "", apag3$pop)
apag3$pop <- as.integer(apag3$pop)

apag4 <- data.frame()

for(ap1 in 1995:2017) {
  
  ap2 <- apag3[apag3$ano == ap1,]
  
  apag4 <- rbind(apag4, ap2)
  
}

bd01_munhist <- apag4
index <- insereindex("bd01_munhist", "banco de dados com série histórica de estastísticas municipais")

apag <- bd01_munhist %>% group_by(ano, uf) %>% summarise(popuf = sum(pop))

apag2 <- bd01_munhist %>% group_by(ano) %>% summarise(popbra = sum(pop))

bd01_munhist$ano_uf <- paste(bd01_munhist$ano, bd01_munhist$uf, sep="_")

apag$ano_uf <- paste(apag$ano, apag$uf, sep="_")

bd01_munhist$popuf <- apag$popuf[match(bd01_munhist$ano_uf, apag$ano_uf)]
bd01_munhist$popbra <- apag2$popbra[match(bd01_munhist$ano, apag2$ano)]
bd01_munhist$popmunufp <- bd01_munhist$pop / bd01_munhist$popuf * 100
bd01_munhist$popmunbrap <- bd01_munhist$pop / bd01_munhist$popbra * 100
bd01_munhist <- bd01_munhist[,c(1, 2, 8, 3:7, 9:12)]

apag2 <- read.csv("dados brutos/TSE_IBGE.csv", sep=";", colClasses = rep("character", 8))

bd01_munhist$cod_tse <- apag2$TSEcod[match(bd01_munhist$cod_ibgedv, apag2$IBGEcod)]
bd01_munhist <- bd01_munhist[, c(1:4, 13, 5:12)]

#criando o dicionário e livro código de bd01
apag <- data.frame(pos = 1:ncol(bd01_munhist))
apag$vars <- names(bd01_munhist)
apag$class <- lapply(bd01_munhist, function(x) class(x))
apag$descr <- c("Ano",
                "Unidade da federação (UF)/ Estado",
                "Join das variáveis ano e uf separados por _",
                "Código IBGE da UF",
                "Código do Tribunal Superior Eleitoral (TSE) do município (5 dígitos)",
                "Código IBGE do município sem o dígito verificador (6 dígitos)",
                "Código IBGE do município com o dígito verificador (7 dígitos)",
                "Nome do município sem acento, em maiúscula no padrão usado pelo IBGE",
                "Absoluto da população residento do município",
                "Absoluto da população residente da uf",
                "Absoluto da população residente no Brasil",
                "Percentual da população do município sobre o total na uf",
                "Percentual da população do município sobre o total no Brasil"
) 

apag$fonte <- "Instituto Brasileiro de Geografia e Estatística (IBGE)"
apag$fonte[5] <- "Tribunal Superior Eleitoral (TSE)"
apag$fonte[9:13] <- "IBGE - Censo 2000 e 2010/ Contagem 1996 e 2007/ Estimado para 1995, 1997 a 1999, 2001 a 2006, 2008 a 2009, 2011 a 2017"

bd01_dicionario <- apag
bd01_dicionario$codigos <- NA
bd01_dicionario <- bd01_dicionario[, c(1, 2, 4, 3, 6, 5)]
bd01_dicionario[,c(1:6)] <- lapply(bd01_dicionario[,c(1:6)], function(x) as.character(x))

index <- insereindex("bd01_dicionario", "dicionário do banco bd01_munhist")

#showapag()
#write.table(bd01_munhist, "histmun.csv", sep=";", row.names = F, fileEncoding = "ISO-8859-1", dec = ".")
#write.table(bd01_dicionario, "dicionario.csv", sep=";", row.names = F, fileEncoding = "ISO-8859-1")
#latextab(bd01_dicionario)
#save.image("histmun.RData")
