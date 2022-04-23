# Instalação e Carregamento dos Pacotes Necessários para a Aula -----------

pacotes <- c("plotly","rgdal","raster","tmap","maptools","sf","rgeos","sp",
             "adehabitatHR","tidyverse","broom","knitr","kableExtra","gridExtra",
             "RColorBrewer","profvis","png","grid","magick","rgl","devtools",
             "GISTools")

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}

# O pacote rayshader que está no CRAN, no momento, possui alguns bugs. A versão
# que está no GitHub do autor do pacote já é mais funcional. Para instalá-la:
devtools::install_github("tylermorganwall/rayshader")

# Para carregar o rayshader
library(rayshader)