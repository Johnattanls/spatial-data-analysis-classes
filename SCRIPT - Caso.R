pacotes <- c("plotly","tidyverse","rgdal","spdep","knitr","kableExtra","tmap",
             "gtools","grid","knitr","kableExtra")

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}


# Carregando um shapefile:
shp_co <- readOGR(dsn = "shapefile_centrooeste", 
                  layer = "centrooeste_shapefile",
                  encoding = "UTF-8", 
                  use_iconv = TRUE)

# Visualizando o shapefile carregado:
tmap_mode("view")

tm_shape(shp = shp_co) +
  tm_borders()

tmap_mode("plot")

# Observando a base de dados do shapefile carregado:
shp_co %>% 
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = TRUE, 
                font_size = 12)

# Carregando uma base de dados:
load("dados_centro_oeste.RData")

# Visualizando a base de dados carregada:
dados_centro_oeste %>% 
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = TRUE, 
                font_size = 12)

# Inserindo a base de dados dados_centro_oeste ao nosso shapefile:
shp_co@data %>% 
  left_join(dados_centro_oeste, by = "CD_GEOCODM") -> shp_co@data

# Observando a nova base de dados do nosso shapefile:
shp_co@data %>% 
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = TRUE, 
                font_size = 12)

# Observando os centroides dos municípios do Centro-Oeste:
tm_shape(shp = shp_co) +
  tm_dots(col = "#39568CFF", size = 0.08)

# Estabelecendo uma vizinhança:
vizinhos_queen <- poly2nb(pl = shp_co,
                          queen = TRUE,
                          row.names = shp_co@data$NM_MUNICIP)


# Há observações com o mesmo nome. Quais são?
shp_co@data$NM_MUNICIP[duplicated(shp_co@data$NM_MUNICIP)]

shp_co@data %>% 
  filter(NM_MUNICIP == "MUNDO NOVO")

# Como resolver a situação? Poderíamos acrescentar a sigla do Estado ao nome do
# município:
shp_co@data %>% 
  mutate(city = paste(NM_MUNICIP, state)) -> shp_co@data

# Estabelecendo uma vizinhança:
vizinhos_queen <- poly2nb(pl = shp_co,
                          queen = TRUE,
                          row.names = shp_co@data$city)

# Dados interessantes sobre a vizinhança estabelecida:
summary(vizinhos_queen)

# Observando a vizinhança estabelecida:
plot.new()
plot(shp_co, border = "lightgray")
plot(vizinhos_queen, 
     coordinates(shp_co), 
     add = TRUE, 
     col = "#FDE725FF")

# Definindo uma matriz de vizinhanças com padronização em linha:
matrizW_queen_linha <- nb2mat(vizinhos_queen,
                              style = "W")

# Observando a matriz de contiguidades, critério queen, padronizada em linha:
colnames(matrizW_queen_linha) <- rownames(matrizW_queen_linha)

View(matrizW_queen_linha)

# Estudo da Autocorrelação Global

# Antes de dar prosseguimento aos estudos das autocorrelações espaciais sobre
# a variável poverty, vamos observar alguns comportamentos:
tm_shape(shp = shp_co) +
  tm_fill(col = "poverty", 
          style = "quantile", 
          n = 5, 
          palette = "plasma",
          legend.hist = TRUE) +
  tm_borders() +
  tm_layout(legend.outside = TRUE)

# Salvando o gráfico da pobreza para uso subsequente:
poverty_plot <- tm_shape(shp = shp_co) +
  tm_fill(col = "poverty", 
          style = "quantile", 
          n = 5, 
          palette = "plasma",
          legend.hist = TRUE) +
  tm_borders() +
  tm_layout(legend.outside = TRUE)

# Será que podemos dizer que há um padrão espacial?


# Autocorrelação Global – a Estatística I de Moran ------------------------

# Para o cálculo da Estatística I de Moran, nosso algoritmo esperará como
# declaração um objeto de classe listw. Como exemplificação, voltaremos a 
# utilizar o objeto matrizW_queen:
listw_queen <- mat2listw(matrizW_queen_linha)

# Após isso, poderemos utilizar a função moran.test():
moran.test(x = shp_co@data$poverty, 
           listw = listw_queen)

###########################################################################

# A quais conclusões preliminares podemos chegar?


# O Diagrama da Estatística I de Moran ------------------------------------
moran.plot(x = shp_co@data$poverty, 
           listw = listw_queen, 
           zero.policy = TRUE,
           xlab = "Poverty", 
           ylab = "Spatially lagged poverty",
           pch = 19)

###########################################################################
#                            Moran Plot na mão                            #
###########################################################################
Wpoverty <- lag.listw(x = listw_queen, 
                      var = shp_co@data$poverty)

vetor_poverty <- shp_co@data$poverty

df <- data.frame(city = shp_co@data$city, poverty = vetor_poverty, Wpoverty)

ggplotly(
  df %>% 
    ggplot(aes(label = city)) +
    geom_point(aes(x = poverty, y = Wpoverty), alpha = 0.5, size = 3) +
    geom_smooth(aes(x = poverty, y = Wpoverty), 
                method = "lm", 
                se = F, 
                color = "#B8DE29FF",
                size = 1.2) +
    geom_vline(xintercept = mean(df$poverty), lty = 2) +
    geom_hline(yintercept = mean(df$Wpoverty), lty = 2) +
    annotate("text", x = 10, y = 65, label = "Low-High", color = "#39568CFF") +
    annotate("text", x = 65, y = 65, label = "High-High", color = "#39568CFF") +
    annotate("text", x = 10, y = 5, label = "Low-Low", color = "#39568CFF") +
    annotate("text", x = 65, y = 5, label = "High-Low", color = "#39568CFF") +
    labs(x = "Poverty",
         y = "Spatially lagged poverty") +
    theme_bw()
)

# A quais conclusões podemos chegar?


# Autocorrelação Local – a Estatística Moran Local ------------------------

# Considerando a variável poverty do objeto shp_co, podemos aferir sua 
# Estatística Moran Local, com o uso da função localmoran(), como se segue:
moran_local <- localmoran(x = shp_co@data$poverty, 
                          listw = listw_queen)



# Juntando os resultados da Estatística Moran Local no dataset do objeto shp_sp:
moran_local_mapa <- cbind(shp_co, moran_local)

# Plotando a Estatística Moran Local de forma espacial, sem problemas na escala
# de cores:
moran_local_mapa@data <- moran_local_mapa@data %>% 
  mutate(faixa_quantis = factor(quantcut(x = Ii, q = 5))) 

tm_shape(shp = moran_local_mapa) +
  tm_fill(col = "faixa_quantis", 
          palette = "plasma") +
  tm_borders() +
  tm_borders() +
  tm_layout(legend.outside = TRUE)

# Salvando o gráfico moran local para comparação subsequente:
moran_local_plot <- tm_shape(shp = moran_local_mapa) +
  tm_fill(col = "faixa_quantis", 
          palette = "plasma") +
  tm_borders() +
  tm_borders() +
  tm_layout(legend.outside = TRUE)

# Criando um espaço no ambiente de visualização do R:
plot.new()
pushViewport(
  viewport(
    layout = grid.layout(1,2)
  )
)

# Comparando os dados da pobreza no Centro-Oeste brasileiro com os valores da
# Estatística Moran Local:
# Passo 4: Executar as plotagens
print(poverty_plot, vp = viewport(layout.pos.col = 1, height = 5))
print(moran_local_plot, vp = viewport(layout.pos.col = 2, height = 5))

# Plotando as labels dadas pelas estatísticas de Moran (LL, LH, HL, HH):
moran_local_mapa <- cbind(moran_local_mapa, 
                          attr(x = moran_local, which = "quadr")[1])

tm_shape(shp = moran_local_mapa) +
  tm_fill(col = "mean", palette = "plasma") +
  tm_borders(col = "gray")


# Estabelecendo uma Clusterização LISA ------------------------------------


# O primeiro passo é o estabelecimento de um objeto que reservará espaços para 
# conter, no futuro, os quadrantes AA, AB, BA e BB:
quadrantes <- vector(mode = "numeric", length = nrow(moran_local))

quadrantes

# Criando um vetor que contenha o centro das observações da variável poverty ao 
# redor de sua média:
centro_poverty <- shp_co@data$poverty - mean(shp_co@data$poverty)

centro_poverty

# Criando um vetor que contenha o centro dos valores da Estatística Moran Local 
# em torno de sua média:
centro_moran_local <- moran_local[,1] - mean(moran_local[,1])

centro_moran_local

# Criando um objeto que guarde a significância a ser adotada:
sig <- 0.05

# Enquadrando nossas observações em seus respectivos quadrantes:
quadrantes[centro_poverty > 0 & centro_moran_local > 0] <- "HH"
quadrantes[centro_poverty > 0 & centro_moran_local < 0] <- "HL"
quadrantes[centro_poverty < 0 & centro_moran_local > 0] <- "LH"
quadrantes[centro_poverty < 0 & centro_moran_local < 0] <- "LL"

quadrantes

# Ajustando a presença da observação em razão de sua significância estatística:
quadrantes[moran_local[,5] > sig] <- "Estatisticamente_não_significante"

quadrantes

# Juntando o objeto quadrantes ao objeto moran_local_mapa
moran_local_mapa@data["quadrantes"] <- factor(quadrantes)


# Versão do gráfico anterior para daltônicos:
tm_shape(shp = moran_local_mapa) +
  tm_polygons(col = "quadrantes", 
              pal = c(HH = "#FDE725FF",
                      HL = "#7AD151FF", 
                      LH = "#2A788EFF", 
                      LL = "#440154FF",
                      Estatisticamente_não_significante = "white")) +
  tm_borders() +
  tm_layout(legend.outside = TRUE)

# A que conclusões podemos chegar?


# Autocorrelação Local -  A Estatística G de Getis e Ord ------------------

# Aqui, por definição, espera-se uma matriz espacial W de distâncias geográficas.
# Obviamente, as análises anteriores podem mudar.

# É difícil assumir um ponto de corte de distâncias. Suponhamos 100km:
vizinhos_distancias <- dnearneigh(coordinates(shp_co), 
                                  d1 = 0, 
                                  d2 = 100, 
                                  longlat = TRUE)

# Observando a vizinhança estabelecida:
plot.new()
plot(shp_co, border = "lightgray")
plot(vizinhos_distancias, 
     coordinates(shp_co), 
     add = TRUE, 
     col = "#FDE725FF")

# Perceberam o problema?

# Vamos para 150:
vizinhos_distancias <- dnearneigh(coordinates(shp_co), 
                                  d1 = 0, 
                                  d2 = 150, 
                                  longlat = TRUE)

# Observando a vizinhança estabelecida:
plot.new()
plot(shp_co, border = "lightgray")
plot(vizinhos_distancias, 
     coordinates(shp_co), 
     add = TRUE, 
     col = "#FDE725FF")

# Gerando a matriz W de distâncias:
matrizW_distancias <- nb2mat(neighbours = vizinhos_distancias,
                             style = "B")

# Para facilitar o estudo da nossa matriz W, podemos comandar:
colnames(matrizW_distancias) <- shp_co@data$city
rownames(matrizW_distancias) <- shp_co@data$city

View(matrizW_distancias)

# Um último ajuste é a transformação do objeto matrizW_distancias em um
# objeto de classe listw:
listw_dist <- mat2listw(x = matrizW_distancias)

# Calculando a Estatística G de Getis e Ord:
g_local <- localG(x = shp_co@data$poverty,
                  listw = listw_dist)

# Juntando as informações do objeto g_local ao nosso shapefile:
mapa_G <- cbind(shp_co, as.matrix(g_local))

head(mapa_G@data)

# Renomeando a nova variável para facilitar:
mapa_G@data %>% 
  rename(estistica_g = 7) -> mapa_G@data

# Plotando a Estatística G de forma espacial sem erros de escala:
mapa_G@data <- mapa_G@data %>% 
  mutate(faixa_quantis = factor(quantcut(x = estistica_g, q = 6))) 

# Versão do gráfico anterior para daltônicos>
tm_shape(mapa_G) + 
  tm_fill("faixa_quantis", 
          palette = "plasma") + 
  tm_borders()

# Fim ---------------------------------------------------------------------