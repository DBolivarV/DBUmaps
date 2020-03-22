#'Grafica Mapas Con las caracteristicas de los suelos, usando la api de google maps, y una data dada
#'
#' @param ApiKey Kei of GoogleCloud
#' @param FileRoute Route File
#' @param Elemento Patron que se quiere graficar
#' @param Intervalo 4 Intervalos.
#'
#' @export

Geomaps<- function(ApiKey, FileRoute,Elemento, Intervalo1, Intervalo2, Intervalo3, Intervalo4)

{
  library(leaflet)
  library(knitr)
  library(rmarkdown)
  library(PKI)
  library(packrat)
  library(base64enc)
  library(ggplot2)
  library(ggmap)

  register_google(key = ApiKey)
  datos2<- read.csv(FileRoute,header = T,sep = ";")
  Localizacion= unique(datos2[1])
  Direccion=Localizacion[,1]
  nombrescol<-names(datos2)
  Caracteristica=datos2[Elemento]
  Carac=Caracteristica[,1]

  parametro<-Carac

  ColorP<-numeric(length(Carac))

  lon<-numeric(length(Direccion))
  lat<-numeric(length(Direccion))

  legenda=sprintf("< %d Muy Bajo", Intervalo1)
  legenda2=sprintf("[%d,%d) Bajo", Intervalo1, Intervalo2)
  legenda3=sprintf("[%d,%d) Medio", Intervalo2, Intervalo3)
  legenda4=sprintf("[%d,%d) Alto", Intervalo3, Intervalo4)
  legenda5=sprintf(">= %d  Muy alto", Intervalo4)

  for (i in 1:length(Direccion))
  {
    coordenada=geocode(as.character(paste(Direccion[i],sep = " ")))
    lon[i]=as.numeric(coordenada[1])
    lat[i]=as.numeric(coordenada[2])
  }

  for (i in 1:length(Direccion))
  {
    longitud <-c(lon[i])
    latitud <- c(lat[i])
  }

  #ColorPa<-GenerarColor(FileRoute, Intervalo1, Intervalo2,Intervalo3,
  #             Intervalo4,Intervalo5)

  for (i in 1:length(parametro))
  {
    if (parametro[i]<Intervalo1)
    {
      ColorP[i]="#0CFA04"
    }
    else if(parametro[i]>=Intervalo1 & parametro[i]<Intervalo2)
    {
      ColorP[i]="#B7FA04"
    }
    else if(parametro[i]>=Intervalo2 & parametro[i]<Intervalo3)
    {
      ColorP[i]="#FAE404"
    }
    else if(parametro[i]>=Intervalo3 & parametro[i]<Intervalo4)
    {
      ColorP[i]="#FA7F04"
    }
    else if(parametro[i]>=Intervalo4)
    {
      ColorP[i]="#FA0F04"
    }
    else
    {
      ColorP[i]="#FFFFFF"
    }

  }
  print(Intervalo1)
  print(Intervalo2)
  print(Intervalo3)
  print(Intervalo4)
  #print(Intervalo5)
  print(ColorP)

  #color <- c(rep("red",10), rep("blue",10))
  tamano <- c(10)
  #titulo <- rep("DBUmaps",length(Direccion))
  dataf <- data.frame(longitud, latitud, ColorP, tamano)
  leaflet(dataf) %>% addTiles() %>%
    addCircleMarkers(lng = lon,lat = lat,radius = 5,color = ColorP,
                     popup =~paste(Direccion," -> ",Carac),
                     popupOptions = popupOptions(minWidth = 200, closeOnClick = TRUE))%>%
    addLegend("bottomright", colors = c("#0CFA04","#B7FA04","#FAE404","#FA7F04","#FA0F04"),
              labels =c(legenda,legenda2,legenda3,legenda4,legenda5),title = paste("Niveles de ",nombrescol[Elemento]))


}

