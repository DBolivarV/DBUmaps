#'Grafica Mapas Con las caracteristicas de los suelos, usando la api de google maps, y una data dada
#'
#' @param ApiKey Kei of GoogleCloud
#' @param FileRoute Route File
#' @param Rango
#'
#' @export

Geomaps<- function(ApiKey, FileRoute, Rango1, Rango2, Rango3, Rango4, Rango5)

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
  Localizacion= unique(datos2$Ubicacion)
  Caracteristica=datos2$Caracteristica
  parametro<-datos2$Caracteristica
  ColorP<-numeric(length(Caracteristica))

  lon<-numeric(length(Localizacion))
  lat<-numeric(length(Localizacion))

  legenda=sprintf(" %d a <%d", 0, Rango1)
  legenda2=sprintf(" %d a <%d", Rango1, Rango2)
  legenda3=sprintf(" %d a <%d", Rango2, Rango3)
  legenda4=sprintf(" %d a <%d", Rango3, Rango4)
  legenda5=sprintf(" %d a <%d", Rango4, Rango5)

  for (i in 1:length(Localizacion))
  {
    coordenada=geocode(as.character(paste("Venezuela",Localizacion[i],sep = " ")))
    lon[i]=as.numeric(coordenada[1])
    lat[i]=as.numeric(coordenada[2])
  }

  for (i in 1:length(Localizacion))
  {
    longitud <-c(lon[i])
    latitud <- c(lat[i])
  }

  #ColorPa<-GenerarColor(FileRoute, Rango1, Rango2,Rango3,
  #             Rango4,Rango5)

  for (i in 1:length(parametro))
  {
    if (parametro[i]<Rango1)
    {
      ColorP[i]="#0CFA04"
    }
    else if(parametro[i]>=Rango1 & parametro[i]<Rango2)
    {
      ColorP[i]="#B7FA04"
    }
    else if(parametro[i]>=Rango2 & parametro[i]<Rango3)
    {
      ColorP[i]="#FAE404"
    }
    else if(parametro[i]>=Rango3 & parametro[i]<Rango4)
    {
      ColorP[i]="#FA7F04"
    }
    else if(parametro[i]>=Rango4 & parametro[i]<Rango5)
    {
      ColorP[i]="#FA0F04"
    }
    else
    {
      ColorP[i]="#FFFFFF"
    }

  }
  print(Rango1)
  print(Rango2)
  print(Rango3)
  print(Rango4)
  print(Rango5)
  print(ColorP)

  #color <- c(rep("red",10), rep("blue",10))
  tamano <- c(10)
  titulo <- rep("DBUmaps",length(Localizacion))
  dataf <- data.frame(longitud, latitud, ColorP, tamano, titulo)
  leaflet(dataf) %>% addTiles() %>%
    addCircleMarkers(lng = lon,lat = lat,radius = 5,color = ColorP,
                     popup =~paste0(Localizacion,Caracteristica),
                     popupOptions = popupOptions(minWidth = 400, closeOnClick = TRUE))%>%
    addLegend("bottomright", colors = c("#0CFA04","#B7FA04","#FAE404","#FA7F04","#FA0F04"),
              labels =c(legenda,legenda2,legenda3,legenda4,legenda5),title = "Valores")


}

