###### Docentes por lugar de último nivel de formación ###

##### Librería requeridas


library(leaflet)
library(rgdal)
library(htmlwidgets)
library(tidyverse)
library(rjson)
library(readxl)
library(extrafont)

divipola.R <- read.table("Data/DIVIPOLA_20160930.csv", sep=";", header=T)


#### Lectura de datos

# Base de datos con solo cabeceras municipales 

cabeceras <- divipola.R %>% select(code_dept=Código.Departamento,
                                   code_mun=Código.Municipio,
                                   departamentos=Nombre.Departamento,
                                   municipios=Nombre.Municipio,
                                   tipo_centro=Tipo.Centro.Poblado,
                                   longitud=Longitud,
                                   latitud=Latitud) %>% 
             filter(tipo_centro == "CABECERA MUNICIPAL (CM)")  


docentes <- read_xlsx("Data/Docentes.xlsx") 

centroids <- read_xlsx("Data/centroids.xlsx") 

# Lista con los nombres ISO en español de los diferentes países.

spanish <- read_xlsx("Data/centroids.xlsx", sheet = "Spanish")

spanish <- na.omit(spanish)


#### Manipulación

# Cuenta el número de docentes por país

cant_doc <- docentes %>% group_by(CODS_PAISU) %>% summarise(Total=n())

# Cuenta de docentes por nivel de formación

cant_docpre <- docentes %>% filter(docentes$FORMACION == "Pregrado"|  docentes$FORMACION == "Especialización") %>% group_by(CODS_PAISU, PAIS_U) %>% summarise(Total=n())
cant_docdoc <- docentes %>% filter(docentes$FORMACION == "Doctorado") %>% group_by(CODS_PAISU, PAIS_U) %>% summarise(Total=n())
cant_docmae <- docentes %>% filter(docentes$FORMACION == "Maestría") %>% group_by(CODS_PAISU, PAIS_U) %>% summarise(Total=n())
cant_docmed <- docentes %>% filter(docentes$FORMACION == "Especialidad Médica") %>% group_by(CODS_PAISU, PAIS_U) %>% summarise(Total=n())

# Cuenta de docentes por sede

cant_docbog <- docentes %>% filter(docentes$SEDE == "Bogotá") %>% group_by(CODS_PAISU, PAIS_U) %>% summarise(Total=n())
cant_docmede <- docentes %>% filter(docentes$SEDE == "Medellín") %>% group_by(CODS_PAISU, PAIS_U) %>% summarise(Total=n())
cant_docpal <- docentes %>% filter(docentes$SEDE == "Palmira") %>% group_by(CODS_PAISU, PAIS_U) %>% summarise(Total=n())
cant_docmani <- docentes %>% filter(docentes$SEDE == "Manizales") %>% group_by(CODS_PAISU, PAIS_U) %>% summarise(Total=n())
cant_docama <- docentes %>% filter(docentes$SEDE == "Amazonía") %>% group_by(CODS_PAISU, PAIS_U) %>% summarise(Total=n())
cant_docori <- docentes %>% filter(docentes$SEDE == "Orinoquía") %>% group_by(CODS_PAISU, PAIS_U) %>% summarise(Total=n())
cant_doccar <- docentes %>% filter(docentes$SEDE == "Caribe") %>% group_by(CODS_PAISU, PAIS_U) %>% summarise(Total=n())

# Cuenta de docentes por sexo

cant_dochom <- docentes %>% filter(docentes$SEXO == "Hombres") %>% group_by(CODS_PAISU, PAIS_U) %>% summarise(Total=n())
cant_docmuj <- docentes %>% filter(docentes$SEXO == "Mujeres") %>% group_by(CODS_PAISU, PAIS_U) %>% summarise(Total=n())


#### Lectura de JSON

countries <- rgdal::readOGR("GeoData/countriesgeo.json")

head(countries@data) # Vista previa del JSON


# Organizar los datos en una matriz

codigos <- matrix(0, nrow = 180, ncol = 18)

for(i in 1:180){
  codigos[i,1] = as.character(countries@data$id[i])
}


for(i in cant_doc$CODS_PAISU){
  codigos[codigos[,1] == i, 2] = cant_doc$Total[cant_doc$CODS_PAISU == i]
}


for(i in codigos[-c(40,148, 69, 91, 140, 143, 176),1]){
  codigos[codigos[,1] == i, 3] = centroids$Longitude[centroids$CODS_PAISU == i]
}

for(i in codigos[-c(40,148, 69, 91, 140, 143, 176),1]){
  codigos[codigos[,1] == i, 4] = centroids$Latitude[centroids$CODS_PAISU == i]
}

for(i in codigos[-c(40,148, 69, 91, 140, 143, 176),1]){
  codigos[codigos[,1] == i, 5] = spanish$Nombre[spanish$ISO == i]
}

for(i in cant_docama$CODS_PAISU){
  codigos[codigos[,1] == i, 6] = cant_docama$Total[cant_docama$CODS_PAISU == i]
}

for(i in cant_docbog$CODS_PAISU){
  codigos[codigos[,1] == i, 7] = cant_docbog$Total[cant_docbog$CODS_PAISU == i]
}

for(i in cant_doccar$CODS_PAISU){
  codigos[codigos[,1] == i, 8] = cant_doccar$Total[cant_doccar$CODS_PAISU == i]
}

for(i in cant_docdoc$CODS_PAISU){
  codigos[codigos[,1] == i, 9] = cant_docdoc$Total[cant_docdoc$CODS_PAISU == i]
}
for(i in cant_dochom$CODS_PAISU){
  codigos[codigos[,1] == i, 10] = cant_dochom$Total[cant_dochom$CODS_PAISU == i]
}
for(i in cant_docmae$CODS_PAISU){
  codigos[codigos[,1] == i, 11] = cant_docmae$Total[cant_docmae$CODS_PAISU == i]
}
for(i in cant_docmani$CODS_PAISU){
  codigos[codigos[,1] == i, 12] = cant_docmani$Total[cant_docmani$CODS_PAISU == i]
}
for(i in cant_docmed$CODS_PAISU){
  codigos[codigos[,1] == i, 13] = cant_docmed$Total[cant_docmed$CODS_PAISU == i]
}
for(i in cant_docmede$CODS_PAISU){
  codigos[codigos[,1] == i, 14] = cant_docmede$Total[cant_docmede$CODS_PAISU == i]
}
for(i in cant_docmuj$CODS_PAISU){
  codigos[codigos[,1] == i, 15] = cant_docmuj$Total[cant_docmuj$CODS_PAISU == i]
}
for(i in cant_docori$CODS_PAISU){
  codigos[codigos[,1] == i, 16] = cant_docori$Total[cant_docori$CODS_PAISU == i]
}

for(i in cant_docpal$CODS_PAISU){
  codigos[codigos[,1] == i, 17] = cant_docpal$Total[cant_docpal$CODS_PAISU == i]
}

for(i in cant_docpre$CODS_PAISU){
  codigos[codigos[,1] == i, 18] = cant_docpre$Total[cant_docpre$CODS_PAISU == i]
}


head(codigos) #Vista previa de los datos

# Adicionar datos ordenados de la matriz en el JSON

countries@data$CANT_DOC <- as.numeric(codigos[,2])
countries@data$LONGITUD <- as.numeric(codigos[,3])
countries@data$LATITUD <- as.numeric(codigos[,4])
countries@data$NOMBRE <- codigos[,5]
countries@data$CANT_DOCAMA <- as.numeric(codigos[,6])
countries@data$CANT_DOCBOG <- as.numeric(codigos[,7])
countries@data$CANT_DOCCAR <- as.numeric(codigos[,8])
countries@data$CANT_DOCDOC <- as.numeric(codigos[,9])
countries@data$CANT_DOCHOM <- as.numeric(codigos[,10])
countries@data$CANT_DOCMAE <- as.numeric(codigos[,11])
countries@data$CANT_DOCMANI <- as.numeric(codigos[,12])
countries@data$CANT_DOCMED <- as.numeric(codigos[,13])
countries@data$CANT_DOCMEDE <- as.numeric(codigos[,14])
countries@data$CANT_DOCMUJ <- as.numeric(codigos[,15])
countries@data$CANT_DOCORI <- as.numeric(codigos[,16])
countries@data$CANT_DOCPAL <- as.numeric(codigos[,17])
countries@data$CANT_DOCPRE <- as.numeric(codigos[,18])

## Se omiten países con información nula de geolocalización, inexistentes o no oficiales.

countries <- countries[-c(40,69, 91,140, 143,148, 176),]

### Cambio para Francia de centroide afectado por isla en el Meditarraneo

countries@data[56,]$LONGITUD <- 2.413913
countries@data[56,]$LATITUD <- 46.766583


#### Base de mapa

#ESRI es un proveedor de bases de mapas con licencia pública
esri <- grep("^Esri", providers, value = T)
esri<- esri[c(11,2,10)]
esri
names.esri <- c("Ligero","Street","Satélite <br>&nbsp;&nbsp;&nbsp;&nbsp; NatGeo")

## Sedes de la Universidad

Sede <- c("Medellín", "Bogotá", "Manizales", "Tumaco", "Palmira", "Arauca", "Caribe", "Amazonas")

sedes <- cabeceras %>% filter(code_mun %in% c(5001, 11001, 17001, 52835, 76520, 81001, 88001, 91001)) %>% mutate(Sede = Sede)

sedes$longitud <- as.numeric(str_replace(sedes$longitud, ",", "."))
sedes$latitud  <- as.numeric(str_replace(sedes$latitud, ",", "."))

sedeIcon <- makeAwesomeIcon (markerColor = "green", iconColor = "white", 
                             fontFamily = "Leonardian", text = "un")


# Aparece el nombre de la sede

label_sede <- sprintf("<strong>%s %s</strong>", 
                      "Sede ", sedes$Sede)%>% lapply(htmltools::HTML)

# Etiquetas con información para cada país

labels_countries <- sprintf(
  "<strong> %s </strong> <br/> %g  docentes" , 
  countries@data$NOMBRE ,  countries@data$CANT_DOC
) %>% lapply(htmltools::HTML)

#### Personalización de mapa

# Paleta de colores

colores <- c('#ffffcc','#c2e699','#78c679','#31a354','#006837')
binpal <- colorBin("Blues", bins=c(0,1, 11, 101, 501, Inf), palette = colores)

# código de mapa con leaflet

countriesmap <- leaflet(countries)

for (k in c(1 , 2 , 3)) {
  countriesmap <- countriesmap %>% addProviderTiles(names(esri[k])  , group = names.esri[k] ,  options =  providerTileOptions(minZoom = 2))
}

countriesmap <- countriesmap %>%
  addLayersControl(baseGroups = names.esri , 
                   options = layersControlOptions(collapsed = T)) %>%
  setView(lat = 0,  lng = 0,  zoom = 2)%>%
  addPolygons(stroke = T ,  smoothFactor = 0.05 ,   color = "gray" ,  weight = 1 , 
              fillColor = ~binpal(CANT_DOC) , 
              label = labels_countries ,  labelOptions = labelOptions( style = list("font-weight" = "normal" ,  padding = "3px 8px") ,   textsize = "12px" , direction = "auto"   ) ,  dashArray = "3" , 
              fillOpacity = 0.6 , 
              highlight = highlightOptions(
                weight = 5 , 
                color = "#666" , 
                dashArray = "" , 
                fillOpacity = 0.7 , 
                bringToFront = F)) %>%   
  addLegend("bottomright" ,  values = ~CANT_DOC, colors = colores ,  
            title = "Docentes",  opacity = 1,  bins=c(0,1, 11, 101, 501, Inf), labels = c("0","1 - 10 ", "11 - 100","101 - 500","Más de 500"))%>%
  addMiniMap(position = "bottomleft" , zoomAnimation = T ,  toggleDisplay = T ,  autoToggleDisplay = T)%>%
  addEasyButton(easyButton(
    icon = "glyphicon-screenshot" ,  title = "Retornar" , 
    onClick = JS("function(btn ,  map){ map.setView(L.latLng(0 , 0) ,  2); }")))%>%
  addLabelOnlyMarkers(lat = ~LATITUD,  lng = ~LONGITUD , label =  ~paste0(countries$NOMBRE) ,   labelOptions = labelOptions(noHide = T ,  direction = 'top' ,  textOnly = T , textsize = "10px") )%>%
  addScaleBar(position = "bottomleft" , scaleBarOptions(metric = T ,  imperial =  F))


countriesmap

saveWidget(countriesmap , file = file.path(getwd() ,  "Mapas" ,  "countriesmapgeneral")  ,  selfcontained = F , libdir = "libraryjs")


### Mapa por nivel de formación

labels_pregrado <- sprintf(
  "<strong> %s </strong> <br/> %g  docentes" , 
  countries@data$NOMBRE ,  countries@data$CANT_DOCPRE
) %>% lapply(htmltools::HTML)

labels_doctorado <- sprintf(
  "<strong> %s </strong> <br/> %g  docentes" , 
  countries@data$NOMBRE ,  countries@data$CANT_DOCDOC
) %>% lapply(htmltools::HTML)


labels_maestria <- sprintf(
  "<strong> %s </strong> <br/> %g  docentes" , 
  countries@data$NOMBRE ,  countries@data$CANT_DOCMAE
) %>% lapply(htmltools::HTML)

labels_medica <- sprintf(
  "<strong> %s </strong> <br/> %g  docentes" , 
  countries@data$NOMBRE ,  countries@data$CANT_DOCMED
) %>% lapply(htmltools::HTML)


# Mapa

countriesmapnivel <- leaflet(countries)

for (k in c(1 , 2 , 3)) {
  countriesmapnivel <- countriesmapnivel %>% addProviderTiles(names(esri[k])  , group = names.esri[k] ,  options =  providerTileOptions(minZoom = 2))
}

countriesmapnivel <- countriesmapnivel %>%
  addLayersControl(baseGroups = names.esri , overlayGroups = c("Todos", "Pregrado/Especialización", "Maestría", "Especialidad Médica", "Doctorado"),
                   options = layersControlOptions(collapsed = T)) %>%
  setView(lat = 42.796626414,  lng = 12.07001339,  zoom = 2)%>%
  addPolygons(stroke = T ,  smoothFactor = 0.05 ,   color = "gray" ,  weight = 1 , 
              fillColor = ~binpal(CANT_DOC) , 
              label = labels_countries ,  labelOptions = labelOptions( style = list("font-weight" = "normal" ,  padding = "3px 8px") ,   textsize = "12px" , direction = "auto"   ) ,  dashArray = "3" , 
              fillOpacity = 0.6 , 
              highlight = highlightOptions(
                weight = 5 , 
                color = "#666" , 
                dashArray = "" , 
                fillOpacity = 0.7 , 
                bringToFront = F), group = "Todos") %>%  
  addPolygons(stroke = T ,  smoothFactor = 0.05 ,   color = "gray" ,  weight = 1 , 
              fillColor = ~binpal(CANT_DOCPRE) , 
              label = labels_pregrado ,  labelOptions = labelOptions( style = list("font-weight" = "normal" ,  padding = "3px 8px") ,   textsize = "12px" , direction = "auto"   ) ,  dashArray = "3" , 
              fillOpacity = 0.6 , 
              highlight = highlightOptions(
                weight = 5 , 
                color = "#666" , 
                dashArray = "" , 
                fillOpacity = 0.7 , 
                bringToFront = F), group = "Pregrado/Especialización") %>%
  addPolygons(stroke = T ,  smoothFactor = 0.05 ,   color = "gray" ,  weight = 1 , 
              fillColor = ~binpal(CANT_DOCMED) , 
              label = labels_medica ,  labelOptions = labelOptions( style = list("font-weight" = "normal" ,  padding = "3px 8px") ,   textsize = "12px" , direction = "auto"   ) ,  dashArray = "3" , 
              fillOpacity = 0.6 , 
              highlight = highlightOptions(
                weight = 5 , 
                color = "#666" , 
                dashArray = "" , 
                fillOpacity = 0.7 , 
                bringToFront = F), group = "Especialidad Médica") %>%
  addPolygons(stroke = T ,  smoothFactor = 0.05 ,   color = "gray" ,  weight = 1 , 
              fillColor = ~binpal(CANT_DOCMAE) , 
              label = labels_maestria ,  labelOptions = labelOptions( style = list("font-weight" = "normal" ,  padding = "3px 8px") ,   textsize = "12px" , direction = "auto"   ) ,  dashArray = "3" , 
              fillOpacity = 0.6 , 
              highlight = highlightOptions(
                weight = 5 , 
                color = "#666" , 
                dashArray = "" , 
                fillOpacity = 0.7 , 
                bringToFront = F), group = "Maestría") %>%
  addPolygons(stroke = T ,  smoothFactor = 0.05 ,   color = "gray" ,  weight = 1 , 
              fillColor = ~binpal(CANT_DOCDOC) , 
              label = labels_doctorado ,  labelOptions = labelOptions( style = list("font-weight" = "normal" ,  padding = "3px 8px") ,   textsize = "12px" , direction = "auto"   ) ,  dashArray = "3" , 
              fillOpacity = 0.6 , 
              highlight = highlightOptions(
                weight = 5 , 
                color = "#666" , 
                dashArray = "" , 
                fillOpacity = 0.7 , 
                bringToFront = F), group = "Doctorado") %>%
  addLegend("bottomright" ,  values = ~CANT_DOC, colors = colores ,  
            title = "Docentes",  opacity = 1,  bins=c(0,1, 11, 101, 501, Inf), labels = c("0","1 - 10 ", "11 - 100","101 - 500","Más de 500"))%>%
  addMiniMap(position = "bottomleft" , zoomAnimation = T ,  toggleDisplay = T ,  autoToggleDisplay = T)%>%
  addEasyButton(easyButton(
    icon = "glyphicon-screenshot" ,  title = "Retornar" , 
    onClick = JS("function(btn ,  map){ map.setView(L.latLng(42.796626414 , 12.07001339) ,  2); }")))%>%
  addLabelOnlyMarkers(lat = ~LATITUD,  lng = ~LONGITUD , label =  ~paste0(countries$NOMBRE) ,   labelOptions = labelOptions(noHide = T ,  direction = 'top' ,  textOnly = T , textsize = "10px") )%>%
  showGroup("Todos")%>%
  hideGroup("Pregrado/Especialización")%>%
  hideGroup("Especialidad Médica")%>%
  hideGroup("Maestría")%>%
  hideGroup("Doctorado")%>%
  addScaleBar(position = "bottomleft" , scaleBarOptions(metric = T ,  imperial =  F))


countriesmapnivel

saveWidget(countriesmapnivel , file = file.path(getwd() ,  "Mapas" ,  "countriesmapnivel")  ,  selfcontained = F , libdir = "libraryjs")



### Mapa por sede de la universidad

labels_amazonas <- sprintf(
  "<strong> %s </strong> <br/> %g  docentes" , 
  countries@data$NOMBRE ,  countries@data$CANT_DOCAMA
) %>% lapply(htmltools::HTML)

labels_bogota <- sprintf(
  "<strong> %s </strong> <br/> %g  docentes" , 
  countries@data$NOMBRE ,  countries@data$CANT_DOCBOG
) %>% lapply(htmltools::HTML)

labels_caribe <- sprintf(
  "<strong> %s </strong> <br/> %g  docentes" , 
  countries@data$NOMBRE ,  countries@data$CANT_DOCCAR
) %>% lapply(htmltools::HTML)


labels_manizales <- sprintf(
  "<strong> %s </strong> <br/> %g  docentes" , 
  countries@data$NOMBRE ,  countries@data$CANT_DOCMANI
) %>% lapply(htmltools::HTML)

labels_medellin <- sprintf(
  "<strong> %s </strong> <br/> %g  docentes" , 
  countries@data$NOMBRE ,  countries@data$CANT_DOCMEDE
) %>% lapply(htmltools::HTML)

labels_orinoquia <- sprintf(
  "<strong> %s </strong> <br/> %g  docentes" , 
  countries@data$NOMBRE ,  countries@data$CANT_DOCORI
) %>% lapply(htmltools::HTML)

labels_palmira <- sprintf(
  "<strong> %s </strong> <br/> %g  docentes" , 
  countries@data$NOMBRE ,  countries@data$CANT_DOCPAL
) %>% lapply(htmltools::HTML)



countriesmapsede <- leaflet(countries)

for (k in c(1 , 2 , 3)) {
  countriesmapsede <- countriesmapsede %>% addProviderTiles(names(esri[k])  , group = names.esri[k] ,  options =  providerTileOptions(minZoom = 2))
}

countriesmapsede <- countriesmapsede %>%
  addLayersControl(baseGroups = names.esri , overlayGroups = c("Todas", "Bogotá", "Medellín", "Manizales", "Palmira", "Orinoquía", "Amazonía", "Caribe"),
                   options = layersControlOptions(collapsed = T)) %>%
  setView(lat = 42.796626414 , lng = 12.07001339,  zoom = 2)%>%
  addPolygons(stroke = T ,  smoothFactor = 0.05 ,   color = "gray" ,  weight = 1 , 
              fillColor = ~binpal(CANT_DOC) , 
              label = labels_countries ,  labelOptions = labelOptions( style = list("font-weight" = "normal" ,  padding = "3px 8px") ,   textsize = "12px" , direction = "auto"   ) ,  dashArray = "3" , 
              fillOpacity = 0.6 , 
              highlight = highlightOptions(
                weight = 5 , 
                color = "#666" , 
                dashArray = "" , 
                fillOpacity = 0.7 , 
                bringToFront = F), group = "Todas") %>%  
  addPolygons(stroke = T ,  smoothFactor = 0.05 ,   color = "gray" ,  weight = 1 , 
              fillColor = ~binpal(CANT_DOCCAR) , 
              label = labels_caribe ,  labelOptions = labelOptions( style = list("font-weight" = "normal" ,  padding = "3px 8px") ,   textsize = "12px" , direction = "auto"   ) ,  dashArray = "3" , 
              fillOpacity = 0.6 , 
              highlight = highlightOptions(
                weight = 5 , 
                color = "#666" , 
                dashArray = "" , 
                fillOpacity = 0.7 , 
                bringToFront = F), group = "Caribe") %>%
  addPolygons(stroke = T ,  smoothFactor = 0.05 ,   color = "gray" ,  weight = 1 , 
              fillColor = ~binpal(CANT_DOCORI) , 
              label = labels_orinoquia ,  labelOptions = labelOptions( style = list("font-weight" = "normal" ,  padding = "3px 8px") ,   textsize = "12px" , direction = "auto"   ) ,  dashArray = "3" , 
              fillOpacity = 0.6 , 
              highlight = highlightOptions(
                weight = 5 , 
                color = "#666" , 
                dashArray = "" , 
                fillOpacity = 0.7 , 
                bringToFront = F), group = "Orinoquía") %>%
  addPolygons(stroke = T ,  smoothFactor = 0.05 ,   color = "gray" ,  weight = 1 , 
              fillColor = ~binpal(CANT_DOCBOG) , 
              label = labels_bogota ,  labelOptions = labelOptions( style = list("font-weight" = "normal" ,  padding = "3px 8px") ,   textsize = "12px" , direction = "auto"   ) ,  dashArray = "3" , 
              fillOpacity = 0.6 , 
              highlight = highlightOptions(
                weight = 5 , 
                color = "#666" , 
                dashArray = "" , 
                fillOpacity = 0.7 , 
                bringToFront = F), group = "Bogotá") %>%
  addPolygons(stroke = T ,  smoothFactor = 0.05 ,   color = "gray" ,  weight = 1 , 
              fillColor = ~binpal(CANT_DOCMEDE) , 
              label = labels_medellin ,  labelOptions = labelOptions( style = list("font-weight" = "normal" ,  padding = "3px 8px") ,   textsize = "12px" , direction = "auto"   ) ,  dashArray = "3" , 
              fillOpacity = 0.6 , 
              highlight = highlightOptions(
                weight = 5 , 
                color = "#666" , 
                dashArray = "" , 
                fillOpacity = 0.7 , 
                bringToFront = F), group = "Medellín") %>%
  addPolygons(stroke = T ,  smoothFactor = 0.05 ,   color = "gray" ,  weight = 1 , 
              fillColor = ~binpal(CANT_DOCMANI) , 
              label = labels_manizales ,  labelOptions = labelOptions( style = list("font-weight" = "normal" ,  padding = "3px 8px") ,   textsize = "12px" , direction = "auto"   ) ,  dashArray = "3" , 
              fillOpacity = 0.6 , 
              highlight = highlightOptions(
                weight = 5 , 
                color = "#666" , 
                dashArray = "" , 
                fillOpacity = 0.7 , 
                bringToFront = F), group = "Manizales") %>%
  addPolygons(stroke = T ,  smoothFactor = 0.05 ,   color = "gray" ,  weight = 1 , 
              fillColor = ~binpal(CANT_DOCPAL) , 
              label = labels_palmira ,  labelOptions = labelOptions( style = list("font-weight" = "normal" ,  padding = "3px 8px") ,   textsize = "12px" , direction = "auto"   ) ,  dashArray = "3" , 
              fillOpacity = 0.6 , 
              highlight = highlightOptions(
                weight = 5 , 
                color = "#666" , 
                dashArray = "" , 
                fillOpacity = 0.7 , 
                bringToFront = F), group = "Palmira") %>%
  addPolygons(stroke = T ,  smoothFactor = 0.05 ,   color = "gray" ,  weight = 1 , 
              fillColor = ~binpal(CANT_DOCAMA) , 
              label = labels_amazonas ,  labelOptions = labelOptions( style = list("font-weight" = "normal" ,  padding = "3px 8px") ,   textsize = "12px" , direction = "auto"   ) ,  dashArray = "3" , 
              fillOpacity = 0.6 , 
              highlight = highlightOptions(
                weight = 5 , 
                color = "#666" , 
                dashArray = "" , 
                fillOpacity = 0.7 , 
                bringToFront = F), group = "Amazonía") %>%
  addLegend("bottomright" ,  values = ~CANT_DOC, colors = colores ,  
            title = "Docentes",  opacity = 1,  bins=c(0,1, 11, 101, 501, Inf), labels = c("0","1 - 10 ", "11 - 100","101 - 500","Más de 500"))%>%
  addMiniMap(position = "bottomleft" , zoomAnimation = T ,  toggleDisplay = T ,  autoToggleDisplay = T)%>%
  addEasyButton(easyButton(
    icon = "glyphicon-screenshot" ,  title = "Retornar" , 
    onClick = JS("function(btn ,  map){ map.setView(L.latLng(42.796626414 , 12.07001339) ,  2); }")))%>%
  addLabelOnlyMarkers(lat = ~LATITUD,  lng = ~LONGITUD , label =  ~paste0(countries$NOMBRE) ,   labelOptions = labelOptions(noHide = T ,  direction = 'top' ,  textOnly = T , textsize = "10px") )%>%
  addAwesomeMarkers(lat=sedes[Sede == "Medellín",]$latitud, lng=sedes[Sede == "Medellín",]$longitud, icon = sedeIcon, label= label_sede, labelOptions=labelOptions(     style = list("font-weight" = "large", padding = "3px 8px"),  textsize = "15px", direction = "auto"), group = "Medellín"  )%>%
  addAwesomeMarkers(lat=sedes[Sede == "Bogotá",]$latitud, lng=sedes[Sede == "Bogotá",]$longitud, icon = sedeIcon, label= label_sede, labelOptions=labelOptions(     style = list("font-weight" = "large", padding = "3px 8px"),  textsize = "15px", direction = "auto"), group = "Bogotá"  )%>%
  addAwesomeMarkers(lat=sedes[Sede == "Palmira",]$latitud, lng=sedes[Sede == "Palmira",]$longitud, icon = sedeIcon, label= label_sede, labelOptions=labelOptions(     style = list("font-weight" = "large", padding = "3px 8px"),  textsize = "15px", direction = "auto"), group = "Palmira"  )%>%
  addAwesomeMarkers(lat=sedes[Sede == "Amazonas",]$latitud, lng=sedes[Sede == "Amazonas",]$longitud, icon = sedeIcon, label= label_sede, labelOptions=labelOptions(     style = list("font-weight" = "large", padding = "3px 8px"),  textsize = "15px", direction = "auto"), group = "Amazonía"  )%>%
  addAwesomeMarkers(lat=sedes[Sede == "Caribe",]$latitud, lng=sedes[Sede == "Caribe",]$longitud, icon = sedeIcon, label= label_sede, labelOptions=labelOptions(     style = list("font-weight" = "large", padding = "3px 8px"),  textsize = "15px", direction = "auto"), group = "Caribe"  )%>%
  addAwesomeMarkers(lat=sedes[Sede == "Manizales",]$latitud, lng=sedes[Sede == "Manizales",]$longitud, icon = sedeIcon, label= label_sede, labelOptions=labelOptions(     style = list("font-weight" = "large", padding = "3px 8px"),  textsize = "15px", direction = "auto"), group = "Manizales"  )%>%
  addAwesomeMarkers(lat=sedes[Sede == "Arauca",]$latitud, lng=sedes[Sede == "Arauca",]$longitud, icon = sedeIcon, label= label_sede, labelOptions=labelOptions(     style = list("font-weight" = "large", padding = "3px 8px"),  textsize = "15px", direction = "auto"), group = "Orinoquía"  )%>%
  showGroup("Todas")%>%
  hideGroup("Medellín")%>%
  hideGroup("Bogotá")%>%
  hideGroup("Palmira")%>%
  hideGroup("Amazonía")%>%
  hideGroup("Caribe")%>%
  hideGroup("Orinoquía")%>%
  hideGroup("Manizales")%>%
  addScaleBar(position = "bottomleft" , scaleBarOptions(metric = T ,  imperial =  F))


countriesmapsede

saveWidget(countriesmapsede , file = file.path(getwd() ,  "Mapas" ,  "countriesmapsede")  ,  selfcontained = F , libdir = "libraryjs")


# Mapa por sexo y universidad de origen


docentes2 <- left_join(docentes, centroids, by=("CODS_PAISU"))
docentes2$Longitude2 <- as.numeric(str_replace(docentes2$Longitude2, ",", "."))
docentes2$Latitude2 <- as.numeric(str_replace(docentes2$Latitude2, ",", "."))

docentes2$Longitude[docentes2$CODS_PAISU == "COL"] <- docentes2$Longitude2[docentes2$CODS_PAISU == "COL"]
docentes2$Latitude[docentes2$CODS_PAISU== "COL"] <- docentes2$Latitude2[docentes2$CODS_PAISU == "COL"]

docentes2$Longitude[docentes2$CODS_PAISU == "DEU"] <- docentes2$Longitude2[docentes2$CODS_PAISU == "DEU"]
docentes2$Latitude[docentes2$CODS_PAISU== "DEU"] <- docentes2$Latitude2[docentes2$CODS_PAISU == "DEU"]

labels_hombres <- sprintf(
  "<strong> %s </strong> <br/> %g  docentes" , 
  countries@data$NOMBRE ,  countries@data$CANT_DOCHOM
) %>% lapply(htmltools::HTML)

labels_mujeres <- sprintf(
  "<strong> %s </strong> <br/> %g  docentes" , 
  countries@data$NOMBRE ,  countries@data$CANT_DOCMUJ
) %>% lapply(htmltools::HTML)



icon_sexo <- icons(
  iconUrl = ifelse(docentes2$SEXO =="Hombres",  "Iconos/boy.png","Iconos/girl.png"), 25, 25,
  iconAnchorY = 13, iconAnchorX = 2
)


# Etiquetas para los íconos

labelsexo <- sprintf(
  "%s <strong> %s </strong> </br> %s ",
  "De", docentes2$UNIVERSIDAD, docentes2$FACULTAD_O
) %>% lapply(htmltools::HTML)

labelhombre <- sprintf(
  "%s <strong> %s </strong>",
  "De", docentes[docentes2$SEXO =="Hombres",]$UNIVERSIDAD
) %>% lapply(htmltools::HTML)

labelmujeres <- sprintf(
  "%s <strong> %s </strong>",
  "De", docentes2[docentes2$SEXO =="Mujeres",]$UNIVERSIDAD
) %>% lapply(htmltools::HTML)



icon_hombres <- icons(
  iconUrl = ifelse(docentes2[docentes2$SEXO =="Hombres",]$SEXO =="Hombres",  "Iconos/boy.png", "Iconos/girl.png"), 25, 25,
  iconAnchorY = 13, iconAnchorX = 2
)


icon_mujeres <- icons(
  iconUrl = ifelse(docentes2[docentes2$SEXO =="Mujeres",]$SEXO =="Mujeres",  "Iconos/girl.png", "Iconos/boy.png"), 25, 25,
  iconAnchorY = 13, iconAnchorX = 2
)


countriesmapsexo <- leaflet(countries)

for (k in c(1 , 2 , 3)) {
  countriesmapsexo <- countriesmapsexo %>% addProviderTiles(names(esri[k])  , group = names.esri[k] ,  options =  providerTileOptions(minZoom = 2))
}

countriesmapsexo <- countriesmapsexo %>%
  addLayersControl(baseGroups = names.esri , overlayGroups = c("Ambos", "Hombres", "Mujeres"),
                   options = layersControlOptions(collapsed = T)) %>%
  setView(lat = 42.796626414 , lng = 12.07001339,  zoom = 2)%>%
  addPolygons(stroke = T ,  smoothFactor = 0.05 ,   color = "gray" ,  weight = 1 , 
              fillColor = ~binpal(CANT_DOC) , 
              label = labels_countries ,  labelOptions = labelOptions( style = list("font-weight" = "normal" ,  padding = "3px 8px") ,   textsize = "12px" , direction = "auto"   ) ,  dashArray = "3" , 
              fillOpacity = 0.6 , 
              highlight = highlightOptions(
                weight = 5 , 
                color = "#666" , 
                dashArray = "" , 
                fillOpacity = 0.7 , 
                bringToFront = F), group = "Ambos") %>%  
  addPolygons(stroke = T ,  smoothFactor = 0.05 ,   color = "gray" ,  weight = 1 , 
              fillColor = ~binpal(CANT_DOCHOM) , 
              label = labels_hombres ,  labelOptions = labelOptions( style = list("font-weight" = "normal" ,  padding = "3px 8px") ,   textsize = "12px" , direction = "auto"   ) ,  dashArray = "3" , 
              fillOpacity = 0.6 , 
              highlight = highlightOptions(
                weight = 5 , 
                color = "#666" , 
                dashArray = "" , 
                fillOpacity = 0.7 , 
                bringToFront = F), group = "Hombres") %>%
  addPolygons(stroke = T ,  smoothFactor = 0.05 ,   color = "gray" ,  weight = 1 , 
              fillColor = ~binpal(CANT_DOCMUJ) , 
              label = labels_mujeres ,  labelOptions = labelOptions( style = list("font-weight" = "normal" ,  padding = "3px 8px") ,   textsize = "12px" , direction = "auto"   ) ,  dashArray = "3" , 
              fillOpacity = 0.6 , 
              highlight = highlightOptions(
                weight = 5 , 
                color = "#666" , 
                dashArray = "" , 
                fillOpacity = 0.7 , 
                bringToFront = F), group = "Mujeres") %>%

  addLegend("bottomright" ,  values = ~CANT_DOC, colors = colores ,  
            title = "Docentes",  opacity = 1,  bins=c(0,1, 11, 101, 501, Inf), labels = c("0","1 - 10 ", "11 - 100","101 - 500","Más de 500"))%>%
  addMiniMap(position = "bottomleft" , zoomAnimation = T ,  toggleDisplay = T ,  autoToggleDisplay = T)%>%
  addEasyButton(easyButton(
    icon = "glyphicon-screenshot" ,  title = "Retornar" , 
    onClick = JS("function(btn ,  map){ map.setView(L.latLng(42.796626414 , 12.07001339) ,  2); }")))%>%
  addLabelOnlyMarkers(lat = ~LATITUD,  lng = ~LONGITUD , label =  ~paste0(countries$NOMBRE) ,   labelOptions = labelOptions(noHide = T ,  direction = 'top' ,  textOnly = T , textsize = "10px") )%>%
  hideGroup("Hombres")%>%
  hideGroup("Mujeres")%>%
  showGroup("Ambos")%>%
  addMarkers(data=docentes2,group = "Ambos", lat = ~Latitude, lng = ~Longitude,  label = labelsexo, labelOptions=labelOptions(direction = "auto", riseOnHover =T,     style = list("font-weight" = "normal", padding = "3px 8px")), icon = icon_sexo, clusterOptions = markerClusterOptions(removeOutsideVisibleBounds = T, animate = T, maxClusterRadius = 70,singleMarkerMode= F, animateAddingMarkers = T ), clusterId = "individuos5") %>%
  addMarkers(data=docentes2[docentes2$SEXO =="Hombres",],group = "Hombres", lat = ~Latitude, lng = ~Longitude,  label = labelhombre, labelOptions=labelOptions(direction = "auto", riseOnHover =T,     style = list("font-weight" = "normal", padding = "3px 8px")), icon = icon_hombres, clusterOptions = markerClusterOptions(removeOutsideVisibleBounds = T, animate = T, maxClusterRadius = 70,singleMarkerMode= F, animateAddingMarkers = T )) %>%
  addMarkers(data=docentes2[docentes2$SEXO =="Mujeres",],group = "Mujeres", lat = ~Latitude, lng = ~Longitude,  label = labelmujeres, labelOptions=labelOptions(direction = "auto", riseOnHover =T,     style = list("font-weight" = "normal", padding = "3px 8px")), icon = icon_mujeres, clusterOptions = markerClusterOptions(removeOutsideVisibleBounds = T, animate = T, maxClusterRadius = 70,singleMarkerMode= F, animateAddingMarkers = T )) %>%

  addEasyButton(easyButton(
    states = list(
      easyButtonState(
        stateName="unfrozen-markers",
        icon="ion-toggle",
        title="Freeze Clusters",
        onClick = JS("
                     function(btn, map) {
                     var clusterManager =
                     map.layerManager.getLayer('cluster', 'individuos5');
                     clusterManager.freezeAtZoom();
                     btn.state('frozen-markers');
                     }")
      ),
      easyButtonState(
        stateName="frozen-markers",
        icon="ion-toggle-filled",
        title="UnFreeze Clusters",
        onClick = JS("
                     function(btn, map) {
                     var clusterManager =
                     map.layerManager.getLayer('cluster', 'individuos5');
                     clusterManager.unfreeze();
                     btn.state('unfrozen-markers');
                     }")
      )
        )
        ))%>%
  addScaleBar(position = "bottomleft" , scaleBarOptions(metric = T ,  imperial =  F))


countriesmapsexo

saveWidget(countriesmapsexo , file = file.path(getwd() ,  "Mapas" ,  "countriesmapsexo")  ,  selfcontained = F , libdir = "libraryjs")


