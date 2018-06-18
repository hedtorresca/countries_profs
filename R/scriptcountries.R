library(leaflet)
library(rgdal)
library(htmlwidgets)
library(tidyverse)
library(rjson)
library(readxl)
library(extrafont)

divipola.R <- read.table("DIVIPOLA_20160930.csv", sep=";", header=T)


# Base de datos con solo cabeceras municipales 

cabeceras <- divipola.R %>% select(code_dept=Código.Departamento,
                                   code_mun=Código.Municipio,
                                   departamentos=Nombre.Departamento,
                                   municipios=Nombre.Municipio,
                                   tipo_centro=Tipo.Centro.Poblado,
                                   longitud=Longitud,
                                   latitud=Latitud) %>% 
  filter(tipo_centro == "CABECERA MUNICIPAL (CM)")  


docentes <- read_xlsx("Docentes.xlsx") 

centroids <- read_xlsx("centroids.xlsx") 

spanish <- read_xlsx("centroids.xlsx", sheet = "Spanish")

spanish <- na.omit(spanish)

cant_doc <- docentes %>% group_by(CODS_PAISU, PAIS_U) %>% summarise(Total=n())

cant_docpre <- docentes %>% filter(docentes$FORMACION == "Pregrado"|  docentes$FORMACION == "Especialización") %>% group_by(CODS_PAISU, PAIS_U) %>% summarise(Total=n())
cant_docdoc <- docentes %>% filter(  docentes$FORMACION == "Doctorado") %>% group_by(CODS_PAISU, PAIS_U) %>% summarise(Total=n())
cant_docmae <- docentes %>% filter(docentes$FORMACION == "Maestría") %>% group_by(CODS_PAISU, PAIS_U) %>% summarise(Total=n())
cant_docmed <- docentes %>% filter(docentes$FORMACION == "Especialidad Médica") %>% group_by(CODS_PAISU, PAIS_U) %>% summarise(Total=n())

cant_docbog <- docentes %>% filter(docentes$SEDE == "Bogotá") %>% group_by(CODS_PAISU, PAIS_U) %>% summarise(Total=n())
cant_docmede <- docentes %>% filter(docentes$SEDE == "Medellín") %>% group_by(CODS_PAISU, PAIS_U) %>% summarise(Total=n())
cant_docpal <- docentes %>% filter(docentes$SEDE == "Palmira") %>% group_by(CODS_PAISU, PAIS_U) %>% summarise(Total=n())
cant_docmani <- docentes %>% filter(docentes$SEDE == "Manizales") %>% group_by(CODS_PAISU, PAIS_U) %>% summarise(Total=n())
cant_docama <- docentes %>% filter(docentes$SEDE == "Amazonía") %>% group_by(CODS_PAISU, PAIS_U) %>% summarise(Total=n())
cant_docori <- docentes %>% filter(docentes$SEDE == "Orinoquía") %>% group_by(CODS_PAISU, PAIS_U) %>% summarise(Total=n())
cant_doccar <- docentes %>% filter(docentes$SEDE == "Caribe") %>% group_by(CODS_PAISU, PAIS_U) %>% summarise(Total=n())


cant_dochom <- docentes %>% filter(docentes$SEXO == "Hombres") %>% group_by(CODS_PAISU, PAIS_U) %>% summarise(Total=n())
cant_docmuj <- docentes %>% filter(docentes$SEXO == "Mujeres") %>% group_by(CODS_PAISU, PAIS_U) %>% summarise(Total=n())




countries <- rgdal::readOGR("countriesgeo.json")

countries@data

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

codigos

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


countries <- countries[-c(40,69, 91,140, 143,148, 176),]

### Cambio para Francia
countries@data[56,]$LONGITUD <- 2.413913
countries@data[56,]$LATITUD <- 46.766583


#ESRI es un proveedor de bases de mapas con licencia pública
esri <- grep("^Esri", providers, value = T)
esri<- esri[c(11,2,10)]
esri
names.esri <- c("Ligero","Street","Satélite <br>&nbsp;&nbsp;&nbsp;&nbsp; NatGeo")

Sede <- c("Medellín", "Bogotá", "Manizales", "Tumaco", "Palmira", "Arauca", "Caribe", "Amazonas")

sedes <- cabeceras %>% filter(code_mun %in% c(5001, 11001, 17001, 52835, 76520, 81001, 88001, 91001)) %>% mutate(Sede = Sede)

##############################################################################
##############################################################################

sedes$longitud <- as.numeric(str_replace(sedes$longitud, ",", "."))
sedes$latitud  <- as.numeric(str_replace(sedes$latitud, ",", "."))

#############################################################################
#############################################################################




#font_import();n

sedeIcon <- makeAwesomeIcon (markerColor = "green", iconColor = "white", 
                             fontFamily = "Leonardian", text = "un")

# aparece el nombre de la sede
label_sede <- sprintf("<strong>%s %s</strong>", 
                      "Sede ", sedes$Sede)%>% lapply(htmltools::HTML)

labels_countries <- sprintf(
  "<strong> %s </strong> <br/> %g  docentes" , 
  countries@data$NOMBRE ,  countries@data$CANT_DOC
) %>% lapply(htmltools::HTML)


colores <- c('#ffffcc','#c2e699','#78c679','#31a354','#006837')
binpal <- colorBin("Blues", bins=c(0,1, 11, 101, 501, Inf), palette = colores)

countriesmap <- leaflet(countries)

for (k in c(1 , 2 , 3)) {
  countriesmap <- countriesmap %>% addProviderTiles(names(esri[k])  , group = names.esri[k] ,  options =  providerTileOptions(minZoom = 2))
}

countriesmap <- countriesmap %>%
  addLayersControl(baseGroups = names.esri , 
                   options = layersControlOptions(collapsed = T)) %>%
  addPolygons(stroke = T ,  smoothFactor = 0.05 ,   color = "gray" ,  weight = 1 , 
              fillColor = ~binpal(CANT_DOC) , 
              label = labels_countries ,  labelOptions = labelOptions( style = list("font-weight" = "normal" ,  padding = "3px 8px") ,   textsize = "12px" , direction = "auto"   ) ,  dashArray = "3" , 
              fillOpacity = 1 , 
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
    onClick = JS("function(btn ,  map){ map.setView(L.latLng(42.796626414 , 12.07001339) ,  2.5); }")))%>%
  addLabelOnlyMarkers(lat = ~LATITUD,  lng = ~LONGITUD , label =  ~paste0(countries$NOMBRE) ,   labelOptions = labelOptions(noHide = T ,  direction = 'top' ,  textOnly = T , textsize = "10px") )%>%
  addScaleBar(position = "bottomleft" , scaleBarOptions(metric = T ,  imperial =  F))


countriesmap



labels_pregrado <- sprintf(
  "<strong> %s </strong> <br/> %g  docentes" , 
  countries@data$NOMBRE ,  countries@data$CANT_DOCPRE
) %>% lapply(htmltools::HTML)

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

labels_doctorado <- sprintf(
  "<strong> %s </strong> <br/> %g  docentes" , 
  countries@data$NOMBRE ,  countries@data$CANT_DOCDOC
) %>% lapply(htmltools::HTML)

labels_hombres <- sprintf(
  "<strong> %s </strong> <br/> %g  docentes" , 
  countries@data$NOMBRE ,  countries@data$CANT_DOCHOM
) %>% lapply(htmltools::HTML)

labels_maestria <- sprintf(
  "<strong> %s </strong> <br/> %g  docentes" , 
  countries@data$NOMBRE ,  countries@data$CANT_DOCMAE
) %>% lapply(htmltools::HTML)

labels_manizales <- sprintf(
  "<strong> %s </strong> <br/> %g  docentes" , 
  countries@data$NOMBRE ,  countries@data$CANT_DOCMANI
) %>% lapply(htmltools::HTML)

labels_medica <- sprintf(
  "<strong> %s </strong> <br/> %g  docentes" , 
  countries@data$NOMBRE ,  countries@data$CANT_DOCMED
) %>% lapply(htmltools::HTML)

labels_medellin <- sprintf(
  "<strong> %s </strong> <br/> %g  docentes" , 
  countries@data$NOMBRE ,  countries@data$CANT_DOCMEDE
) %>% lapply(htmltools::HTML)

labels_mujeres <- sprintf(
  "<strong> %s </strong> <br/> %g  docentes" , 
  countries@data$NOMBRE ,  countries@data$CANT_DOCMUJ
) %>% lapply(htmltools::HTML)

labels_orinoquia <- sprintf(
  "<strong> %s </strong> <br/> %g  docentes" , 
  countries@data$NOMBRE ,  countries@data$CANT_DOCORI
) %>% lapply(htmltools::HTML)

labels_palmira <- sprintf(
  "<strong> %s </strong> <br/> %g  docentes" , 
  countries@data$NOMBRE ,  countries@data$CANT_DOCPAL
) %>% lapply(htmltools::HTML)



colores <- c('#ffffcc','#c2e699','#78c679','#31a354','#006837')
binpal <- colorBin("Blues", bins=c(0,1, 11, 101, 501, Inf), palette = colores)

countriesmap <- leaflet(countries)

for (k in c(1 , 2 , 3)) {
  countriesmap <- countriesmap %>% addProviderTiles(names(esri[k])  , group = names.esri[k] ,  options =  providerTileOptions(minZoom = 2))
}

countriesmap <- countriesmap %>%
  addLayersControl(baseGroups = names.esri , 
                   options = layersControlOptions(collapsed = T)) %>%
  addPolygons(stroke = T ,  smoothFactor = 0.05 ,   color = "gray" ,  weight = 1 , 
              fillColor = ~binpal(CANT_DOC) , 
              label = labels_countries ,  labelOptions = labelOptions( style = list("font-weight" = "normal" ,  padding = "3px 8px") ,   textsize = "12px" , direction = "auto"   ) ,  dashArray = "3" , 
              fillOpacity = 1 , 
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
    onClick = JS("function(btn ,  map){ map.setView(L.latLng(42.796626414 , 12.07001339) ,  2.5); }")))%>%
  addLabelOnlyMarkers(lat = ~LATITUD,  lng = ~LONGITUD , label =  ~paste0(countries$NOMBRE) ,   labelOptions = labelOptions(noHide = T ,  direction = 'top' ,  textOnly = T , textsize = "10px") )%>%
  addScaleBar(position = "bottomleft" , scaleBarOptions(metric = T ,  imperial =  F))


countriesmap



countriesmapsede <- leaflet(countries)

for (k in c(1 , 2 , 3)) {
  countriesmapsede <- countriesmapsede %>% addProviderTiles(names(esri[k])  , group = names.esri[k] ,  options =  providerTileOptions(minZoom = 2))
}

countriesmapsede <- countriesmapsede %>%
  addLayersControl(baseGroups = names.esri , overlayGroups = c("Todas", "Bogotá", "Medellín", "Manizales", "Palmira", "Orinoquía", "Amazonía", "Caribe"),
                   options = layersControlOptions(collapsed = T)) %>%
  addPolygons(stroke = T ,  smoothFactor = 0.05 ,   color = "gray" ,  weight = 1 , 
              fillColor = ~binpal(CANT_DOC) , 
              label = labels_countries ,  labelOptions = labelOptions( style = list("font-weight" = "normal" ,  padding = "3px 8px") ,   textsize = "12px" , direction = "auto"   ) ,  dashArray = "3" , 
              fillOpacity = 1 , 
              highlight = highlightOptions(
                weight = 5 , 
                color = "#666" , 
                dashArray = "" , 
                fillOpacity = 0.7 , 
                bringToFront = F), group = "Todas") %>%  
  addPolygons(stroke = T ,  smoothFactor = 0.05 ,   color = "gray" ,  weight = 1 , 
              fillColor = ~binpal(CANT_DOCCAR) , 
              label = labels_caribe ,  labelOptions = labelOptions( style = list("font-weight" = "normal" ,  padding = "3px 8px") ,   textsize = "12px" , direction = "auto"   ) ,  dashArray = "3" , 
              fillOpacity = 1 , 
              highlight = highlightOptions(
                weight = 5 , 
                color = "#666" , 
                dashArray = "" , 
                fillOpacity = 0.7 , 
                bringToFront = F), group = "Caribe") %>%
  addPolygons(stroke = T ,  smoothFactor = 0.05 ,   color = "gray" ,  weight = 1 , 
              fillColor = ~binpal(CANT_DOCORI) , 
              label = labels_orinoquia ,  labelOptions = labelOptions( style = list("font-weight" = "normal" ,  padding = "3px 8px") ,   textsize = "12px" , direction = "auto"   ) ,  dashArray = "3" , 
              fillOpacity = 1 , 
              highlight = highlightOptions(
                weight = 5 , 
                color = "#666" , 
                dashArray = "" , 
                fillOpacity = 0.7 , 
                bringToFront = F), group = "Orinoquía") %>%
  addPolygons(stroke = T ,  smoothFactor = 0.05 ,   color = "gray" ,  weight = 1 , 
              fillColor = ~binpal(CANT_DOCBOG) , 
              label = labels_bogota ,  labelOptions = labelOptions( style = list("font-weight" = "normal" ,  padding = "3px 8px") ,   textsize = "12px" , direction = "auto"   ) ,  dashArray = "3" , 
              fillOpacity = 1 , 
              highlight = highlightOptions(
                weight = 5 , 
                color = "#666" , 
                dashArray = "" , 
                fillOpacity = 0.7 , 
                bringToFront = F), group = "Bogotá") %>%
  addPolygons(stroke = T ,  smoothFactor = 0.05 ,   color = "gray" ,  weight = 1 , 
              fillColor = ~binpal(CANT_DOCMEDE) , 
              label = labels_medellin ,  labelOptions = labelOptions( style = list("font-weight" = "normal" ,  padding = "3px 8px") ,   textsize = "12px" , direction = "auto"   ) ,  dashArray = "3" , 
              fillOpacity = 1 , 
              highlight = highlightOptions(
                weight = 5 , 
                color = "#666" , 
                dashArray = "" , 
                fillOpacity = 0.7 , 
                bringToFront = F), group = "Medellín") %>%
  addPolygons(stroke = T ,  smoothFactor = 0.05 ,   color = "gray" ,  weight = 1 , 
              fillColor = ~binpal(CANT_DOCMANI) , 
              label = labels_manizales ,  labelOptions = labelOptions( style = list("font-weight" = "normal" ,  padding = "3px 8px") ,   textsize = "12px" , direction = "auto"   ) ,  dashArray = "3" , 
              fillOpacity = 1 , 
              highlight = highlightOptions(
                weight = 5 , 
                color = "#666" , 
                dashArray = "" , 
                fillOpacity = 0.7 , 
                bringToFront = F), group = "Manizales") %>%
  addPolygons(stroke = T ,  smoothFactor = 0.05 ,   color = "gray" ,  weight = 1 , 
              fillColor = ~binpal(CANT_DOCPAL) , 
              label = labels_palmira ,  labelOptions = labelOptions( style = list("font-weight" = "normal" ,  padding = "3px 8px") ,   textsize = "12px" , direction = "auto"   ) ,  dashArray = "3" , 
              fillOpacity = 1 , 
              highlight = highlightOptions(
                weight = 5 , 
                color = "#666" , 
                dashArray = "" , 
                fillOpacity = 0.7 , 
                bringToFront = F), group = "Palmira") %>%
  addPolygons(stroke = T ,  smoothFactor = 0.05 ,   color = "gray" ,  weight = 1 , 
              fillColor = ~binpal(CANT_DOCAMA) , 
              label = labels_amazonas ,  labelOptions = labelOptions( style = list("font-weight" = "normal" ,  padding = "3px 8px") ,   textsize = "12px" , direction = "auto"   ) ,  dashArray = "3" , 
              fillOpacity = 1 , 
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
    onClick = JS("function(btn ,  map){ map.setView(L.latLng(42.796626414 , 12.07001339) ,  2.5); }")))%>%
  addLabelOnlyMarkers(lat = ~LATITUD,  lng = ~LONGITUD , label =  ~paste0(countries$NOMBRE) ,   labelOptions = labelOptions(noHide = T ,  direction = 'top' ,  textOnly = T , textsize = "10px") )%>%
  addAwesomeMarkers(lat=sedes$latitud, lng=sedes$longitud, icon = sedeIcon, label= label_sede, labelOptions=labelOptions(     style = list("font-weight" = "large", padding = "3px 8px"),  textsize = "15px", direction = "auto"), group = "Todas"  )%>%
  addAwesomeMarkers(lat=sedes[Sede == "Medellín",]$latitud, lng=sedes[Sede == "Medellín",]$longitud, icon = sedeIcon, label= label_sede, labelOptions=labelOptions(     style = list("font-weight" = "large", padding = "3px 8px"),  textsize = "15px", direction = "auto"), group = "Medellín"  )%>%
  addAwesomeMarkers(lat=sedes[Sede == "Bogotá",]$latitud, lng=sedes[Sede == "Bogotá",]$longitud, icon = sedeIcon, label= label_sede, labelOptions=labelOptions(     style = list("font-weight" = "large", padding = "3px 8px"),  textsize = "15px", direction = "auto"), group = "Bogotá"  )%>%
  addAwesomeMarkers(lat=sedes[Sede == "Palmira",]$latitud, lng=sedes[Sede == "Palmira",]$longitud, icon = sedeIcon, label= label_sede, labelOptions=labelOptions(     style = list("font-weight" = "large", padding = "3px 8px"),  textsize = "15px", direction = "auto"), group = "Palmira"  )%>%
  addAwesomeMarkers(lat=sedes[Sede == "Amazonía",]$latitud, lng=sedes[Sede == "Amazonía",]$longitud, icon = sedeIcon, label= label_sede, labelOptions=labelOptions(     style = list("font-weight" = "large", padding = "3px 8px"),  textsize = "15px", direction = "auto"), group = "Amazonía"  )%>%
  addAwesomeMarkers(lat=sedes[Sede == "Caribe",]$latitud, lng=sedes[Sede == "Caribe",]$longitud, icon = sedeIcon, label= label_sede, labelOptions=labelOptions(     style = list("font-weight" = "large", padding = "3px 8px"),  textsize = "15px", direction = "auto"), group = "Caribe"  )%>%
  addAwesomeMarkers(lat=sedes[Sede == "Manizales",]$latitud, lng=sedes[Sede == "Manizales",]$longitud, icon = sedeIcon, label= label_sede, labelOptions=labelOptions(     style = list("font-weight" = "large", padding = "3px 8px"),  textsize = "15px", direction = "auto"), group = "Manizales"  )%>%
  addAwesomeMarkers(lat=sedes[Sede == "Orinoquía",]$latitud, lng=sedes[Sede == "Orinoquía",]$longitud, icon = sedeIcon, label= label_sede, labelOptions=labelOptions(     style = list("font-weight" = "large", padding = "3px 8px"),  textsize = "15px", direction = "auto"), group = "Orinoquía"  )%>%
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




countriesmapnivel <- leaflet(countries)

for (k in c(1 , 2 , 3)) {
  countriesmapnivel <- countriesmapnivel %>% addProviderTiles(names(esri[k])  , group = names.esri[k] ,  options =  providerTileOptions(minZoom = 2))
}

countriesmapnivel <- countriesmapnivel %>%
  addLayersControl(baseGroups = names.esri , overlayGroups = c("Todos", "Pregrado/Especialización", "Maestría", "Especialidad Médica", "Doctorado"),
                   options = layersControlOptions(collapsed = T)) %>%
  addPolygons(stroke = T ,  smoothFactor = 0.05 ,   color = "gray" ,  weight = 1 , 
              fillColor = ~binpal(CANT_DOC) , 
              label = labels_countries ,  labelOptions = labelOptions( style = list("font-weight" = "normal" ,  padding = "3px 8px") ,   textsize = "12px" , direction = "auto"   ) ,  dashArray = "3" , 
              fillOpacity = 1 , 
              highlight = highlightOptions(
                weight = 5 , 
                color = "#666" , 
                dashArray = "" , 
                fillOpacity = 0.7 , 
                bringToFront = F), group = "Todos") %>%  
  addPolygons(stroke = T ,  smoothFactor = 0.05 ,   color = "gray" ,  weight = 1 , 
              fillColor = ~binpal(CANT_DOCPRE) , 
              label = labels_pregrado ,  labelOptions = labelOptions( style = list("font-weight" = "normal" ,  padding = "3px 8px") ,   textsize = "12px" , direction = "auto"   ) ,  dashArray = "3" , 
              fillOpacity = 1 , 
              highlight = highlightOptions(
                weight = 5 , 
                color = "#666" , 
                dashArray = "" , 
                fillOpacity = 0.7 , 
                bringToFront = F), group = "Pregrado/Especialización") %>%
  addPolygons(stroke = T ,  smoothFactor = 0.05 ,   color = "gray" ,  weight = 1 , 
              fillColor = ~binpal(CANT_DOCMED) , 
              label = labels_medica ,  labelOptions = labelOptions( style = list("font-weight" = "normal" ,  padding = "3px 8px") ,   textsize = "12px" , direction = "auto"   ) ,  dashArray = "3" , 
              fillOpacity = 1 , 
              highlight = highlightOptions(
                weight = 5 , 
                color = "#666" , 
                dashArray = "" , 
                fillOpacity = 0.7 , 
                bringToFront = F), group = "Especialidad Médica") %>%
  addPolygons(stroke = T ,  smoothFactor = 0.05 ,   color = "gray" ,  weight = 1 , 
              fillColor = ~binpal(CANT_DOCMAE) , 
              label = labels_maestria ,  labelOptions = labelOptions( style = list("font-weight" = "normal" ,  padding = "3px 8px") ,   textsize = "12px" , direction = "auto"   ) ,  dashArray = "3" , 
              fillOpacity = 1 , 
              highlight = highlightOptions(
                weight = 5 , 
                color = "#666" , 
                dashArray = "" , 
                fillOpacity = 0.7 , 
                bringToFront = F), group = "Maestría") %>%
  addPolygons(stroke = T ,  smoothFactor = 0.05 ,   color = "gray" ,  weight = 1 , 
              fillColor = ~binpal(CANT_DOCDOC) , 
              label = labels_doctorado ,  labelOptions = labelOptions( style = list("font-weight" = "normal" ,  padding = "3px 8px") ,   textsize = "12px" , direction = "auto"   ) ,  dashArray = "3" , 
              fillOpacity = 1 , 
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
    onClick = JS("function(btn ,  map){ map.setView(L.latLng(42.796626414 , 12.07001339) ,  2.5); }")))%>%
  addLabelOnlyMarkers(lat = ~LATITUD,  lng = ~LONGITUD , label =  ~paste0(countries$NOMBRE) ,   labelOptions = labelOptions(noHide = T ,  direction = 'top' ,  textOnly = T , textsize = "10px") )%>%
  showGroup("Todos")%>%
  hideGroup("Pregrado/Especialización")%>%
  hideGroup("Especialidad Médica")%>%
  hideGroup("Maestría")%>%
  hideGroup("Doctorado")%>%

  addScaleBar(position = "bottomleft" , scaleBarOptions(metric = T ,  imperial =  F))


countriesmapnivel


icon_sexo <- icons(
  iconUrl = ifelse(docentes$SEXO =="Hombres",  "Iconos/boy.png","Iconos/girl.png"), 25, 25,
  iconAnchorY = 13, iconAnchorX = 2
)

labelsexo <- sprintf(
  "%s <strong> %s </strong>",
  "DE", docentes$PAIS_U
) %>% lapply(htmltools::HTML)

icon_hombres <- icons(
  iconUrl = ifelse(docentes$SEXO =="Hombres",  "Iconos/boy.png"), 25, 25,
  iconAnchorY = 13, iconAnchorX = 2
)



icon_mujeres <- icons(
  iconUrl = ifelse(docentes$SEXO =="Mujeres",  "Iconos/girl.png"), 25, 25,
  iconAnchorY = 13, iconAnchorX = 2
)


countriesmapsexo <- leaflet(countries)

for (k in c(1 , 2 , 3)) {
  countriesmapsexo <- countriesmapsexo %>% addProviderTiles(names(esri[k])  , group = names.esri[k] ,  options =  providerTileOptions(minZoom = 2))
}

countriesmapsexo <- countriesmapsexo %>%
  addLayersControl(baseGroups = names.esri , overlayGroups = c("Ambos", "Hombres", "Mujeres"),
                   options = layersControlOptions(collapsed = T)) %>%
  addPolygons(stroke = T ,  smoothFactor = 0.05 ,   color = "gray" ,  weight = 1 , 
              fillColor = ~binpal(CANT_DOC) , 
              label = labels_countries ,  labelOptions = labelOptions( style = list("font-weight" = "normal" ,  padding = "3px 8px") ,   textsize = "12px" , direction = "auto"   ) ,  dashArray = "3" , 
              fillOpacity = 1 , 
              highlight = highlightOptions(
                weight = 5 , 
                color = "#666" , 
                dashArray = "" , 
                fillOpacity = 0.7 , 
                bringToFront = F), group = "Ambos") %>%  
  addPolygons(stroke = T ,  smoothFactor = 0.05 ,   color = "gray" ,  weight = 1 , 
              fillColor = ~binpal(CANT_DOCHOM) , 
              label = labels_hombres ,  labelOptions = labelOptions( style = list("font-weight" = "normal" ,  padding = "3px 8px") ,   textsize = "12px" , direction = "auto"   ) ,  dashArray = "3" , 
              fillOpacity = 1 , 
              highlight = highlightOptions(
                weight = 5 , 
                color = "#666" , 
                dashArray = "" , 
                fillOpacity = 0.7 , 
                bringToFront = F), group = "Hombres") %>%
  addPolygons(stroke = T ,  smoothFactor = 0.05 ,   color = "gray" ,  weight = 1 , 
              fillColor = ~binpal(CANT_DOCMUJ) , 
              label = labels_mujeres ,  labelOptions = labelOptions( style = list("font-weight" = "normal" ,  padding = "3px 8px") ,   textsize = "12px" , direction = "auto"   ) ,  dashArray = "3" , 
              fillOpacity = 1 , 
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
    onClick = JS("function(btn ,  map){ map.setView(L.latLng(42.796626414 , 12.07001339) ,  2.5); }")))%>%
  addLabelOnlyMarkers(lat = ~LATITUD,  lng = ~LONGITUD , label =  ~paste0(countries$NOMBRE) ,   labelOptions = labelOptions(noHide = T ,  direction = 'top' ,  textOnly = T , textsize = "10px") )%>%
  hideGroup("Hombres")%>%
  hideGroup("Mujeres")%>%
  showGroup("Ambos")%>%
  # addMarkers(data=docentes,group = "Ambos", lat = ~lat_asp, lng = ~long_asp,  label = labelsexo, labelOptions=labelOptions(direction = "auto",zoomAnimation=T, riseOnHover =T,     style = list("font-weight" = "normal", padding = "3px 8px")), icon = icon_sexo, clusterOptions = markerClusterOptions(removeOutsideVisibleBounds = T, animate = T, maxClusterRadius = 70,singleMarkerMode= F, animateAddingMarkers = T ), clusterId = "individuos5") %>% 
  # addMarkers(data=docentes,group = "Hombres", lat = ~lat_asp, lng = ~long_asp,  label = labelsexo, labelOptions=labelOptions(direction = "auto",zoomAnimation=T, riseOnHover =T,     style = list("font-weight" = "normal", padding = "3px 8px")), icon = icon_hombres, clusterOptions = markerClusterOptions(removeOutsideVisibleBounds = T, animate = T, maxClusterRadius = 70,singleMarkerMode= F, animateAddingMarkers = T ), clusterId = "individuos5") %>% 
  # addMarkers(data=docentes,group = "Mujeres", lat = ~lat_asp, lng = ~long_asp,  label = labelsexo, labelOptions=labelOptions(direction = "auto",zoomAnimation=T, riseOnHover =T,     style = list("font-weight" = "normal", padding = "3px 8px")), icon = icon_mujeres, clusterOptions = markerClusterOptions(removeOutsideVisibleBounds = T, animate = T, maxClusterRadius = 70,singleMarkerMode= F, animateAddingMarkers = T ), clusterId = "individuos5") %>% 
  # 
  # addEasyButton(easyButton(
  #   states = list(
  #     easyButtonState(
  #       stateName="unfrozen-markers",
  #       icon="ion-toggle",
  #       title="Freeze Clusters",
  #       onClick = JS("
  #                    function(btn, map) {
  #                    var clusterManager =
  #                    map.layerManager.getLayer('cluster', 'individuos5');
  #                    clusterManager.freezeAtZoom();
  #                    btn.state('frozen-markers');
  #                    }")
  #     ),
  #     easyButtonState(
  #       stateName="frozen-markers",
  #       icon="ion-toggle-filled",
  #       title="UnFreeze Clusters",
  #       onClick = JS("
  #                    function(btn, map) {
  #                    var clusterManager =
  #                    map.layerManager.getLayer('cluster', 'individuos5');
  #                    clusterManager.unfreeze();
  #                    btn.state('unfrozen-markers');
  #                    }")
  #     )
  #       )
  #       ))%>%
  addScaleBar(position = "bottomleft" , scaleBarOptions(metric = T ,  imperial =  F))


countriesmapsexo



