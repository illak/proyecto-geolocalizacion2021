library(sf)
library(tidyverse)


establecimientos_ed <- st_layers("WFS:https://idecor-ws.mapascordoba.gob.ar/geoserver/idecor/Establecimientos_educativos/wfs?getcapabilities")
departamentos_cba <- st_layers("WFS:https://idecor-ws.mapascordoba.gob.ar/geoserver/idecor/departamentos/wfs?getcapabilities")

baseurl1 <- "https://idecor-ws.mapascordoba.gob.ar/geoserver/idecor/Establecimientos_educativos/wfs?request=GetFeature&service=WFS&typeName="
capa_wfs1 <- "idecor:Establecimientos_educativos"
establecimientos <- st_read(paste0(baseurl1,capa_wfs1))

baseurl2 <- "https://idecor-ws.mapascordoba.gob.ar/geoserver/idecor/departamentos/wfs?request=GetFeature&service=WFS&typeName="
capa_wfs2 <- "idecor:departamentos"
departamentos <- st_read(paste0(baseurl2, capa_wfs2))


# Casteamos a GEOMETRYCOLLECTION sino no funciona el join (ES UN MULTISURFACE)
departamentos <- st_cast(departamentos, "GEOMETRYCOLLECTION") %>% st_collection_extract("POLYGON")

# Esto NO funciona
establecimientos_filtrados <- st_join(establecimientos, departamentos)

# Esto SI funciona
establecimientos_filtrados <- st_filter(establecimientos, dep2, .predicate = st_intersects)

ggplot() +
  geom_sf(data = dep2) +
  geom_sf(data = establecimientos_filtrados)