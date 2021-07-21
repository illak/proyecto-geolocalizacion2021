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


# some points are out of the polygon, lets filter out
# so we get only points in the polygon
points_sf_joined <- 
  st_join(establecimientos, departamentos) # spatial join to get intersection of points and poly


dep2 <- st_cast(departamentos, "GEOMETRYCOLLECTION")

est2 <- st_join(establecimientos, dep2)


ggplot() +
  geom_sf(data = departamentos) 
  #geom_sf(data = est2)
