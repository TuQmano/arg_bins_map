#####3 ARGENTINA HEXAGONAL
#####
#####
#####  Isnpirado en https://fishandwhistle.net/post/2019/canada-ridings-hex/
#####
#####

# CARGO LIBERIAS 

library(sf)
library(tidyverse)
library(geogrid)
library(gganimate)
library(transformer)


#### PROVINCIAS #####




# CARGO DATA


mapa_sf_provs <- read_sf("data/GRED_Argentina.shp") %>%  
  rename(provincia = CST_N) %>% 
  arrange(provincia) %>% 
  mutate(id = row_number()) %>%
  mutate(provincia = case_when(
    id == 2 ~ "Córdoba",
    id == 8 ~ "Entre Ríos",
    id == 15~ "Neuquén",
    id == 16 ~ "Río Negro",
    id == 24 ~ "Tucumán",
    T~provincia
  )) %>% 
  mutate(prov = case_when(
    provincia == "Buenos Aires" ~ "PBA",
    provincia == "Córdoba" ~ "COR",
    provincia == "Capital Federal" ~ "CABA",
    provincia == "Catamarca" ~ "CAT",
    provincia == "Chaco" ~ "CHA",
    provincia == "Chubut" ~ "CHU",
    provincia == "Corrientes" ~ "CTS",
    provincia == "Entre Ríos" ~ "ERI",
    provincia == "Formosa" ~ "FOR",
    provincia == "Jujuy" ~ "JUJ",
    provincia == "La Pampa" ~ "PAM",
    provincia == "La Rioja" ~ "LRI",
    provincia == "Mendoza" ~ "MZA",
    provincia == "Misiones" ~ "MIS",
    provincia == "Neuquén" ~ "NQN",
    provincia == "Río Negro" ~ "RNE",
    provincia == "Salta" ~ "SAL",
    provincia == "San Juan" ~ "SJU",
    provincia == "San Luis" ~ "SLU",
    provincia == "Santa Cruz" ~ "SCU",
    provincia == "Santa Fe" ~ "SFE",
    provincia == "Santiago del Estero" ~ "STG",
    provincia == "Tierra del Fuego" ~ "TDF",
    provincia == "Tucumán" ~ "TUC",
  )) %>% 
  select(id, prov, provincia) %>% 
  print()


mapa_sf_provs %>% 
  ggplot() +
  geom_sf(aes(fill = prov))+
  theme_void()


### CALCULO GRILLA DE HEXAGONOS

mapa_grilla_provs<- mapa_sf_provs %>%
  calculate_grid(grid_type = "hexagonal", seed = 34)


mapa_hex_provs <- assign_polygons(mapa_sf_provs, mapa_grilla_provs) 


datos <- mapa_hex_provs %>% 
  select(id, prov, provincia, row, col, everything()) 

ggplot(datos) +
  geom_sf(aes(fill = provincia))+
  geom_sf_text(aes(label = prov))+
  theme(legend.position = "none")












mapas_combinados <- rbind(
  mapa_sf_provs %>% 
    mutate(type = "geographic") %>% 
    st_set_crs(NA) %>% 
    select(prov, type, geometry),
  datos %>%
    mutate(type = "hex") %>% 
    st_set_crs(NA) %>% 
    select(prov, type, geometry)
)

plot_obj <- ggplot(mapas_combinados) +
  geom_sf(aes(fill = prov)) +
  theme_void() +
  geom_sf_text(aes(label = prov)) +
  transition_states(type, transition_length = 1, state_length = 5)

transformer <- animate(plot_obj, width = 700, height = 700, res = 96)



# GUARDO GIF TRANSICION ENTRE MAPA POLYGONOS A HEX MAP 

#anim_save(animation = transformer, filename = "provs.gif")

  







#### DEPARTAMENTOS ####
#
## CARGO DATA
#
#
mapa_sf <- read_sf("data/secciones_electorales.shp") %>%
  st_transform(54032) %>% 
  st_simplify(dTolerance = 100) %>%
  st_transform(4326) %>% 
  print()


mapa_sf %>% 
  ggplot() +
  geom_sf(aes(fill = provincia)) +
  theme_void()+
  theme(legend.position = "none")


### CALCULO GRILLA DE HEXAGONOS

mapa_sf_grilla <- mapa_sf %>%
  calculate_grid(grid_type = "hexagonal", seed = 1938)

mapa_hex <- assign_polygons(mapa_sf, mapa_sf_grilla)

ggplot(mapa_hex) +
  geom_sf(aes(fill = provincia))

