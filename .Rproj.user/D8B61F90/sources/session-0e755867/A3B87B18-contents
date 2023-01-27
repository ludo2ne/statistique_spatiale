# ------------------------------------------------------------------------------------
# Introduction à la statistique spatiale - TP2
# DENEUVILLE Ludovic
# ENSAI 2023
# ------------------------------------------------------------------------------------

# https://r-spatial.github.io/sf/
# https://r-spatial.github.io/sf/reference/st.html

library(sf)
library(dplyr)


# 1. Importer le fond communal
communes <- st_read("fonds/commune_francemetro_2021.shp", options = "ENCODING=WINDOWS-1252")


# 2. résumé/descriptif du contenu de l'objet importé
str(communes)
summary(communes)


# 3. Regarder la dernière colonne
View(communes)


# 4. Afficher le système de projection de la table (Lambert-93)
st_crs(communes)

communes_Bretagne <- communes %>% 
  filter(reg == 53) %>% 
  select(code, libelle, epc, dep, surf)

# 6. Assurez-vous que cette nouvelle table est toujours un objet sf
str(communes_Bretagne)


# 7. Appliquer la fonction plot sur votre table
plot(communes_Bretagne)         # il y a autant de plot qu'il y a de variables


# 8. Faire la question précédente en utilisant st_geometry
plot(st_geometry(communes_Bretagne))


# 9. Variable de surface
communes_Bretagne <- communes_Bretagne %>%
  mutate(surf2 = st_area(geometry))

str(communes_Bretagne)


# 10. Modiﬁer la variable créée pour la convertir en km2
communes_Bretagne$surf2 <- surf2 / 1000000


# 12. 
dept_bretagne <- group_by(communes_Bretagne, dep) %>% 
  summarise(surf = sum(surf))
# , geometry = sum(geometry)

layout(matrix(c(1,1)))
plot(st_geometry(dept_bretagne))


# 13. 
dept_bretagne <- communes_Bretagne %>%
  group_by(dep) %>%
  summarize(geometry = st_union(geometry))


# 14. 
centroid_dept_bretagne <- st_centroid(dept_bretagne)   # POINTS

plot(st_geometry(centroid_dept_bretagne), add = TRUE, col="red", pch = 3 )

centroid_dept_bretagne$dept_lib <- c("Cotes d'Armor", "Finistère", "Ille et Vilaine", "Morbihan")

centroid_coords <- st_coordinates(centroid_dept_bretagne)
centroid_coords <- st_drop_geometry(bind_cols(centroid_coords, dep = centroid_dept_bretagne$dep, dept_lib = centroid_dept_bretagne$dept_lib))

text(x = centroid_coords$X, 
     y = centroid_coords$Y, 
     pos = 3,                              # au dessus
     labels = centroid_coords$dept_lib)


# 15. retrouver dans quelle commune se situe le centroïde de chaque département
tmp <- centroid_coords %>%
  rowwise() %>%
  mutate(point = list(st_point(c(X,Y))))

centroid_coords$point <- st_sfc(tmp$point) # Conversion de l'objet en type geometry

# on recherche les communes (polygones) dans lesquelles se trouvent les centroides (points)
st_intersects(centroid_dept_bretagne, communes_Bretagne)

which(lengths(st_intersects(centroid_dept_bretagne, communes_Bretagne)) > 0)

communes_centroides <- communes_Bretagne %>% slice(148, 476, 647, 1092)
plot(st_geometry(communes_centroides), add = TRUE )


# 16. même question avec la fonction st_intersection
#   ca renvoie directement les communes
st_intersection(centroid_dept_bretagne, communes_Bretagne)

# Cette fois ci l'ordre a une importance
#  within verifie que le parametre 1 est contenu entierement dans le parametre 2
st_within(centroid_dept_bretagne, communes_Bretagne)


# 17. Calculer la distance séparant les centroïdes des départements et leur chefs-lieux
st_distance(communes_centroides[1,], communes_Bretagne %>% filter(libelle == "Saint-Brieuc"))
plot(communes_Bretagne %>% filter(libelle == "Saint-Brieuc"), add = TRUE, col = "green")

st_distance(communes_centroides[2,], communes_Bretagne %>% filter(libelle == "Quimper"))
plot(communes_Bretagne %>% filter(libelle == "Quimper"), add = TRUE, col = "orange")

st_distance(communes_centroides[3,], communes_Bretagne %>% filter(libelle == "Rennes"))
plot(communes_Bretagne %>% filter(libelle == "Rennes"), add = TRUE, col = "purple")

st_distance(communes_centroides[4,], communes_Bretagne %>% filter(libelle == "Vannes"))
plot(communes_Bretagne %>% filter(libelle == "Vannes"), add = TRUE, col = "pink")


# 18. Quelles sont les communes à moins de 20 km de chaque centroïde

zones_20km <- st_buffer(centroid_dept_bretagne, 20000)

plot(st_geometry(dept_bretagne))
plot(zones_20km, add = TRUE)

# Communes comprises dans la zone de 20km
st_intersection(zones_20km, communes_Bretagne) 


layout(matrix(c(1,1)))
plot(st_geometry(dept_bretagne))
plot(st_geometry(st_intersection(zones_20km, communes_Bretagne)), add = TRUE)

# Compter par département
st_intersection(zones_20km, communes_Bretagne)  %>%
  count(dep)


# 19. Changer le système de projection des communes bretonnes pour le mettre en WGS84

communes_Bretagne                         # Projected CRS: RGF93 / Lambert-93
st_transform(communes_Bretagne, 4326)     # Geodetic CRS:  WGS 84

wgs84 <- st_transform(communes_Bretagne, 4326)

 
plot(st_geometry(wgs84), col=rainbow(10))
plot(st_geometry(communes_Bretagne), col=rainbow(10))


# 20. Calcul des aires des communes avec le nouveau système de projection
surf3 <- st_area(st_transform(communes_Bretagne, 4326)$geometry)
communes_Bretagne$surf3 <- surf3
