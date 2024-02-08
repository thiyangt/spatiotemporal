## 21 Jan 2024
## https://www.paulamoraga.com/book-spatial/the-sf-package-for-spatial-vector-data.html

library(sf)
library(mapview)
library(viridis)
library(ggplot2)
library(plotly)
library(raster)

## Types of spatial data
## number of sudden infant deaths in each of the
## counties of North Carolina, USA, in 1974 from the sf package
d <- st_read(system.file("shape/nc.shp", package = "sf"),
             quiet = TRUE)
d
## quite = TRUE ogical; suppress info on name, driver, size and spatial reference, or signaling no or multiple layers
## Method 1
mapview(d, zcol = "SID74")

## Method 2
d$vble <- d$SID74  #sudden infant deaths in 1974
d$vble2 <- d$SID79 #  sudden infant deaths in 1979
g1 <- ggplot(d) + geom_sf(aes(fill = vble)) +
  scale_fill_viridis() + theme_bw()
ggplotly(g1)

## Exercise:
## household income in $1000 USD in neighborhoods in Columbus, Ohio, in 1980
library(spData)
library(ggplot2)
d <- st_read(system.file("shapes/columbus.shp",
                         package = "spData"), quiet = TRUE)



ggplot(d) + geom_sf(aes(fill = INC))
## Method 3
## open-source JavaScript library for interactive maps.
## The sf object that we pass to leaflet() needs to have a geographic coordinate reference system (CRS) indicating latitude and longitude (EPSG code 4326).
## Here, we use the st_transform() function of sf to transform the data d which has CRS given by EPSG code 4267 to CRS with EPSG code 4326.
st_crs(d)$epsg
d <- st_transform(d, 4326)
library(leaflet)
pal <- colorNumeric(palette = "YlOrRd", domain = d$vble)
l <- leaflet(d) |> addTiles() |>
  addPolygons(color = "white", fillColor = ~ pal(vble),
              fillOpacity = 0.8) |>
  addLegend(pal = pal, values = ~vble, opacity = 0.8)
l

l |>  addMiniMap()

# Saves map.html
library(htmlwidgets)
saveWidget(widget = l, file = "map.html")

# Takes a screenshot of the map.html created
# and saves it as map.png
library(webshot)
# webshot::install_phantomjs()
webshot(url = "map.html", file = "map.png")

## Using map view
mapview(d, zcol = "vble")
library(RColorBrewer)
pal <- colorRampPalette(brewer.pal(9, "YlOrRd"))
mapview(d, zcol = "vble", map.types = "CartoDB.DarkMatter",
        col.regions = pal, layer.name = "SDI")
map1 <- mapview(d, zcol = "vble")
leaflet::addMiniMap(map1@map)

## Example
library(sp)
library(sf)
library(mapview)

data(meuse)
meuse <- st_as_sf(meuse, coords = c("x", "y"), crs = 28992)
mapview(meuse, zcol = "lead",  map.types = "CartoDB.Voyager")

## Two maps
library(leaflet.extras2)
library(RColorBrewer)
pal <- colorRampPalette(brewer.pal(9, "YlOrRd"))

# common legend
at <- seq(min(c(d$vble, d$vble2)), max(c(d$vble, d$vble2)),
          length.out = 8)

m1 <- mapview(d, zcol = "vble", map.types = "CartoDB.Positron",
              col.regions = pal, at = at)
m2 <- mapview(d, zcol = "vble2", map.types = "CartoDB.Positron",
              col.regions = pal, at = at)

m <- leafsync::sync(m1, m2)
m

## Point data
library(maps)
d <- world.cities
View(d)
# Select South Africa
d <- d[which(d$country.etc == "South Africa"), ]
# Transform data to sf object
d <- st_as_sf(d, coords = c("long", "lat"))
# Assign CRS
st_crs(d) <- 4326
d$vble <- d$pop
d$size <- sqrt(d$vble)/100
ggplot(d) + geom_sf(aes(col = vble, size = size)) +
  scale_color_viridis()

pal <- colorNumeric(palette = "viridis", domain = d$vble)
leaflet(d) |> addTiles() |>
  addCircles(lng = st_coordinates(d)[, 1],
             lat = st_coordinates(d)[, 2],
             radius = ~sqrt(vble)*10,
             color = ~pal(vble), popup = ~name) |>
  addLegend(pal = pal, values = ~vble, position = "bottomright")

## Raster data
library(terra)
filename <- system.file("ex/elev.tif", package = "terra")
r <- rast(filename)
# Transform data to sf object
d <- st_as_sf(as.data.frame(r, xy = TRUE), coords = c("x", "y"))
# Assign CRS
st_crs(d) <- 4326
# Plot
ggplot(d) + geom_sf() +
  geom_raster(data = as.data.frame(r, xy = TRUE),
              aes(x = x, y = y, fill = elevation))

## To use the leaflet and mapview packages,
## we transform the data from class terra to RasterLayer with the raster::brick() function.
library(raster)
rb <- raster::brick(r)

pal <- colorNumeric("YlOrRd", values(r),
                    na.color = "transparent")
leaflet() |> addTiles() |>
  addRasterImage(rb, colors = pal, opacity = 0.8) %>%
  addLegend(pal = pal, values = values(r), title = "elevation")

## map view
mapview(rb, layer = "elevation")


###
### Spatio-temporal data
# devtools::install_github("Paula-Moraga/SpatialEpiApp")
library(SpatialEpiApp)
library(sf)
library(ggplot2)
library(viridis)

# map
f <- file.path("SpatialEpiApp/data/Ohio/fe_2007_39_county/",
               "fe_2007_39_county.shp")
pathshp <- system.file(f, package = "SpatialEpiApp")
map <- st_read(pathshp, quiet = TRUE)

# data
namecsv <- "SpatialEpiApp/data/Ohio/dataohiocomplete.csv"
d <- read.csv(system.file(namecsv, package = "SpatialEpiApp"))

# data are disaggregated by gender and race
# aggregate to get population in each county and year
d <- aggregate(x = d$n, by = list(county = d$NAME, year = d$year),
               FUN = sum)
names(d) <- c("county", "year", "population")

# join map and data
mapst <- dplyr::left_join(map, d, by = c("NAME" = "county"))

# map population by year
# facet_wrap() splits data into subsets and create multiple plots
ggplot(mapst, aes(fill = log(population))) + geom_sf() +
  facet_wrap(~ year, ncol = 7) +
  scale_fill_viridis("log(population)") +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank())

##
## Spatial-functional data
library(sf)
library(geoFourierFDA)
library(rnaturalearth)

# Map Canada
map <- rnaturalearth::ne_states("Canada", returnclass = "sf")

# Coordinates of stations
d <- data.frame(canada$m_coord)
d$location <- attr(canada$m_coord, "dimnames")[[1]]
d <- st_as_sf(d, coords = c("W.longitude", "N.latitude"))
st_crs(d) <- 4326

# Plot Canada map and location of stations
ggplot(map) + geom_sf() + geom_sf(data = d, size = 6) +
  geom_sf_label(data = d, aes(label = location), nudge_y = 2)
