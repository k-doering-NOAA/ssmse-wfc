# some standard map packages.
#install.packages(c("maps", "mapdata", "ggmap"))

#based on : https://eriqande.github.io/rep-res-web/lectures/making-maps-with-R.html

library(ggplot2)
library(ggmap)
library(maps)
library(mapdata)

usa_dat <- map_data("usa") # we already did this, but we can do it again
ggplot() + 
  geom_polygon(data = usa_dat, aes(x=long, y = lat, group = group), 
               fill = nmfspalette::nmfs_palette("oceans")(1)) + 
  coord_fixed(xlim = c(-96, -78),  ylim = c(25, 35), ratio = 1.3)+
  theme_nothing()
ggsave("figures/GOM_outline.png")
  

