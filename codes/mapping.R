library(sf)
library(tmap)

nuts3shapefile <- read_sf("yourfilepath.shp") # for the attached map you need to download the specific shape files from Eurostat. I used NUTS3
nrw_nuts <- merge(x=nuts3shapefile , y=your_covid_data,
                  by.x= "NUTS_ID",
                  by.y= "NUTS3",
                  all.x=F, all.y=T) # match shape data with covid data


mapNRW <- tm_shape(yourdata) +
  tm_fill("COVIDcasevariable", palette = "Blues", legend.show = F) +
  tm_borders("black") + tm_add_legend(title = "legendtitle", labels = c("legenddescription"), col = RColorBrewer::brewer.pal(5, "Blues")) +
  tm_layout(title = "maptitle", title.fontfamily = "sans", title.size = 2.0,
            title.fontface= "bold", title.position= c(0.05,0.95),frame=F,
            legend.outside = F, legend.frame = F,
            legend.position = c(0.8,0.0), legend.width = 1,
            legend.title.size = 2.0, legend.text.size= 1.5,
            legend.title.fontfamily ="sans",legend.title.fontface ="bold")
mapNRW