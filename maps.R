require(rgdal)
require(ggplot2)

fn <- here('indata/mapfile.Rdata')
if(!file.exists(fn)){
  fn <- file.path(tempdir(), "gadm36_gdb.zip", fsep = "\\")
  
  download.file("https://biogeo.ucdavis.edu/data/gadm3.6/gadm36_shp.zip", fn)
  utils::unzip(fn, exdir = tempdir())
  shp <- readOGR(dsn = file.path(tempdir(), "gadm36.shp"), stringsAsFactors = F)
  save(mapfile,file=fn)
} else {
  load(fn)
}

summary(shp@data)


# draw the map without attributes
map <- ggplot() + geom_polygon(data = shp, aes(x = long, y = lat, group = group), colour = "black", fill = NA)

## Regions defined for each Polygons
map + theme_void()

shp_df <- broom::tidy(shp, region = "NAME_1")
lapply(shp_df, class)

head(shp_df)

map <- ggplot() + geom_polygon(data = shp, aes(x = long, y = lat, group = group), colour = "black", fill = NA)

## Regions defined for each Polygons
cnames <- aggregate(cbind(long, lat) ~ id, data=shp_df, FUN=mean)
map + geom_text(data = cnames, aes(x = long, y = lat, label = id), size = 4) + theme_void()


library(rworldmap)
summary_countries <- new_df_births%>%group_by(country, iso3)%>%summarise_at(key_parms, funs(sum), na.rm=T) 
summary_countries$pregTBI_best_r <- summary_countries$pregTBI_best/summary_countries$births_best * 1000
summary_countries$ppTBI_best_r <- summary_countries$ppTBI_best/summary_countries$births_best * 1000

summary_countries <- as.data.frame(summary_countries)

mapped_data <-  joinCountryData2Map(summary_countries, joinCode = "ISO3", 
                                   nameJoinColumn = "iso3")
# display the mapped data
par(mai=c(0,0,0.2,0),xaxs="i",yaxs="i")


mapCountryData(mapped_data, nameColumnToPlot = "pregTBI_best_r", colourPalette = "white2Black")

mapParams <- mapCountryData( mapped_data, nameColumnToPlot="pregTBI_best_r"
                             , addLegend=FALSE )
do.call( addMapLegend, c(mapParams, legendWidth=0.5, legendMar = 2, sigFigs = 1))


## Bind in world data
df <- getTBinR::who_shapefile %>% 
  left_join(summary_countries, c("id" = "iso3"))
df$pregTBI_best_r <- ifelse(is.na(df$pregTBI_best_r), 0, df$pregTBI_best_r)


# using ggplot
# using the shapefile used in the WHO TB report
theme_bare <- theme(
  axis.line = element_blank(), 
  axis.text.x = element_blank(), 
  axis.text.y = element_blank(),
  axis.ticks = element_blank(), 
  axis.title.x = element_blank(), 
  axis.title.y = element_blank(),
  legend.text=element_text(size=7),
  legend.title=element_text(size=8),
  panel.background = element_blank(),
  panel.border = element_rect(colour = "gray", fill=NA, size=0.5)
)

na.value.forplot <- 'white'

# Pregnancy
pregnancy <- ggplot(df, 
               aes(x = long, 
                   y = lat, 
                   text = country,
                   fill = pregTBI_best_r)) +
  geom_polygon(aes(group = group), color = "black", size = 0.3, na.rm = TRUE) +
  coord_equal() +
  ggthemes::theme_map() +
  theme(legend.position = "bottom") +
  scale_fill_gradient(high = "#e34a33", low = "#fee8c8", guide = "colorbar", na.value = na.value.forplot) +
  ggtitle("Global burden of TB during pregnancy") +
  guides(fill = guide_legend(title = "Estimated TB incidence (all forms) per 1000 pregnant women")) +
  # labs(caption = "Source: World Health Organisation") +
  scale_color_viridis_c(
                      option = "magma") + theme_bare

# Postpartum
postpartum <- ggplot(df, 
               aes(x = long, 
                   y = lat, 
                   text = country,
                   group = group,
                   fill = ppTBI_best_r)) +
  geom_polygon(aes(group = group), color = "black", size = 0.3, na.rm = TRUE) +
  coord_equal() +
  ggthemes::theme_map() +
  theme(legend.position = "bottom") +
  # scale_fill_gradient(high = "#e34a33", low = "#fee8c8", guide = "colorbar") +
  ggtitle("Global burden of TB during postpartum") +
  guides(fill = guide_legend(title = "Estimated TB incidence (all forms) per 1000 pregnant women")) +
  # labs(caption = "Source: World Health Organisation") +
  scale_color_viridis_c(
    option = "magma") + theme_bare