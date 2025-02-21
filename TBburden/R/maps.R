

source(here::here("TBburden/R", "pregTB_births2023.R"))

library(ggplot2)
library(getTBinR)
library(scales)

key_parms <- c("pregTBI_best","pregTBIwidthSq",
               "ppTBI_best", "ppTBIwidthSq",
               "births_best")

# prepare data 
summary_countries <- new_df_births_adjusted %>% 
  group_by(country, iso3) %>% 
  summarise_at(all_of(key_parms), ~sum(., na.rm=T)) 
summary_countries$pregTBI_best_r <- summary_countries$pregTBI_best/summary_countries$births_best * 1000
summary_countries$ppTBI_best_r <- summary_countries$ppTBI_best/summary_countries$births_best * 1000

summary_countries <- as.data.frame(summary_countries)

# using ggplot
# using the shapefile used in the WHO TB report

## Bind in world data
plot_df <- getTBinR::who_shapefile %>% 
  left_join(summary_countries, c("id" = "iso3"))
# plot_df$pregTBI_best_r <- ifelse(is.na(plot_df$pregTBI_best_r), 0, plot_df$pregTBI_best_r)
length(unique(plot_df$country))

theme_bare <- theme(
  axis.line = element_blank(), 
  axis.text.x = element_blank(), 
  axis.text.y = element_blank(),
  axis.ticks = element_blank(), 
  axis.title.x = element_blank(), 
  axis.title.y = element_blank(),
  legend.text=element_text(size=16),
  legend.title=element_text(size=18),
  panel.background = element_blank(),
  panel.border = element_rect(colour = "gray", fill=NA, size=0.5)
)

na.value.forplot <- 'white'

# Pregnancy
pregnancy <- ggplot(plot_df, 
                    aes(x = long, 
                        y = lat, 
                        text = country,
                        fill = pregTBI_best)) +
  geom_polygon(aes(group = group), color = "black", size = 0.3, na.rm = TRUE) +
  coord_equal() +
  ggthemes::theme_map() +
  theme(legend.position = "bottom") +
  scale_fill_gradient(high = "#e34a33", low = "#fee8c8", guide = "colorbar", na.value = na.value.forplot, labels = comma) +
  ggtitle("Global burden of TB during pregnancy") +
  guides(fill = guide_colourbar(title = "Estimated number of TB incident cases (all forms)", barwidth = 30)) +
  theme_bare
  

# Postpartum
postpartum <- ggplot(plot_df, 
               aes(x = long, 
                   y = lat, 
                   text = country,
                   fill = ppTBI_best)) +
  geom_polygon(aes(group = group), color = "black", size = 0.3, na.rm = TRUE) +
  coord_equal() +
  ggthemes::theme_map() +
  theme(legend.position = "bottom") +
  scale_fill_gradient(high = "#e34a33", low = "#fee8c8", guide = "colorbar", na.value = na.value.forplot, labels = comma) +
  ggtitle("Global burden of TB during postpartum") +
  guides(fill = guide_colorbar(title = "Estimated number of TB incident cases (all forms)", barwidth = 30)) +
  # labs(caption = "Source: World Health Organisation") +
  theme_bare

# Pregnancy
pregnancy1 <- ggplot(plot_df, 
                    aes(x = long, 
                        y = lat, 
                        text = country,
                        fill = pregTBI_best_r)) +
  geom_polygon(aes(group = group), color = "black", size = 0.3, na.rm = TRUE) +
  coord_equal() +
  ggthemes::theme_map() +
  theme(legend.position = "bottom") +
  scale_fill_gradient(high = "#e34a33", low = "#fee8c8", guide = "colorbar", na.value = na.value.forplot, labels = comma) +
  # ggtitle("Global burden of TB during pregnancy") +
  guides(fill = guide_colourbar(title = "Estimated number of TB incident cases per 1000 pregnant women", barwidth = 23)) +
  theme_bare


# Postpartum
postpartum1 <- ggplot(plot_df, 
                     aes(x = long, 
                         y = lat, 
                         text = country,
                         fill = ppTBI_best_r)) +
  geom_polygon(aes(group = group), color = "black", size = 0.3, na.rm = TRUE) +
  coord_equal() +
  ggthemes::theme_map() +
  theme(legend.position = "bottom") +
  scale_fill_gradientn(colours = c("#fee8c8","#e34a33"), na.value = na.value.forplot, 
                       guide="colourbar")+
                       # values=c(0,0.0024999991480808,0.05,4.5230826273651)^ 0.2313782,
                       # limits=c(0,1.5001),breaks=c(0,0.05,4.5230826273651)^ 0.2313782, 
                       # labels=c(0,1,1.5), geom_tile(aes(fill=ppTBI_best_r^0.2313782),colour="grey50", size=0.1)) +
  # ggtitle("Global burden of TB during postpartum") +
  guides(fill = guide_colorbar(title = "Estimated number of TB incident cases per 1000 pregnant women", barwidth = 23)) +
  # labs(caption = "Source: World Health Organisation") +
  theme_bare 
  

ggsave(plot=pregnancy,filename=here::here("TBburden/plots/TB incidence map during pregnancy.png"),
       width=12, height=8, dpi=600)
ggsave(plot=postpartum,filename=here::here("TBburden/plots/TB incidence map during postpartum.png"),
       width=14, height=8, dpi=600)

ggsave(plot=pregnancy1,filename=here::here("TBburden/plots/TB incidence map during pregnancy per 1000 pregnancies.png"),
       width=14, height=8, dpi=600)
ggsave(plot=postpartum2,filename=here::here("TBburden/plots/TB incidence map during postpartum per 1000 pregnancies.png"),
       width=15, height=8, dpi=600)


# # using the rworldmap package - needs more work to improve the maps
# library(rworldmap)
# library(RColorBrewer)
# library(classInt)
# 
# 
# # joining data to map data 
# mapped_data <-  joinCountryData2Map(summary_countries, joinCode = "ISO3", 
#                                     nameJoinColumn = "iso3", mapResolution = "coarse")
# 
# #getting class intervals 
# classInt <- classIntervals( mapped_data[["pregTBI_best_r"]] ,n=4, style = "jenks") 
# catMethod = classInt[["brks"]]
# 
# 
# #getting colours 
# colourPalette <- brewer.pal(5,'RdPu')
# colourPalette1 <- c('#f1eef6','#d7b5d8','#df65b0','#dd1c77','#980043')
# colourPalette2 <- c('#fef0d9','#fdcc8a','#fc8d59','#d7301f')
# colourPalette3 <- c('#feebe2','#fbb4b9','#f768a1','#ae017e')
# colourPalette4 <- c('#ffffd4','#fed98e','#fe9929','#cc4c02')
# #plot map 
# map1 <- mapDevice() #create world map shaped window
# mapParams <- mapCountryData(mapped_data 
#                             ,nameColumnToPlot="pregTBI_best_r" 
#                             ,addLegend=FALSE 
#                             ,catMethod = catMethod 
#                             ,colourPalette=colourPalette2)
# 
# # # display the mapped data
# # par(mai=c(0,0,0.2,0),xaxs="i",yaxs="i")
# # 
# # 
# # mapCountryData(mapped_data, nameColumnToPlot = "pregTBI_best_r", colourPalette = "terrain")
# 
# # mapParams <- mapCountryData( mapped_data, nameColumnToPlot="pregTBI_best_r"
# #                              , addLegend=FALSE )
# #adding legend 
# do.call(addMapLegend ,c(mapParams 
#                         ,legendLabels="all" 
#                         ,legendWidth=0.5 
#                         ,legendIntervals="data" 
#                         ,legendMar = 2
#                         ,sigFigs = 1))
# 
# 
# 
# # using the rgdal package === not working 
# require(rgdal)
# require(ggplot2)
# 
# fn <- here('indata/shp.Rdata')
# if(!file.exists(fn)){
#   fn <- file.path(tempdir(), "gadm36_gdb.zip", fsep = "\\")
#   
#   download.file("https://biogeo.ucdavis.edu/data/gadm3.6/gadm36_shp.zip", fn)
#   utils::unzip(fn, exdir = tempdir())
#   shp <- readOGR(dsn = file.path(tempdir(), "gadm36.shp"), stringsAsFactors = F)
#   save(shp,file=fn)
# } else {
#   load(fn)
# }
# 
# summary(shp@data)
# 
# 
# # draw the map without attributes
# map <- ggplot() + geom_polygon(data = shp, aes(x = long, y = lat, group = group), colour = "black", fill = NA)
# 
# ## Regions defined for each Polygons
# map + theme_void()
# 
# shp_df <- broom::tidy(shp, region = "NAME_1")
# lapply(shp_df, class)
# 
# head(shp_df)
# 
# map <- ggplot() + geom_polygon(data = shp, aes(x = long, y = lat, group = group), colour = "black", fill = NA)
# 
# ## Regions defined for each Polygons
# cnames <- aggregate(cbind(long, lat) ~ id, data=shp_df, FUN=mean)
# map + geom_text(data = cnames, aes(x = long, y = lat, label = id), size = 4) + theme_void()