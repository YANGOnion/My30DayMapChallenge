
############################## 10 Elevation ##############################

# MODIS data in Google Earth Engine

############################## 11 Elevation ############################## 

# Download DEM data from https://www.hydrosheds.org/page/overview

library(rayshader)
library(magrittr)
library(raster)

localtif=raster('D:/Data/RiverNet/HydroBasin_Asia/as_dem_15s_grid/as_dem_15s')
localtif=crop(localtif,extent(110,111,34,35))
elmat=as.matrix(localtif)
elmat %>%
  sphere_shade(texture = "imhof2") %>%
  add_water(detect_water(elmat,300), color = "imhof2") %>%
  add_shadow(ray_shade(elmat, zscale = 3), 0.5) %>% # 0.5 lower limit for how much the image will be darkened
  add_shadow(ambient_shade(elmat), 0.5) %>%
  plot_3d(elmat, zscale = 100, fov = 0, theta = 135, zoom = 0.75, phi = 45, windowsize = c(2000, 1600),
          water = TRUE, waterdepth = 0, wateralpha = 0.5, watercolor = "lightblue",
          waterlinecolor = "white", waterlinealpha = 0.5)
render_snapshot(clear=TRUE)


############################## 12 Movement ############################## 

library(maps)
library(maptools)
baseMap=map("world2",fill=T,plot=F)
baseMap=map2SpatialPolygons(baseMap,IDs=sapply(strsplit(baseMap$names,":"),function(x) x[1]),proj4string=CRS("+init=epsg:4326"))

library(raster)
library(rasterVis)
u=raster('input/u_1110.tif')
v=raster('input/v_1110.tif')

slope <- sqrt(u^2 + v^2)
aspect <- atan2(u, v)
vectorplot(brick(u,v)*5, isField = "dXY", region = slope, margin = T, col.arrows='orange',
           narrows = 2000,length=0.05,lwd.arrows=2,par.settings=BuRdTheme())+
  layer(sp.polygons(baseMap))

############################## 13 Tracks ############################## 

# Download tropical cyclone data from http://tcdata.typhoon.org.cn/zjljsjj_zlhq.html

library(lubridate)

dt=fread('input/CH2018BST.txt',fill=T)
nameindex=which(sapply(1:nrow(dt),function(i){dt[i,V1]==66666}))
nameindex=c(nameindex,nrow(dt)+1)
names=dt[V8!="",V8]
dt[,V10:=unlist(sapply(1:(length(nameindex)-1),function(i){rep(names[i],nameindex[i+1]-nameindex[i])}))]
dt=dt[V1!=66666][V10!="(nameless)"][,.(time=ymd_h(V1),I=V2,lat=V3/10,lon=V4/10,pres=V5,wnd=V6,name=V10)]
dt=dt[,.SD[order(time)],by=name]
dt=dt[name!='HECTOR']
dt=dt[,.SD[,.(time,I,lat,lon,pres,wnd,start=as.Date(time)[1],end=as.Date(time)[.N])],by=name]
dt[,duration:=paste(start,'to',end,sep=' ')]

library(ggplot2)
ggplot(dt)+geom_path(aes(lon,lat))+geom_point(aes(lon,lat,size=wnd))+facet_wrap(.~name)

library(OpenStreetMap)
map <- openmap(c(70,72), c(0,180), zoom = NULL,
               type = "stamen-watercolor")
map.latlon <- openproj(map, projection = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
autoplot(map.latlon)+geom_path(data=dt,aes(lon,lat,color=name))+
  geom_point(data=dt,aes(lon,lat,color=name))

autoplot(map.latlon)+geom_path(data=dt,aes(lon,lat),alpha=0.75,size=2,
                               arrow=arrow(type="open",length=unit(0.1,"inches")))+
  geom_text(aes(lon,lat,label=duration),data=dt[,.(duration=duration[1]),by=name][,.(name,duration,lon=120,lat=65)])+
  facet_wrap(.~name,nrow=4)+xlab('lon')+ylab('lat')+theme_bw(base_size=15)

############################## 14 Boundaries ############################## 

# Precipitation data from CGDPA

library(raster)
r2=raster('input/06_15_rain.tif')
thres=800
r2[r2<=thres]=0
r2[r2>thres]=1

library(OpenStreetMap)
library(ggplot2)
library(ggspatial)
map <- openmap(c(60,70), c(15,140), zoom = NULL,type = "stamen-terrain")
map.latlon <- openproj(map, projection = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
autoplot(map.latlon)+layer_spatial(r2,alpha=0.3)+theme_minimal(base_size=15)+
  scale_fill_distiller(palette = "Spectral",na.value = NA)+xlab('lon')+ylab('lat')

############################## 15 Names ############################## 

library(raster)
library(stringr)
p=shapefile("input/Office2016/Office2016.shp",use_iconv=TRUE, encoding = "UTF-8")

p$type=sapply(data.frame(p)[,3],function(x){
  if(is.na(x)) return("其他")
  if(str_detect(x,"大厦")){
    return("大厦")
  }else if(str_detect(x,"中心")){
    return("中心")
  }else if(str_detect(x,"广场")){
    return("广场")
  }else{
    return("其他")
  }
})
shapefile(p,"input/Office2016/Office2016.shp",overwrite=T)

############################## 18 Globe ##############################

library(raster)
library(ggplot2)
library(magrittr)
r=readAll(raster('input/aridity1.tif'))
r=aggregate(r,6)
df=as.data.frame(r,xy=T)
names(df)=c('longitude','latitude','Aridity')
WorldData <- map_data('world')
WorldData <- fortify(WorldData)

lapply(1:36,function(i){
  ggplot()+geom_point(data=df,aes(x=longitude,y=latitude,color=Aridity),size=2)+
    geom_map(data=WorldData,map=WorldData,aes(x=long,y=lat,group=group,map_id=region),
             alpha=0,colour="grey",size=0.5)+
    coord_map("orthographic",orientation=c(15,seq(360,10,-10)[i],0))+scale_color_viridis_c(trans='sqrt')
  ggsave(paste("input/globe/map",ifelse(i<10,paste("0",i,sep=''),i),".png",sep=''),device="png",width=5,height=5,units='in',dpi=150)
})

library(magick)

list.files(path = "input/globe/", pattern = "*.png", full.names = T) %>% 
  purrr::map(image_read) %>% # reads each path file
  image_join() %>% # joins image
  image_animate(fps=4) %>% # animates, can opt for number of loops
  image_write("map.gif") # write to current dir

############################## 19 20 Urban, Rural ##############################

# Land cover data in Google Earth Engine

############################## 23 24 Population, Statistics ##############################

library(raster)
gdp=raster('D:/Data/China_Society/gdp2015/gdp2015')
pop=raster('D:/Data/China_Society/pop2015/tpop2015')
prov=shapefile('D:/Data/Boundaries/sheng/CN-sheng-A.shp',encoding='UTF-8')
prov=prov[prov$SHENG_%in%data.table(data.frame(prov))[,.SD[order(AREA,decreasing=T)][1,],by=SHENG_ID][,SHENG_],]
plot(pop)

r=aggregate(pop,10,fun=sum)
library(tmap)
tm=tm_shape(r)+tm_raster('gdp2015',style = "fixed",
                      breaks=c(0,400,2000,20000,60000,500000,5000000,45000000),title='GDP 2015')+
  tm_shape(prov)+tm_borders("gray37")+
  tm_compass(type='8star',position=c('right','top'))+tm_scale_bar(text.size=0.75)+
  tm_style("cobalt")+tm_grid(projection = "longlat", labels.size = 0.75, lwd = 0.25,labels.col = 'black')+
  tm_layout(legend.title.size=2,legend.text.size=0.8)
tmap_save(tm, filename = "graphics/24_Statistics.tiff",width=10,height=7,units='in',dpi=120)

tm=tm_shape(r)+tm_raster('tpop2015',style = "fixed",palette ='-Spectral',midpoint=0,
                      breaks=c(0,200,1000,6000,20000,50000,500000,2600000),title='Population 2015')+
  tm_shape(prov)+tm_borders("gray37")+
  tm_compass(type='8star',position=c('right','top'))+tm_scale_bar(text.size=0.75)+
  tm_style("cobalt")+tm_grid(projection = "longlat", labels.size = 0.75, lwd = 0.25,labels.col = 'black')+
  tm_layout(legend.title.size=2,legend.text.size=0.8)
tmap_save(tm, filename = "graphics/23_Population.tiff",width=10,height=7,units='in',dpi=120)

