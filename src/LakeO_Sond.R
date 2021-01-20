## 
## Lake Okeechobee Sonde Data
##
## Code was compiled by Paul Julian
## contact info: paul.julian@floridadep.gov

## BAD 
## https://www.tidyverse.org/articles/2017/12/workflow-vs-script/
## Clears Everything...start fresh.
rm(list=ls(all=T));cat("\014");dev.off()

## Libraries
#devtools::install_github("SwampThingPaul/AnalystHelper")
library(AnalystHelper);
library(plyr)
library(reshape)
library(zoo)

# GIS libraries 
library(rgdal)
library(rgeos)
library(raster)
library(tmap)

wd="C:/Julian_LaCie/_GitHub/owper-EstuarySalinityUpdate"

paths=paste0(wd,c("/Plots/","/Export/","/Data/","/GIS","/src/"))
# Folder.Maker(paths);#One and done. Creates folders in working directory.
plot.path=paths[1]
export.path=paths[2]
data.path=paths[3]
GIS.path=paths[4]

GIS.path="C:/Julian_LaCie/_GISData"

nad83.pro=CRS("+init=epsg:4269")
wgs84=CRS("+init=epsg:4326")
utm17=CRS("+init=epsg:26917")

tmap_mode("view")
# -------------------------------------------------------------------------
lakeO=spTransform(readOGR(paste0(GIS.path,"/SFWMD"),"LakeOkeechobee_general"),utm17)
wmd.mon=spTransform(readOGR(paste0(GIS.path,"/SFWMD_Monitoring_20210119"),"DBHYDRO_SITE_STATION"),utm17)
wmd.mon=subset(wmd.mon,STATION%in%c("L001-S", "L005-S","L006-S", "LZ40-S", "POLESOUT1S", "POLESOUT3B")&ACTIVITY_S=="Logged")
canal=spTransform(readOGR(paste0(GIS.path,"/SFER_GIS_Geodatabase.gdb"),"SFWMD_Canals"),utm17)

plot(lakeO)
plot(wmd.mon,pch=21,bg="dodgerblue1",add=T)
plot(canal,col="skyblue",add=T)

library(ceramic)
public.token="pk.eyJ1IjoicGp1bGlhbiIsImEiOiJjanllbmJ0eXkxMzV0M2dzNXh5NGRlYXdqIn0.g4weKGOt1WdNZLg2hxBz1w"
Sys.setenv(MAPBOX_API_KEY=public.token)

roi=extent(spTransform(gBuffer(lakeO,width=10000),wgs84))
im <- cc_location(roi)
plotRGB(im)
im=projectRaster(im,crs=utm17)
im=setValues(im,scales::rescale(values(im), c(0,255)))
plotRGB(im)


plotRGB(im,ext=raster::extent(gBuffer(lakeO,width=5000)))
plot(wmd.mon,pch=21,bg="dodgerblue1",col="white",add=T)
raster::text(wmd.mon,"SITE",col="white",pos=4,cex=0.75)
plot(canal,col="skyblue",add=T,xpd=F)
mapmisc::scaleBar(utm17,"bottomright",bty="n",cex=1,seg.len=4)

# -------------------------------------------------------------------------


dates=date.fun(c("2020-05-01","2021-01-19"))

c(date.fun(Sys.Date()-lubridate::duration(3,"months")),date.fun(Sys.Date()+lubridate::duration(5,"days")))


sites.dat=data.frame(Station.ID=c(rep("L001",5),rep("POLESOUT1",5),rep("POLESOUT3",5),rep("L005",5),rep("LZ40",5),rep("L006",5)),
                     param=rep(c("CF","CU","PF","PU","TURB"),6),
                     DBKEY.da=c(39923,39924,39925,39926,39922,
                                39959,39960,39961,39962,39958,
                                39975,39976,39977,39981,39974,
                                39886,39888,39889,39890,39885,
                                39941,39942,39943,39944,39940,
                                39905,39906,39907,39908,39904),depth="surface")
sonde.dat=data.frame()
for(i in 1:length(unique(sites.dat$Station.ID))){
  site.tmp=subset(sites.dat,Station.ID==unique(sites.dat$Station.ID)[i])
  site.tmp$DBKEY.da=as.character(site.tmp$DBKEY.da)
  tmp.dat=DBHYDRO_daily(dates[1],dates[2],site.tmp$DBKEY.da)
  tmp.dat$DBKEY=as.character(tmp.dat$DBKEY)
  tmp.dat=merge(tmp.dat,site.tmp,by.x="DBKEY",by.y="DBKEY.da")
  sonde.dat=rbind(sonde.dat,tmp.dat)  
  print(i)
}

plot(Data.Value~Date,subset(sonde.dat,DBKEY==39923))
plot(Data.Value~Date,subset(sonde.dat,DBKEY==39924))
plot(Data.Value~Date,subset(sonde.dat,DBKEY==39925))
plot(Data.Value~Date,subset(sonde.dat,DBKEY==39926))
plot(Data.Value~Date,subset(sonde.dat,DBKEY==39922))


sonde.dat.xtab=cast(sonde.dat,Station.ID+Date~param,value="Data.Value",mean)
sonde.dat.xtab$Date=date.fun(sonde.dat.xtab$Date)
sonde.dat.xtab$Date.num=as.numeric(sonde.dat.xtab$Date)
sonde.dat.xtab$phy_chl=with(sonde.dat.xtab,PU/(PU+CU))


# tiff(filename=paste0(plot.path,"LakeO_sonde_map.tiff"),width=3.5,height=3.75,units="in",res=200,type="windows",compression=c("lzw"),bg="white")
# png(filename=paste0(plot.path,"LakeO_sonde_map.png"),width=3.5,height=3.75,units="in",res=200,type="windows",bg="white")
plotRGB(im)# ,ext=raster::extent(gBuffer(lakeO,width=1)))
plot(wmd.mon,pch=21,bg="dodgerblue1",col="white",add=T,cex=1.25)
raster::text(wmd.mon,"SITE",col="white",pos=4,cex=0.75)
# plot(canal,col="skyblue",add=T,xpd=F)
mapmisc::scaleBar(utm17,"bottomright",bty="n",cex=1,seg.len=4,col="white")
dev.off()

# tiff(filename=paste0(plot.path,"LakeO_sonde.tiff"),width=8,height=5,units="in",res=200,type="windows",compression=c("lzw"),bg="white")
# xlim.val=date.fun(dates);xmaj=seq(xlim.val[1],xlim.val[2],"4 months");xmin=seq(xlim.val[1],xlim.val[2],"1 months")
period.date=c(date.fun(Sys.Date()-lubridate::duration(1,"months")),date.fun(Sys.Date()+lubridate::duration(5,"days")))
xlim.val=date.fun(period.date);xmaj=seq(xlim.val[1],xlim.val[2],"28 days");xmin=seq(xlim.val[1],xlim.val[2],"7 days")
par(family="serif",mar=c(1,1.5,0.25,0.25),oma=c(2.5,5.5,1,0.5));
layout(matrix(c(1:24),4,6,byrow=F))


site.list=unique(sites.dat$Station.ID)
for(i in 1:length(site.list)){
tmp.dat=subset(sonde.dat.xtab,Station.ID==site.list[i])
axisln.val=3
ylim.val=c(0,100);by.y=50;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
plot(CF~Date,tmp.dat,ylim=ylim.val,xlim=xlim.val,type="n",axes=F,ylab=NA,xlab=NA)
abline(h=ymaj,v=xmaj,lty=3,col="grey")
with(tmp.dat,pt_line(Date,CU,2,"mediumspringgreen",1,21,"mediumspringgreen",pt.lwd=0.1))
# tmp.loess=loess(CU~Date.num,tmp.dat)
# fit=predict(tmp.loess,tmp.dat)
# lines(tmp.dat$Date,fit,col="indianred1",lwd=2)
axis_fun(1,xmaj,xmin,NA)
if(i==1){axis_fun(2,ymaj,ymin,ymaj)}else{axis_fun(2,ymaj,ymin,NA)}
box(lwd=1)
if(i==1){mtext(side=2,line=axisln.val,"Chlorophyll\n(\u03BCg L\u207B\u00B9)")}
mtext(side=3,adj=0,site.list[i],cex=0.75)

ylim.val=c(0,1500);by.y=500;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
plot(PF~Date,tmp.dat,ylim=ylim.val,xlim=xlim.val,type="n",axes=F,ylab=NA,xlab=NA)
abline(h=ymaj,v=xmaj,lty=3,col="grey")
with(tmp.dat,pt_line(Date,PU,2,"lightseagreen",1,21,"lightseagreen",pt.lwd=0.1))
# if(sum(is.na(tmp.dat$PU))/nrow(tmp.dat)==1){NA}else{
#   tmp.loess=loess(PU~Date.num,tmp.dat)
#   fit=predict(tmp.loess,tmp.dat)
#   lines(tmp.dat$Date,fit,col="indianred1",lwd=2)}
axis_fun(1,xmaj,xmin,NA)
if(i==1){axis_fun(2,ymaj,ymin,ymaj)}else{axis_fun(2,ymaj,ymin,NA)}
box(lwd=1)
if(i==1){mtext(side=2,line=axisln.val,"Phycocyanin\n(\u03BCg L\u207B\u00B9)")}

ylim.val=c(0,1.1);by.y=0.5;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
plot(phy_chl~Date,tmp.dat,ylim=ylim.val,xlim=xlim.val,type="n",axes=F,ylab=NA,xlab=NA)
abline(h=ymaj,v=xmaj,lty=3,col="grey")
with(tmp.dat,pt_line(Date,phy_chl,2,"grey",1,21,"grey",pt.lwd=0.1))
# if(sum(is.na(tmp.dat$phy_chl))/nrow(tmp.dat)==1){NA}else{
#   tmp.loess=loess(phy_chl~Date.num,tmp.dat)
#   fit=predict(tmp.loess,tmp.dat)
#   lines(tmp.dat$Date,fit,col="indianred1",lwd=2)}
# axis_fun(1,xmaj,xmin,NA)
if(i==1){axis_fun(2,ymaj,ymin,format(ymaj))}else{axis_fun(2,ymaj,ymin,NA)}
box(lwd=1)
if(i==1){mtext(side=2,line=axisln.val,expression(paste(frac("Phyco","Phyco + Chl"))),adj=0.5)}

ylim.val=c(0,250);by.y=50;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
plot(TURB~Date,tmp.dat,ylim=ylim.val,xlim=xlim.val,type="n",axes=F,ylab=NA,xlab=NA)
abline(h=ymaj,v=xmaj,lty=3,col="grey")
with(tmp.dat,pt_line(Date,TURB,2,"grey",1,21,"lightgoldenrod2",pt.lwd=0.1))
# tmp.loess=loess(TURB~Date.num,tmp.dat)
# fit=predict(tmp.loess,tmp.dat)
# lines(tmp.dat$Date,fit,col="indianred1",lwd=2)
axis_fun(1,xmaj,xmin,format(xmaj,"%m-%d-%y"),line=-0.5)
if(i==1){axis_fun(2,ymaj,ymin,ymaj)}else{axis_fun(2,ymaj,ymin,NA)}
box(lwd=1)
if(i==1){mtext(side=2,line=axisln.val,"Turbidity\n(NTU)")}
}
mtext(side=1,outer=T,line=1,"Date (Month-Day-Year)")

dev.off()
