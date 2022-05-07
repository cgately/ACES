make_shape <- function(x,proj,outname){
  coordinates(x) <- ~lon+lat
  projection(x) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
  crs <- CRS(proj)
  fp <- spTransform(x,crs)
  writeOGR(fp,dsn='.',layer=outname,driver='ESRI Shapefile',overwrite=T)
}

air.co <- fread('nei2014_aircraft_CO.csv')
air.co[,factor:=130] # lbs CO / Thousand Gallons jet fuel from Vulcan Appendix A.1 MAT_ID=159
air.co[,fuelqty:=total_emissions*2000/factor]
air.co[,measure:="1000 Gallons"]
air.co[,co2measure:="1000 Gallons"]
air.co[,co2factor:=9.57 * 1000] # kg CO2 per 1000 gallon jet fuel
#Source: https://www.eia.gov/environment/emissions/co2_vol_mass.php
air.co[level4=='Piston',co2factor:=8.35 * 1000] # kg CO2 per 1000 gallon aviation gasoline
air.co[,fuel:='jet fuel']
air.co[level4=='Piston',fuel:='avgas']
air.co[st_usps_cd=='DC',fuel:='avgas']
air.co[st_usps_cd=='DC',co2factor:=8.35 * 1000]
air <- air.co[,.(st_usps_cd,latitude_msr,longitude_msr,fuel,fuelqty,co2factor)]
setnames(air,c('state','lat','lon','fuel','fuelConsumed','co2factor'))
air <- air[!state %in% c('AK','HI','PR')]
setkey(air,state,fuel)

seds <- fread('Complete_SEDS.csv')
seds <- seds[,2:5,with=F]
setnames(seds,c('code','state','year','value'))
seds <- seds[!state %in% c('AK','HI','PR','US','X5','X3')]
seds <- seds[code %in% c('AVACP','JFACP','JNACP')]
seds[,sedsUnit:='Thousand barrels']
fips <- fread('state_fips_codes.csv')
fips[,st:=sprintf("%02.0f",STATEFP)]
setkey(fips,state)
setkey(seds,state)
seds <- fips[seds]
seds <- seds[year %in% 2012:2017]
sw <- dcast.data.table(seds,st + state + code + sedsUnit ~ year,value.var='value',fun.aggregate = sum)
setnames(sw,c('st','state','code','sedsUnit',paste0('s',2012:2017)))
sw[code=='AVACP',code:='avgas'][code %in% c('JFACP','JNACP'),code:='jet fuel']
setnames(sw,'code','fuel')
sw <- sw[,lapply(.SD,sum),by=list(st,state,fuel,sedsUnit),.SDcols=c('s2012','s2013','s2014','s2015','s2016','s2017')]
for (c in c(5:10)) set(sw, j=c, value=sw[[c]]*42)
sw[,sedsUnit:='Thousand Gallons']
sw[,st:=NULL]
setkey(sw,state,fuel)

am <- merge(air,sw,key=c('state','fuel'),all.x=T)
am[,s14:=sum(fuelConsumed),by=list(state,fuel)][,share:=fuelConsumed/s14][,diff:=s2014-s14][,fuel2:=fuelConsumed + (share*diff)]
am[,kgFFco2_2014:=fuel2*co2factor]
am[,kgFFco2_2012:=s2012/s2014*kgFFco2_2014]
am[,kgFFco2_2013:=s2013/s2014*kgFFco2_2014]
am[,kgFFco2_2015:=s2015/s2014*kgFFco2_2014]
am[,kgFFco2_2016:=s2016/s2014*kgFFco2_2014]
am[,kgFFco2_2017:=s2017/s2014*kgFFco2_2014]
am[,ptID:=.I]
setkey(am,ptID)

out <- copy(am[,.(ptID,lat,lon,kgFFco2_2012,kgFFco2_2013,kgFFco2_2014,kgFFco2_2015,kgFFco2_2016,kgFFco2_2017)])
setnames(out,c('ptID','lat','lon','co2_2012','co2_2013','co2_2014','co2_2015','co2_2016','co2_2017'))
coordinates(out) <- ~lon+lat
projection(out) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
writeOGR(out,dsn='.',layer='aces_airports_kgco2',driver='ESRI Shapefile',overwrite=T)
## Intersect in ArcGIS

infile <- shapefile('aces_airports_1km.shp')
ap <- data.table(ptID=infile@data$ptID,cellid=infile@data$cellid)
setkey(ap,ptID)

am <- ap[am]
cs <- colnames(am)[20:25]
am <- am[,lapply(.SD,sum),by=cellid,.SDcols=(cs)]
saveRDS(am,'airplane_kgco2_2012_2017.rds')

a1 <- fread('airport_code_lat_lon_xwalk.csv')
a1 <- a1[IATA!='',.(IATA,Latitude,Longitude,Name)]
setnames(a1,c('IATA','lat_dd','lon_dd','name'))

a2<-fread('our_airports.csv')
setnames(a2, 'iata_code', 'IATA')
a2<-a2[IATA!='',.(IATA,latitude_deg,longitude_deg,name)]
setnames(a2,'latitude_deg','lat_dd')
setnames(a2,'longitude_deg','lon_dd')

a3<-fread('GlobalAirportDatabase.csv')
setnames(a3,2,'IATA')
setnames(a3,3,'name')
setnames(a3,6:8,c('lat_deg','lat_min','lat_sec'))
setnames(a3,10:12,c('lon_deg','lon_min','lon_sec'))
a3 <- a3[lon_deg!=0 & lat_deg!=0]
a3[,lat:=paste(lat_deg,lat_min,lat_sec,sep=' ')][,lon:=paste(lon_deg,lon_min,lon_sec,sep=' ')]
a3[,lat_dd:=conv_unit(a3$lat,'deg_min_sec','dec_deg')]
a3[,lon_dd:=conv_unit(a3$lon,'deg_min_sec','dec_deg')]
a3<-a3[IATA!='',.(IATA,lat_dd,lon_dd,name)]

iata <- rbind(a1,a2,a3)
iata[,lat_dd:=round(as.numeric(lat_dd),6)][,lon_dd:=round(as.numeric(lon_dd),6)]
iata <- iata[IATA!='N/A']
fwrite(iata,'all_airport_lat_lon.csv')
setnames(iata,c('IATA','lat','lon','name'))
wgs <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs "
make_shape(iata,wgs,'all_airports')

ms <- data.table(mt=c('jan','feb','mar','apr','may','jun','jul','aug','sep','oct','nov','dec'),mon=seq(1,12))
setkey(ms,mt)
for (y in 2012:2017){
  day <- fread(paste0('daily_flights_',y,'.csv'))
  setnames(day,'airport','IATA')
  dd <- read.table(text=day$date,sep='/')
  day <- cbind(day,dd)
  setnames(day,c('IATA','date','dep','arr','mon','day','year'))
  day[,date:=paste0(year,sprintf("%02d",mon),sprintf("%02d",day))]
  setkey(day,IATA,date)
  times <- fread(paste0('times',y,'.csv'))
  t2 <- times[,.(date)]
  t2 <- unique(t2)
  t2[,date:=as.character(date)]
  t3 <- do.call("rbind", replicate(length(unique(day$IATA)), t2, simplify = FALSE))
  t3[,IATA:=sort(rep(unique(day$IATA),length(t2$date)))]
  setkey(t3,IATA,date)
  t3 <- day[t3]
  setorder(t3,IATA,date)
  t3[,dep:=na.locf0(dep),by=IATA][,arr:=na.locf0(arr),by=IATA]
  t3[,dep:=na.locf0(dep,fromLast = T),by=IATA][,arr:=na.locf0(arr,fromLast = T),by=IATA]
  t3[,dtot:=dep+arr][,ann:=sum(dtot),by=IATA][,dsh:=dtot/ann]
  t3[is.na(year),year:=as.numeric(substr(date,1,4))]
  t3[is.na(mon),mon:=as.numeric(substr(date,5,6))]
  t3[is.na(day),day:=as.numeric(substr(date,7,8))]

  hr <- fread(paste0('hourly_flights_',y,'.csv'))
  setnames(hr,c('IATA','samp','hr','hrUTC','dep','arr','mdep','marr'))
  hr[,dep:=gsub(',','',dep)][,arr:=gsub(',','',arr)][,mdep:=gsub(',','',mdep)][,marr:=gsub(',','',marr)]
  hr[,dep:=as.numeric(dep)][,arr:=as.numeric(arr)][,mdep:=as.numeric(mdep)][,marr:=as.numeric(marr)]
  hr <- unique(hr,by=c('IATA','samp','hr'))
  hr[,mt:=tolower(substr(samp,1,3))]
  setkey(hr,mt)
  hr <- ms[hr]
  fll <- data.table(IATA=sort(rep(sort(unique(hr$IATA)),24*12)),mon=rep(sort(rep(1:12,24)),length(unique(hr$IATA))) ,hr=rep(rep(0:23,12),length(unique(hr$IATA))))
  setkey(fll,IATA,mon,hr)
  setkey(hr,IATA,mon,hr)
  hr2 <- hr[fll]
  hr2[,dep:=na.locf0(dep),by=list(IATA,mon)]
  hr2[,arr:=na.locf0(arr),by=list(IATA,mon)]
  hr2[,mdep:=na.locf0(mdep),by=list(IATA,mon)]
  hr2[,marr:=na.locf0(marr),by=list(IATA,mon)]
  hr2[,dtot:=dep+arr][,ann:=sum(dtot),by=list(IATA,mon)][,sh:=dtot/ann]
  hr2[,md:=mdep+marr][,mann:=sum(md),by=list(IATA,mon)][,hrsh:=md/mann]
  hr2[,x:=shift(hrUTC)+1]
  hr2[is.na(hrUTC),hrUTC:=x]
  hr2 <- hr2[,.(IATA,mon,hr,hrUTC,hrsh)]
  setkey(hr2,mon,IATA)
  setkey(t3,mon,IATA)
  saveRDS(t3,paste0('day_shares_',y,'.rds'))
  saveRDS(hr2,paste0('hour_shares_',y,'.rds'))
  dfac <- iata[IATA %in% unique(day$IATA)]
  hfac <- iata[IATA %in% unique(hr2$IATA)]
}
for (y in 2012:2017){
  m <- fread(paste0('airmatch',y,'.csv'))
  setnames(m,paste0('dayports',y,'_IATA'),'IATA')
  setnames(m,paste0('hourports',y,'_IATA'),'IATA2')
  m <- m[,.(IATA,IATA2)]
  m <- unique(m,by='IATA')
  day <- readRDS(paste0('day_shares_',y,'.rds'))
  hr2 <- readRDS(paste0('hour_shares_',y,'.rds'))
  setnames(hr2,'IATA','IATA2')
  setkey(m,IATA)
  setkey(day,IATA)
  day <- m[day]
  day <- day[!is.na(IATA2)]
  setkey(day,mon,IATA2)
  setkey(hr2,mon,IATA2)
  full <- merge(day,hr2,all=T,allow.cartesian=T,key=c('mon','IATA2'))
  full <- full[!is.na(dsh) & !is.na(hrsh)]
  full[,datetime:=as.POSIXct(paste0(year,'-',sprintf("%02d",mon),'-',sprintf("%02d",day),' ',sprintf("%02d",hrUTC),":00:00"),tz='UTC')]
  airshare <- full[,.(IATA,datetime,date,hr,hrUTC,dsh,hrsh)]
  airshare[,frac:=dsh*hrsh]
  times <- fread(paste0('times',y,'.csv'))
  t2 <- times[,.(datetime,chr)]
  setkey(t2,datetime)
  airshare[,datetime:=as.numeric(paste0(date,sprintf("%02d",hr)))]
  setkey(airshare,datetime)
  airshare <- t2[airshare]
  airshare <- airshare[,.(datetime,chr,frac,IATA)]
  airshare[,year:=as.numeric(substr(datetime,1,4))]
  setkey(airshare,year,IATA)
  saveRDS(airshare,paste0('airshare',y,'.rds'))
  hrly_facs <- iata[IATA %in% unique(airshare$IATA)]
  setnames(hrly_facs,c('IATA','lat','lon','name'))
  rm(airshare,full);gc()
  }

# Used the Near tool in ArcGIS to match facilities with emissions to airports with hourly data
# and to 1km cellid and timezone codes

air <- fread('airports_kgco2_1km_2012_2017.csv')

airshare <- readRDS('airshare2012.rds')
air12 <- air[,.(cellid,ptID,co2_2012,iata12,zone,tz_name1st)]
setnames(air12,'iata12','IATA')
air12[,year:=2012]
setkey(air12,year,IATA)
hr12 <- merge(air12,airshare,all=T, allow.cartesian=T,key=c('year','IATA'))
rm(airshare);gc()

airshare <- readRDS('airshare2013.rds')
air13 <- air[,.(cellid,ptID,co2_2013,iata13,zone,tz_name1st)]
setnames(air13,'iata13','IATA')
air13[,year:=2013]
setkey(air13,year,IATA)
hr13 <- air13[airshare]
rm(airshare);gc()

airshare <- readRDS('airshare2014.rds')
air14 <- air[,.(cellid,ptID,co2_2014,iata14,zone,tz_name1st)]
setnames(air14,'iata14','IATA')
air14[,year:=2014]
setkey(air14,year,IATA)
hr14 <- air14[airshare]
rm(airshare);gc()

airshare <- readRDS('airshare2015.rds')
air15 <- air[,.(cellid,ptID,co2_2015,iata15,zone,tz_name1st)]
setnames(air15,'iata15','IATA')
air15[,year:=2015]
setkey(air15,year,IATA)
hr15 <- air15[airshare]
rm(airshare);gc()

airshare <- readRDS('airshare2016.rds')
air16 <- air[,.(cellid,ptID,co2_2016,iata16,zone,tz_name1st)]
setnames(air16,'iata16','IATA')
air16[,year:=2016]
setkey(air16,year,IATA)
hr16 <- air16[airshare]
rm(airshare);gc()

airshare <- readRDS('airshare2017.rds')
air17 <- air[,.(cellid,ptID,co2_2017,iata17,zone,tz_name1st)]
setnames(air17,'iata17','IATA')
air17[,year:=2017]
setkey(air17,year,IATA)
hr17 <- air17[airshare]
rm(airshare);gc()


tz_adjust <- function(x){
  east <- x[tz_name1st=='America/New_York']
  cent <- x[tz_name1st=='America/Chicago']
  moun <- x[tz_name1st=='America/Denver']
  west <- x[tz_name1st=='America/Los_Angeles']

  east[,time:=as.POSIXct(paste0(substr(datetime,1,4),'-',substr(datetime,5,6),'-',substr(datetime,7,8),' ',substr  (datetime,9,10),':00:00'),tz='America/New_York')]
  cent[,time:=as.POSIXct(paste0(substr(datetime,1,4),'-',substr(datetime,5,6),'-',substr(datetime,7,8),' ',substr  (datetime,9,10),':00:00'),tz='America/Chicago')]
  cent[,time:=with_tz(time,'America/New_York')]
  moun[,time:=as.POSIXct(paste0(substr(datetime,1,4),'-',substr(datetime,5,6),'-',substr(datetime,7,8),' ',substr  (datetime,9,10),':00:00'),tz='America/Denver')]
  moun[,time:=with_tz(time,'America/New_York')]
  west[,time:=as.POSIXct(paste0(substr(datetime,1,4),'-',substr(datetime,5,6),'-',substr(datetime,7,8),' ',substr  (datetime,9,10),':00:00'),tz='America/Los_Angeles')]
  west[,time:=with_tz(time,'America/New_York')]
  hr <- rbind(west,east,cent,moun)
  hr <- hr[,.(cellid,elec,chr,time,tzg,tz,kgBIOco2,kgFFco2)]
  hr[tzg<(-8),tzg:=-8][tzg>(-5),tzg:=-5]
  hr[,tzdiff:=tzg-tz]
  hr[tzdiff!=0,time:=time-(60*60*tzdiff)]

    if (yr %in% c(2012,2016)){
      hr[time==paste0((yr+1),'-01-01 00:00:00 EST'),time:=time-(8784*3600)]
      hr[time==paste0((yr+1),'-01-01 01:00:00 EST'),time:=time-(8784*3600)]
      hr[time==paste0((yr+1),'-01-01 02:00:00 EST'),time:=time-(8784*3600)]
    } else {
      hr[time==paste0((yr+1),'-01-01 00:00:00 EST'),time:=time-(8760*3600)]
      hr[time==paste0((yr+1),'-01-01 01:00:00 EST'),time:=time-(8760*3600)]
      hr[time==paste0((yr+1),'-01-01 02:00:00 EST'),time:=time-(8760*3600)]
    }


}
