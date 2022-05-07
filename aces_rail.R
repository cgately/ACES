
lp <- fread('nei2014_locomotives_point_CO.csv')
setnames(lp,'st_usps_cd','state')
setnames(lp,'state_and_county_fips_code','fips')
lnp <- fread('nei2014_locomotives_nonpoint_CO.csv')
lnr <- fread('nei2014_railroad_equipment_CO2.csv')

rail <- rbind(lp,lnp,lnr,fill=T)
rail <- rail[,.(state,fips,scc,latitude_msr,longitude_msr,level1,level2,level3,level4,uom,total_emissions)]
setkey(rail,state)
fips <- fread('state_fips_codes.csv')
fips[,st:=STATEFP][,STATEFP:=NULL]
setkey(fips,state)
rail <- fips[rail]

# Rail CO emission factors obtained from EPA Emission Factors for Locomotives Report: EPA-420-F-09-025, Tables 1,2,3   //  Office of Transportation and Air Quality EPA-420-F-09-025 April 2009

rail[,kgFFco2:=total_emissions * 907185 / 1.28 / 20.8 * 10.07391]
# (short tons CO) * (grams per short ton) / (gCO per bhp-hr) / (bhp-hr per gallon) * (kg CO2/ gallon) --- Factors correspond to "Large Line-Haul and Passenger" in Table 3 and "Line Haul Emission Factors" from Table 1. CO2 factor from Vulcan Table A.3, matID 44 --> (0.0735 tonnes CO2 / 10^6btu) * (137.06 10^6btu/10^3gal)

rail[level4=="Line Haul Locomotives: Commuter Lines",kgFFco2:=total_emissions * 907185 / 1.28 / 18.2 * 10.07391]

# (short tons CO) * (grams per short ton) / (gCO per bhp-hr) / (bhp-hr per gallon) * (kg CO2/ gallon) --- Factors correspond to "Small Line-Haul" in Table 3 and "Line Haul Emission Factors" from Table 1. CO2 factor from Vulcan Table A.3, matID 44 --> (0.0735 tonnes CO2 / 10^6btu) * (137.06 10^6btu/10^3gal)

rail[level4=="Yard Locomotives",kgFFco2:=total_emissions * 907185 / 1.83 / 15.2 * 10.07391]
# (short tons CO) * (grams per short ton) / (gCO per bhp-hr) / (bhp-hr per gallon) * (kg CO2/ gallon) --- Factors correspond to "Switch Emission Factors" in Table 2 and to "Switching" factor in Table 3. CO2 factor from Vulcan Table A.3, matID 44 --> (0.0735 tonnes CO2 / 10^6btu) * (137.06 10^6btu/10^3gal)
fwrite(rail,'rail_raw_all.csv')

rail.pt <- rail[!is.na(latitude_msr)]
setnames(rail.pt,'latitude_msr','lat')
setnames(rail.pt,'longitude_msr','lon')
rail.pt<-rail.pt[,lapply(.SD,sum,na.rm=T),by=list(lat,lon),.SDcols='kgFFco2']

vlcc <- "+proj=lcc +lat_1=33 +lat_2=45 +lat_0=40 +lon_0=-97 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"
make_shape <- function(x,proj,outname){
  coordinates(x) <- ~lon+lat
  projection(x) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
  crs <- CRS(proj)
  fp <- spTransform(x,crs)
  writeOGR(fp,dsn='.',layer=outname,driver='ESRI Shapefile',overwrite=T)
}
make_shape(rail.pt,vlcc,'railyard_point_kgco2')

rail.np <- rail[is.na(latitude_msr)]
rail.np[,scc:=as.character(scc)]
rail.np[scc=='2285002008',scc:='2285002006'] # Amtrak is a Class I railroad
rail.np[scc %in% c('2285002009','2285002010'),scc:='2285002007']

rail.np<-rail.np[,lapply(.SD,sum,na.rm=T),by=list(fips,scc),.SDcols='kgFFco2']
rail.np[,scc:=as.character(scc)]
setkey(rail.np,fips,scc)

sh <- fread("rail_shape_frac_cnty.csv")
setnames(sh,c('fips','shapeID','scc','shp_act','cnty_act','frac'))
sh <- sh[!is.na(frac)]
sh[,fips:=as.numeric(fips)]
sh[,scc:=as.character(scc)]
setkey(sh,fips,scc)
sh <- rail.np[sh]
sh[,kgFFco2:=frac*kgFFco2]
sh <- sh[,.(kgFFco2,shapeID)]
setkey(sh,shapeID)

rw <- fread('railway_1km_inter.csv')
setnames(rw,c('oid','shapeID','len_meters','cellid','shp_len'))
rw[,fr:=shp_len/len_meters]
rw <- rw[,.(shapeID,fr,cellid)]
setkey(rw,shapeID)

rw <- sh[rw]
rw[,kgFFco2:=fr*kgFFco2]
rsum <- rw[,lapply(.SD,sum,na.rm=T),by=list(cellid),.SDcols='kgFFco2']

ry <- fread('railyard_point_1km.csv')
ry[,OBJECTID:=NULL]
ry <- rbind(rsum,ry)
ry <- ry[,lapply(.SD,sum,na.rm=T),by=list(cellid),.SDcols='kgFFco2']

fwrite(ry,'rail_kgco2_1km.csv')


