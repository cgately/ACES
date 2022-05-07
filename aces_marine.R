
shiplanes <- fread('shippingLanes_1km_inter.csv')
shiplanes[,frac:=Shape_Area/area_m2]
shps <- shiplanes[,.(ShapeID,cellid,frac)]
setkey(shps,ShapeID)

port <- fread('ports_1km_inter.csv')
port[,frac:=Shape_Area/area_m2]
prts <- port[,.(ShapeID,cellid,frac)]
setkey(prts,ShapeID)

frac <- fread('marine_shape_frac_cnty_co2.csv')
setnames(frac,c('fips','ShapeID','scc','EmissionTypeCode','Pollutant','ShapeCO2','CountyCO2','frac'))
frac[,st:=substr(sprintf("%05d",fips),1,2)]
frac <- frac[!st %in% c('02','15','72','78','85')]

marine <- fread('nei2014_marine_nonpoint_CO.csv')


#////////////////////////////////////////////////////////////////////////////////////////////////////////////
# Marine emission factors are obtained from EPA Current Methodologies in Preparing Mobile Source Port-Related Emission Inventories Final Report April 2009
#////////////////////////////////////////////////////////////////////////////////////////////////////////////

# We assume all marine vessels that burn Residual Oil as fuel have the emissions characteristics of Ocean-Going Vessels, and are all 'Medium-Speed Diesel'. We use Table 2-9: Emission Factors for OGV Main Engines.

# for NEI "Underway emissions" and use Table 2-16: Auxiliary Engine Emission Factors for NEI "Port Emissions", in both cases partition emission factors by fuel type (RO = Residual Oil, MDO = Marine Diesel Oil)

marine$kgco2[marine$level3=="Residual" & marine$level4=="Port emissions"]<-marine$total_emissions[marine$level3=="Residual" & marine$level4=="Port emissions"] * 907185 / 1.1 * 722.54 / 1000 # (short tons CO) * (grams per short ton) / (gCO per kWh) * (gCO2 per kWh) / (grams per kg)

marine$kgco2[marine$level3=="Residual" & marine$level4=="Underway emissions"]<-marine$total_emissions[marine$level3=="Residual" & marine$level4=="Underway emissions"] * 907185 / 1.1 * 677.91 / 1000 # (short tons CO) * (grams per short ton) / (gCO per kWh) * (gCO2 per kWh) / (grams per kg)

# We assume that marine vessels that burn Diesel fuel in the 'Port Emissions' category are 50% Ocean-Going Vessels and 50% Harbor craft. For Diesel-burning Ocean-Going Vessels in port, we use Table 2-16: Auxiliary Engine Emission Factors, Fuel Type = MDO

# We assume that marine vessels that burn Diesel fuel in the 'Underway emissions' category are 100% Harbor Craft.

# For Category 1 Harbor Craft, we use a CO emission factor of 1.5 g / kWh  which is the median value for Tier 0 / Tier 1 / Category 1 vessels. We use a CO emission factor of 1.1 for Category 2 Harbor Craft. We assume their are no Tier 2 craft. CO2 emission factor for all Harbor Craft is 690g/kWh , as in Table 3.8

# We partition Harbor craft activity (both in port and underway) as 25% Category 2 vessels and 75% Category 1 vessels, following EPA Report: Regulatory Impact Analysis-Control of Emissions of Air Pollution from Locomotive Engines and Marine Compression Ignition Engines Less than 30 Liters per Cylinder. EPA420-R-08-001a. May 2008.

marine$kgco2[marine$level3=="Diesel" & marine$level4=="Port emissions"]<- (marine$total_emissions[marine$level3=="Diesel" & marine$level4=="Port emissions"] / 2 * 907185 / 1.1 * 690.71 / 1000) + (marine$total_emissions[marine$level3=="Diesel" & marine$level4=="Port emissions"] / 2 * 0.75 * 907185 / 1.5 * 690 / 1000) + (marine$total_emissions[marine$level3=="Diesel" & marine$level4=="Port emissions"] / 2 * 0.25 * 907185 / 1.1 * 690 / 1000)
# { (short tons CO) / (2) * (grams per short ton) / (gCO per kWh) * (gCO2 per kWh) / (grams per kg) } + { (short tons CO) / (2) * (0.75) * (grams per short ton) / (gCO per kWh) * (gCO2 per kWh) / (grams per kg) } + { (short tons CO) / (2) * (0.25)  * (grams per short ton) / (gCO per kWh) * (gCO2 per kWh) / (grams per kg) }

marine$kgco2[marine$level3=="Diesel" & marine$level4=="Underway emissions"]<-(marine$total_emissions[marine$level3=="Diesel" & marine$level4=="Underway emissions"] * 0.75 * 907185 / 1.5 * 690 / 1000) + (marine$total_emissions[marine$level3=="Diesel" & marine$level4=="Underway emissions"] * 0.25 * 907185 / 1.1 * 690 / 1000)
# (short tons CO) * 0.75 * (grams per short ton) / (gCO per kWh) * (gCO2 per kWh) / (grams per kg) + (short tons CO) * 0.25 * (grams per short ton) / (gCO per kWh) * (gCO2 per kWh) / (grams per kg)


mar.sum <- marine[,lapply(.SD,sum,na.rm=T),by=list(fips,scc),.SDcols='kgco2']
setkey(mar.sum,fips,scc)
setkey(frac,fips,scc)

frac <- mar.sum[frac]
frac[!is.na(kgco2),ShapeCO2:=frac*kgco2]
frac <- frac[,lapply(.SD,sum,na.rm=T),by=ShapeID,.SDcols='ShapeCO2']
setnames(frac,'ShapeCO2','kgFFco2')
setkey(frac,ShapeID)
fwrite(frac,'cmv_polygon_kgco2.csv')
shps <- frac[shps]
shps[,kgFFco2:=frac*kgFFco2]
shps[,kgBIOco2:=0]
out <- shps[,lapply(.SD,sum,na.rm=T),by=cellid,.SDcols=c('kgBIOco2','kgFFco2')]
prts <- frac[prts]
prts[,kgFFco2:=frac*kgFFco2]
prts[,kgBIOco2:=0]
out2 <- prts[,lapply(.SD,sum,na.rm=T),by=cellid,.SDcols=c('kgBIOco2','kgFFco2')]
out <- rbind(out,out2)
out <- out[,lapply(.SD,sum,na.rm=T),by=cellid,.SDcols=c('kgBIOco2','kgFFco2')]
fwrite(out,'cmv_kgco2_1km.csv')
