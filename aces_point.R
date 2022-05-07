##-------- NEI data processing ------------##


# Load 2014 NEI CO emissions facilities
n1 <- fread('2014neiv1_facility_process_byregions/process_12345.csv')
n1 <- n1[pollutant_cd=='CO']
n2 <- fread('2014neiv1_facility_process_byregions/process_678910.csv')
n2 <- n2[pollutant_cd=='CO']

nei <- rbind(n1,n2)
setnames(nei,'eis_facility_site_id','orisID')
fwrite(nei,'nei2014_CO_all_raw.csv')

nei <- fread('nei2014_CO_all_raw.csv')
nei[,scc:=as.character(scc)]
nei <- nei[,lapply(.SD,sum),by=list(orisID,scc,st_usps_cd,state_and_county_fips_code,facility_site_name,pollutant_cd,uom,latitude_msr,longitude_msr),.SDcols='total_emissions']
setkey(nei,scc)
nei <- nei[latitude_msr!="." & longitude_msr!="."  ]

# Merge SCC code attributes
wscc <- fread('WebFIRESCCs.csv')
setnames(wscc,tolower(colnames(wscc)))
wscc <- wscc[,.(scc,scc_l1,scc_l2,scc_l3,scc_l4)]
wscc[,scc:=as.integer64(scc)]
setnames(wscc,c('scc','level1','level2','level3','level4'))
setkey(wscc,scc)
wscc <- wscc[!is.na(scc)]
scodes<-fread("scc_list.csv") #SCC code descriptions per EPA
scodes <- rbind(scodes,wscc,fill=T)
scodes[,scc:=as.integer64(scc)]
scodes <- unique(scodes,by='scc')
sc <- fread('all_scc_codes.csv')
setnames(sc,tolower(colnames(sc)))
sc <- sc[,c(1,12:17)]
setnames(sc,c('scc','level4','level1','level3','level2','sector','short_name'))
scodes[,scc:=as.character(scc)]
scodes <- rbind(sc,scodes,fill=T)
scodes <- unique(scodes,by='scc')
scodes <- scodes[!is.na(scc)]

setkey(scodes,scc)
fwrite(scodes,'scc_codes_all_descriptions.csv')
nei <- scodes[nei]
nei[,category:=NULL][,EIS:=NULL]

# Export other sector emissions
air <- nei[sector %in% c('Mobile - Aircraft')]
fwrite(air,'../../Air/nei2014_aircraft_point_CO.csv')
rail<- nei[sector %in% c('Mobile - Locomotives')]
fwrite(rail,'../../Rail/nei2014_locomotives_point_CO.csv')
marine <- nei[sector %in% c("Mobile - Commercial Marine Vessels")]
fwrite(marine,'../../Marine/nei2014_marine_point_CO.csv')
nonroad <- nei[sector %in% c('Mobile - Non-Road Equipment - Diesel','Mobile - Non-Road Equipment - Gasoline','Mobile - Non-Road Equipment - Other')]
fwrite(nonroad,'../../Nonroad/nei2014_nonroad_point_CO.csv')

# Subset to only fuel combustion emissions
nei <- nei[substr(sector,1,9)=='Fuel Comb']

# Hand fix some SCC codes
nei[scc=='10100317',scc:='10100316'][scc=='10200506',scc:='10200502'][scc=='20100907',scc:='20100902'][scc=='20200909',scc:='20200901'][scc=='20300209',scc:='20300202']

# Load set of emissions factors
ef<-fread("fire_clean_ef.csv")
ef <- ef[nei_pollutant_code=='CO']
ef <- ef[,.(scc,nei_pollutant_code,factor,unit,measure,material)]
ef2 <- fread('WebFIREFactors.csv')
ef2 <- ef2[NEI_POLLUTANT_CODE=='CO']
setnames(ef2,tolower(colnames(ef2)))
ef2 <- ef2[,.(scc,nei_pollutant_code,factor,unit,measure,material)]
ef2[,factor:=as.numeric(factor)]
ef <- rbind(ef,ef2)
ef <- unique(ef, by='scc')
ef[,scc:=as.character(scc)]
setkey(ef,scc)
fwrite(ef,'all_ef_clean.csv')
nei <- ef[nei]

# Split matches and no matches
match <- nei[!is.na(factor)]
nm <- nei[is.na(factor)]
setkey(nm,scc)

# Clean SCC codes / EFs
source('scc_fix.R')
setnames(af,'factor','f2')
af[,u2:=tstrsplit(unitFactor,'/')[[1]]][,m2:=tstrsplit(unitFactor,'/')[[2]]]
af <- af[,.(scc,f2,u2,m2)]
af[,scc:=as.character(scc)]
setkey(af,scc)
nm <- af[nm]
nm[is.na(unit),unit:=u2][is.na(factor),factor:=f2][is.na(measure),measure:=m2]
nm[,u2:=NULL][,f2:=NULL][,m2:=NULL]

# Gather matched data
match <- rbind(match,nm[!is.na(factor)])
setkey(match,scc)

# Split out still un-matched data
nm <- nm[is.na(factor)]
nm <- nm[!level1 %in% c('Industrial Processes','Petroleum and Solvent Evaporation','Waste Disposal','MACT Source Categories','Mobile Sources','Chemical Evaporation')]
nm <- nm[level3 %in% c('Residual oil','Distillate Oil','Commercial/Institutional','Distillate Oil (Diesel)','Natural Gas',' Gasified Coal','Kerosene/Naphtha (Jet Fuel)','Subbituminous coal','Lignite','Gasoline','Liquified Petroleum Gas (LPG)')]
nm <- nm[level4 %in% c("Boiler, Atmospheric Fluidized Bed Combustion: Bubbling Bed","Boiler > 100 Million BTU/hr","Industrial Boiler","Turbine: Cogeneration","Butane: Reciprocating","Reciprocating: Exhaust","Turbine: Exhaust","Turbine")]
nm <- nm[,.(scc,sector,level1,level2,level3,level4,orisID,facility_site_name,latitude_msr,longitude_msr,pollutant_cd,uom,total_emissions,factor,unit,measure)]
setkey(nm,scc)
nm <- nm[!is.na(factor)]

cs <- colnames(nm)
match <- match[,(cs),with=F]
nei_cln <- rbind(match,nm)
setorder(nei_cln,scc)

nei_cln[,fuelConsumed:=total_emissions*2000/factor][,fuelUnit:=measure]

# Run Nonpoint NEI
npall <- fread('2014v1_nonpoint.csv')
np <- npall[pollutant_cd=='CO']
np <- np[,c(1,3,6,11,13,14)]
setnames(np,c('fips','state','scc','pollutant_cd','total_emissions','uom'))


fips <- fread('state_fips_codes.csv')
fips[,st:=sprintf("%02.0f",STATEFP)]
setkey(fips,state)
setkey(np,state)
np <- fips[np]
np <- np[!state %in% c('','AK','HI','PR','US','VI','DM')]
np[,st_name:=NULL][,STATEFP:=NULL][,st:=as.integer(st)]
np <- np[total_emissions>0]
np[,scc:=as.character(scc)]
setkey(np,scc)
np <- scodes[np]


# Pre-emptive Scc changes
np[scc %in% c("2103004001","2103004002","2104004001","2104004002","2102004001","2102004002"),scc:="20300101"]
#EIS Short name = Int Comb /Comm-Inst /Distillate Oil (Diesel) /Reciprocating
np[scc %in% c("2102008000","2103008000"),scc:="10300910"]
#EIS Short name = Ext Comb /Comm-Inst /Wood/Bark Waste /Fuel cell/Dutch oven boilers **
np[scc %in% c("2104008210","2104008310","2104008510","2104008610","2104008700","2104009000"),scc:="2104008051"]
#Stationary Fuel Comb /Residential /Wood /Non-catalytic Woodstoves: Non-EPA certified
np[scc %in% c("2104008220","2104008320"),scc:="2104008050"]
#Stationary Fuel Comb /Residential /Wood /Non-catalytic Woodstoves: EPA certified
np[scc %in% c("2102008000","2103008000","2104008100","2104008320","2104008330","2104008400","2104008210","2104008310","2104008510","2104008610","2104008700","2104009000"),scc:="2104008001"]
#Stationary Fuel Comb /Residential /Wood /Fireplaces: General
np[scc=="2102012000",scc:="10201302"] # Waste Oil in Industrial Boilers
np[scc=="2102007000",scc:="10201002"]

# Export other sector emissions

rail<- np[sector %in% c('Mobile - Locomotives')]
fwrite(rail,'../../Rail/nei2014_locomotives_nonpoint_CO.csv')
marine <- np[sector %in% c("Mobile - Commercial Marine Vessels")]
fwrite(marine,'../../Marine/nei2014_marine_nonpoint_CO.csv')


np <- np[level1=="Stationary Source Fuel Combustion" & level2=='Industrial']

# Merge with emission factors for CO to fuel consumption conversion

ef3 <- fread('nei_EF.csv')
ef3 <- ef3[pollutant_cd=='CO']
ef <- rbind(ef,ef3,fill=T)
ef <- unique(ef,by='scc')
setkey(ef,scc)
np <- ef[np]

# Split matches and no matches
match <- np[!is.na(factor)]
nm <- np[is.na(factor)]
setkey(nm,scc)

# Merge nonmatches with alternative EFs
nm <- af[nm]
nm[is.na(unit),unit:=u2][is.na(factor),factor:=f2][is.na(measure),measure:=m2]
nm[,u2:=NULL][,f2:=NULL][,m2:=NULL]

# Gather matched data
match <- rbind(match,nm[!is.na(factor)])

np_cln <- match[,.(state,st,fips,scc,sector,level1,level2,level3,level4,uom,total_emissions,factor,unit,measure)]
setkey(np_cln,scc)
setorder(np_cln,scc)

np_cln[,fuelConsumed:=total_emissions*2000/factor][,fuelUnit:=measure]
setkey(np_cln,scc)


# Match CO2 emissions factors for points
vc1 <- fread('vulcan_CO2_EF.csv')
vc2 <- fread('vulcan_co2_measure.csv')
setnames(vc2,c('matID','u2','m2'))
setnames(vc1,c('matID','tonnesCO2_per_mmbtu','note1','material_name','note2','heat_content','note3','units_heat_content'))
setkey(vc1,matID)
setkey(vc2,matID)
vc1 <- vc2[vc1]
vc1 <- vc1[!is.na(tonnesCO2_per_mmbtu)]
vc1[matID %in% c(162,44),m2:='1000 Gallons']
vc1[matID %in% c(162,44),u2:='Lb']
fwrite(vc1,'all_vulcan_co2_emissions_factors.csv')

sc2 <- scodes[scc%in% unique(nei_cln$scc)]
sc2b <- scodes[scc%in% unique(np_cln$scc)]
sc2 <- rbind(sc2,sc2b)
sc2 <- unique(sc2)
setkey(sc2,scc)

vmat[,scc:=as.character(scc)]
setkey(vmat,scc)
sc3 <- vmat[sc2]
sc3[,material_name:=tolower(level3)]
sc3[material_name %in% c("commercial/institutional","industrial"),material_name:=tolower(level4) ]
sc3[material_name %in% c('landfill gas','digester gas'),material_name:='natural gas']
sc3[grep('anthracite',material_name),material_name:='anthracite']
sc3[grepl('^bituminous',material_name),material_name:='bituminous coal']
sc3[grepl('^subbituminous',material_name),material_name:='subbituminous coal']
sc3[grep('lignite',material_name),material_name:='lignite']
sc3[grep('waste oil',material_name),material_name:='waste oil']
sc3[grep('liquified petroleum gas',material_name),material_name:='lpg']
sc3[grep('wood',material_name),material_name:='wood and bark']
sc3[,m1:=matID][,matID:=NULL]
vc1[,material_name:=tolower(material_name)]
vca <- vc1[,.(matID,material_name)]
setkey(sc3,material_name)
setkey(vca,material_name)

sc3 <- vca[sc3]
sc3[is.na(matID),matID:=m1][,m1:=NULL]

setkey(sc3,matID)
setkey(vc1,matID)
sc3 <- vc1[sc3]
sc3 <- sc3[!is.na(matID)]
sc3 <- sc3[,.(scc,matID,u2,m2,tonnesCO2_per_mmbtu,material_name,heat_content,units_heat_content)]
setkey(sc3,scc)
sco <- scodes[sc3]
fwrite(sco,'scc_codes_and_emissions_factors.csv')
nco2 <- sc3[nei_cln]
nco2 <- nco2[!is.na(matID)]
nco2[units_heat_content=="106BTU/103GAL",measure_co2:="1000 Gallons"]
nco2[units_heat_content=="106BTU/106FT3",measure_co2:="Million Cubic Feet"]
nco2[units_heat_content=="106BTU/TON",measure_co2:="Tons"]
nco2[m2=='Short Ton',m2:='Tons']
nco2[is.na(measure_co2),unit_co2:=m2]
nco2[measure=='E3GAL', measure:='1000 Gallons']
nco2[measure=='10^6Ft^3',measure:="Million Cubic Feet"]
nco2[,unit_co2:=u2]
nco2[,fact_co2:=tonnesCO2_per_mmbtu*heat_content*2204.62]
nco2[,fuelUnit:=measure]
nco2 <- nco2[!is.na(fact_co2)]  # Drop the un-matchable Solid Waste and Dual Fuel facilities

# (tonnes CO2 / MMBTU) * (MMBTU / Fuel Measure) * 2204.62 lbs / metric tonne) --> matches WEBFire emission factor units

# Must match the units of fuel consumed
a<-nco2[measure==measure_co2]
b<-nco2[measure!=measure_co2]
b[,fact_co2:=tonnesCO2_per_mmbtu*2204.62]
b[,measure_co2:='Million Btus']
a <- rbind(a,b)
a[,kgco2:=fuelConsumed*fact_co2*0.453592] # fuel consumed > lbs CO2 > kg CO2
a[,kgCO:=total_emissions * 907.185] # short ton to kg conversion for CO

# Tidy up some variables
setnames(a,'latitude_msr','lat')
setnames(a,'longitude_msr','lon')
sts <- nei[,.(orisID,st_usps_cd,state_and_county_fips_code)]
setnames(sts,c('orisID','st','fips'))
sts <- unique(sts)
setkey(sts,orisID)
setkey(a,orisID)
a <- sts[a]

# Is facility an electric power generating facility?
a[,elec:=0]
a[grep('Electric Generation',a$sector),elec:=1]
a[elec==0 & level2=='Electric generation',elec:=1]
a[material_name %in% c('natural gas','process gas','gas','landfill gas','digester gas','refinery gas') & elec==1,`:=`(fuel='gas',code='NGEIP')]
a[material_name %in% c('natural gas','process gas','gas','landfill gas','digester gas','refinery gas') & elec==0,`:=`(fuel='gas',code='NGICP')]
a[material_name %in% c('liquified petroleum gas (lpg)','lpg','propane') & elec==1,`:=`(fuel='lpg',code='NGEIP')]
a[material_name %in% c('liquified petroleum gas (lpg)','lpg','propane') & elec==0,`:=`(fuel='lpg',code='HLICB',fuelConsumed=fuelConsumed*heat_content/1000,fuelUnit='Billion BTU')]
a[material_name %in% c('distillate oil (diesel)','distillate oil (no 1&2)','diesel','jet fuel','jet kerosene','distillate oil (no 4)','distillate oil','diesel kerosene') & elec==1,`:=`(fuel='distillate oil / jet kerosene',code='DFEIP')]
a[material_name %in% c('distillate oil (diesel)','distillate oil (no 1&2)','diesel','distillate oil (no 4)','distillate oil') & elec==0,`:=`(fuel='distillate oil',code='DFICP')]
a[material_name %in% c('jet fuel','jet kerosene','diesel kerosene') & elec==0,`:=`(fuel='jet fuel / kerosene',code='KSICP')]
a[material_name %in% c('residual oil (no 5)','residual oil (no 6)','residual oil') & elec==1,`:=`(fuel='residual oil',code='RFEIP')]
a[material_name %in% c('residual oil (no 5)','residual oil (no 6)','residual oil') & elec==0,`:=`(fuel='residual oil',code='RFICP')]
a[material_name=='crude oil' & elec==1,`:=`(fuel='crude oil',code='RFEIP')]
a[material_name=='crude oil' & elec==0,`:=`(fuel='crude oil',code='COICP')]
a[material_name %in% c('waste oil: vaporizing burner','waste oil: air atomized burner','waste oil','dual fuel (oil/gas)','oil') & elec==1,`:=`(fuel='other oil',code='RFEIP')]
a[material_name %in% c('waste oil: vaporizing burner','waste oil: air atomized burner','waste oil','dual fuel (oil/gas)','oil') & elec==0,`:=`(fuel='other oil',code='OPICB')]
a[material_name=='gasoline' & elec==0,`:=`(fuel='gasoline',code='MGICP')]
a[material_name %in% c('lignite','coke','subbituminous coal','coke oven gas or blast furnace gas','coke oven gas','anthracite','anthracite coal','bituminous coal') & elec==1, `:=`(fuel='coal',code='CLEIP')]
a[material_name %in% c('lignite','subbituminous coal','anthracite','anthracite coal','bituminous coal') & elec==0, `:=`(fuel='coal',code='CLICP')]
a[material_name %in% c('coke','coke oven gas or blast furnace gas','coke oven gas') & elec==0,`:=`(fuel='coke',code='PCICB',fuelConsumed=fuelConsumed*heat_content/1000,fuelUnit='Billion BTU')]
a[material_name %in% c('wood and bark','wood/bark waste','solid waste','liquid waste') & elec==1,`:=`(fuel='wood/waste',code='WWEIB')]
a[material_name %in% c('wood and bark','wood/bark waste','solid waste','liquid waste') & elec==0,`:=`(fuel='wood/waste',code='WWICB')]

# Export final "raw" version
# Add FRS code if available
foris <- fread('foris.csv')
setkey(foris,orisID)
setkey(a,orisID)
a <- foris[a]
a <- a[,.(orisID,frs,lat,lon,st,fips,facility_site_name,
          scc,matID,level1,level2,level3,level4,sector,
          pollutant_cd,uom,total_emissions,factor,unit,measure,
          fact_co2,unit_co2,measure_co2,
          tonnesCO2_per_mmbtu,u2,m2,material_name,heat_content,
          units_heat_content,fuelConsumed,fuelUnit,kgCO,kgco2,elec,fuel,code)]

fwrite(a,'nei_all_facilities_kgco2.csv')
a <- fread('nei_all_facilities_kgco2.csv')

# Adjust fuel consumption numbers to match units in SEDS data
a[fuelUnit=='Million Btus',`:=`(fuelUnit='Billion BTU',fuelConsumed=fuelConsumed/1000)]

# Parse fossil vs bio emissions
a[,kgBIOco2:=0]
a[fuel=='wood/waste',kgBIOco2:=kgco2]
a[level3 %in% c('Digester Gas','Landfill Gas','Liquid Waste','Solid Waste','Wood/Bark Waste'),kgBIOco2:=kgco2]
a[(fuel=='wood/waste' | level3 %in% c('Digester Gas','Landfill Gas','Liquid Waste','Solid Waste','Wood/Bark Waste')),kgco2:=0]
b <- a[,lapply(.SD,sum,na.rm=T),by=list(orisID,frs,lat,lon,st,fips,fuel,fuelUnit,code,elec),.SDcols=c('kgco2','kgBIOco2','fuelConsumed')]
setnames(b,'st','state')
fwrite(b,'nei_point_complete_short.csv')

#### NEI Points complete ####
####################################################################################################

## Match CO2 factors for nonpoint

np_cln <- sc3[np_cln]
np_cln <- np_cln[!is.na(matID)]

np_cln[units_heat_content=="106BTU/103GAL",measure_co2:="1000 Gallons"]
np_cln[units_heat_content=="106BTU/106FT3",measure_co2:="Million Cubic Feet"]
np_cln[units_heat_content=="106BTU/TON",measure_co2:="Tons"]
np_cln[m2=='Short Ton',m2:='Tons']
np_cln[is.na(measure_co2),unit_co2:=m2]

np_cln[measure=='E3GAL', measure:='1000 Gallons']
np_cln[measure=='10^6Ft^3',measure:="Million Cubic Feet"]
np_cln[measure=='E6FT3',measure:="Million Cubic Feet"]
np_cln[measure=='TON',measure:="Tons"]

np_cln[,unit_co2:=u2]
np_cln[,fact_co2:=tonnesCO2_per_mmbtu*heat_content*2204.62]
np_cln[,fuelUnit:=measure]

# (tonnes CO2 / MMBTU) * (MMBTU / Fuel Measure) * 2204.62 lbs / metric tonne) --> matches WEBFire emission factor units
#
np_cln[,kgco2:=fuelConsumed*fact_co2*0.453592] # fuel consumed > lbs CO2 > kg CO2
np_cln[,kgCO:=total_emissions * 907.185] # short ton to kg conversion for CO

# Is facility an electric power generating facility?

np_cln[,elec:=0]

# Clean fuel type columns for grouping later

np_cln[material_name %in% c('natural gas','process gas','gas','landfill gas','digester gas','refinery gas') & elec==0,`:=`(fuel='gas',code='NGICP')]
np_cln[material_name %in% c('liquified petroleum gas (lpg)','lpg','propane') & elec==0,`:=`(fuel='lpg',code='HLICB',fuelConsumed=fuelConsumed*heat_content/1000,fuelUnit='Billion BTU')]
np_cln[material_name %in% c('distillate oil (diesel)','distillate oil (no 1&2)','diesel','distillate oil (no 4)','distillate oil') & elec==0,`:=`(fuel='distillate oil',code='DFICP')]
np_cln[material_name %in% c('jet fuel','jet kerosene','kerosene') & elec==0,`:=`(fuel='jet fuel / kerosene',code='KSICP')]
np_cln[material_name %in% c('residual oil (no 5)','residual oil (no 6)','residual oil') & elec==0,`:=`(fuel='residual oil',code='RFICP')]
np_cln[material_name=='crude oil' & elec==0,`:=`(fuel='crude oil',code='COICP')]
np_cln[material_name %in% c('waste oil: vaporizing burner','waste oil: air atomized burner','waste oil','dual fuel (oil/gas)','oil') & elec==0,`:=`(fuel='other oil',code='OPICB')]
np_cln[material_name=='gasoline' & elec==0,`:=`(fuel='gasoline',code='MGICP')]
np_cln[material_name %in% c('lignite','bituminous/subbituminous','anthracite','anthracite coal','bituminous coal') & elec==0, `:=`(fuel='coal',code='CLICP')]
np_cln[material_name %in% c('wood','wood and bark','wood/bark waste','solid waste','liquid waste') & elec==0,`:=`(fuel='wood/waste',code='WWICB')]

np_cln <- np_cln[,.(state,st,fips,
          scc,matID,level1,level2,level3,level4,sector,
          uom,total_emissions,factor,unit,measure,
          fact_co2,unit_co2,measure_co2,
          tonnesCO2_per_mmbtu,u2,m2,material_name,heat_content,
          units_heat_content,fuelConsumed,fuelUnit,kgCO,kgco2,elec,fuel,code)]

fwrite(np_cln,'../../Nonpoint/Data/nei_all_nonpoint_kgco2.csv')

np_cln <- fread('../../Nonpoint/Data/nei_all_nonpoint_kgco2.csv')

# Parse fossil vs bio emissions
np_cln[,kgBIOco2:=0]
np_cln[fuel=='wood/waste',kgBIOco2:=kgco2]
np_cln[fuel=='wood/waste',kgco2:=0]
setnames(np_cln,'kgco2','kgFFco2')
npb <- np_cln[,lapply(.SD,sum,na.rm=T),by=list(state,st,fips,fuel,fuelUnit,code,elec),.SDcols=c('kgBIOco2','kgFFco2','fuelConsumed')]
fwrite(npb,'nei_nonpoint_complete_short.csv')

npb <- fread('nei_nonpoint_complete_short.csv')
b <- fread('nei_point_complete_short.csv')
setnames(b,'kgco2','kgFFco2')

all <- rbind(b,npb,fill=T)
all[,st:=NULL]
all <- all[state!='PR']
fips <- fread('state_fips_codes.csv')
setnames(fips,'STATEFP','st')
fips[,st_name:=NULL]
setkey(fips,state)
setkey(all,state)
all <- fips[all]
all[,st_fuel:=sum(fuelConsumed),by=list(st,code)][,st_ff:=sum(kgFFco2),by=list(st,code)][,st_bio:=sum(kgBIOco2),by=list(st,code)]


# Need to merge in SEDS data to make state-level adjustments
seds <- fread('Complete_SEDS.csv')
seds <- seds[,2:5,with=F]
setnames(seds,c('code','state','year','value'))
setkey(seds,code)
codes <- fread('SEDS_elec_industrial.csv')
setkey(codes,code)
seds <- codes[seds]
fips <- fread('state_fips_codes.csv')
fips[,st:=sprintf("%02.0f",STATEFP)]
setkey(fips,state)
setkey(seds,state)
seds <- fips[seds]
seds <- seds[!state %in% c('AK','HI','PR','US')]
seds <- seds[!is.na(desc)]
seds <- seds[year %in% 2012:2017]
sw <- dcast.data.table(seds,st + state + code + sedsUnit ~ year,value.var='value',fun.aggregate = sum)
setnames(sw,c('st','state','code','sedsUnit',paste0('s',2012:2017)))
sw[sedsUnit=='Thousand short tons',`:=`(sedsUnit='Tons',s2012=s2012*1000,s2013=s2013*1000,s2014=s2014*1000,s2015=s2015*1000,s2016=s2016*1000,s2017=s2017*1000)]
sw[sedsUnit=='Thousand barrels',`:=`(sedsUnit='1000 Gallons',s2012=s2012*42,s2013=s2013*42,s2014=s2014*42,s2015=s2015*42,s2016=s2016*42,s2017=s2017*42)]
sw[sedsUnit=='Million cubic feet',sedsUnit:='Million Cubic Feet']
setkey(sw,state,code)
setkey(all,state,code)

all <- sw[all]
all <- all[!state %in% c('AK','HI','PR')]
all <- all[fuelConsumed>0]

# Adjust all fuel consumption and emissions to match 2014 SEDS
keep <- all[ all[, do.call(pmin, .SD) == 0, .SDcols=c('s2012','s2013','s2014','s2015','s2016','s2017')] ]
keep[,kgFFco2_2012:=as.numeric(kgFFco2)][,kgFFco2_2013:=as.numeric(kgFFco2)][,kgFFco2_2015:=as.numeric(kgFFco2)][,kgFFco2_2016:=as.numeric(kgFFco2)][,kgFFco2_2017:=as.numeric(kgFFco2)]
keep[,kgBIOco2_2012:=as.numeric(kgBIOco2)][,kgBIOco2_2013:=as.numeric(kgBIOco2)][,kgBIOco2_2015:=as.numeric(kgBIOco2)][,kgBIOco2_2016:=as.numeric(kgBIOco2)][,kgBIOco2_2017:=as.numeric(kgBIOco2)]
setnames(keep,'kgFFco2','kgFFco2_2014')
setnames(keep,'kgBIOco2','kgBIOco2_2014')

all <- all[ all[, do.call(pmin, .SD) > 0, .SDcols=c('s2012','s2013','s2014','s2015','s2016','s2017')] ]
all[,diff:=s2014-st_fuel][,frac:=fuelConsumed/st_fuel][,adj:=diff*frac][,fuel2:=fuelConsumed+adj][,s2:=sum(fuel2),by=list(state,code)][,chk:=s2014-s2]

# Adjust CO2 downwards only if necessary, don't adjust upwards
all[adj<0,kgFFco2:=kgFFco2/fuelConsumed*fuel2][adj<0,kgBIOco2:=kgBIOco2/fuelConsumed*fuel2]

# Generate time series using SEDS year-to-year differences to scale NEI emissions
all[,`:=`(s2012=s2012/s2014,s2013=s2013/s2014,s2015=s2015/s2014,s2016=s2016/s2014,s2017=s2017/s2014)][,s2014:=1]
all[,kgFFco2_2012:=as.numeric(kgFFco2*s2012)][,kgFFco2_2013:=as.numeric(kgFFco2*s2013)][,kgFFco2_2014:=as.numeric(kgFFco2*s2014)][,kgFFco2_2015:=as.numeric(kgFFco2*s2015)][,kgFFco2_2016:=as.numeric(kgFFco2*s2016)][,kgFFco2_2017:=as.numeric(kgFFco2*s2017)]
all[,kgBIOco2_2012:=as.numeric(kgBIOco2*s2012)][,kgBIOco2_2013:=as.numeric(kgBIOco2*s2013)][,kgBIOco2_2014:=as.numeric(kgBIOco2*s2014)][,kgBIOco2_2015:=as.numeric(kgBIOco2*s2015)][,kgBIOco2_2016:=as.numeric(kgBIOco2*s2016)][,kgBIOco2_2017:=as.numeric(kgBIOco2*s2017)]
all[,kgFFco2:=NULL][,kgBIOco2:=NULL]

all <- rbind(all,keep,fill=T)
all <- all[fuelConsumed>0]
fwrite(all,'temp_all_complete_point_nonpoint.csv')
all <- fread('temp_all_complete_point_nonpoint.csv')
nonpt <- all[is.na(orisID),.(state,st,fips,fuel,fuel2,fuelUnit,kgFFco2_2012,kgFFco2_2013,kgFFco2_2014,kgFFco2_2015,kgFFco2_2016,kgFFco2_2017,kgBIOco2_2012,kgBIOco2_2013,kgBIOco2_2014,kgBIOco2_2015,kgBIOco2_2016,kgBIOco2_2017)]
setnames(nonpt,'fuel2','fuelConsumed')

ids <- colnames(nonpt)[1:3]
vars <- colnames(nonpt)[7:18]
npm <- melt.data.table(nonpt,id.vars=(ids),measure=patterns('kgFFco2_','kgBIOco2_'),variable.name='year',value.name=c('kgFFco2','kgBIOco2'))
npm[,year:=2011+(as.integer(year))]
npm[,sect:='np_ind']
npm <- npm[,lapply(.SD,sum),by=list(state,st,fips,year,sect),.SDcols=c('kgFFco2','kgBIOco2')]

setorder(npm,fips,year)
fwrite(npm,'nonpoint_industrial_kgco2_2012_2017.csv')

# Split out point sources

pt <- all[!is.na(orisID),.(orisID,frs,lat,lon,state,st,elec,fuel,fuel2,fuelUnit,kgFFco2_2012,kgFFco2_2013,kgFFco2_2014,kgFFco2_2015,kgFFco2_2016,kgFFco2_2017,kgBIOco2_2012,kgBIOco2_2013,kgBIOco2_2014,kgBIOco2_2015,kgBIOco2_2016,kgBIOco2_2017)]
setnames(pt,'fuel2','fuelConsumed')
pt[,frs:=as.character(frs)]
ids <- colnames(pt)[1:7]
vars <- colnames(pt)[11:22]
pm <- melt.data.table(pt,id.vars=(ids),measure=patterns('kgFFco2_','kgBIOco2_'),variable.name='year',value.name=c('kgFFco2','kgBIOco2'))
pm[,year:=2011+(as.integer(year))]
fwrite(pm,'point_kgco2_2012_2017.csv')


## Restart Point
gp <- fread('ghgrp_final_kgco2.csv')
gp <- gp[!is.na(lat) & !is.na(lon)]
gp[,kgCEMco2:=as.numeric(kgCEMco2)]
setnames(gp,'kgco2','kgFFco2')
gp <- gp[,.(frs,ghgpID,eia860,orisID,state,year,lat,lon,kgFFco2,kgBIOco2,kgCEMco2,elec)]
gp[is.na(kgBIOco2),kgBIOco2:=0][is.na(kgFFco2),kgFFco2:=0][is.na(kgCEMco2),kgCEMco2:=0][,source:='ghgp']
gp[,totCO2:=as.numeric(kgFFco2+kgBIOco2+kgCEMco2)]
gp <- gp[totCO2>0]
gp[,frs:=as.character(frs)]
gxy <- copy(gp[year==2014])
coordinates(gxy) <- ~lon+lat
projection(gxy) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
writeOGR(gxy,dsn='.',layer='GHGP_pts',driver='ESRI Shapefile',overwrite=T)
pm <- fread('point_kgco2_2012_2017.csv')
ff <- fread('frs_oris_ghgp_egrid_xwalk.csv')
glist <- fread('glist.csv')

# Drop facilities covered by GHGP
f2 <- ff[!is.na(oris)&!is.na(ghgpID),oris]
ghgp_oris <- unique(c(unique(gp$orisID),unique(glist$orisID),unique(f2)))
nei_ghgp <- pm[orisID %in% ghgp_oris]
fwrite(nei_ghgp,'nei_facilities_in_ghgp.csv')
nei_ghgp[,neiCO2:=as.numeric(kgFFco2+kgBIOco2)]
nei_ghgp <- nei_ghgp[,lapply(.SD,sum),by=list(orisID,year),.SDcols=c('neiCO2')]
setkey(nei_ghgp,orisID,year)
pf <- pm[!orisID %in% ghgp_oris]
pf <- pf[,lapply(.SD,sum),by=list(orisID,lat,lon,state,st,elec,year),.SDcols=c('kgFFco2','kgBIOco2')]
pf[,source:='nei'][,totCO2:=as.numeric(kgFFco2+kgBIOco2)][,tco2:=totCO2/1000]

# Drop all facilities with emissions >25000 tons
pf <- pf[tco2<25000]
pfu <- pf[,.(orisID,lat,lon)]
setkey(pfu,orisID)
pxy <- unique(pfu,by='orisID')
coordinates(pxy) <- ~lon+lat
projection(pxy) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
writeOGR(pxy,dsn='.',layer='nonGHGP_NEI_pts',driver='ESRI Shapefile',overwrite=T)

# ran the near tool in ArcGIS to identify unmatched nei facilities > 500m of GHGP facilities
# Anything closer gets dropped

h <- fread('nei_above500m_dist_ghgp.csv')
setnames(h,c('OBJECTID','orisID','ghgpID'))
h <- h[,.(orisID,ghgpID)]
h <- unique(h,by='orisID')
a <- fread('nei_all_facilities_kgco2.csv')
setkey(h,orisID)
hn <- a[,.(orisID,facility_site_name)]
setkey(hn,orisID)
h <- hn[h]
h <- unique(h)
h[,nei_name:=tolower(facility_site_name)]
setkey(h,ghgpID)
gh <- fread('ghgp_allyears_raw.csv')
gh <- gh[,.(ghgpID,name)]
setnames(gh,'name','ghgp_name')
gh <- unique(gh)
setkey(gh,ghgpID)
h <- gh[h]
h <- unique(h)

h[,ghgp_name:=tolower(ghgp_name)]
h <- h[,.(ghgpID,orisID,nei_name,ghgp_name)]

h[,ghgp_name:=gsub('&','and',ghgp_name)]
h[,nei_name:=gsub('&','and',nei_name)]
h[,ghgp_name:=gsub('^ | $','',ghgp_name)]
h[,nei_name:=gsub('^ | $','',nei_name)]
h[,ghgp_name:=gsub('^co | co$','',ghgp_name)]
h[,nei_name:=gsub('^co | co$','',nei_name)]
h[,ghgp_name:=gsub(' co ',' ',ghgp_name)]
h[,nei_name:=gsub(' co ',' ',nei_name)]
h[,ghgp_name:=gsub(' co ',' ',ghgp_name)]
h[,nei_name:=gsub(' ',' ',nei_name)]
h[,nei_name:=gsub("'|,|\\(|\\)|-|/|\\.|\\\\|inc|the|llc|of","",nei_name)]
h[,ghgp_name:=gsub("'|,|\\(|\\)|-|/|\\.|\\\\|inc|the|llc|of","",ghgp_name)]
h[,ghgp_name:=gsub('\\s+',' ',ghgp_name)]
h[,nei_name:=gsub('\\s+',' ',nei_name)]

nl <- str_split(h$nei_name,' ')
gl <- str_split(h$ghgp_name,' ')
h[,cnt:=0]
zz<-list()
for(i in 1:length(nl)){
  zz[[i]] <- intersect(nl[[i]],gl[[i]])
  h$cnt[i] <- length(zz[[i]])
}

h3<-h[cnt>2]
h1 <- h[cnt<=2]

pf <- pf[orisID %in% h1$orisID]
fwrite(pf,'nonGHGP_point_kgco2_2012_2017.csv')

# Recombine with GHGP points

fullpts <- rbind(gp,pf,fill=T)
fullpts[is.na(kgCEMco2),kgCEMco2:=0]
fullpts[kgFFco2<1,kgFFco2:=0][kgBIOco2<1,kgBIOco2:=0]
fullpts[,sumCO2:=kgFFco2+kgBIOco2+kgCEMco2]
fullpts <- fullpts[sumCO2>0]
fullpts <- fullpts[,.(frs,ghgpID,orisID,eia860,st,state,year,lat,lon,kgFFco2,kgBIOco2,kgCEMco2,elec,source,totCO2)]
setnames(fullpts,'totCO2','all_kgco2')
fwrite(fullpts,'all_points_kgco2_2012_2017.csv')
#

fullpts <- fread('all_points_kgco2_2012_2017.csv')
fullpts[,frs:=as.character(frs)][,kgCEMco2:=as.character(kgCEMco2)]
f12 <- fullpts[year==2012]
f13 <- fullpts[year==2013]
f14 <- fullpts[year==2014]
f15 <- fullpts[year==2015]
f16 <- fullpts[year==2016]
f17 <- fullpts[year==2017]
vlcc <- "+proj=lcc +lat_1=33 +lat_2=45 +lat_0=40 +lon_0=-97 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"

make_shape <- function(x,proj,outname){
  coordinates(x) <- ~lon+lat
  projection(x) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
  crs <- CRS(proj)
  fp <- spTransform(x,crs)
  writeOGR(fp,dsn='.',layer=outname,driver='ESRI Shapefile',overwrite=T)
}
make_shape(f12,vlcc,'point_kgco2_2012')
make_shape(f13,vlcc,'point_kgco2_2013')
make_shape(f14,vlcc,'point_kgco2_2014')
make_shape(f15,vlcc,'point_kgco2_2015')
make_shape(f16,vlcc,'point_kgco2_2016')
make_shape(f17,vlcc,'point_kgco2_2017')


###-------------------------
# make emissions factor tables for manuscript
# - - - - - #
a <- fread('nei_all_facilities_kgco2.csv')
a[unit=='lbsCO',unit:='Lb']
a <- a[,.(scc,level1,level2,level3,level4,uom,factor,unit,measure,fact_co2,unit_co2,measure_co2,u2,m2,material_name,fuelUnit,fuel,code)]
a[,CO_ef_units:=paste0(unit,' / ',measure)]
a[,CO2_ef_units:=paste0(unit_co2,' / ',measure_co2)]
setnames(a,'factor','CO_emission_factor')
setnames(a,'fact_co2','CO2_emission_factor')
setnames(a,'fuel','ACES_fuel_category')
setnames(a,'code','SEDS_code')
a <- a[,.(scc,level1,level2,level3,level4,ACES_fuel_category,SEDS_code,CO_emission_factor,CO_ef_units,CO2_emission_factor,CO2_ef_units)]
b <- copy(a)

np_cln <- fread('../../Nonpoint/Data/nei_all_nonpoint_kgco2.csv')
a <- np_cln[,.(scc,level1,level2,level3,level4,uom,factor,unit,measure,fact_co2,unit_co2,measure_co2,u2,m2,material_name,fuelUnit,fuel,code)]
a[unit=='lbsCO',unit:='Lb']
a[,CO_ef_units:=paste0(unit,' / ',measure)]
a[,CO2_ef_units:=paste0(unit_co2,' / ',measure_co2)]
setnames(a,'factor','CO_emission_factor')
setnames(a,'fact_co2','CO2_emission_factor')
setnames(a,'fuel','ACES_fuel_category')
setnames(a,'code','SEDS_code')
a <- a[,.(scc,level1,level2,level3,level4,ACES_fuel_category,SEDS_code,CO_emission_factor,CO_ef_units,CO2_emission_factor,CO2_ef_units)]
bb <- rbind(b,a)
bb <- unique(bb)
setorder(bb,scc)
bb[,CO_ef_units:=gsub('Tons','Ton',CO_ef_units)][,CO2_ef_units:=gsub('Tons','Ton',CO2_ef_units)]
bb[,CO_ef_units:=gsub('Gallons','Gallon',CO_ef_units)][,CO2_ef_units:=gsub('Gallons','Gallon',CO2_ef_units)]
bb[,CO_ef_units:=gsub('LB','Lb',CO_ef_units)][,CO2_ef_units:=gsub('LB','Lb',CO2_ef_units)]
fwrite(bb,'../../aces_all_emissions_factors_by_scc.csv')
