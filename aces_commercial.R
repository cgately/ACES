require(data.table)
require(bit64)
require(stringr)

###   SEDS data on annual state-level fuel consumption by fuel type and end-use sector
###   downloaded from https://www.eia.gov/state/seds/seds-data-fuel.php?sid=US#DataFiles
###   on 02/12/2018

args <- commandArgs(TRUE)
yr <- as.numeric(args[1])

DIR=''

times<-fread(paste0(DIR,'times',yr,'.csv'))
td<-times[,.(datetime,chr,hour,day)]
setkey(td,datetime)

seds <- fread(paste0(DIR,'Complete_SEDS.csv'))
seds <- seds[,2:5,with=F]
setnames(seds, c('code','state','year','value'))
setkey(seds,code)

codes <- fread(paste0(DIR,'seds_code_descriptions.csv'))
setnames(codes,c('code','desc','unit'))
setkey(codes,code)
seds <- codes[seds]
seds[code=='HLCCB' & is.na(unit),unit:='Billion Btu']

fips <- fread(paste0(DIR,'state_fips_codes.csv'))
fips[,st:=sprintf("%02.0f",STATEFP)]
setkey(fips,state)

setkey(seds,state)
seds <- fips[seds]
seds <- seds[state!='US']
seds <- seds[year==yr]

com.fuels <- data.table('code'=c('CLCCP','DFCCP','KSCCP','HLCCB','NGCCB','PCCCP','RFCCP','WDCCB','WSCCB'),'fuel'=c('coal','dist_oil','kerosene','lpg','gas','coke','res_oil','wood','waste'))
setkey(com.fuels,code)

## Commercial Fuel Consumption
seds.com <- seds[code %in% com.fuels$code]
setkey(seds.com,code)
seds.com <- com.fuels[seds.com]

n2 <- gsub(' ','_',sort(unique(paste(seds.com$fuel,seds.com$unit))))
com.wide <- dcast.data.table(seds.com,state~fuel,fun.aggregate=mean,value.var='value')
setnames(com.wide,c('state',n2))

# The following Conversion and Emissions factors were obtained from:

# Source 1: https://www.epa.gov/sites/production/files/2018-03/documents/emission-factors_mar_2018_0.pdf
# Source 2: https://www.eia.gov/environment/emissions/co2_vol_mass.php
# Source 3: 'Code of Federal Regulations Title 40 Protection of Environment Parts 96 to 99, Part 98, Subpart C, Table C-1, "Default CO2 Emissions Factors and High Heat Values for Various Types of Fuel", page 730. (40 CFR Ch. I (7-1-18 Edition)). Accessed on 02/22/2019 at https://www.ecfr.gov/cgi-bin/text-idx?SID=54aa6b65d6e9377d08c57013a2b5d928&mc=true&node=ap40.23.98_138.1&rgn=div9

# Unit Conversions (all Source #1 except coke)
# Coke source is https://www.eia.gov/dnav/pet/TblDefs/pet_pnp_refp_tbldef2.asp

com.wide[,coal_MBtu:=coal_Thousand_short_tons*1000*21.39]
com.wide[,coke_MBtu:=coke_Thousand_barrels*1000*6.024]
com.wide[,dist_oil_MBtu:=dist_oil_Thousand_barrels*1000*42*mean(c(0.139,0.138,0.146))]
com.wide[,kerosene_MBtu:=kerosene_Thousand_barrels*1000*42*0.135]
com.wide[,lpg_MBtu:=lpg_Billion_Btu*1000]
com.wide[,natgas_MBtu:=gas_Billion_Btu*1000]
com.wide[,res_oil_MBtu:=res_oil_Thousand_barrels*1000*42*mean(c(0.14,0.15))]
com.wide[,waste_MBtu:=waste_Billion_Btu*1000]
com.wide[,wood_MBtu:=wood_Billion_Btu*1000]

# CO2 estimates

com.wide[,coal_kgco2:=94.27 * coal_MBtu]  # Source 1
com.wide[,coke_kgco2:=102.41 * coke_MBtu]  # Source 1
com.wide[,dist_kgco2:=mean(c(73.25,73.96,75.04)) * dist_oil_MBtu]  # Source 1
com.wide[,kero_kgco2:=75.2 * kerosene_MBtu]  # Source 1
com.wide[,lpg_kgco2:=64.01 * lpg_MBtu]  # Source 1
com.wide[,gas_kgco2:=53.06 * natgas_MBtu]  # Source 1
com.wide[,res_kgco2:=mean(c(72.93,75.1)) * res_oil_MBtu]  # Source 1
com.wide[,wood_kgco2:=93.8 * wood_MBtu]  # Source 1
com.wide[,waste_kgco2:=90.7 * waste_MBtu]  # Source 1
com.wide[,kgco2:=coal_kgco2 + coke_kgco2 + dist_kgco2 + kero_kgco2 + lpg_kgco2 + gas_kgco2 + res_kgco2 + wood_kgco2 + waste_kgco2]
com.wide[,bio_frac:=(wood_kgco2+waste_kgco2)/kgco2]

setkey(com.wide,state)
com.wide <- fips[com.wide]
com.co2 <- com.wide[,.(st,kgco2)]
setkey(com.co2,st)

st.com.bio.frac <- com.wide[,.(st,bio_frac)]
st.com.bio.frac[,st:=as.numeric(st)]
setkey(st.com.bio.frac,st)
saveRDS(st.com.bio.frac,paste0(DIR,'st_com_biofrac.rds'))
        
# Monthly fuel consumption for natural gas from https://www.eia.gov/opendata/bulkfiles.php 
# Extracted and cleaned using E:/ACES/Scripts/com_NG_monthly_proc.sh

rmon <- fread(paste0(DIR,'st_com_gas_monthly.csv'))
setnames(rmon,c('state','YYYYMM','gasMCF'))
rmon[,year:=as.numeric(substr(YYYYMM,1,4))][,mon:=as.numeric(substr(YYYYMM,5,6))][,gasMCF:=as.numeric(gasMCF)]
rmon<-rmon[year %in% seq(2011,2017) & state!='US']
rmon <- rmon[!is.na(mon)]
setkey(rmon,state)
rmon <- fips[rmon]
rmon<-rmon[,.(st,gasMCF,year,mon,YYYYMM)]
rmon[,fips:=paste0('s',st)]
rmon[,st:=substr(fips,2,3)]
setkey(rmon,st)
rmon[,st_sum:=sum(gasMCF,na.rm=T),by=c('st','year')][,fips:=NULL]

rmon <- com.co2[rmon]
rmon[,mon_frac:=gasMCF/st_sum]
rmon[,kgco2:=kgco2*mon_frac]
rmon <- rmon[,.(st,year,mon,kgco2)]
setkey(rmon,st,year,mon)

### Building square footage by building use type by census block from HAZUS_v3 database
## Already cleaned in aces_residential.R script

haz3 <- readRDS(paste0(DIR,'hazus_blk_sqft_clean.rds'))
haz_sqft <- haz3[,.(CensusBlock,GEOID,com_sqft,bg_com_sqft)]
haz_sqft[,st:=substr(GEOID,1,2)]
haz_sqft[,st_sqft:=sum(com_sqft),by=list(st)][,bg_st_frac:=bg_com_sqft/st_sqft][,mean_bg_frac:=mean(bg_st_frac,na.rm=T),by=list(st)][,bg_sqft_adj:=bg_st_frac/mean_bg_frac]
sqft <- haz_sqft[,.(GEOID,bg_st_frac,bg_sqft_adj)]
sqft <- unique(sqft)
setkey(sqft,GEOID)

stc<-readRDS(paste0(DIR,'statefips.rds'))
stc <- stc[!stc %in% c('02','15')]

da <- data.table()
for (s in stc){  
  print(paste0('Starting ',s))
  dt <- readRDS(paste0(yr,'/bg_rap_state',s,'.rds'))
  dt <- dt[,.(st,year,mon,datetime,GEOID,HDH)]
  
# Calculate each blockgroup's monthly HDH fraction of state monthly total Heating Degree Hours
  
  dt[,bg_mon_HDH:=sum(HDH),by=list(GEOID,mon)]
  dt[,st_mon_HDH:=sum(HDH),by=list(st,mon)]
  dt[,bg_mon_frac:=bg_mon_HDH/st_mon_HDH]

# Calculate adjustment factor based on each BG's relationship to statewide mean BG HDH each month
  
  dt[,mean_bgHDH:=mean(bg_mon_HDH,na.rm=T),by=list(st,mon)]
  dt[,bg_adjHDH:=bg_mon_HDH/mean_bgHDH]

  dt <- dt[,.(st,year,mon,datetime,GEOID,HDH,bg_adjHDH)]
  setkey(dt,datetime)  
  dt <- td[dt]

# Adjust monthly emissions to reflect BG share of state commercial square footage
# adjusted up or down according to BG share of state monthly HDH
  dy<-data.table()
  
  for (m in seq(1,12)){
    dm <- dt[mon==m]
    adj <- dt[mon==m & day==1 & hour==0]
    setkey(adj,st,year,mon)
    adj <- rmon[adj]
    setkey(adj,GEOID)
    adj <- sqft[adj]
    adj <- adj[!is.na(bg_sqft_adj)]
    adj[,bg_mon_co2:=kgco2 * bg_st_frac]
    adj[,bg_mon_co2_adj:=bg_mon_co2*bg_adjHDH]
    adj[,s2:=sum(bg_mon_co2_adj)][,adj2:=bg_mon_co2_adj/s2]
    adj[,bg_mon_kgco2:=kgco2*adj2]
    adj <- adj[,.(st,year,mon,GEOID,bg_mon_kgco2)]
    setkey(adj,st,year,mon,GEOID)
    setkey(dm,st,year,mon,GEOID)
  
    # Calculate each blockgroup's hourly HDH fraction of it's own monthly total HDH
    dm <- adj[dm]
    dm <- dm[!is.na(bg_mon_kgco2)]
    dm[,monHDH:=sum(HDH),by=list(GEOID,mon)][,hr_mon_fracHDH:=HDH/monHDH]
    dm[,hr_kgco2:=bg_mon_kgco2 * hr_mon_fracHDH]
    dy <- rbind(dy,dm)
    rm(adj,dm);gc()
    message('Month ',m,' complete')
  }
  dy <- dy[,.(year,chr,datetime,st,GEOID,hr_kgco2)]
  setnames(dy,'hr_kgco2','kgco2_total')
  dy[,st:=as.numeric(st)]
  setkey(dy,st)
  dy <- st.com.bio.frac[dy]
  dy[,kgBIOco2:=kgco2_total*bio_frac]
  dy[,kgFFco2:=kgco2_total-kgBIOco2]
  saveRDS(dy,paste0(DIR,'stCO2/',yr,'/bg_hourly_kgco2_st',s,'_',yr,'.rds'))
  dy <- dy[,lapply(.SD,sum,na.rm=T),by=list(year,GEOID),.SDcols=c('kgBIOco2','kgFFco2')]
  da <- rbind(da,dy)
  print(paste0('Completed ',s))          
  warnings()
}
da[,GEOID:=str_pad(as.character(GEOID),12,'left','0')]
fwrite(da,paste0(DIR,'bg_com_kgco2_',yr,'.csv'))

setkey(da,GEOID)

grid <- fread(paste0(DIR,'block_group_1km_inter.csv'))
cs <- colnames(grid)[c(2:4,7)]
grid <- grid[,(cs),with=F]
grid[,GEOID:=as.character(GEOID)]
grid[nchar(GEOID)<12,GEOID:=paste0('0',GEOID)]
setkey(grid,GEOID)

da <- grid[da]
da[,kgFFco2:=kgFFco2/bg_area_m2*inter_area_m2][,kgBIOco2:=kgBIOco2/bg_area_m2*inter_area_m2]
da <- da[,lapply(.SD,sum),by=c('cellid'),.SDcols=c('kgBIOco2','kgFFco2')]
fwrite(da,paste0(DIR,'grid1km_com_kgco2_',yr,'.csv'))
print('gridded com kgco2 complete')
