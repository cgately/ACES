library(data.table)
library(bit64)

VLCC_CRS = "+proj=lcc +lat_1=33 +lat_2=45 +lat_0=40 +lon_0=-97 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"
RAP_CRS = "+proj=lcc +lat_1=25 +lat_2=25 +lat_0=25 +lon_0=265 +x_0=0 +y_0=0 +a=6371229 +b=6371229 +units=m +no_defs"

###   SEDS data on annual state-level fuel consumption by fuel type and end-use sector
###   downloaded from https://www.eia.gov/state/seds/seds-data-fuel.php?sid=US#DataFiles
###   on 02/12/2018

args <- commandArgs(TRUE)
yr <- as.numeric(args[1])

DIR=''

# Load datetime data
times<-fread(paste0(DIR,'times',yr,'.csv'))
td<-times[,.(datetime,chr)]
setkey(td,datetime)

# Load SEDS data
seds <- fread(paste0(DIR,'Complete_SEDS.csv'))
seds <- seds[,2:5,with=F]
setnames(seds, c('code','state','year','value'))
setkey(seds,code)
codes <- fread(paste0(DIR,'seds_code_descriptions.csv'))
setnames(codes,c('code','desc','unit'))
setkey(codes,code)
seds <- codes[seds]
fips <- fread(paste0(DIR,'state_fips_codes.csv'))
fips[,st:=sprintf("%02.0f",STATEFP)]
setkey(fips,state)
setkey(seds,state)
seds <- fips[seds]
seds <- seds[state!='US']
seds <- seds[year==yr]

res.fuels <- data.table('code'=c('CLRCP','DFRCP','KSRCP','HLRCB','NGRCB','WDRCB'),'fuel'=c('coal_kshortton','oil_kbarrel','kerosene_kbarrel','lpg_BBtu','gas_BBtu','wood_BBtu'))
setkey(res.fuels,code)

## Residential Fuel Consumption
seds.res <- seds[code %in% res.fuels$code]
setkey(seds.res,code)
seds.res <- res.fuels[seds.res]
res.wide <- dcast.data.table(seds.res,state~fuel,fun.aggregate=mean,value.var='value')

# Unit Conversions
res.wide[,kerosene_gal:=kerosene_kbarrel*42000]
res.wide[,oil_gal:=oil_kbarrel*42000]
res.wide[,lpg_MBtu:=lpg_BBtu*1000]
res.wide[,gas_MBtu:=gas_BBtu*1000]
res.wide[,wood_MBtu:=wood_BBtu*1000]

# CO2 estimates
# Emissions factors obtained from https://www.eia.gov/environment/emissions/co2_vol_mass.php
# except for wood combustion emissions obtained from: https://www.epa.gov/sites/production/files/2018-03/documents/emission-factors_mar_2018_0.pdf

res.wide[,coal_kgco2:=2100.82*coal_kshortton*1000]  # 2100.82 kgCO2 / short ton coal
res.wide[,oil_kgco2:=(10.16*oil_gal) + (9.75*kerosene_gal)]                 # 10.16 kgCO2 / gallon heating oil and 9.75 kgCO2 / gallon kerosene
res.wide[,lpg_kgco2:=64.01*lpg_MBtu]                # 64.01 kgCO2 / Million BTU LPG
res.wide[,gas_kgco2:=53.07*gas_MBtu]                # 10.16 kgCO2 / Million BTU natural gas
res.wide[,wood_kgco2:=93.8*wood_MBtu]  # 93.8 kgCO2 / Million BTU wood 

setkey(res.wide,state)
res.wide <- fips[res.wide]
res.co2 <- res.wide[,.(st,gas_kgco2,lpg_kgco2,oil_kgco2,coal_kgco2,wood_kgco2)]
setkey(res.co2,st)

# Load household end-use shares by heating fuel from RECS
# Source: https://www.eia.gov/consumption/residential/ 
# RECS 2015, tables CE 4.7 - 4.10 and CE 5.4
# Note that Pool and Hot-tub gas consumption have been combined with Space Heating (as an end-use subject to HDH-based seasonal variation)

recs <- fread(paste0(DIR,'RECS_meanHH_fuel_shares.csv'))
recs[,gas_other:=gas_water+gas_clothes+gas_cooking]
recs[,lpg_other:=lpg_water+lpg_clothes+lpg_cooking]
recs[,oil_other:=oil_water][,coal_space:=oil_space]
recs[,coal_other:=oil_other][,wood_space:=oil_space]
recs[,wood_other:=oil_other]
recs <- recs[,.(st,gas_space,gas_other,lpg_space,lpg_other,oil_space,oil_other,coal_space,coal_other,wood_space,wood_other)]
recs[,st:=sprintf("%02.0f",st)]
setkey(recs,st)

# Monthly fuel consumption for natural gas from https://www.eia.gov/opendata/bulkfiles.php 

rmon <- fread(paste0(DIR,'st_res_gas_monthly.csv'))
setnames(rmon,c('state','YYYYMM','gasMCF'))
rmon[,year:=as.numeric(substr(YYYYMM,1,4))][,mon:=as.numeric(substr(YYYYMM,5,6))][,gasMCF:=as.numeric(gasMCF)]
rmon<-rmon[year %in% seq(2011,2017) & state!='US']
setkey(rmon,state)
rmon <- fips[rmon]
rmon<-rmon[,.(st,gasMCF,year,mon,YYYYMM)]
rmon[,fips:=paste0('s',st)]
rmon[,st:=substr(fips,2,3)]
setkey(rmon,st)
setorder(rmon,st,year,mon)
rmon[,gasMCF:=na.approx(gasMCF,rule=2),by=st]
rmon[,st_sum:=sum(gasMCF,na.rm=T),by=c('st','year')][,fips:=NULL]

rmon <- res.co2[rmon]
rmon <- recs[rmon]
rmon[,mon_frac:=gasMCF/st_sum][,gasMCF:=NULL][,st_sum:=NULL]
cols <- c('gas_kgco2','lpg_kgco2','oil_kgco2','coal_kgco2','wood_kgco2')
for (c in cols) set(rmon,j=c,value=(rmon[[c]]*rmon[['mon_frac']]))
rmon <- rmon[,.(st,year,mon,gas_kgco2,lpg_kgco2,oil_kgco2,coal_kgco2,wood_kgco2,gas_space,lpg_space,oil_space,coal_space,wood_space)]
setkey(rmon,st,year,mon)

## Load ACS data on household fuel use by block group

# Need to make changes to blockgroup ID based on changes post-2010 Census as documented at
# https://www.census.gov/programs-surveys/acs/technical-documentation/table-and-geography-changes/2012/geography-changes.html
# also here:  https://www.census.gov/geo/reference/county-changes.html
# and here : https://www.census.gov/programs-surveys/acs/technical-documentation/table-and-geography-changes/2011/geography-changes.html

acs <- fread(paste0(DIR,'ACS_HH_Fuels_',yr,'_blockgroup.csv'))
acs[,GEOID:=as.character(sprintf("%012.0f",as.numeric(GEOID)))]

# first make county code changes, then update GEOID
acs[,fips:=substr(GEOID,1,5)][,id2:=substr(GEOID,6,12)]
acs[fips=='51019',fips:='51515'][fips=='02270',fips:='02158'][fips=='46113',fips:='46102']
acs[,GEOID:=paste0(fips,id2)]

# next make tract level changes, then update GEOID again
acs[,tract:=substr(GEOID,1,11)][,bg:=substr(GEOID,12,12)]
# Los Angeles, CA
acs[tract=='06037930401',tract:='06037137000'][tract=='06037800204',tract:='06037137000']
# Pima, AZ
acs[tract=='04019002701',tract:='04019002704'][tract=='04019002903',tract:='04019002906'][tract=='04019410501',tract:='04019004118'][tract=='04019410502',tract:='04019004121'][tract=='04019410503',tract:='04019004125'][tract=='04019470400',tract:='04019005200'][tract=='04019470500',tract:='04019005300']
acs[tract=='36053940101',tract:='36053030101'][tract=='36053940102',tract:='36053030102'][tract=='36053940103',tract:='36053030103'][tract=='36053940200',tract:='36053030200'][tract=='36053943200',tract:='36053030300'][tract=='36053940401',tract:='36053030401'][tract=='36053940403',tract:='36053030403'][tract=='36053940600',tract:='36053030600'][tract=='36053940700',tract:='36053030402'][tract=='36053940000',tract:='36053024800'][tract=='36053940100',tract:='36053024700']

acs[,GEOID:=paste0(tract,bg)]
acs[,Total_HH_est:=sum(Total_HH_est),by='GEOID']
acs[,Gas_HH_est:=sum(Gas_HH_est),by='GEOID']
acs[,Oil_HH_est:=sum(Oil_HH_est),by='GEOID']
acs[,LPG_HH_est:=sum(LPG_HH_est),by='GEOID']
acs[,Coal_HH_est:=sum(Coal_HH_est),by='GEOID']
acs[,Wood_HH_est:=sum(Wood_HH_est),by='GEOID']
acs[,st:=substr(GEOID,1,2)]
acs[,st_HH:=sum(Total_HH_est),by='st']
acs[,st_gasHH:=sum(Gas_HH_est),by='st']
acs[,st_lpgHH:=sum(LPG_HH_est),by='st']
acs[,st_oilHH:=sum(Oil_HH_est),by='st']
acs[,st_woodHH:=sum(Wood_HH_est),by='st']
acs[,st_coalHH:=sum(Coal_HH_est),by='st']
acs[,gasHH_frac:=Gas_HH_est/st_gasHH]
acs[,lpgHH_frac:=LPG_HH_est/st_lpgHH]
acs[,oilHH_frac:=Oil_HH_est/st_oilHH]
acs[,coalHH_frac:=Coal_HH_est/st_coalHH]
acs[,woodHH_frac:=Wood_HH_est/st_woodHH]

acs <- acs[,.(GEOID,Total_HH_est,gasHH_frac,lpgHH_frac,oilHH_frac,coalHH_frac,woodHH_frac)]
setkey(acs,GEOID)

### Building square footage by building use type by census block from HAZUS_v3 database
hazus=fread('E:/ACES/Commercial/Hazus_v3/all_state_sqft_block.csv')
hazus[,st:=substr(CensusBlock,1,2)]
hazus[,res_sqft:=RES1F+RES2F+RES3AF+RES3BF+RES3CF+RES3DF+RES3EF+RES3FF+RES4F+RES5F+RES6F]
hazus[,com_sqft:=COM1F+COM2F+COM3F+COM4F+COM5F+COM6F+COM7F+COM8F+COM9F+COM10F+AGR1F+REL1F+GOV1F+GOV2F+EDU1F+EDU2F]
hazus[,ind_sqft:=IND1F+IND2F+IND3F+IND4F+IND5F+IND6F]
hazus[,CensusBlock:=as.character(sprintf("%015.0f",as.numeric(CensusBlock)))]
haz_blk <- hazus[,.(CensusBlock,res_sqft,com_sqft,ind_sqft)]
setkey(haz_blk,CensusBlock)
bdir<-'E:/ACES/Census/Blockgroup_2000_2010_xwalk/'
xw.list<-list.files(path=bdir,pattern='TAB2000')
fx<-function(x){
  fread(x,colClasses = 'character')
}
ll<-lapply(paste0(bdir,xw.list),fx)
xw<-rbindlist(ll)
xw[,blk00:=paste0(STATE_2000,COUNTY_2000,TRACT_2000,BLK_2000)][,blk10:=paste0(STATE_2010,COUNTY_2010,TRACT_2010,BLK_2010)][,area_int:=as.numeric(AREALAND_INT)][,land00:=as.numeric(AREALAND_2000)][,water00:=as.numeric(AREAWATER_2000)][,land10:=as.numeric(AREALAND_2010)][,water10:=as.numeric(AREAWATER_2010)][,frac:=area_int/land00]
xw<-xw[,.(blk00,land00,water00,blk10,land10,water10,area_int,frac)]
setnames(xw,'blk00','CensusBlock')
setkey(xw,CensusBlock)
xw <- haz_blk[xw]
xw<-xw[!is.na(res_sqft) & land00>0]
xw[,res2:=res_sqft*frac][,com2:=com_sqft*frac][,ind2:=ind_sqft*frac]
haz2 <- xw[,lapply(.SD,sum),by='blk10',.SDcols=c('res2','com2','ind2')]
setnames(haz2,c('CensusBlock','res_sqft','com_sqft','ind_sqft'))
newblk<-unique(haz2$CensusBlock)
haz_blk<-haz_blk[!CensusBlock %in% newblk]

### final manual fix of changes to census block groups / blocks as described here:
### https://www.census.gov/programs-surveys/acs/technical-documentation/table-and-geography-changes/2011/geography-changes.html
### and here: https://www2.census.gov/geo/pdfs/reference/Geography_Notes.pdf
 
haz3<-rbind(haz_blk,haz2)
haz3[,fips:=substr(CensusBlock,1,5)][,id2:=substr(CensusBlock,6,16)]
haz3[fips=='51019',fips:='51515'][fips=='02270',fips:='02158'][fips=='46113',fips:='46102']
haz3[,CensusBlock:=paste0(fips,id2)]
## next make tract level changes, then update again
haz3[,tract:=substr(CensusBlock,1,11)][,blk:=substr(CensusBlock,12,16)][,bg:=substr(CensusBlock,12,12)][,blk2:=substr(CensusBlock,13,16)]
## Los Angeles, CA
haz3[tract=='06037930401',tract:='06037137000'][tract=='06037800204',tract:='06037137000']
haz3[tract=='06037137000' & bg=='3',bg:='2']
haz3[tract=='06037137000' & bg=='2',blk:=paste0(bg,blk2)]
## Pima, AZ
haz3[tract=='04019002701',tract:='04019002704'][tract=='04019002903',tract:='04019002906'][tract=='04019410501',tract:='04019004118'][tract=='04019410502',tract:='04019004121'][tract=='04019410503',tract:='04019004125'][tract=='04019470400',tract:='04019005200'][tract=='04019470500',tract:='04019005300']
## Madison and Oneida, NY
haz3[tract=='36053940101',tract:='36053030101'][tract=='36053940102',tract:='36053030102'][tract=='36053940103',tract:='36053030103'][tract=='36053940200',tract:='36053030200'][tract=='36053940300',tract:='36053030300'][tract=='36053940401',tract:='36053030401'][tract=='36053940403',tract:='36053030403'][tract=='36053940600',tract:='36053030600'][tract=='36053940700',tract:='36053030402'][tract=='36065940000',tract:='36065024800'][tract=='36065940100',tract:='36065024700'][tract=='36065940200',tract:='36065024900']
haz3[,CensusBlock:=paste0(tract,blk)]
haz3[CensusBlock=='360650230002035', CensusBlock:='360650249001035']
haz3[CensusBlock=='360650230002045', CensusBlock:='360650249001045']
haz3[CensusBlock=='360650230002046', CensusBlock:='360650249001046']
haz3[CensusBlock=='360650230002043', CensusBlock:='360650249003043']
haz3[CensusBlock=='360650230002044', CensusBlock:='360650249003044']
haz3[CensusBlock=='360650230002047', CensusBlock:='360650249003047']
haz3[CensusBlock=='360650230002048', CensusBlock:='360650249003048']
haz3[,GEOID:=substr(CensusBlock,1,12)]
haz3[,bg_res_sqft:=sum(res_sqft),by='GEOID'][,blk_frac_res:=res_sqft/bg_res_sqft]
haz3[,bg_com_sqft:=sum(com_sqft),by='GEOID'][,blk_frac_com:=com_sqft/bg_com_sqft]
haz3[,bg_ind_sqft:=sum(ind_sqft),by='GEOID'][,blk_frac_ind:=ind_sqft/bg_ind_sqft]
haz_blk<-haz3[,.(CensusBlock,res_sqft,com_sqft,ind_sqft)]
haz_sqft <- unique(haz3[,.(GEOID,bg_res_sqft)])
setkey(haz_sqft,GEOID)
haz_sqft[,st:=substr(GEOID,1,2)]

hh.chk<-merge(haz_sqft,acs,key='GEOID',all=T)
hh <- hh.chk[!is.na(Total_HH_est) & Total_HH_est>0]
hh[,st:=substr(GEOID,1,2)]
hh[,sqft_HH:=bg_res_sqft/Total_HH_est]
hh[,st_sqft_HH:=mean(sqft_HH,na.rm=T),by='st']
hh[,frac_sqft:=sqft_HH/st_sqft_HH]
hh <- hh[!st %in% c('15','72')]
hh <- hh[,.(GEOID,Total_HH_est,gasHH_frac,lpgHH_frac,oilHH_frac,coalHH_frac,woodHH_frac,frac_sqft)]
rm(hh.chk,acs,codes,fips,recs,res.co2,res.fuels,res.wide,seds,seds.res);gc()

stc<-readRDS(paste0(DIR,'statefips.rds'))
stc <- stc[!stc %in% c('02','72','15')]

dy<-data.table()

for (s in stc){
  
  print(paste0('Starting ',s))
  dt <- readRDS(paste0(DIR,'stRAP/',yr,'/bg_rap_state',s,'.rds'))
  
  # Calculate each months share of annual Heating Degree Hours for the state
  dt[,day:=substr(datetime,7,8)][,monHDH:=sum(HDH),by=list(mon)]
  dt[,yrHDH:=sum(HDH),by='year'][,mfracHDH:=monHDH/yrHDH]
  adj <- dt[day=='01',.(st,year,mon,mfracHDH)]
  adj <- unique(adj)
  setkey(adj,st,year,mon)
  adj <- rmon[adj]
  dt<-dt[,.(st,year,mon,datetime,GEOID,HDH)]
  
 
  adj[,yr_gas:=sum(gas_kgco2),by='year'][,yr_gas_space:=yr_gas*gas_space][,yr_gas_other:=yr_gas-yr_gas_space]
  adj[,yr_lpg:=sum(lpg_kgco2),by='year'][,yr_lpg_space:=yr_lpg*lpg_space][,yr_lpg_other:=yr_lpg-yr_lpg_space]
  adj[,yr_oil:=sum(oil_kgco2),by='year'][,yr_oil_space:=yr_oil*oil_space][,yr_oil_other:=yr_oil-yr_oil_space]
  adj[,yr_coal:=sum(coal_kgco2),by='year'][,yr_coal_space:=yr_coal*coal_space][,yr_coal_other:=yr_coal-yr_coal_space]
  adj[,yr_wood:=sum(wood_kgco2),by='year'][,yr_wood_space:=yr_wood*wood_space][,yr_wood_other:=yr_wood-yr_wood_space]
  adj<-adj[,.(st,year,yr_gas_space,yr_gas_other,yr_lpg_space,yr_lpg_other,yr_oil_space,yr_oil_other,yr_coal_space,yr_coal_other,yr_wood_space,yr_wood_other)]
  adj<-unique(adj)
  setkey(adj,st,year)
  setkey(dt,datetime)
  dt<-td[dt]
  setkey(dt,GEOID)
  
  dt <- hh[dt]
  dt <- dt[!is.na(Total_HH_est) & Total_HH_est>0]
  
  # Blockgroup adjustment factor is blockgroup's total HDH relative to mean value of same across blockgroups
  dt[,bg_yr_HDH:=sum(HDH),by=c('GEOID','year')]
  dt[,mean_bgHDH:=mean(bg_yr_HDH,na.rm=T),by=c('st','year')]
  dt[,bg_adjHH:=bg_yr_HDH/mean_bgHDH]
  setkey(dt,st,year)
  dt <- adj[dt]
  
  dt[,gas2:=yr_gas_space*gasHH_frac*bg_adjHH*frac_sqft][,s2:=sum(gas2,na.rm=T),by=list(st,year,chr)][,gas_frac:=gas2/s2][,bg_yr_gas_space:=yr_gas_space*gas_frac]
  dt[,gas2:=yr_gas_other*gasHH_frac*frac_sqft][,s2:=sum(gas2,na.rm=T),by=list(st,year,chr)][,gas_frac:=gas2/s2][,bg_yr_gas_other:=yr_gas_other*gas_frac]
  dt[,lpg2:=yr_lpg_space*lpgHH_frac*bg_adjHH*frac_sqft][,s2:=sum(lpg2,na.rm=T),by=list(st,year,chr)][,lpg_frac:=lpg2/s2][,bg_yr_lpg_space:=yr_lpg_space*lpg_frac]
  dt[,lpg2:=yr_lpg_other*lpgHH_frac*frac_sqft][,s2:=sum(lpg2,na.rm=T),by=list(st,year,chr)][,lpg_frac:=lpg2/s2][,bg_yr_lpg_other:=yr_lpg_other*lpg_frac]
  dt[,oil2:=yr_oil_space*oilHH_frac*bg_adjHH*frac_sqft][,s2:=sum(oil2,na.rm=T),by=list(st,year,chr)][,oil_frac:=oil2/s2][,bg_yr_oil_space:=yr_oil_space*oil_frac]
  dt[,oil2:=yr_oil_other*oilHH_frac*frac_sqft][,s2:=sum(oil2,na.rm=T),by=list(st,year,chr)][,oil_frac:=oil2/s2][,bg_yr_oil_other:=yr_oil_other*oil_frac]
  dt[,coal2:=yr_coal_space*coalHH_frac*bg_adjHH*frac_sqft][,s2:=sum(coal2,na.rm=T),by=list(st,year,chr)][,coal_frac:=coal2/s2][,bg_yr_coal_space:=yr_coal_space*coal_frac]
  dt[,coal2:=yr_coal_other*coalHH_frac*frac_sqft][,s2:=sum(coal2,na.rm=T),by=list(st,year,chr)][,coal_frac:=coal2/s2][,bg_yr_coal_other:=yr_coal_other*coal_frac]
  dt[,wood2:=yr_wood_space*woodHH_frac*bg_adjHH*frac_sqft][,s2:=sum(wood2,na.rm=T),by=list(st,year,chr)][,wood_frac:=wood2/s2][,bg_yr_wood_space:=yr_wood_space*wood_frac]
  dt[,wood2:=yr_wood_other*woodHH_frac*frac_sqft][,s2:=sum(wood2,na.rm=T),by=list(st,year,chr)][,wood_frac:=wood2/s2][,bg_yr_wood_other:=yr_wood_other*wood_frac]
  dt[,HDH_hr_frac:=HDH/bg_yr_HDH][,yr_hrs:=max(chr),by=list(year)]
  
  dt<-dt[,.(year,mon,datetime,chr,st,GEOID,yr_hrs,HDH_hr_frac,bg_yr_gas_other,bg_yr_gas_space,bg_yr_lpg_other,bg_yr_lpg_space,bg_yr_oil_other,bg_yr_oil_space,bg_yr_coal_other,bg_yr_coal_space,bg_yr_wood_other,bg_yr_wood_space)]
  gc()
  dt[is.na(dt)]<-0
  
  # Space heating assigned to hour by hour's share of blockgroup annual HDH
  # Other end use emissions assigned flat hourly time structure
  
  cs2 <- c('bg_yr_gas_space','bg_yr_lpg_space','bg_yr_oil_space','bg_yr_coal_space','bg_yr_wood_space')
  for (c in cs2) set(dt,j=c,value=(dt[[c]]*dt[['HDH_hr_frac']]))
  
  cs3 <- c('bg_yr_gas_other','bg_yr_lpg_other','bg_yr_oil_other','bg_yr_coal_other','bg_yr_wood_other')
  for (c in cs3) set(dt,j=c,value=(dt[[c]]/dt[['yr_hrs']]))
  setnames(dt,cs2,gsub('bg_yr','kgco2',cs2))
  setnames(dt,cs3,gsub('bg_yr','kgco2',cs3))
  
  dt[,kgco2_gas:=kgco2_gas_other+kgco2_gas_space]
  dt[,kgco2_lpg:=kgco2_lpg_other+kgco2_lpg_space]
  dt[,kgco2_oil:=kgco2_oil_other+kgco2_oil_space]
  dt[,kgco2_coal:=kgco2_coal_other+kgco2_coal_space]
  dt[,kgco2_wood:=kgco2_wood_other+kgco2_wood_space]
  dt[,kgco2_total:=kgco2_gas+kgco2_lpg+kgco2_oil+kgco2_coal+kgco2_wood]
  dt[,kgFFco2:=kgco2_total-kgco2_wood]
  dt[,kgBIOco2:=kgco2_wood]
  dt[,biofrac:=kgBIOco2/kgco2_total]
  dt<-dt[,.(year,chr,datetime,st,GEOID,kgBIOco2,kgFFco2)]
  saveRDS(dt,paste0(DIR,'stCO2/',yr,'/bg_hourly_kgco2_st',s,'_',yr,'.rds'))
  
  # Sum to annual and append for annual file output
  dt<-dt[,lapply(.SD,sum),by=list(year,GEOID),.SDcols=c('kgBIOco2','kgFFco2')]
  dy<-rbind(dy,dt)
  print(paste0('Completed ',s))          
  warnings()
}

# Calculate annual gridded emissions on ACES 1km VLCC grid
setkey(dy,GEOID)
grid <- fread(paste0(DIR,'block_group_1km_inter.csv'))
cs <- colnames(grid)[c(2:4,7)]
grid <- grid[,(cs),with=F]
grid[,GEOID:=as.character(GEOID)]
grid[nchar(GEOID)<12,GEOID:=paste0('0',GEOID)]
setkey(grid,GEOID)

dy <- grid[dy]
dy[,kgBIOco2:=kgBIOco2/bg_area_m2*inter_area_m2]
dy[,kgFFco2:=kgFFco2/bg_area_m2*inter_area_m2]
dy <- dy[,lapply(.SD,sum),by=c('cellid'),.SDcols=c('kgBIOco2','kgFFco2')]
fwrite(dy,paste0(DIR,'grid1km_res_kgco2_',yr,'.csv'))
print('gridded res kgco2 complete')

