
tig <- fread('aces_tiger_cnty_UA_inter_v2.csv')
tig <- tig[,.(OBJECTID,GEOID,UACE10,MTFCC,fclass,Shape_Length)]
setnames(tig,c('roadID','fips','ua','mtfcc','tfcl','len_meters'))
tig[,st:=as.numeric(substr(sprintf("%05d",fips),1,2))]
tig[ua==99999 & mtfcc=='S1100',tfcl:=1]
tig[ua==99999 & mtfcc=='S1630',tfcl:=1]
tig[ua==99999 & mtfcc=='S1200',tfcl:=6]
tig[ua==99999 & mtfcc=='S1400',tfcl:=8]
tig[ua<99999 & mtfcc=='S1100',tfcl:=12]
tig[ua<99999 & mtfcc=='S1630',tfcl:=12]
tig[ua<99999 & mtfcc=='S1200',tfcl:=16]
tig[ua<99999 & mtfcc=='S1400',tfcl:=19]
setkey(tig,st,fips,tfcl)
tig <- tig[!is.na(tfcl)]
tig <- tig[!st %in% c(2,15,60,66,69,72,78)]
fwrite(tig,'aces_tiger_final_fcl.csv')
fdt <- tig[,.(roadID)]
setkey(fdt,roadID)
tlen <- tig[,.(roadID,len_meters)]
setkey(tlen,roadID)
saveRDS(tlen,'roadID_len_meters.rds')

# VM4 vehicle shares and fuel economies
vm4 <- fread('vm4_2010_2017_fixed.csv')
setkey(vm4,year,st,vfcl)

mf21 <- fread('mf21_1980_2017.csv')
mf21[,st_gas:=as.numeric(st_gas)]
mf21[,st_kgco2:=st_gas*8.572896 + st_diesel*10.16047]
mf21 <- mf21[!st %in% c(2,15)]
setkey(mf21,year,st)

mf21a <- mf21[,lapply(.SD,sum),by=list(year),.SDcols=c('st_gas','st_diesel','st_kgco2')]
xw <- data.table(fcl=c(1,2,6,7,8,9,11,12,14,16,17,19),vfcl=c(1,2,3,3,3,3,4,4,5,5,5,5))
setkey(xw,fcl)

# VM2 state total VMT by fclass
v2<-fread('vm2_2012_2017.csv',colClasses = 'double')
vm2<-melt.data.table(v2,id.vars=c('year','st'),variable.name='fcl',value.name='avm2')
vm2[,fcl:=gsub('fcl','',fcl)]
vm2[,fcl:=as.integer(fcl)]
vm2 <- vm2[,lapply(.SD,sum),by=list(year,st,fcl),.SDcols=c('avm2')]
setkey(vm2,year,st,fcl)

# Load HPMS data (Shapefiles merged, reprojected, and intersected with county/Urbanized Area shapefile)

yrs <- c(2013,2014,2015,2016,2017)
for (yr in yrs){
  
  h3<-fread(paste0('hpms',yr,'_cnty_UA_inter.csv'),colClasses = 'numeric')
  setnames(h3,c('st','fips','ua','ua2','fcl','aadt','len_meters','begin','end'))
  h3 <- h3[!st %in% c(2,15)]
  h3[,year:=yr]
  h3[,fips:=sprintf("%05d",fips)]
  h3[,st:=as.numeric(substring(fips,1,2))]
  h3[,len_miles:=end-begin]
  h3<-h3[len_miles>0]
  if(yr!=2016){
    h3[,avmt:=365*aadt*len_meters/1000*0.621371]
  } else {
    h3[,avmt:=366*aadt*len_meters/1000*0.621371]
  }
  h3[ua==0,ua:=ua2]
  h3[ua==99999 & ua2!=99999,ua:=ua2]
  h3[,ua2:=NULL]
  h3 <- unique(h3)
  setnames(h3,'fcl','fclass')
  h3[,fclass:=as.integer(fclass)]
  h3[ua!=99999 & fclass==9,fclass:=19]
  # Recode functional classes to match traditional HPMS codes
  h3[ua==99999 & fclass==0,fcl:=9]
  h3[ua==99999 & fclass==1,fcl:=1]
  h3[ua==99999 & fclass==2,fcl:=2]
  h3[ua==99999 & fclass==3,fcl:=2]
  h3[ua==99999 & fclass==4,fcl:=6]
  h3[ua==99999 & fclass==5,fcl:=7]
  h3[ua==99999 & fclass==6,fcl:=8]
  h3[ua==99999 & fclass==7,fcl:=9]
  h3[ua!=99999 & fclass==0,fcl:=19]
  h3[ua!=99999 & fclass==1,fcl:=11]
  h3[ua!=99999 & fclass==2,fcl:=12]
  h3[ua!=99999 & fclass==3,fcl:=14]
  h3[ua!=99999 & fclass==4,fcl:=16]
  h3[ua!=99999 & fclass==5,fcl:=17]
  h3[ua!=99999 & fclass==6,fcl:=17]
  h3[ua!=99999 & fclass==7,fcl:=19]
  h3 <- h3[,lapply(.SD,sum),by=list(year,st,fips,fcl),.SDcols=c('avmt','len_meters')]
  h3[,st_avmt:=sum(avmt),by=list(st,fcl)]
  setkey(h3,year,st,fcl)
  h3<-merge(vm2,h3,key=c('year','st','fcl'),all=T)
  h3 <- h3[year==yr & !st %in% c(2,15)]
  h3[is.na(avmt) & avm2>0, st_avmt:=avm2]
  h3 <- h3[!is.na(st_avmt)]
  hm <- h3[is.na(fips)]
  hm <- hm[,.(year,st,fcl,avm2,st_avmt)]
  h3 <- h3[!is.na(fips)]
  hf <- h3[st %in% unique(hm$st)]
  hf <- unique(hf[,.(year,st,fips)])
  setkey(hf,year,st)
  setkey(hm,year,st)
  hf<-hm[hf]
  hf[,avmt:=0][,len_meters:=0]
  h3 <- rbind(h3,hf)
  h3[,urb:=0]
  h3[fcl>10,urb:=1]
  h3[,st_ru:=sum(avmt),by=list(st,year,urb)]
  h3[,cnty_ru:=sum(avmt),by=list(fips,year,urb)]
  h3 <- h3[avm2>0]
  # Calculate difference between HPMS and VM-2 state total VMT by fclass
  h3[,vmt_diff:=avm2-st_avmt]
  # Remove surplus VMT from HPMS to maintain match with VM-2
  h3[vmt_diff<0,avmt:=avmt/st_avmt*vmt_diff+avmt]
  h3[,st_avmt:=sum(avmt),by=list(st,fcl)]
  h3[,vmt_diff:=avm2-st_avmt]
  h3[vmt_diff<0 & vmt_diff>(-1),vmt_diff:=0]
  if(min(h3$vmt_diff<0)){
    message("ERROR - overestimates of VMT remain in HPMS")
  }
  
  # Positive adjustment factor for major roads is the road's share of statewide VMT in that fclass
  h3[!fcl %in% c(8,9,17,19),avmt2:=(avmt/st_avmt*vmt_diff)+avmt]
  # Positive adjustment factor for minor/local roads is the county of the road's share of statewide urban/rural VMT
  h3[fcl %in% c(8,9,17,19),avmt2:=(cnty_ru/st_ru*vmt_diff)+avmt]
  # Calculate VM-2 AVMT that is still unaccounted for
  h3[,sum_chk:=sum(avmt2),by=list(st,fcl)][,missing:=avm2-sum_chk]
  
  # Create TIGER fclass codes
  h3[fcl %in% c(1,2),tfcl:=1]
  h3[fcl %in% c(6,7),tfcl:=6]
  h3[fcl %in% c(8,9),tfcl:=8]
  h3[fcl %in% c(11,12,14),tfcl:=12]
  h3[fcl %in% c(16,17),tfcl:=16]
  h3[fcl==19,tfcl:=19]
  
  # change TIGER fclass codes for county/tfcl codes identified in loess post-processing
  h3[,code:=paste(year,fips,tfcl,sep='_')]
  setkey(h3,code)
  newcodes <- fread('new_tfcl.csv')
  setkey(newcodes,code)
  newcodes[,fcl2:=as.numeric(fcl2)]
  h3 <- newcodes[h3]
  h3[!is.na(fcl2),tfcl:=fcl2]
  # Extract missing VMT county/fclass pairs
  miss <- unique(h3[,.(year,st,fcl,tfcl,missing)])
  # Ignore small amounts of missing VMT
  miss <- miss[missing>1]
  # Aggregate to TIGER fclass
  miss <- miss[,lapply(.SD,sum),by=list(year,st,tfcl),.SDcols='missing']
  setkey(miss,st,tfcl)
  t2 <- tig[,lapply(.SD,sum),by=list(st,fips,tfcl),.SDcols='len_meters']
  t2[,st_meters:=sum(len_meters),.(st,tfcl)]
  t2[,cnty_meters:=sum(len_meters),.(fips,tfcl)]
  t2[,frac_meters:=cnty_meters/st_meters]
  setkey(t2,st,tfcl)
  t2 <- miss[t2]
  t2 <- t2[!is.na(missing)]
  # Use each county/fclass pair's share of state/fclass total road length (in TIGER) to partition out missing VMT
  t2[,avmt:=frac_meters*missing][,fcl:=tfcl]
  t2 <- t2[,.(year,st,fips,fcl,tfcl,avmt)]
  # Append records to original data.table
  h3 <- h3[,.(year,st,fips,fcl,tfcl,avmt2)]
  setnames(h3,'avmt2','avmt')
  h3 <- rbind(h3,t2)
  h3[st==11 & tfcl<10,avmt:=0]
  h3 <- h3[,lapply(.SD,sum),by=list(year,st,fips,fcl,tfcl),.SDcols='avmt']
  
  # Check for discrepancies between adjusted HPMS VMT and VM-2 at state level (fclass-total discrepancies are OK due to TIGER fclass recoding)
  h3[,st_avmt:=sum(avmt),by=list(st)]
  h3[,sf_avmt:=sum(avmt),by=list(st,fcl)]
  setkey(h3,year,st)
  vm2s <- vm2[,lapply(.SD,sum),by=list(year,st),.SDcols='avm2']
  setnames(vm2s,'avm2','svm2')
  setkey(vm2s,year,st)
  h3<-merge(vm2s,h3,key=c('year','st'),all=T)
  h3 <- h3[year==yr & !st %in% c(2,15)]
  # Final Adjustment for all roads is the cnty/fclass share of statewide VMT
  h3[,svmt_diff:=svm2-st_avmt]
  h3[,avmt2:=(avmt/st_avmt*svmt_diff)+avmt]
  h3[,avmt:=NULL]
  setnames(h3,'avmt2','avmt')
  
  # Confirm complete match between VMT and VM-2 at state level
  h3[,st_avmt:=sum(avmt),by=list(st)]
  h3[,svmt_diff:=svm2-st_avmt]
  h3[,spct:=round(svmt_diff/svm2*100,2)]
  message(paste0('min = ',min(h3$spct),'%  max = ',max(h3$spct),'%'))
  if(max(h3$spct)>5 | min(h3$spct)<(-5)){
    message('WARNING - Missing VMT > 5%')
    break
  }
  
  setkey(h3,fcl)
  h3 <- xw[h3]
  setkey(h3,year,st,vfcl)
  h3 <- vm4[h3]
  setkey(h3,year,st)
  h3 <- mf21[h3]
  cs <- c('v1','v2','v3','v4','v5')
  for (j in cs) set(h3, j=j,value=h3[[j]]*h3$avmt)
  h3[,f1:=v1/m1][,f2:=v2/m2][,f3:=v3/m3][,f4g:=v4/m4*0.77][,f4d:=v4/m4*0.23][,f5:=v5/m5]
  h3[,mgas:=f1+f2+f4g][,mdies:=f3+f4d+f5]
  h3[,sum_gas:=sum(mgas),by=list(year,st)][,gas2:=mgas/sum_gas*st_gas]
  h3[,sum_dies:=sum(mdies),by=list(year,st)][,dies2:=mdies/sum_dies*st_diesel]
  h3[,chk_gas2:=sum(gas2),by=list(year,st)][,chk_pct:=chk_gas2-st_gas]
  
  h3[fips==17175 & tfcl==16,tfcl:=6][fips==17175 & tfcl==19,tfcl:=8]
  h3[fips==17151 & tfcl==16,tfcl:=6][fips==17151 & tfcl==19,tfcl:=8]
  h4 <- h3[,lapply(.SD,sum),by=list(year,st,fips,tfcl),.SDcols=c('avmt','gas2','dies2')]
  h4[,fips:=as.numeric(fips)]
  setkey(h4,st,fips,tfcl)
  t3 <- merge(tig,h4,key=c('fips','tfcl'),all=T,allow.cartesian=T)
  no_tig <- t3[is.na(len_meters)]
  no_hp <- t3[is.na(gas2)]
  t3 <- t3[!is.na(len_meters) & !is.na(gas2)]
  tc <- unique(t3[,.(fips,tfcl)])
  tw <- dcast.data.table(tc,fips~tfcl,value.var='tfcl')
  setnames(tw,c('fips','f1','f6','f8','f12','f16','f19'))
  setkey(tw,fips)
  setkey(no_tig,fips)
  no_tig <- tw[no_tig]
  no_tig[tfcl==1 & !is.na(f12),tfcl2:=12]
  no_tig[tfcl==1 & !is.na(f6) & is.na(f12),tfcl2:=6]
  no_tig[tfcl==1 & !is.na(f16) & is.na(f12) & is.na(f6),tfcl2:=16]
  no_tig[tfcl==1 & !is.na(f8) & is.na(f6),tfcl2:=8]
  no_tig[tfcl==1 & !is.na(f19) & is.na(f8),tfcl2:=19]
  no_tig[tfcl==6 & !is.na(f16),tfcl2:=16]
  no_tig[tfcl==6 & !is.na(f8) & is.na(f16),tfcl2:=8]
  no_tig[tfcl==6 & !is.na(f19) & is.na(f16) & is.na(f8),tfcl2:=19]
  no_tig[tfcl==8 & !is.na(f19),tfcl2:=19]
  no_tig[tfcl==8 & !is.na(f6) & is.na(f19),tfcl2:=6]
  no_tig[tfcl==8 & !is.na(f16) & is.na(f19) & is.na(f6),tfcl2:=16]
  no_tig[tfcl==12 & !is.na(f1),tfcl2:=1]
  no_tig[tfcl==12 & !is.na(f16) & is.na(f1),tfcl2:=16]
  no_tig[tfcl==12 & !is.na(f6) & is.na(f16) & is.na(f1),tfcl2:=6]
  no_tig[tfcl==12 & !is.na(f19) & is.na(f16),tfcl2:=19]
  no_tig[tfcl==16 & !is.na(f6),tfcl2:=6]
  no_tig[tfcl==16 & !is.na(f19) & is.na(f6),tfcl2:=19]
  no_tig[tfcl==16 & !is.na(f8) & is.na(f6) & is.na(f19),tfcl2:=8]
  no_tig[tfcl==19 & !is.na(f8),tfcl2:=8]
  no_tig[tfcl==19 & !is.na(f16) & is.na(f8),tfcl2:=16]
  no_tig[tfcl==19 & !is.na(f6) & is.na(f16) & is.na(f8),tfcl2:=6]
  nt2 <- unique(no_tig[,.(fips,tfcl,tfcl2)])
  setkey(nt2,fips,tfcl)
  setkey(h4,fips,tfcl)
  h4 <- nt2[h4]
  h4[!is.na(tfcl2),tfcl:=tfcl2]
  h4[,tfcl2:=NULL]
  h4 <- h4[,lapply(.SD,sum),by=list(year,st,fips,tfcl),.SDcols=c('avmt','gas2','dies2')]
  setkey(h4,st,fips,tfcl)
  t3 <- merge(tig,h4,key=c('fips','tfcl'),all=T,allow.cartesian=T)
  no_tig <- t3[is.na(len_meters)]
  if (dim(no_tig)[1]!=0){
    message('ERROR! --- Emissions not fully assigned')
  }
  t3 <- t3[!is.na(year)]
  t3[,sum_meters:=sum(len_meters),by=list(fips,tfcl)]
  # kg CO2 released by burning 1 gallon of E10 gasoline (10% ethanol)
  t3[,kgco2_gas:=gas2*8.572896]
  # kg CO2 released by burning 1 gallon of diesel fuel
  t3[,kgco2_dies:=dies2*10.16047]
  t3[,kgFFco2:=kgco2_dies+kgco2_gas][,kgFFco2_m:=kgFFco2/sum_meters][,gas_m:=gas2/sum_meters][,dies_m:=dies2/sum_meters]
  t3[,avmt:=avmt/sum_meters*len_meters]
  saveRDS(t3,paste0('Output/aces_darte_onroad_kgco2_',yr,'.rds'))

  m4 <- dcast.data.table(t3[,.(year,roadID,kgFFco2_m)],roadID~year,fun=mean,value.var=c('kgFFco2_m'))
  rm(t3);gc()
  m4[is.na(m4)] <- 0
  setkey(m4,roadID)
  fdt <- m4[fdt]
  rm(m4);gc()
}

# Load in and tidy DARTE data
ad<-fread('all_darte_data.csv')
ad <- ad[,.(year,state,fips,fclass,gas_shr,avmt,v_1,v_2,v_3,v_4,v_5,mpg1,mpg2,mpg3,mpg4,mpg5)]
setnames(ad,c('year','st','fips','fcl','gas_shr','avmt','v1','v2','v3','v4','v5','m1','m2','m3','m4','m5'))
setkey(ad,year,st)
ad[fcl %in% c(1,2),tfcl:=1]
ad[fcl %in% c(6,7),tfcl:=6]
ad[fcl %in% c(8,9),tfcl:=8]
ad[fcl %in% c(11,12,14),tfcl:=12]
ad[fcl %in% c(16,17),tfcl:=16]
ad[fcl==19,tfcl:=19]
# change TIGER fclass codes for county/tfcl codes identified in loess post-processing
ad[,code:=paste(year,fips,tfcl,sep='_')]
setkey(ad,code)
ad <- newcodes[ad]
ad[!is.na(fcl2),tfcl:=fcl2]

setkey(ad,fcl)
ad <- xw[ad]
setkey(ad,year,st)
ad <- mf21[ad]
ad[,f1:=v1/m1][,f2:=v2/m2][,f3:=v3/m3][,f4g:=v4/m4*gas_shr][,f4d:=v4/m4*(1-gas_shr)][,f5:=v5/m5]
ad[,mgas:=f1+f2+f4g][,mdies:=f3+f4d+f5]
ad[,sum_gas:=sum(mgas),by=list(year,st)][,gas2:=mgas/sum_gas*st_gas]
ad[,sum_dies:=sum(mdies),by=list(year,st)][,dies2:=mdies/sum_dies*st_diesel]
ad[,chk_gas2:=sum(gas2),by=list(year,st)][,chk_pct:=chk_gas2-st_gas]



ad <- ad[,.(year,st,fips,tfcl,avmt,gas2,dies2)]
setkey(ad,fips,tfcl)
setkey(tig,st,fips,tfcl)
for (yr in 1980:2012){
  h4 <- ad[year==yr]
  h4[fips==17175 & tfcl==16,tfcl:=6][fips==17175 & tfcl==19,tfcl:=8]
  h4[fips==17151 & tfcl==16,tfcl:=6][fips==17151 & tfcl==19,tfcl:=8]
  h4[fips==46113,fips:=46102]
  h4[fips==51515,fips:=51019]
  h4 <- h4[,lapply(.SD,sum),by=list(year,st,fips,tfcl),.SDcols=c('avmt','gas2','dies2')]
  h4[,fips:=as.numeric(fips)]
  setkey(h4,fips,tfcl)
  t3<-merge(tig,h4,key=c('fips','tfcl'),all=T,allow.cartesian=T)
  no_tig <- t3[is.na(len_meters)]
  no_hp <- t3[is.na(gas2)]
  t3 <- t3[!is.na(len_meters) & !is.na(gas2)]
  tc <- unique(tig[,.(fips,tfcl)])
  tw <- dcast.data.table(tc,fips~tfcl,value.var='tfcl')
  setnames(tw,c('fips','f1','f6','f8','f12','f16','f19'))
  setkey(tw,fips)
  setkey(no_tig,fips)
  no_tig <- tw[no_tig]
  no_tig[tfcl==1 & !is.na(f12),tfcl2:=12]
  no_tig[tfcl==1 & !is.na(f6) & is.na(f12),tfcl2:=6]
  no_tig[tfcl==1 & !is.na(f16) & is.na(f12) & is.na(f6),tfcl2:=16]
  no_tig[tfcl==6 & !is.na(f16),tfcl2:=16]
  no_tig[tfcl==6 & !is.na(f8) & is.na(f16),tfcl2:=8]
  no_tig[tfcl==6 & !is.na(f19) & is.na(f16) & is.na(f8),tfcl2:=19]
  no_tig[tfcl==8 & !is.na(f19),tfcl2:=19]
  no_tig[tfcl==8 & !is.na(f6) & is.na(f19),tfcl2:=6]
  no_tig[tfcl==8 & !is.na(f16) & is.na(f19) & is.na(f6),tfcl2:=16]
  no_tig[tfcl==12 & !is.na(f1),tfcl2:=1]
  no_tig[tfcl==12 & !is.na(f16) & is.na(f1),tfcl2:=16]
  no_tig[tfcl==12 & !is.na(f6) & is.na(f16) & is.na(f1),tfcl2:=6]
  no_tig[tfcl==16 & !is.na(f6),tfcl2:=6]
  no_tig[tfcl==16 & !is.na(f19) & is.na(f6),tfcl2:=19]
  no_tig[tfcl==16 & !is.na(f8) & is.na(f6) & is.na(f19),tfcl2:=8]
  no_tig[tfcl==19 & !is.na(f8),tfcl2:=8]
  no_tig[tfcl==19 & !is.na(f16) & is.na(f8),tfcl2:=16]
  no_tig[tfcl==19 & !is.na(f6) & is.na(f16) & is.na(f8),tfcl2:=6]
  nt2 <- unique(no_tig[,.(fips,tfcl,tfcl2)])
  setkey(nt2,fips,tfcl)
  setkey(h4,fips,tfcl)
  h4 <- nt2[h4]
  h4[!is.na(tfcl2),tfcl:=tfcl2]
  h4[,tfcl2:=NULL]
  h4 <- h4[,lapply(.SD,sum),by=list(year,st,fips,tfcl),.SDcols=c('avmt','gas2','dies2')]
  setkey(h4,st,fips,tfcl)
  t3 <- merge(tig,h4,key=c('st','fips','tfcl'),all=T,allow.cartesian=T)
  no_tig <- t3[is.na(len_meters)]
  if (dim(no_tig)[1]!=0){
    message('ERROR! --- Emissions not fully assigned')
  }
  t3 <- t3[!is.na(year)]
  t3[,sum_meters:=sum(len_meters),by=list(fips,tfcl)]
  t3[,gas_m:=gas2/sum_meters][,dies_m:=dies2/sum_meters]
  t3[,gas2:=gas_m*len_meters][,dies2:=dies_m*len_meters]
  # kg CO2 released by burning 1 gallon of E10 gasoline (10% ethanol)
  t3[,kgco2_gas:=gas2*8.572896]
  # kg CO2 released by burning 1 gallon of diesel fuel
  t3[,kgco2_dies:=dies2*10.16047]
  t3[,kgFFco2:=kgco2_gas+kgco2_dies]
  t3[,kgFFco2_m:=kgFFco2/len_meters]
  t3[,avmt:=avmt/sum_meters*len_meters]
  saveRDS(t3,paste0('Output/aces_darte_onroad_kgco2_',yr,'.rds'))
  
  m4 <- dcast.data.table(t3[,.(year,roadID,kgFFco2_m)],roadID~year,fun=mean,value.var=c('kgFFco2_m'))
  rm(t3);gc()
  m4[is.na(m4)] <- 0
  setkey(m4,roadID)
  fdt <- m4[fdt]
  rm(m4);gc()
}

rm(list=setdiff(ls(), c("fdt")));gc()
tlen <- readRDS('roadID_len_meters.rds')
setkey(tlen,roadID)
asum <- tlen[fdt]
rm(list=setdiff(ls(), "asum"));gc()
cs <- colnames(asum)[3:78]
for (j in cs) set(asum, j=j,value=asum[[j]]*asum[['len_meters']])
setnames(asum,c('roadID','len_meters',gsub('_m','',cs)))
saveRDS(asum,'roadID_kgco2_1980_2017.rds')

#####
#----
#####

tig <- fread('aces_tiger_final_fcl.csv')
setkey(tig,roadID)
asum <- tig[asum]

asum[,len_meters:=NULL]
fn <- c('roadID',names(asum)[grepl("FF",names(asum))])
ff <- asum[,(fn),with=F]
setkey(ff,roadID)
ff <- tig[ff]
ff <- ff[,c('st',(fn)),with=FALSE]
mrf <- melt.data.table(ff,id.vars=c('roadID','st'),variable.name='year',variable.factor=FALSE,value.name='kgFFco2')
mrf[,year:=as.numeric(gsub('kgFFco2_','',year))]
setkey(mrf,roadID,year)
setkey(mrf,roadID)
mrf <- tig[mrf]
mrf <- mrf[,.(roadID,st,fips,tfcl,year,kgFFco2)]
mrf[,fsum:=sum(kgFFco2),by=list(year,st,fips,tfcl)]
mrf[,fsh:=kgFFco2/fsum]
saveRDS(mrf,'Output/onroad_long_kgco2_v1.rds')
