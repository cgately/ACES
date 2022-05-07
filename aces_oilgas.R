
c4 <- fread('SMOKE_4km_grid.csv')
setorder(c4,-centY,centX)
c4[,idx:=rep(seq(1,1332),1008)][,idy:=sort(rep(seq(1,1008),1332))]
c4[,old_oid:=OID]
write.dbf(c4,'SMOKE_4km_ids.dbf')

prod <- fread('oilgas_production_CO2_by_cnty.csv')
prod <- prod[,c(3,9,11:13,17)]
setnames(prod,c('fips','scc','process','poll','value','unit'))
prod <- prod[poll=='CO2' & unit!='']

expl <- fread('oilgas_exploration_CO2_by_cnty.csv')
expl <- expl[,c(3,9,11:13,17)]
setnames(expl,c('fips','scc','process','poll','value','unit'))
expl <- expl[poll=='CO2' & unit!='']

og <- rbind(prod,expl)
og[unit=='G',value:=value/1000]
og[unit=='TON',value:=value*1000]
og[unit=='LB',value:=value*0.453592]
og[,unit:=NULL][,scc:=as.character(scc)]
setnames(og,'value','kgco2')
setkey(og,scc)

scodes <- fread('np_oilgas_scc_codes.csv')
scodes[,scc:=as.character(scc)]
setkey(scodes,scc)
og <- scodes[og]
fwrite(og,'all_oilgas_kgco2_cnty.csv')
og <- og[,lapply(.SD,sum),by=list(fips),.SDcols='kgco2']
setkey(og,fips)
fwrite(og,'all_oilgas_kgco2_cnty_sum.csv')
write.dbf(og,'all_oilgas_kgco2_cnty_sum.dbf')

wells <- fread('Smoke_spatial_surrogates/CONUS4_2014_30apr2019/all_oil_and_gas_wells.txt')
setnames(wells,c('srgdesc','fips','idx','idy','frac'))
wells[,grid_id:=paste(idx,idy,sep='_')]
setkey(wells,fips)

wells <- og[wells]
wells <- wells[!is.na(kgco2)]
wells[,kgco2:=frac*kgco2]
og4km <- wells[,lapply(.SD,sum),by=list(grid_id),.SDcols='kgco2']
setkey(og4km,grid_id)

g4 <- fread('E:/ACES/Grids/SMOKE_4km_1km_inter.csv')
g4 <- g4[,.(cellid,grid_id,Shape_Area)]
setkey(g4,grid_id)

g4 <- og4km[g4]
g4[is.na(kgco2),kgco2:=0]
g4[,kgco2:=kgco2/16e6*Shape_Area]
g1 <- g4[,lapply(.SD,sum),by=list(cellid),.SDcols='kgco2']
write.dbf(g1,'oilgas_kgco2_1km.dbf')
saveRDS(g1,'oilgas_kgco2_1km.rds')
