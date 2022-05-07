library(data.table)

for (yr in 2012:2017){
  og <- fread('nonpoint_industrial_kgco2_2012_2017.csv')
  og <- og[year==yr]
setkey(og,fips)

il <- fread('Smoke_spatial_surrogates/CONUS4_2014_30apr2019/ind_land.csv')
setnames(il,c('srgdesc','fips','idx','idy','frac'))
setkey(il,fips)
il[,grid_id:=paste(idx,idy,sep='_')]
setkey(il,fips)

ind <- og[il]
ind <- ind[!is.na(kgFFco2)]
ind[,kgFFco2:=frac*kgFFco2][,kgBIOco2:=frac*kgBIOco2]
og4km <- ind[,lapply(.SD,sum),by=list(grid_id),.SDcols=c('kgBIOco2','kgFFco2')]
setkey(og4km,grid_id)

g4 <- fread('SMOKE_4km_1km_inter.csv')
setkey(g4,grid_id)
g4 <- og4km[g4]
g4[is.na(kgFFco2),kgFFco2:=0][is.na(kgBIOco2),kgBIOco2:=0]
g4[,kgFFco2:=kgFFco2/16e6*Shape_Area][,kgBIOco2:=kgBIOco2/16e6*Shape_Area]
g1 <- g4[,lapply(.SD,sum),by=list(cellid),.SDcols=c('kgBIOco2','kgFFco2')]
g1[,year:=yr]
saveRDS(g1,paste0('nonpoint_kgco2_1km_',yr,'.rds'))

}
