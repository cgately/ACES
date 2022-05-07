fl <- list.files(pattern='xlsx')
fx <- function(x){
  as.data.table(read_excel(x))
}
fs <- lapply(fl,fx)
nr <- rbindlist(fs,fill=T)
nr[,ft:=fuelTypeID]
nr[is.na(ft),ft:=fueltypeID]
nr[,fueltypeID:=NULL][,fuelTypeID:=NULL]
setnames(nr,'ft','fuelTypeID')

nr <- nr[,lapply(.SD,sum,na.rm=T),by=list(monthID,dayID,countyID,fuelTypeID,sectorID),.SDcols='emissionQuant']
setkey(nr,fuelTypeID)

# Add a description of fuel type -- Source = MOVES User Guide 2014a
fuels <- data.table(fuel = c('gasoline','CNG','LPG','diesel','marine_diesel'), fuelTypeID=c(1,3,4,23,24))
setkey(fuels,fuelTypeID)
nr <- fuels[nr]

sect <- readRDS('../moves_sectorID_descriptions.rds')
setkey(sect,sectorID)
setkey(nr,sectorID)
nr <- sect[nr]
nr[,kgco2:=emissionQuant/1000]
nr[,kgBIOco2:=0][fuel=='gasoline',kgBIOco2:=0.1 * kgco2]
nr[,kgFFco2:=kgco2-kgBIOco2]
nr[,year:=2017]
setnames(nr,'monthID','mon')
nr[,st:=as.numeric(substr(sprintf("%05d",countyID),1,2))]
setnames(nr,'countyID','fips')
nr <- nr[,lapply(.SD,sum,na.rm=T),by=list(year,st,fips,mon,dayID,description),.SDcols=c('kgBIOco2','kgFFco2')]

saveRDS(nr,paste0('nonroad_moves_kgco2_daily.rds'))

times <- fread(paste0('../times',yr,'.csv'))
times[,dayID:=5][dow %in% c(1,7),dayID:=2]
setkey(times,mon,dayID)
setkey(nr,mon,dayID)

nrt <- merge(nr,times,all=T,allow.cartesian=T,key=c('mon','dayID'))
nrt[,kgBIOco2:=kgBIOco2/24]
nrt[,kgFFco2:=kgFFco2/24]
nrt <- nrt[,.(year,datetime,chr,st,fips,description,kgBIOco2,kgFFco2)]
