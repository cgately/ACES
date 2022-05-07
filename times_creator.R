require(data.table)

times <- data.table('year'=rep(2017,8760),'mon'=sort(rep(c(rep(1,31),rep(2,28),rep(3,31),rep(4,30),rep(5,31),rep(6,30),rep(7,31),rep(8,31),rep(9,30),rep(10,31),rep(11,30),rep(12,31)),24)),'day'=c(sort(rep(seq(1,31),24)),sort(rep(seq(1,28),24)),sort(rep(seq(1,31),24)),sort(rep(seq(1,30),24)),sort(rep(seq(1,31),24)),sort(rep(seq(1,30),24)),sort(rep(seq(1,31),24)),sort(rep(seq(1,31),24)),sort(rep(seq(1,30),24)),sort(rep(seq(1,31),24)),sort(rep(seq(1,30),24)),sort(rep(seq(1,31),24))),'dow'=head(rep(c(rep(6,24),rep(7,24),rep(1,24),rep(2,24),rep(3,24),rep(4,24),rep(5,24)),53),8760),'hour'=rep(seq(0,23),365),'chr'=seq(1:8760),'cday'=sort(rep(seq(1,365),24)))
times[,date:=paste0(year,sprintf("%02d",mon),sprintf("%02d",day))]
times[,datetime:=paste0(year,sprintf("%02d",mon),sprintf("%02d",day),sprintf("%02d",hour))]

write.csv(times,'/projectnb/buultra/gately/times2017.csv',row.names=F)
