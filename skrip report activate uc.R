function_report_activate <- function(supplier, dir = "C:/Users/pupupar/Documents/Import Activate ke SQL Sementara")
{
setwd(dir)
files = list.files(pattern="*.csv")
#kalau ada case unmatched column name, run satu2 ya, skipnya = 1
myfiles = do.call(rbind, lapply(files, function(x) read.csv(x,  header = FALSE, 
			 sep = ";", stringsAsFactors=FALSE,skip=0)))
myfiles$Kode = substr(myfiles[,1],1,2)
myfiles[,2:3] <- lapply(myfiles[,2:3], as.factor)
myfiles$Tanggal_Aktivasi = format(as.Date(substr(files[[1]],12,19),"%Y%m%d"),format = "%d/%m/%Y")
myfiles <- myfiles[!duplicated(myfiles["uniquecode"]),]
myfiles$Supplier <- Supplier
myfiles <- myfiles[order(myfiles$Kode,myfiles$Tanggal_Aktivasi),]

for ( i in 1:ceiling(nrow(myfiles)/1000000) ){
if( 1000000*i <= nrow(myfiles) ){
k = 1000000 * i
}
else { 
k = nrow(myfiles)
}
j = 1 + 1000000 *(i-1)
x = myfiles [j:k,]
tgl = substr(files[[1]],12,19)
write.table(myfiles[j:k,],
file=paste(dir,"/","Report","_","Pura","_",
substr(files[[1]],12,19),"_",paste(0,i,sep=""),".csv",sep=""),row.names=FALSE)
}
print(summary(myfiles))
print(table(myfiles$Kode))
}

function_report_activate("Daiprint")

#kalau ada case uc yang di activate kurang atau lebih dan pengen ngecek

#pilih kode unik yang kurangnya
uc_kurang <- myfiles[myfiles$Kode == "kodenya",]
selected <- c("DM","FA","YK")
uc_kurang <- myfiles[myfiles$Kode %in% selected,]

#save hasilnya
for ( i in 1:ceiling(nrow(myfiles)/1000000) ){
if( 1000000*i <= nrow(myfiles) ){
k = 1000000 * i
}
else { 
k = nrow(myfiles)
}
j = 1 + 1000000 *(i-1)
x = myfiles [j:k,]
tgl = substr(files[[1]],12,19)
write.table(myfiles[j:k,],
file=paste(dir,"/","Report","_","Pura","_",
substr(files[[1]],12,19),"_",paste(0,i,sep=""),".csv",sep=""),row.names=FALSE)
}
write.table(uc_kurang, file=paste(directory,"/kurang_uc.csv",sep=""),
row.names=FALSE,col.names=FALSE)

#ubah nama column
names(myfiles)[1] <- "uniquecode"
names(myfiles)[2] <- "status"

#cek yang invalid
selected <- c("DM","FA","YK")
uc_invalid <- myfiles[myfiles$status %in% selected,c(1,3)]
write.table(uc_invalid, file=paste(dir,"/uc_invalid.csv",sep=""),
row.names=FALSE,col.names=TRUE,quote=TRUE)

