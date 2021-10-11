function_import_uc <- function(batch,tanggal,supplier,
dir = "C:/Users/pupupar/Documents/Import Generate ke SQL Sementara")
{
setwd(dir)
files = list.files(pattern="*.csv")
myfiles = do.call(rbind, lapply(files, function(x) read.csv(x,  header = FALSE, 
			 sep = "~", stringsAsFactors=FALSE)))
names(myfiles)[1]<-"Kode_Unik"
myfiles$Batch <- batch
myfiles$Tanggal_Batch <- format(as.Date(tanggal, "%d/%m/%Y"),format="%d/%m/%Y")
myfiles$Supplier <- supplier
myfiles[c("Tanggal_Aktivasi","Tanggal_Submit","Member_id")] <- NA
myfiles <- myfiles [c(4,2,3,1,5:ncol(myfiles))]
for ( i in 1:ceiling(nrow(myfiles)/1000000) ){
if( 1000000*i <= nrow(myfiles) ){
k = 1000000 * i
}
else { 
k = nrow(myfiles)
}
j = 1 + 1000000 *(i-1)
write.table(myfiles[j:k,],
file=paste(dir,"/",
myfiles[1,2],"_",paste(0,i,sep=""),".csv",sep=""),row.names=FALSE,na="")
}
}

# cara input ke function
# 1. "nama_batch"
# 2. "tanggal format excel"
# 3. "nama_supplier"


function_import_uc("21Juni2019_Daiprint","21/06/2019","Daiprint")