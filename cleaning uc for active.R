function_activate_cleans <- function(directory = "C:/Users/pupupar/Documents/Activate Sementara") 
{

# set folder tempat kerja
setwd(directory)

#bikin folder buat save hasil cleans
dir.create("Cleans")

# Get the files names
files = list.files(pattern="*.csv")

# First apply read.csv, then rbind,skip = 1 artinya row pertama di skip
# sep pake "~" soalnya kadang setelah ; dikasih spasi, kalau sep ="" 
# nanti kebaca jadi newrow
myfiles = do.call(rbind, lapply(files, function(x) read.csv(x,  header = FALSE, 
			 sep = "~", stringsAsFactors=FALSE, skip=1 )))

#remove yang afkir
myfiles <- subset(myfiles, nchar(myfiles[,1]) < 16)

#remove 
myfiles[] <- lapply(myfiles, gsub, pattern=';', replacement='')

#apply trim 
myfiles[] <- lapply(myfiles, trimws, which="both")

#hitung banyaknya jumlah character
myfiles[,2] = nchar(myfiles[,1])

#cleansing terakhir untuk mastiin kodenya 11 angka
myfiles = subset(myfiles, V2 = 11, select = V1)

#print kode
myfiles[,2] = as.factor(substr(myfiles[,1],1,2))

#remove duplicate
myfiles_uniq <- myfiles[!duplicated(myfiles["V1"]),]

#bagi bagi row jadi 20k per file untuk diupload ke snr

for ( i in 1:ceiling(nrow(myfiles)/20000) ){
if( 20000*i <= nrow(myfiles) ){
k = 20000 * i
}
else { 
k = nrow(myfiles)
}
j = 1 + 20000 *(i-1)
x = myfiles [j:k,]
write.table(myfiles[j:k,1], file=paste(directory,"/Cleans/ActivateUC","_",i+100,".csv",sep=""),
row.names=FALSE,col.names=FALSE)
}
print (paste("Jumlah Total Unicode yang baik", nrow(myfiles)))
print (summary(myfiles))
print (table(myfiles$V2))
print (summary(myfiles_uniq))
print (table(myfiles_uniq$V2))
}

#input function adalah directory folder, ganti "\" dengan "/" pake Ctrl+H
function_activate_cleans()

#pilih kode unik yang kurangnya
uc_kurang <- myfiles_uniq[myfiles$Kode == "kodenya",]
selected <- c("DM","FA","YK")
uc_kurang01 <- myfiles_uniq[myfiles_uniq$V2 %in% selected,]

#save hasilnya
write.table(uc_kurang01, file=paste(directory,"/kurang_uc_asli.csv",sep=""),
row.names=FALSE,col.names=TRUE)

