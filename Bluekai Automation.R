
# Defining Paths using readline prompts
INPUT_FILEPATH = readline(prompt = "Enter the path of input data file with '/' delimited only: ")
FILE_NAME = readline(prompt = 'Enter the input file name in .csv format: ')
setwd(readline(prompt = "Enter the path to store the files: "))
KEY_VALUE = readline(prompt = "Enter the key value pair: ")

#Defining paths and file names using prompts
FILE_NAME_CSV = paste(paste(INPUT_FILEPATH, FILE_NAME, sep = "/"),'.csv', sep="")
FILE_NAME_TSV = (paste(FILE_NAME,'.tsv', sep=""))
FILE_NAME_GZ   = (paste(FILE_NAME,'.gz', sep=""))
FILE_NAME_TRIGGER = (paste(FILE_NAME,'.TRIGGER',sep=""))             



data = read.csv(FILE_NAME_CSV)
data = data.frame(data)


#Summary statistics
summary(data)
names(data)
head(data)


#rename 1st col with COOKIE_ID, clearing extra columns
names(data)[1]='COOKIE_ID'
data=data['COOKIE_ID']
head(data)

#Insert 2nd column with taxonomy information
data[2] = KEY_VALUE
names(data)[2] = 'CODE'
head(data)


#Write data to a tsv file (If row.names skipped, it gives row numbers) without col names without quotes
write.table(data, file=FILE_NAME_TSV, 
            sep='\t', row.names = FALSE, col.names = FALSE, quote = FALSE)


#gzipping file
library(R.utils)
gzip(FILE_NAME_TSV, FILE_NAME_GZ)



#Process to create trigger file
#MD5SUM - md5 check sum
library(tools)
a=md5sum(FILE_NAME_GZ)
typeof(a)
checksum=print(a[[1]], quote=FALSE)

# File size of .gz
file_size=file.size(FILE_NAME_GZ)
file_size


#Formatting trigger file
x=paste("FILE=",FILE_NAME_GZ,sep="")
y=paste("SIZE=",file_size,sep="")
z=paste("MD5SUM=",checksum,sep="")
triggerfile=paste(c(x,y,z),sep="\n")
triggerfile

#Creating trigger file name (paste as concatenate and print to remove quotes)
DATA_TRIG=print(FILE_NAME_TRIGGER,quote=F)
DATA_TRIG

write.table(triggerfile,DATA_TRIG, row.names = F, col.names = F, quote = F)
