#vandana work
setwd("/Users/kartikeya kirar/Desktop/vandana/")

packages <- c("readxl","rlist")
for (p in packages){
  if (!require(p, character.only = TRUE))
  {
    install.packages(p, repos = "http://cran.us.r-project.org")
  }
}
for (p in packages){
  library(p, character.only = TRUE)
}


list_dir<-list.files(path = "rawData",recursive = T,full.names = T,pattern = "xlsx")
complete_dt<-list()
k<-1
for(file_name in list_dir){
dt1<-read_excel(file_name)
dt1<-data.frame(dt1,stringsAsFactors = F)
dt1<-dt1[rowSums(is.na(dt1)) != ncol(dt1),]
#dt2<-as.data.frame(dt2)

#split_index<-as.data.frame(apply(dt1,2,function(x){x=="From Date"}),stringsAsFactors = F)
split(dt1, cumsum(1:nrow(dt1) %in% which(dt1[,1]=="From Date")))->split_index
dt_len<-length(split_index)


info<-split_index[[1]][c(4:6),c(1:2)]
info<-data.frame(t(info),row.names =NULL,stringsAsFactors = F)
colnames(info)<-info[1,]
info<-info[-1,]
dtt<-list()
#dttt<-list()
for(i in 1:(dt_len-1)){
  ind<- which(split_index[[i+1]][,1]=='From Date')
  ed<- which(grepl("00:00",as.data.frame(split_index[[i+1]][,1])[,1])[-1]==F)[1]
  if(is.na(ed)){
    ed<-nrow(split_index[[i+1]])
    
  }
 dt<- split_index[[i+1]][ind:ed,]
 colnames(dt)<-dt[1,]
 dt<-dt[-1,]
 dates<-dt[,1:2]
 dtt[[i]]<-dt[,-c(1:2)]
 
}

list.cbind(dtt)->consolidate
cbind(dates,consolidate)->consolidate
cbind(consolidate,info)->consolidate

nam<-gsub("/","_",file_name,perl = T)
nam<-gsub(" ","_",nam,perl = T)
nam<-gsub(".xlsx","",nam,perl = T)
write.csv(consolidate,file = paste0("data/",nam,".csv"))

######################################################
#calculation sheet
indC<-c("From Date" ,  "To Date" , "CO" ,"NOx",  "Ozone"  ,     "PM2.5", "SO2" , "State"    ,   "City"     ,   "Station")
rt<-indC[!indC %in% colnames(consolidate)]
if(length(rt)!=0){
  consolidate[,rt]<-NA
}
subindex<-subset(consolidate,select = c("From Date" ,  "To Date" , "CO" ,"NOx",  "Ozone"  ,     "PM2.5", "SO2" , "State"    ,   "City"     ,   "Station"))

pm25Fun<-function(value){value<-as.numeric(value);ifelse(is.na(value),0,ifelse(value<=30,value*50/30,ifelse((value>30 & value<=60),50+(value-30)*50/30,ifelse((value>60 & value<=90),100+(value-60)*100/30,ifelse((value>90 & value<=120),200+(value-90)*(100/30),ifelse((value>120 & value<=250),300+(value-120)*(100/130),ifelse(value>250,400+(value-250)*(100/130))))))))}
subindex$PM2.5<-unlist(lapply(subindex$PM2.5,pm25Fun))

so2Fun<-function(value){value<-as.numeric(value);ifelse(is.na(value),0,ifelse(value<=40,value*50/40,ifelse((value>40 & value<=80),50+(value-40)*50/40,ifelse((value>80 & value<=380),100+(value-80)*100/300,ifelse((value>380 & value<=800),200+(value-380)*(100/420),ifelse((value>800 & value<=1600),300+(value-800)*(100/800),ifelse(value>1600,400+(value-1600)*(100/800))))))))}
subindex$SO2<-unlist(lapply(subindex$SO2,so2Fun))

noFun<-function(value){value<-as.numeric(value);ifelse(is.na(value),0,ifelse(value<=40,value*50/40,ifelse((value>40 & value<=80),50+(value-40)*50/40,ifelse((value>80 & value<=180),100+(value-80)*100/100,ifelse((value>180 & value<=280),200+(value-180)*(100/100),ifelse((value>280 & value<=400),300+(value-280)*(100/120),ifelse(value>400,400+(value-400)*(100/120))))))))}
subindex$NOx<-unlist(lapply(subindex$NOx,noFun))

coFun<-function(value){value<-as.numeric(value);ifelse(is.na(value),0,ifelse(value<=1,value*50/1,ifelse((value>1 & value<=2),50+(value-1)*50/1,ifelse((value>2 & value<=10),100+(value-2)*100/8,ifelse((value>10 & value<=17),200+(value-10)*(100/7),ifelse((value>17 & value<=34),300+(value-17)*(100/17),ifelse(value>34,400+(value-34)*(100/17))))))))}
subindex$CO<-unlist(lapply(subindex$CO,coFun))

o3Fun<-function(value){value<-as.numeric(value);ifelse(is.na(value),0,ifelse(value<=50,value*50/50,ifelse((value>50 & value<=100),50+(value-50)*50/50,ifelse((value>100 & value<=168),100+(value-100)*100/68,ifelse((value>168 & value<=208),200+(value-168)*(100/40),ifelse((value>208 & value<=748),300+(value-208)*(100/539),ifelse(value>748,400+(value-400)*(100/539))))))))}
subindex$Ozone<-unlist(lapply(subindex$Ozone,o3Fun))

check<-function(value){ifelse((is.na(value) | value<=0),0,1)}

subindex$Ozone_C<-unlist(lapply(subindex$Ozone,check))
subindex$CO_C<-unlist(lapply(subindex$CO,check))
subindex$NOx_C<-unlist(lapply(subindex$NOx,check))
subindex$PM2.5_C<-unlist(lapply(subindex$PM2.5,check))
subindex$SO2_C<-unlist(lapply(subindex$SO2,check))

aqi<-list()
for(i in 1:nrow(subindex)){
aqi[i]<-ifelse(subindex$PM2.5_C[i] & (subindex$PM2.5_C[i]+subindex$Ozone_C[i]+subindex$CO_C[i]+subindex$NOx_C[i]+subindex$SO2_C[i])>=3,max((subindex$PM2.5[i]+subindex$Ozone[i]+subindex$CO[i]+subindex$NOx[i]+subindex$SO2[i])),"Atleast 3 inputs*")
}

subindex$AQI<-unlist(aqi)
complete_dt[[k]]<-subindex
k<-k+1
write.csv(consolidate,file = paste0("data/",nam,"_INDEXED.csv"))

}

dt<-plyr::ldply(complete_dt,data.frame)
save(dt,"consolidate.rda")
