library(xml2)
library(dplyr)
library(sf)
library(ggplot2)
library(httr)
library(TDX)
library(data.table)
library(stringr)
library(utils)
library(filesstrings)

windowsFonts(A=windowsFont("標楷體"))
windowsFonts(B=windowsFont("Times New Roman"))

get_ptx_data=function (app_id, app_key, url){
  Sys.setlocale("LC_ALL", "C")
  xdate <- format(as.POSIXlt(Sys.time(), tz = "GMT"), "%a, %d %b %Y %H:%M:%S GMT")
  sig <- hmac_sha1(app_key, paste("x-date:", xdate))
  authorization <- paste0("hmac username=\"", app_id, "\", ", 
                          "algorithm=\"hmac-sha1\", ", "headers=\"x-date\", ", 
                          "signature=\"", sig, "/\"", sep = "")
  auth_header <- c(Authorization = authorization, `x-date` = as.character(xdate))
  dat <- GET(url, config = httr::config(ssl_verifypeer = 0L), 
             add_headers(.headers = auth_header))
  # print(http_status(dat)$message)
  Sys.setlocale(category = "LC_ALL", locale = "cht")
  return(content(dat))
}

app_id="app_id"
app_key="app_key"

client_id="client_id"
client_secret="client_secret"
access_token=get_token(client_id, client_secret)


# YouBike歷史資料下載
history_date=as.Date(as.Date("2021/6/1"):as.Date("2022/2/28"), origin="1970/1/1")

# date_count=data.frame()
# for (i in c(1:length(history_date))){
#   url=paste0("https://ptx.transportdata.tw:443/MOTCAdvanced/v2/Historical/Bike/Availability/Taipei?Dates=", history_date[i], "&%24format=JSONL&Meta=true")
#   check_meta=get_ptx_data(app_id, app_key, url)
#   Sys.sleep(1)
#   date_count=rbind(date_count, data.frame(Date=history_date[i], Count=check_meta))
# }

# date_count$Count=as.numeric(substr(date_count$Count, regexpr("Count", date_count$Count)+7, regexpr("SizeBytes", date_count$Count)-3))

# date_count=read.csv("./date_count.csv")

temp=data.frame(c())
for (i in c(which(date_count$Date=="2021-06-17"):length(history_date))){
  while(nrow(temp)!=date_count$Count[i]){
    url=paste0("https://ptx.transportdata.tw:443/MOTCAdvanced/v2/Historical/Bike/Availability/Taipei?Dates=", history_date[i], "&%24format=CSV")
    temp=get_ptx_data(app_id, app_key, url)
    print(paste0("Parse: ", nrow(temp), " / Database: ", date_count$Count[i]))
  }
  
  write.csv(temp, paste0("./YouBike Availability/Availability_", history_date[i], ".csv"), row.names=F)
  temp=data.frame()
  print(history_date[i])
  Sys.sleep(1)
}



# youbike2.0_station=data.frame()
for (i in as.character(as.IDate(c(as.IDate("2021-05-01"):as.IDate("2022-03-31"))))){
  url=paste0(url="https://ptx.transportdata.tw:443/MOTCAdvanced/v2/Historical/Bike/Station/Date/", i, "/City/Taipei?%24format=CSV")
  temp=get_ptx_data(app_id, app_key, url)
  if (length(temp)!=0){
    youbike2.0_station=rbind(youbike2.0_station, cbind(DATE=i, temp))
    cat(i)
  }else{
    cat(paste0(i, " cannot download\n"))
  }
  Sys.sleep(1)
}

write.csv(youbike2.0_station, "C:/Users/ASUS/Desktop/Travel Demand Analysis/Term Project/Analysis/Data/YouBike_Station.csv", row.names=F)





# dir_files=dir("C:/Users/ASUS/Desktop/Travel Demand Analysis/Term Project/Analysis/Data/business_data", full.names=T)
# business_data=do.call(rbind, lapply(dir_files, fread, encoding="UTF-8"))
# business_data=select(business_data, c(1,2,4,5,6))
# colnames(business_data)=c("ID","NAME","CAPITAL","DATE","ADDRESS")
# business_data=filter(business_data, CAPITAL>0, CAPITAL!="null")
# Geocoding(access_token, business_data$ADDRESS[1:100])


write.csv(business_data, "./business_data.csv", row.names=F)



dir_files=dir("D:/Taipei Smart Card Data/Data/youbike data collection/YouBike1", full.names=T)
ubike1=do.call(rbind, lapply(dir_files, fread, encoding="UTF-8"))
ubike1$YEAR=year(ubike1$V1)
ubike1$MONTH=month(ubike1$V1)
ubike1_total=group_by(ubike1, YEAR, MONTH)%>%
  summarise(count=n())
rm(ubike1)


dir_files=dir("D:/Taipei Smart Card Data/Data/youbike data collection/YouBike2", full.names=T)
ubike2=do.call(rbind, lapply(dir_files, fread, encoding="UTF-8"))
ubike2$YEAR=year(ubike2$rent_time)
ubike2$MONTH=month(ubike2$rent_time)
ubike2_total=group_by(ubike2, YEAR, MONTH)%>%
  summarise(count=n())
ubike2_count=distinct(ubike2, rent_station, YEAR, MONTH)%>%
  group_by(YEAR, MONTH)%>%
  summarise(count=n())
rm(ubike2)

temp=rbind(cbind(TYPE="YouBike 1.0", ubike1_total), cbind(TYPE="YouBike 2.0", ubike2_total))
temp$month=paste0(temp$YEAR, "-", str_pad(temp$MONTH, 2, "left", 0))
temp=filter(temp, YEAR<2022, MONTH<=11)
ubike2_count$month=paste0(ubike2_count$YEAR, "-", str_pad(ubike2_count$MONTH, 2, "left", 0))

png("./YouBike 總騎乘量.png", width=1152*2, height=709*2, res=200)
ggplot()+
  geom_bar(data=ubike2_count, aes(x=month, y=count/2), stat="identity", color=NA, fill="#D2E9FF", color=NA)+
  geom_point(data=temp, aes(x=month, y=count/10000, color=TYPE), size=2)+
  geom_line(data=temp, aes(x=month, y=count/10000, color=TYPE, group=TYPE), size=1)+
  scale_color_brewer(palette="Set1")+
  scale_y_continuous(name="總騎乘量 (萬次)", sec.axis=sec_axis(~.*2, name="YouBike 2.0 站點數"))+
  xlab("月份")+
  theme_minimal()+
  theme(legend.text=element_text(family="B", size=18, face="bold"),
        legend.title=element_blank(),
        legend.position=c(0.87, 0.95),
        axis.title.x=element_text(family="A", size=20),
        axis.title.y.left=element_text(family="A", size=20),
        axis.title.y.right=element_text(family="A", size=20, color="#005AB5", face="bold", vjust=2),
        axis.text.x=element_text(family="B", size=15, angle=45, vjust=0.5, face="bold"),
        axis.text.y.left=element_text(family="B", size=15),
        axis.text.y.right=element_text(family="B", size=15, color="#377EB8"))
dev.off()




# 自政府資料開放平臺下載 臺北市公共自行車租借紀錄資料
# YouBike 1.0 https://data.gov.tw/dataset/139301
# YouBike 2.0 https://data.gov.tw/dataset/150635

ubike1_list=fread("https://tcgbusfs.blob.core.windows.net/dotapp/youbike_ticket_opendata/YouBikeHis.csv", encoding="UTF-8")
ubike1_list$fileinfo=paste0(substr(ubike1_list$fileinfo, 1, 4), str_pad(gsub("月", "", substr(ubike1_list$fileinfo, 6, 10)), 2, "left", 0))

for (i in c(4:nrow(ubike1_list))){
  download.file(URLencode(ubike1_list$fileURL[i]), "./temp.zip")
  unzip("./temp.zip")
  move_files(paste0(ubike1_list$fileinfo[i], ".csv"), "./YouBike1")
  file.rename(from=paste0("YouBike1/", ubike1_list$fileinfo[i], ".csv"), to=paste0("YouBike1/YouBike1_", ubike1_list$fileinfo[i], ".csv"))
  file.remove("./temp.zip")
  
  print(paste0(i, " / ", nrow(ubike1_list)))
}




