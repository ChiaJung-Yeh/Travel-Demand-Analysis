library(xml2)
library(dplyr)
library(sf)
library(ggplot2)
library(httr)
library(data.table)
library(tmap)
library(TWspdata)
library(betareg)
library(TDX)
library(lme4)
library(dotwhisker)
library(broom.mixed)
library(arm)
library(sjPlot)
library(texreg)
library(GWmodel)
library(ggnewscale)
library(ggrepel)
library(stargazer)
library(blme)
library(cowplot)
library(ggtext)

windowsFonts(A=windowsFont("標楷體"))
windowsFonts(B=windowsFont("Times New Roman"))

tmap_mode("view")

taipei=filter(taiwan_town, COUNTYNAME=="臺北市")%>%
  st_transform(crs=3826)
taipei_vill=filter(taiwan_village, COUNTYNAME=="臺北市")%>%
  st_transform(crs=3826)



#---YouBike 1.0 站點資料---#
YouBike_Station1=fread("C:/Users/ASUS/Desktop/Travel Demand Analysis/Term Project/Analysis/Data/YouBike_Station.csv")%>%
  filter(grepl("YouBike1.0_", StationNameZh_tw), DATE=="2021-11-01")%>%
  rename(StationName=StationNameZh_tw)%>%
  dplyr::select(StationUID, StationName, PositionLat, PositionLon, BikesCapacity)%>%
  mutate(geometry=st_as_sfc(paste0("POINT(", PositionLon, " ", PositionLat, ")")))%>%
  st_sf(crs=4326)%>%
  st_transform(crs=3826)
YouBike_Station1=cbind(YouBike_Station1, st_coordinates(YouBike_Station1))%>%
  dplyr::select(-PositionLat, -PositionLon)

YouBike_Station1$StationName=gsub("YouBike1.0_", "", YouBike_Station1$StationName)
YouBike_Station1$StationName[YouBike_Station1$StationUID=="TPE0185"]="瑠公公園"

# 注意：瑠公公園已撤站，11月份以後的起訖對資料中並無該站


#---社經變數貼附---#
# 擷取 YouBike 1.0 社經變數
social_data=fread("C:/Users/ASUS/Desktop/Travel Demand Analysis/Term Project/Analysis/Data/social_data.csv", encoding="UTF-8")
social_data$VILLCODE=as.character(social_data$VILLCODE)
taipei_vill=left_join(taipei_vill, social_data)

# social economic factor
YouBike_Station1$POP_DENSITY=st_interpolate_aw(taipei_vill["POP_DENSITY"], st_buffer(YouBike_Station1, 300), extensive=F)$POP_DENSITY
YouBike_Station1$IC_MED=st_interpolate_aw(taipei_vill["IC_MED"], st_buffer(YouBike_Station1, 300), extensive=F)$IC_MED
YouBike_Station1$District=st_intersection(YouBike_Station1, taipei)$TOWNNAME

# MRT
mrt_station=Rail_Station(app_id, app_key, "TRTC", dtype="sf")%>%
  st_transform(crs=3826)
ubike_mrt=st_distance(YouBike_Station1, mrt_station[st_nearest_feature(YouBike_Station1, mrt_station),], by_element=T)
YouBike_Station1$MRT_DIST=as.numeric(ubike_mrt)

# shop
shop=fread("C:/Users/ASUS/Desktop/Travel Demand Analysis/Term Project/Analysis/Data/POI/shop.csv")%>%
  mutate(geometry=st_as_sfc(paste0("POINT(", lon, " ", lat, ")")))%>%
  st_sf(crs=4326)%>%
  st_transform(crs=3826)
shop_num=st_intersection(st_buffer(YouBike_Station1, 300), shop)%>%
  st_drop_geometry()%>%
  group_by(StationUID)%>%
  summarise(SHOP_NUM=n())
YouBike_Station1=left_join(YouBike_Station1, shop_num)
YouBike_Station1$SHOP_NUM[is.na(YouBike_Station1$SHOP_NUM)]=0

# school
university=filter(taiwan_school, grepl(paste0("大學|醫學院|警察專科學校|戲曲學院"), name), !grepl(paste0("附設國小|附設國中|附小|附中"), name), grepl("臺北市", address))%>%
  mutate(geometry=st_as_sfc(paste0("POINT(", lon, " ", lat, ")")))%>%
  st_sf(crs=4326)%>%
  st_transform(crs=3826)
high_school=filter(taiwan_school, grepl(paste0("高中|高級中學|實驗中學|女中|中學"), name), !grepl(paste0("附設國小|附設國中|附小"), name), grepl("臺北市", address))%>%
  mutate(geometry=st_as_sfc(paste0("POINT(", lon, " ", lat, ")")))%>%
  st_sf(crs=4326)%>%
  st_transform(crs=3826)

temp=st_intersection(st_buffer(YouBike_Station1, 300), university)
YouBike_Station1$university=ifelse(YouBike_Station1$StationUID %in% temp$StationUID,  1, 0)

temp=st_intersection(st_buffer(YouBike_Station1, 300), high_school)
YouBike_Station1$high_school=ifelse(YouBike_Station1$StationUID %in% temp$StationUID,  1, 0)

# park
# park=fread("C:/Users/ASUS/Desktop/Travel Demand Analysis/Term Project/Analysis/Data/POI/leisure.csv")%>%
#   filter(leisure=="park", grepl("公園|綠地", name))%>%
#   mutate(geometry=st_as_sfc(paste0("POINT(", lon, " ", lat, ")")))%>%
#   st_sf(crs=4326)%>%
#   st_transform(crs=3826)
# temp=st_intersection(st_buffer(YouBike_Station1, 300), park)
# YouBike_Station1$park=ifelse(YouBike_Station1$StationUID %in% temp$StationUID,  1, 0)

park=read_sf("C:/Users/ASUS/Desktop/Travel Demand Analysis/Term Project/Analysis/Data/POI/PARK.geojson")%>%
  st_transform(crs=3826)
temp=st_intersection(YouBike_Station1, park)%>%
  st_drop_geometry()%>%
  group_by(StationUID)%>%
  summarise(park_area=sum(area)/1000000)
YouBike_Station1=left_join(YouBike_Station1, temp)
YouBike_Station1$park_area=ifelse(is.na(YouBike_Station1$park_area), 0, YouBike_Station1$park_area)


# bike route
bike_route=Bike_Shape(app_id, app_key, "Taipei", dtype="sf")%>%
  st_transform(crs=3826)
temp=st_intersection(st_buffer(YouBike_Station1, 300), bike_route)
temp=cbind(temp, bike_route=as.numeric(st_length(temp)))%>%
  st_drop_geometry()%>%
  group_by(StationUID)%>%
  summarise(bike_route=sum(bike_route))
YouBike_Station1=left_join(YouBike_Station1, temp)
YouBike_Station1$bike_route[is.na(YouBike_Station1$bike_route)]=0

# parking area
url="https://tdx.transportdata.tw/api/basic/v1/Parking/OffStreet/CarPark/City/Taipei?%24format=XML"
x = GET(url, add_headers(Accept = "application/+json", Authorization = paste("Bearer", access_token)))
x = content(x)
parking_area=data.frame(CarParkID=xml_text(xml_find_all(x, xpath = ".//d1:CarParkID")),
                        CarParkName=xml_text(xml_find_all(x, xpath = ".//d1:CarParkName//d1:Zh_tw")),
                        PositionLat=xml_text(xml_find_all(x, xpath = ".//d1:CarParkPosition//d1:PositionLat")),
                        PositionLon=xml_text(xml_find_all(x, xpath = ".//d1:CarParkPosition//d1:PositionLon")))
url="https://tdx.transportdata.tw/api/basic/v1/Parking/OffStreet/ParkingSpace/City/Taipei?%24format=XML"
x = GET(url, add_headers(Accept = "application/+json", Authorization = paste("Bearer", access_token)))
x = content(x)
temp=data.frame(CarParkID=xml_text(xml_find_all(x, xpath = ".//d1:CarParkID")),
                TotalSpaces=xml_text(xml_find_all(x, xpath = ".//d1:TotalSpaces")))
parking_area=left_join(parking_area, temp)%>%
  mutate(geometry=st_as_sfc(paste0("POINT(", PositionLon, " ", PositionLat, ")")))%>%
  st_sf(crs=4326)%>%
  st_transform(crs=3826)
temp=st_intersection(st_buffer(YouBike_Station1, 300), parking_area)%>%
  st_drop_geometry()%>%
  group_by(StationUID)%>%
  summarise(TotalSpaces=sum(as.numeric(TotalSpaces)))
YouBike_Station1=left_join(YouBike_Station1, temp)
YouBike_Station1$TotalSpaces[is.na(YouBike_Station1$TotalSpaces)]=0

# 汽機車持有率、駕照數
car_data=fread("C:/Users/ASUS/Desktop/Travel Demand Analysis/Term Project/Analysis/Data/car_data.csv")%>%
  arrange(District)
temp=group_by(filter(social_data, COUNTYNAME=="臺北市"), TOWNNAME)%>%
  summarise(POP=sum(P_CNT))%>%
  rename(District=TOWNNAME)%>%
  arrange(District)
car_data$car_license=as.numeric(car_data$car_license)/temp$POP
car_data$scooter_license=as.numeric(car_data$scooter_license)/temp$POP
car_data$car_number=as.numeric(car_data$car_number)/temp$POP
car_data$scooter_number=as.numeric(car_data$scooter_number)/temp$POP

YouBike_Station1=left_join(YouBike_Station1, car_data)

# sidewalk
TP_SIDEWALK=read_sf("C:/Users/ASUS/Desktop/Travel Demand Analysis/Term Project/Analysis/Data/TP_SIDEWALK/TP_SIDEWALK.shp")
temp=st_intersection(st_buffer(YouBike_Station1, 300), TP_SIDEWALK)
temp$cover_area=as.numeric(st_area(temp))
temp$cover_area=ifelse(temp$cover_area>temp$Shap_AR, temp$Shap_AR, temp$cover_area)
temp$SidewalkLength=temp$SW_LENG*temp$cover_area/temp$Shap_AR
temp=group_by(temp, StationUID)%>%
  st_drop_geometry()%>%
  summarise(SidewalkLength=sum(SidewalkLength))
YouBike_Station1=left_join(YouBike_Station1, temp)
YouBike_Station1$SidewalkLength[is.na(YouBike_Station1$SidewalkLength)]=0

# write.csv(st_drop_geometry(YouBike_Station1), "./YouBike_Station1.csv", row.names=F)



#---YouBike 2.0 站點資料---#
YouBike_Station2=fread("C:/Users/ASUS/Desktop/Travel Demand Analysis/Term Project/Analysis/Data/YouBike_Station.csv")%>%
  filter(grepl("YouBike2.0_", StationNameZh_tw), DATE %in% as.IDate(c(as.IDate("2021-08-01"):as.IDate("2021-11-30"))))%>%
  rename(StationName=StationNameZh_tw)

ubike2_idname=dplyr::select(YouBike_Station2, StationUID, StationName)%>%
  distinct()

ubike2_idname$StationName=gsub("YouBike2.0_", "", ubike2_idname$StationName)
ubike2_idname$StationName[ubike2_idname$StationUID=="TPE500101153"]="瑠公公園"
ubike2_idname$StationName[ubike2_idname$StationUID=="TPE500113031"]="糖廍文化園區"

# 檢查StationUID是否有配對至兩個以上的StationName (共4個StationUID)
group_by(ubike2_idname, StationUID)%>%
  summarise(count=n())%>%
  arrange(desc(count))

# 重複StationUID者，StationName以/合併
ubike2_idname_merge=group_by(ubike2_idname, StationUID)%>%
  summarise(StationName=paste(StationName, collapse="/"))

YouBike_Station2=group_by(YouBike_Station2, StationUID)%>%
  slice(DATE_START=which.min(DATE))%>%
  dplyr::select(StationUID, PositionLat, PositionLon, BikesCapacity)%>%
  left_join(ubike2_idname_merge)%>%
  mutate(geometry=st_as_sfc(paste0("POINT(", PositionLon, " ", PositionLat, ")")))%>%
  st_sf(crs=4326)%>%
  st_transform(crs=3826)

YouBike_Station2=cbind(YouBike_Station2, st_coordinates(YouBike_Station2))%>%
  dplyr::select(-PositionLat, -PositionLon)


#---社經變數貼附---#
# social economic factor
YouBike_Station2$POP_DENSITY=st_interpolate_aw(taipei_vill["POP_DENSITY"], st_buffer(YouBike_Station2, 300), extensive=F)$POP_DENSITY
YouBike_Station2$IC_MED=st_interpolate_aw(taipei_vill["IC_MED"], st_buffer(YouBike_Station2, 300), extensive=F)$IC_MED
YouBike_Station2$District=st_intersection(YouBike_Station2, taipei)$TOWNNAME

# MRT
mrt_station=Rail_Station(app_id, app_key, "TRTC", dtype="sf")%>%
  st_transform(crs=3826)
ubike_mrt=st_distance(YouBike_Station2, mrt_station[st_nearest_feature(YouBike_Station2, mrt_station),], by_element=T)
YouBike_Station2$MRT_DIST=as.numeric(ubike_mrt)

# shop
shop_num=st_intersection(st_buffer(YouBike_Station2, 300), shop)%>%
  st_drop_geometry()%>%
  group_by(StationUID)%>%
  summarise(SHOP_NUM=n())
YouBike_Station2=left_join(YouBike_Station2, shop_num)
YouBike_Station2$SHOP_NUM[is.na(YouBike_Station2$SHOP_NUM)]=0

# school
temp=st_intersection(st_buffer(YouBike_Station2, 300), university)
YouBike_Station2$university=ifelse(YouBike_Station2$StationUID %in% temp$StationUID,  1, 0)

temp=st_intersection(st_buffer(YouBike_Station2, 300), high_school)
YouBike_Station2$high_school=ifelse(YouBike_Station2$StationUID %in% temp$StationUID,  1, 0)

# park
# park=fread("C:/Users/ASUS/Desktop/Travel Demand Analysis/Term Project/Analysis/Data/POI/leisure.csv")%>%
#   filter(leisure=="park", grepl("公園|綠地", name))%>%
#   mutate(geometry=st_as_sfc(paste0("POINT(", lon, " ", lat, ")")))%>%
#   st_sf(crs=4326)%>%
#   st_transform(crs=3826)
# temp=st_intersection(st_buffer(YouBike_Station2, 300), park)
# YouBike_Station2$park=ifelse(YouBike_Station2$StationUID %in% temp$StationUID,  1, 0)

temp=st_intersection(YouBike_Station2, park)%>%
  st_drop_geometry()%>%
  group_by(StationUID)%>%
  summarise(park_area=sum(area)/1000000)
YouBike_Station2=left_join(YouBike_Station2, temp)
YouBike_Station2$park_area=ifelse(is.na(YouBike_Station2$park_area), 0, YouBike_Station2$park_area)

# bike route
bike_route=Bike_Shape(app_id, app_key, "Taipei", dtype="sf")%>%
  st_transform(crs=3826)
temp=st_intersection(st_buffer(YouBike_Station2, 300), bike_route)
temp=cbind(temp, bike_route=as.numeric(st_length(temp)))%>%
  st_drop_geometry()%>%
  group_by(StationUID)%>%
  summarise(bike_route=sum(bike_route))
YouBike_Station2=left_join(YouBike_Station2, temp)
YouBike_Station2$bike_route[is.na(YouBike_Station2$bike_route)]=0

# parking area
temp=st_intersection(st_buffer(YouBike_Station2, 300), parking_area)%>%
  st_drop_geometry()%>%
  group_by(StationUID)%>%
  summarise(TotalSpaces=sum(as.numeric(TotalSpaces)))
YouBike_Station2=left_join(YouBike_Station2, temp)
YouBike_Station2$TotalSpaces[is.na(YouBike_Station2$TotalSpaces)]=0

# 汽機車持有率、駕照數
YouBike_Station2=left_join(YouBike_Station2, car_data)

# sidewalk
temp=st_intersection(st_buffer(YouBike_Station2, 300), TP_SIDEWALK)
temp$cover_area=as.numeric(st_area(temp))
temp$cover_area=ifelse(temp$cover_area>temp$Shap_AR, temp$Shap_AR, temp$cover_area)
temp$SidewalkLength=temp$SW_LENG*temp$cover_area/temp$Shap_AR
temp=group_by(temp, StationUID)%>%
  st_drop_geometry()%>%
  summarise(SidewalkLength=sum(SidewalkLength))
YouBike_Station2=left_join(YouBike_Station2, temp)
YouBike_Station2$SidewalkLength[is.na(YouBike_Station2$SidewalkLength)]=0

# write.csv(st_drop_geometry(YouBike_Station2), "./YouBike_Station2.csv")


# tm_shape(YouBike_Station1)+
#   tm_dots(col="red")+
#   tm_shape(YouBike_Station2)+
#   tm_dots(col="blue")



temp=st_intersection(st_buffer(YouBike_Station1, 300), YouBike_Station2)%>%
  st_drop_geometry()%>%
  group_by(StationUID)%>%
  summarise(ub2_station=n())
YouBike_Station1=left_join(YouBike_Station1, temp)
YouBike_Station1$ub2_station[is.na(YouBike_Station1$ub2_station)]=0


temp=st_intersection(st_buffer(YouBike_Station2, 300), YouBike_Station1)%>%
  st_drop_geometry()%>%
  group_by(StationUID)%>%
  summarise(ub1_station=n())
YouBike_Station2=left_join(YouBike_Station2, temp)
YouBike_Station2$ub1_station[is.na(YouBike_Station2$ub1_station)]=0



#---電子票證分析---#
ubike1_card=fread("C:/Users/ASUS/Desktop/Travel Demand Analysis/Term Project/Analysis/Data/YouBike Smart Card/202111_YouBike1.0.csv", encoding="UTF-8")
ubike2_card=fread("C:/Users/ASUS/Desktop/Travel Demand Analysis/Term Project/Analysis/Data/YouBike Smart Card/202111_YouBike2.0.csv", encoding="UTF-8")
colnames(ubike1_card)=c("rent_time","rent_station","return_time","return_station","rent","infodate")

# 整理騎乘時間
ubike1_card=mutate(ubike1_card, ride_time=case_when(
  nchar(ubike1_card$rent)==7 ~ as.numeric(substr(ubike1_card$rent, 1, 1))*60+as.numeric(substr(ubike1_card$rent, 3, 4))+as.numeric(substr(ubike1_card$rent, 6, 7))/60,
  nchar(ubike1_card$rent)==8 ~ as.numeric(substr(ubike1_card$rent, 1, 2))*60+as.numeric(substr(ubike1_card$rent, 4, 5))+as.numeric(substr(ubike1_card$rent, 7, 8))/60,
  nchar(ubike1_card$rent)==9 ~ as.numeric(substr(ubike1_card$rent, 1, 3))*60+as.numeric(substr(ubike1_card$rent, 5, 6))+as.numeric(substr(ubike1_card$rent, 8, 9))/60
))

ubike2_card=mutate(ubike2_card, ride_time=case_when(
  nchar(ubike2_card$rent)==7 ~ as.numeric(substr(ubike2_card$rent, 1, 1))*60+as.numeric(substr(ubike2_card$rent, 3, 4))+as.numeric(substr(ubike2_card$rent, 6, 7))/60,
  nchar(ubike2_card$rent)==8 ~ as.numeric(substr(ubike2_card$rent, 1, 2))*60+as.numeric(substr(ubike2_card$rent, 4, 5))+as.numeric(substr(ubike2_card$rent, 7, 8))/60,
  nchar(ubike2_card$rent)==9 ~ as.numeric(substr(ubike2_card$rent, 1, 3))*60+as.numeric(substr(ubike2_card$rent, 5, 6))+as.numeric(substr(ubike2_card$rent, 8, 9))/60
))


#---YouBike 1.0電子票證---#
ubike1_card[ubike1_card$rent_station=="信義杭州路口(中華電信總公司"]$rent_station="信義杭州路口(中華電信總公司)"
ubike1_card[ubike1_card$rent_station=="捷運科技大樓站(台北教育大學"]$rent_station="捷運科技大樓站(台北教育大學)"
ubike1_card[ubike1_card$return_station=="信義杭州路口(中華電信總公司"]$return_station="信義杭州路口(中華電信總公司)"
ubike1_card[ubike1_card$return_station=="捷運科技大樓站(台北教育大學"]$return_station="捷運科技大樓站(台北教育大學)"

temp1=group_by(ubike1_card, rent_station)%>%
  rename(StationName=rent_station)%>%
  summarise(count=n())
temp2=group_by(ubike1_card, return_station)%>%
  rename(StationName=return_station)%>%
  summarise(count=n())
temp=rbind(temp1, temp2)%>%
  group_by(StationName)%>%
  summarise(flow=sum(count))

YouBike_Station1=left_join(YouBike_Station1, temp)

# 將110年11月已停止營運(無騎乘量)的站點去除
YouBike_Station1=filter(YouBike_Station1, !is.na(flow))


#---YouBike 2.0電子票證---#
ubike2_card[grepl("僑安地下停車場", ubike2_card$rent_station)]$rent_station="僑安地下停車場(2號出口)東南側"
ubike2_card[grepl("僑安地下停車場", ubike2_card$return_station)]$return_station="僑安地下停車場(2號出口)東南側"
ubike2_card[grepl("公公園", ubike2_card$rent_station)]$rent_station="瑠公公園"
ubike2_card[grepl("公公園", ubike2_card$return_station)]$return_station="瑠公公園"
ubike2_card[grepl("新生高架停車場", ubike2_card$rent_station)]$rent_station="新生高架停車場(林森北路107巷口)"
ubike2_card[grepl("新生高架停車場", ubike2_card$return_station)]$return_station="新生高架停車場(林森北路107巷口)"
ubike2_card[grepl("糖?文化園區", ubike2_card$rent_station)]$rent_station="糖廍文化園區"
ubike2_card[grepl("糖?文化園區", ubike2_card$return_station)]$return_station="糖廍文化園區"

temp1=group_by(ubike2_card, rent_station)%>%
  rename(StationName=rent_station)%>%
  summarise(count=n())
temp2=group_by(ubike2_card, return_station)%>%
  rename(StationName=return_station)%>%
  summarise(count=n())
temp=rbind(temp1, temp2)%>%
  group_by(StationName)%>%
  summarise(flow=sum(count))

YouBike_Station2=left_join(YouBike_Station2, temp)

# 將110年11月無騎乘量的站點去除
YouBike_Station2=filter(YouBike_Station2, !is.na(flow))


png("./figure/YouBike1_flow.png", width=603*2, height=790*2, res=200)
ggplot()+
  geom_sf(data=taipei, color="#F0F0F0", fill="#BEBEBE")+
  geom_sf(data=arrange(YouBike_Station1, desc(flow)), aes(color=flow, size=flow))+
  scale_color_distiller(palette="YlOrRd", direction=1, breaks=seq(0, 35000, by=5000), name="總租還量")+
  scale_size_continuous(range=c(0,4), breaks=seq(0, 35000, by=5000), name="總租還量")+
  geom_sf(data=st_intersection(taipei_mrt_route, taipei), color="#7B7B7B")+
  ggtitle("YouBike 1.0")+
  guides(color=guide_legend(), size=guide_legend())+
  theme_void()+
  theme(legend.title=element_text(family="A", size=20),
        legend.text=element_text(family="B", size=18),
        legend.position=c(0.9, 0.78),
        plot.title=element_text(family="B", size=20, hjust=0.5))
dev.off()

png("./figure/YouBike2_flow.png", width=603*2, height=790*2, res=200)
ggplot()+
  geom_sf(data=taipei, color="#F0F0F0", fill="#BEBEBE")+
  geom_sf(data=arrange(YouBike_Station2, desc(flow)), aes(color=flow, size=flow, alpha=flow))+
  scale_color_distiller(palette="YlOrRd", direction=1, breaks=seq(0, 60000, by=10000), name="總租還量")+
  scale_size_continuous(range=c(0,4), breaks=seq(0, 60000, by=10000), name="總租還量")+
  scale_alpha_continuous(range=c(0.45, 1), breaks=seq(0, 60000, by=10000), name="總租還量")+
  geom_sf(data=st_intersection(taipei_mrt_route, taipei), color="#7B7B7B")+
  ggtitle("YouBike 2.0")+
  guides(color=guide_legend(), size=guide_legend())+
  theme_void()+
  theme(legend.title=element_text(family="A", size=20),
        legend.text=element_text(family="B", size=18),
        legend.position=c(0.9, 0.78),
        plot.title=element_text(family="B", size=20, hjust=0.5))
dev.off()



temp=rbind(cbind(SEQ=c(1:100), TYPE="YouBike 1.0", arrange(YouBike_Station1, desc(flow))[1:100, c("StationName", "flow")]),
           cbind(SEQ=c(1:100), TYPE="YouBike 2.0", arrange(YouBike_Station2, desc(flow))[1:100, c("StationName", "flow")]))

png("./figure/YouBike站點總租還量排名.png", width=1220*2, height=670*2, res=200)
ggplot()+
  geom_point(data=temp, aes(x=SEQ, y=flow, color=TYPE))+
  geom_line(data=temp, aes(x=SEQ, y=flow, color=TYPE))+
  scale_color_brewer(palette="Set1")+
  geom_text_repel(data=filter(temp, SEQ==1), aes(x=SEQ+10, y=flow, label=StationName, color=TYPE), family="A", size=7, fontface="bold", show.legend=F, nudge_y=1)+
  xlab("站點排名") + ylab("總租還量")+
  theme_minimal()+
  theme(legend.title=element_blank(),
        legend.text=element_text(family="B", size=20, face="bold"),
        axis.title=element_text(family="A", size=25, face="bold"),
        axis.text=element_text(family="B", size=20, face="bold"),
        legend.position=c(0.85,0.95))
dev.off()






#---Model formulation---#
ubike1_lm1=lm(flow ~ BikesCapacity+POP_DENSITY+IC_MED+MRT_DIST+SHOP_NUM+university+high_school+park_area+bike_route+SidewalkLength+scooter_number+car_license+ub2_station, data=YouBike_Station1)
summary(ubike1_lm1)
plot(ubike1_lm1)
car::vif(ubike2_lm1)

ubike2_lm1=lm(flow ~ BikesCapacity+POP_DENSITY+IC_MED+MRT_DIST+SHOP_NUM+university+high_school+park_area+bike_route+SidewalkLength+scooter_number+car_license+ub1_station, data=YouBike_Station2)
summary(ubike2_lm1)
plot(ubike2_lm1)


tab_model(ubike1_lm1, ubike2_lm1)
stargazer(list(ubike1_lm1, ubike2_lm1), type="html", out="Linear Model.doc", column.labels=c("YouBike 1.0","YouBike 2.0"), single.row=T)


ubike1_lmm1=lmer(log(flow) ~ scale(BikesCapacity)+scale(POP_DENSITY)+scale(IC_MED)+scale(MRT_DIST)+scale(SHOP_NUM)+scale(university)+scale(high_school)+scale(park_area)+
                   +scale(bike_route)+scale(SidewalkLength)+scale(scooter_number)+scale(car_license)+scale(ub2_station) + (1+scale(MRT_DIST)+scale(BikesCapacity)|District), data=YouBike_Station1)
summary(ubike1_lmm1)
ranef(ubike1_lmm1)
plot(ubike1_lmm1)


ubike2_lmm1=lmer(log(flow) ~ scale(BikesCapacity)+scale(POP_DENSITY)+scale(IC_MED)+scale(MRT_DIST)+scale(SHOP_NUM)+scale(university)+scale(high_school)+scale(park_area)
                 +scale(bike_route)+scale(SidewalkLength)+scale(scooter_number)+scale(car_license)+scale(ub1_station) + (1+scale(MRT_DIST)+scale(BikesCapacity)|District), data=YouBike_Station2)
summary(ubike2_lmm1)
ranef(ubike2_lmm1)
plot(ubike2_lmm1, type=c("p", "smooth"), xlab="Fitted Values", ylab="Residual")


tab_model(ubike1_lmm1, ubike2_lmm1)
stargazer(list(ubike1_lmm1, ubike2_lmm1), type="html", out="Multilevel Model.doc", column.labels=c("YouBike 1.0","YouBike 2.0"), single.row=T)


# YouBike 1.0 random effect
temp=data.frame(ranef(ubike1_lmm1)$District)
temp=cbind(row.names(temp), temp)
colnames(temp)=c("TOWNNAME","Intercept","MRT_DIST","SHOP_NUM")
temp=left_join(taipei, temp)
temp$Intercept=temp$Intercept+fixef(ubike1_lmm1)["(Intercept)"]
temp$MRT_DIST=temp$MRT_DIST+fixef(ubike1_lmm1)["scale(MRT_DIST)"]
temp$SHOP_NUM=temp$SHOP_NUM+fixef(ubike1_lmm1)["scale(SHOP_NUM)"]

ggplot()+
  geom_sf(data=temp, aes(fill=Intercept))+
  scale_fill_distiller(palette="RdYlGn")+
  theme_void()

ggplot()+
  geom_sf(data=temp, aes(fill=MRT_DIST))+
  scale_fill_distiller(palette="RdYlGn", direction=1)+
  theme_void()

ggplot()+
  geom_sf(data=temp, aes(fill=SHOP_NUM))+
  scale_fill_distiller(palette="RdYlGn")+
  theme_void()




# YouBike 2.0 random effect
temp=data.frame(ranef(ubike2_lmm1)$District)
temp=cbind(row.names(temp), temp)
colnames(temp)=c("TOWNNAME","Intercept","MRT_DIST","BikesCapacity")
temp=left_join(taipei, temp)
temp$Intercept=temp$Intercept+fixef(ubike2_lmm1)["(Intercept)"]
temp$MRT_DIST=temp$MRT_DIST+fixef(ubike2_lmm1)["scale(MRT_DIST)"]
temp$BikesCapacity=temp$BikesCapacity+fixef(ubike2_lmm1)["scale(BikesCapacity)"]

p1=ggplot()+
  geom_sf(data=temp, aes(fill=Intercept))+
  scale_fill_distiller(palette="RdYlGn", name="截矩")+
  theme_void()+
  theme(legend.title=element_text(family="A", size=25),
        legend.text=element_text(family="B", size=20),
        legend.position=c(0.97,0.8))

p2=ggplot()+
  geom_sf(data=temp, aes(fill=MRT_DIST))+
  scale_fill_distiller(palette="RdYlGn", name="與捷運站距離")+
  theme_void()+
  theme(legend.title=element_text(family="A", size=25),
        legend.text=element_text(family="B", size=20),
        legend.position=c(0.97,0.8))

p3=ggplot()+
  geom_sf(data=temp, aes(fill=BikesCapacity))+
  scale_fill_distiller(palette="RdYlGn", name="站點車樁數")+
  theme_void()+
  theme(legend.title=element_text(family="A", size=25),
        legend.text=element_text(family="B", size=20),
        legend.position=c(0.97,0.8))

png("./figure/random_rffect.png", width=1900*2, height=620*2, res=200)
plot_grid(p1, p2, p3, ncol=3)
dev.off()


#---GWmodel---#
YouBike_Station1sp=as(YouBike_Station1, Class="Spatial")
bw1=bw.ggwr(flow ~ BikesCapacity+POP_DENSITY+MRT_DIST+SHOP_NUM+university+bike_route+scooter_number+car_license, data=YouBike_Station1sp, dMat=gw.dist(coordinates(YouBike_Station1sp)), kernel="gaussian")
ubike1_gwr1=ggwr.basic(flow ~ BikesCapacity+POP_DENSITY+MRT_DIST+SHOP_NUM+university+bike_route+scooter_number+car_license, bw=bw1, data=YouBike_Station1sp, dMat=gw.dist(coordinates(YouBike_Station1sp)), kernel="gaussian")
ubike1_gwr1_sf=st_as_sf(ubike1_gwr1$SDF)

quantile(ubike1_gwr1_sf$Intercept_TV)
quantile(ubike1_gwr1_sf$BikesCapacity_TV)
quantile(ubike1_gwr1_sf$POP_DENSITY_TV)
quantile(ubike1_gwr1_sf$MRT_DIST_TV)
quantile(ubike1_gwr1_sf$SHOP_NUM_TV)
quantile(ubike1_gwr1_sf$university_TV)
quantile(ubike1_gwr1_sf$bike_route_TV)
quantile(ubike1_gwr1_sf$scooter_number_TV)
quantile(ubike1_gwr1_sf$car_license_TV)


ggplot()+
  geom_sf(data=taipei, color="#F0F0F0", fill="#BEBEBE")+
  geom_sf(data=ubike1_gwr1_sf, aes(color=university))+
  scale_color_distiller(palette="RdYlGn", direction=1)+
  geom_sf(data=filter(ubike1_gwr1_sf, abs(university_TV)<1.96), color="#9D9D9D")+
  theme_void()

p1=ggplot()+
  geom_sf(data=taipei, color="#F0F0F0", fill="#BEBEBE")+
  geom_sf(data=ubike1_gwr1_sf, aes(color=MRT_DIST))+
  scale_color_distiller(palette="RdYlGn", name="與捷運站距離")+
  geom_sf(data=filter(ubike1_gwr1_sf, abs(MRT_DIST_TV)<1.96), color="#9D9D9D")+
  theme_void()+
  theme(legend.title=element_text(family="A", size=20),
        legend.text=element_text(family="B", size=18),
        legend.position=c(0.9, 0.78),
        plot.title=element_text(family="B", size=20, hjust=0.5))

p2=ggplot()+
  geom_sf(data=taipei, color="#F0F0F0", fill="#BEBEBE")+
  geom_sf(data=ubike1_gwr1_sf, aes(color=BikesCapacity))+
  scale_color_distiller(palette="RdYlGn", name="站點車樁數")+
  geom_sf(data=filter(ubike1_gwr1_sf, abs(BikesCapacity_TV)<1.96), color="#9D9D9D")+
  theme_void()+
  theme(legend.title=element_text(family="A", size=20),
        legend.text=element_text(family="B", size=18),
        legend.position=c(0.9, 0.78),
        plot.title=element_text(family="B", size=20, hjust=0.5))

ggplot()+
  geom_sf(data=taipei, color="#F0F0F0", fill="#BEBEBE")+
  geom_sf(data=ubike1_gwr1_sf, aes(color=SHOP_NUM))+
  scale_color_distiller(palette="RdYlGn", direction=1)+
  geom_sf(data=filter(ubike1_gwr1_sf, abs(SHOP_NUM_TV)<1.96), color="#9D9D9D")+
  theme_void()+
  theme(legend.title=element_text(family="A", size=20),
        legend.text=element_text(family="B", size=18),
        legend.position=c(0.9, 0.78),
        plot.title=element_text(family="B", size=20, hjust=0.5))


YouBike_Station2sp=as(YouBike_Station2, Class="Spatial")
bw2=bw.ggwr(flow ~ BikesCapacity+POP_DENSITY+MRT_DIST+SHOP_NUM+university+bike_route+scooter_number+car_license+ub1_station, data=YouBike_Station2sp, dMat=gw.dist(coordinates(YouBike_Station2sp)), kernel="gaussian")
ubike2_gwr1=ggwr.basic(flow ~ BikesCapacity+POP_DENSITY+MRT_DIST+SHOP_NUM+university+bike_route+scooter_number+car_license, bw=bw2, data=YouBike_Station2sp, dMat=gw.dist(coordinates(YouBike_Station2sp)), kernel="gaussian")
ubike2_gwr1_sf=st_as_sf(ubike2_gwr1$SDF)

quantile(ubike2_gwr1_sf$Intercept_TV)
quantile(ubike2_gwr1_sf$BikesCapacity_TV)
quantile(ubike2_gwr1_sf$POP_DENSITY_TV)
quantile(ubike2_gwr1_sf$MRT_DIST_TV)
quantile(ubike2_gwr1_sf$SHOP_NUM_TV)
quantile(ubike2_gwr1_sf$university_TV)
quantile(ubike2_gwr1_sf$bike_route_TV)
quantile(ubike2_gwr1_sf$scooter_number_TV)
quantile(ubike2_gwr1_sf$car_license_TV)


p3=ggplot()+
  geom_sf(data=taipei, color="#F0F0F0", fill="#BEBEBE")+
  geom_sf(data=ubike2_gwr1_sf, aes(color=MRT_DIST))+
  scale_color_distiller(palette="RdYlGn", name="與捷運站距離")+
  geom_sf(data=filter(ubike2_gwr1, abs(MRT_DIST_TV)<1.96), color="#9D9D9D")+
  theme_void()+
  theme(legend.title=element_text(family="A", size=20),
        legend.text=element_text(family="B", size=18),
        legend.position=c(0.9, 0.78),
        plot.title=element_text(family="B", size=20, hjust=0.5))

p4=ggplot()+
  geom_sf(data=taipei, color="#F0F0F0", fill="#BEBEBE")+
  geom_sf(data=ubike2_gwr1_sf, aes(color=BikesCapacity))+
  scale_color_distiller(palette="RdYlGn", name="站點車樁數")+
  geom_sf(data=filter(ubike2_gwr1, abs(BikesCapacity_TV)<1.96), color="#9D9D9D")+
  theme_void()+
  theme(legend.title=element_text(family="A", size=20),
        legend.text=element_text(family="B", size=18),
        legend.position=c(0.9, 0.78),
        plot.title=element_text(family="B", size=20, hjust=0.5))

  
png("./figure/GWR_model.png", width=800*3, height=790*3, res=200)
plot_grid(
  plot_grid(p1, p2, labels="YouBike 1.0", label_fontfamily="B", label_x=0.75, label_size=30),
  plot_grid(p3, p4, labels="YouBike 2.0", label_fontfamily="B", label_x=0.75, label_size=30),
  ncol=1
)
dev.off()


# 各模型解釋力(R square)
rsqu=rbind(
  data.frame(TYPE="YouBike 1.0", MODEL="LM", R=summary(ubike1_lm1)$adj.r.squared),
  data.frame(TYPE="YouBike 2.0", MODEL="LM", R=summary(ubike2_lm1)$adj.r.squared),
  data.frame(TYPE="YouBike 1.0", MODEL="MLM", R=0.526),
  data.frame(TYPE="YouBike 2.0", MODEL="MLM", R=0.405),
  data.frame(TYPE="YouBike 1.0", MODEL="GWR", R=ubike1_gwr1$GW.diagnostic$pseudo.R2),
  data.frame(TYPE="YouBike 2.0", MODEL="GWR", R=ubike2_gwr1$GW.diagnostic$pseudo.R2)
)
rsqu$MODEL=factor(rsqu$MODEL, c("LM","MLM","GWR"))

png("./figure/模式解釋力比較圖.png", width=1000*2, height=730*2, res=200)
ggplot()+
  geom_bar(data=rsqu, aes(x=MODEL, y=R, fill=TYPE, group=TYPE), stat="identity", position="dodge", color=NA)+
  scale_fill_brewer(palette="Set1", name="<span style='font-family:serif;'>YouBike</span>系統")+
  scale_x_discrete(labels=c("多元線性迴歸","多層次迴歸","地理加權迴歸"))+
  xlab("模型") + ylab("R Square")+
  labs(caption="多元線性迴歸為 Adjusted R-Square\n多層次迴歸(YouBike 1.0)為 Margina R-Square\n多層次迴歸(YouBike 2.0)為 Conditional R-Square\n地理加權迴歸為 Pseudo R-square", family="A")+
  theme_minimal()+
  theme(legend.title=element_markdown(family="A", size=20),
        legend.text=element_text(family="B", size=18),
        axis.title.x=element_text(family="A", size=20),
        axis.title.y=element_text(family="B", size=20),
        axis.text.x=element_text(family="A", size=18),
        axis.text.y=element_text(family="B", size=18),
        plot.caption=element_text(family="A", hjust=0, face="bold", size=12))
dev.off()


