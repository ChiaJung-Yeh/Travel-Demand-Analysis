library(xml2)
library(dplyr)
library(sf)
library(ggplot2)
library(httr)
library(data.table)
library(tmap)
library(TWspdata)
library(betareg)
library(bayesplot)
library(TDX)
library(lme4)
library(dotwhisker)
library(broom.mixed)
library(arm)
library(sjPlot)
library(texreg)
library(ggtext)
library(stargazer)

windowsFonts(A=windowsFont("標楷體"))
windowsFonts(B=windowsFont("Times New Roman"))

tmap_mode("view")

taipei=filter(taiwan_town, COUNTYNAME=="臺北市")%>%
  st_transform(crs=3826)

taipei_vill=filter(taiwan_village, COUNTYNAME=="臺北市")%>%
  st_transform(crs=3826)




#---YouBike 站點資料---#
YouBike_Station1=fread("./YouBike_Station1.csv")%>%
  mutate(geometry=st_as_sfc(paste0("POINT(", X, " ", Y, ")")))%>%
  st_sf(crs=3826)
YouBike_Station1$StationName[YouBike_Station1$StationUID=="TPE0185"]="瑠公公園"

YouBike_Station2=fread("./YouBike_Station2.csv")%>%
  mutate(geometry=st_as_sfc(paste0("POINT(", X, " ", Y, ")")))%>%
  st_sf(crs=3826)
YouBike_Station2$StationName[YouBike_Station2$StationUID=="TPE500101153"]="瑠公公園"
YouBike_Station2$StationName[YouBike_Station2$StationUID=="TPE500113031"]="糖廍文化園區"

tm_shape(YouBike_Station1)+
  tm_dots(col="red")+
  tm_shape(YouBike_Station2)+
  tm_dots(col="blue")




#---讀取電子票證資料--#
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



# YouBike 1.0
ubike1_card[ubike1_card$rent_station=="信義杭州路口(中華電信總公司"]$rent_station="信義杭州路口(中華電信總公司)"
ubike1_card[ubike1_card$rent_station=="捷運科技大樓站(台北教育大學"]$rent_station="捷運科技大樓站(台北教育大學)"
ubike1_card[ubike1_card$return_station=="信義杭州路口(中華電信總公司"]$return_station="信義杭州路口(中華電信總公司)"
ubike1_card[ubike1_card$return_station=="捷運科技大樓站(台北教育大學"]$return_station="捷運科技大樓站(台北教育大學)"

ubike1_card_summary=group_by(ubike1_card, rent_station, return_station)%>%
  summarise(count=n(),
            ride_time=mean(ride_time))%>%
  left_join(st_drop_geometry(YouBike_Station1)[, c("StationUID","StationName","X","Y","BikesCapacity","POP_DENSITY","IC_MED","District","MRT_DIST","SHOP_NUM","university","high_school","park_area","bike_route")], by=c("rent_station"="StationName"))%>%
  rename(StationUID1_O=StationUID, X1_O=X, Y1_O=Y, BikesCapacity1_O=BikesCapacity, POP_DENSITY1_O=POP_DENSITY, IC_MED1_O=IC_MED, District1_O=District, MRT_DIST1_O=MRT_DIST, SHOP_NUM1_O=SHOP_NUM, university1_O=university, high_school1_O=high_school, park_area1_O=park_area, bike_route1_O=bike_route)%>%
  left_join(st_drop_geometry(YouBike_Station1)[, c("StationUID","StationName","X","Y","BikesCapacity","POP_DENSITY","IC_MED","District","MRT_DIST","SHOP_NUM","university","high_school","park_area","bike_route")], by=c("return_station"="StationName"))%>%
  rename(StationUID1_D=StationUID, X1_D=X, Y1_D=Y, BikesCapacity1_D=BikesCapacity, POP_DENSITY1_D=POP_DENSITY, IC_MED1_D=IC_MED, District1_D=District, MRT_DIST1_D=MRT_DIST, SHOP_NUM1_D=SHOP_NUM, university1_D=university, high_school1_D=high_school, park_area1_D=park_area, bike_route1_D=bike_route)

unique(filter(ubike1_card_summary, is.na(StationUID1_O))$rent_station)
unique(filter(ubike1_card_summary, is.na(StationUID1_D))$return_station)

ubike1_card_summary=filter(ubike1_card_summary, !is.na(StationUID1_D))%>%
  mutate(geometry=st_as_sfc(paste0("LINESTRING(", X1_O, " ", Y1_O, ",", X1_D, " ", Y1_D, ")")))%>%
  st_sf(crs=3826)

classInt::classIntervals(ubike1_card_summary$count, n=10)
temp=filter(ubike1_card_summary, count>=200)

temp=mutate(temp, class=case_when(
  count <= quantile(temp$count)[2] ~ paste0(round(quantile(temp$count)[1], 0), "~", round(quantile(temp$count)[2], 0)),
  count <= quantile(temp$count)[3] ~ paste0(round(quantile(temp$count)[2], 0), "~", round(quantile(temp$count)[3], 0)),
  count <= quantile(temp$count)[4] ~ paste0(round(quantile(temp$count)[3], 0), "~", round(quantile(temp$count)[4], 0)),
  count <= quantile(temp$count)[5] ~ paste0(round(quantile(temp$count)[4], 0), "~", round(quantile(temp$count)[5], 0))
))
temp$class=factor(temp$class, levels=unique(temp$class)[order(unique(temp$class))])

png("./figure/youbike1_ride.png", width=730*2, height=790*2, res=200)
ggplot()+
  geom_sf(data=taipei, fill="#BEBEBE", color="#F0F0F0")+
  geom_sf(data=st_intersection(taipei_mrt_route, taipei), color="#7B7B7B")+
  geom_sf(data=YouBike_Station1, size=0.5)+
  geom_sf(data=temp, aes(color=class))+
  scale_color_brewer(palette="YlOrRd", direction=1, name="<span style='font-family:serif;'>YouBike 1.0</span><br>起訖對騎乘量")+
  theme_void()+
  theme(legend.title=element_markdown(family="A", size=20, face="bold"),
        legend.text=element_text(family="B", size=15),
        legend.position=c(0.82, 0.8))
dev.off()




# YouBike 2.0
ubike2_card[grepl("僑安地下停車場", ubike2_card$rent_station)]$rent_station="僑安地下停車場(2號出口)東南側"
ubike2_card[grepl("僑安地下停車場", ubike2_card$return_station)]$return_station="僑安地下停車場(2號出口)東南側"
ubike2_card[grepl("公公園", ubike2_card$rent_station)]$rent_station="瑠公公園"
ubike2_card[grepl("公公園", ubike2_card$return_station)]$return_station="瑠公公園"
ubike2_card[grepl("新生高架停車場", ubike2_card$rent_station)]$rent_station="新生高架停車場(林森北路107巷口)"
ubike2_card[grepl("新生高架停車場", ubike2_card$return_station)]$return_station="新生高架停車場(林森北路107巷口)"
ubike2_card[grepl("糖?文化園區", ubike2_card$rent_station)]$rent_station="糖廍文化園區"
ubike2_card[grepl("糖?文化園區", ubike2_card$return_station)]$return_station="糖廍文化園區"

ubike2_card_summary=group_by(ubike2_card, rent_station, return_station)%>%
  summarise(count=n(),
            ride_time=mean(ride_time))%>%
  left_join(st_drop_geometry(YouBike_Station2)[, c("StationUID","StationName","X","Y","BikesCapacity","POP_DENSITY","IC_MED","District","MRT_DIST","SHOP_NUM")], by=c("rent_station"="StationName"))%>%
  rename(StationUID2_O=StationUID, X2_O=X, Y2_O=Y, BikesCapacity2_O=BikesCapacity, POP_DENSITY2_O=POP_DENSITY, IC_MED2_O=IC_MED, District2_O=District, MRT_DIST2_O=MRT_DIST, SHOP_NUM2_O=SHOP_NUM)%>%
  left_join(st_drop_geometry(YouBike_Station2)[, c("StationUID","StationName","X","Y","BikesCapacity","POP_DENSITY","IC_MED","District","MRT_DIST","SHOP_NUM")], by=c("return_station"="StationName"))%>%
  rename(StationUID2_D=StationUID, X2_D=X, Y2_D=Y, BikesCapacity2_D=BikesCapacity, POP_DENSITY2_D=POP_DENSITY, IC_MED2_D=IC_MED, District2_D=District, MRT_DIST2_D=MRT_DIST, SHOP_NUM2_D=SHOP_NUM)


# 整理後只剩下新北市的站點和維修中心無法配對
unique(filter(ubike2_card_summary, is.na(StationUID2_O))$rent_station)
unique(filter(ubike2_card_summary, is.na(StationUID2_D))$return_station)

ubike2_card_summary=filter(ubike2_card_summary, !is.na(StationUID2_O), !is.na(StationUID2_D))%>%
  mutate(geometry=st_as_sfc(paste0("LINESTRING(", X2_O, " ", Y2_O, ",", X2_D, " ", Y2_D, ")")))%>%
  st_sf(crs=3826)

classInt::classIntervals(ubike2_card_summary$count, n=10)
temp=filter(ubike2_card_summary, count>=200)


temp=mutate(temp, class=case_when(
  count <= quantile(temp$count)[2] ~ paste0(round(quantile(temp$count)[1], 0), "~", round(quantile(temp$count)[2], 0)),
  count <= quantile(temp$count)[3] ~ paste0(round(quantile(temp$count)[2], 0), "~", round(quantile(temp$count)[3], 0)),
  count <= quantile(temp$count)[4] ~ paste0(round(quantile(temp$count)[3], 0), "~", round(quantile(temp$count)[4], 0)),
  count <= quantile(temp$count)[5] ~ paste0(round(quantile(temp$count)[4], 0), "~", round(quantile(temp$count)[5], 0))
))
temp$class=factor(temp$class, levels=unique(temp$class)[order(unique(temp$class))])

png("./figure/youbike2_ride.png", width=773*2, height=784*2, res=200)
ggplot()+
  geom_sf(data=filter(taiwan_town, COUNTYNAME=="臺北市"), fill="#BEBEBE", color="#F0F0F0")+
  geom_sf(data=st_intersection(taipei_mrt_route, taipei), color="#7B7B7B")+
  geom_sf(data=YouBike_Station2, size=0.5)+
  geom_sf(data=temp, aes(color=class))+
  scale_color_brewer(palette="YlOrRd", direction=1, name="<span style='font-family:serif;'>YouBike 2.0</span><br>起訖對騎乘量")+
  theme_void()+
  theme(legend.title=element_markdown(family="A", size=20, face="bold"),
        legend.text=element_text(family="B", size=15),
        legend.position=c(0.82, 0.8))
dev.off()


tm_shape(temp)+
  tm_lines(col="count", style="fisher")+
  tm_shape(YouBike_Station2)+
  tm_dots()



# 起訖對最短距離資料合併
temp1=fread("C:/Users/ASUS/Desktop/Travel Demand Analysis/Term Project/Analysis/Data/YouBike OD Distance/YouBike1_OD_dist.txt")%>%
  mutate(StationUID1_O=substr(Name, 1, 7),
         StationUID1_D=substr(Name, 11, 17))%>%
  rename(Distance=Total_Length)%>%
  dplyr::select(StationUID1_O, StationUID1_D, Distance)

temp2=fread("C:/Users/ASUS/Desktop/Travel Demand Analysis/Term Project/Analysis/Data/YouBike OD Distance/YouBike2_OD_dist.txt")%>%
  mutate(StationUID2_O=substr(Name, 1, 12),
         StationUID2_D=substr(Name, 16, 50))%>%
  rename(Distance=Total_Length)%>%
  dplyr::select(StationUID2_O, StationUID2_D, Distance)

ubike1_card_summary=left_join(ubike1_card_summary, temp1)
ubike2_card_summary=left_join(ubike2_card_summary, temp2)


# 前十起訖對統計
write.csv(arrange(st_drop_geometry(ubike1_card_summary), desc(count))[1:20, c("rent_station", "return_station", "count")], "./ubike1_card_summary.csv", row.names=F)
write.csv(arrange(st_drop_geometry(ubike2_card_summary), desc(count))[1:20, c("rent_station", "return_station", "count")], "./ubike2_card_summary.csv", row.names=F)


# 計算平均起訖對最短旅行距離 (YouBike 2.0與YouBike 1.0 差不多，前者略高)
sum(ubike1_card_summary$Distance*ubike1_card_summary$count)/sum(ubike1_card_summary$count)
sum(ubike2_card_summary$Distance*ubike2_card_summary$count)/sum(ubike2_card_summary$count)

# 計算平均起訖對最短旅行時間 (YouBike 2.0與YouBike 1.0 差不多，前者略高)
sum(ubike1_card_summary$ride_time*ubike1_card_summary$count)/sum(ubike1_card_summary$count)
sum(ubike2_card_summary$ride_time*ubike2_card_summary$count)/sum(ubike2_card_summary$count)


# 繪製最短旅行距離分布圖
png("./figure/distance_density_plot.png", width=1240*2, height=660*2, res=200)
ggplot()+
  geom_density(aes(x=rep(ubike1_card_summary$Distance[ubike1_card_summary$Distance!=0]/1000, times=ubike1_card_summary$count[ubike1_card_summary$Distance!=0])), color="red")+
  geom_density(aes(x=rep(ubike2_card_summary$Distance[ubike2_card_summary$Distance!=0]/1000, times=ubike2_card_summary$count[ubike2_card_summary$Distance!=0])), color="blue")+
  xlab("騎乘起訖對距離 (公里)") + ylab("密度")+
  theme_minimal()+
  theme(axis.title=element_text(family="A", size=20),
        axis.text=element_text(family="B", size=15))
dev.off()


# 繪製騎乘時間分布圖
png("./figure/time_density_plot.png", width=1240*2, height=660*2, res=200)
ggplot()+
  geom_density(aes(x=filter(ubike1_card, ride_time<=180)$ride_time), color="red")+
  geom_density(aes(x=filter(ubike2_card, ride_time<=180)$ride_time), color="blue")+
  xlab("騎乘時間 (分鐘)") + ylab("密度")+
  theme_minimal()+
  theme(axis.title=element_text(family="A", size=20),
        axis.text=element_text(family="B", size=15))
dev.off()


# 計算附近2.0站點開放時間
ust2=fread("C:/Users/ASUS/Desktop/Travel Demand Analysis/Term Project/Analysis/Data/YouBike_Station.csv")%>%
  filter(grepl("YouBike2.0_", StationNameZh_tw), DATE %in% as.IDate(c(as.IDate("2021-08-01"):as.IDate("2021-11-30"))))%>%
  group_by(StationUID)%>%
  summarise(DATE=min(DATE))
ust2$OPENDAY=as.numeric(difftime(as.Date("2021-11-30"), as.Date(ust2$DATE))/3600/24)


# 整理1.0與2.0交集站點 (100公尺範圍內)
ubike1_buffer=st_buffer(YouBike_Station1[, c("StationUID")], 100)%>%
  rename(StationUID1=StationUID)%>%
  st_intersection(YouBike_Station2[, c("StationUID")])%>%
  rename(StationUID2=StationUID)%>%
  st_drop_geometry()

ubike1_buffer_count=st_buffer(YouBike_Station1[, c("StationUID","BikesCapacity")], 100)%>%
  rename(StationUID1=StationUID)%>%
  st_intersection(YouBike_Station2[, c("StationUID","BikesCapacity")])%>%
  rename(StationUID2=StationUID)%>%
  st_drop_geometry()%>%
  rename(BikesCapacity1=BikesCapacity, BikesCapacity2=BikesCapacity.1)%>%
  left_join(ust2[, c("StationUID","OPENDAY")], by=c("StationUID2"="StationUID"))%>%
  group_by(StationUID1, BikesCapacity1)%>%
  summarise(BikesCapacity2=sum(BikesCapacity2),
            OPENDAY=mean(OPENDAY),
            ubike2_count=n())





# 
ubike_compare=filter(ubike1_card_summary, StationUID1_O %in% ubike1_buffer$StationUID1,
                     StationUID1_D %in% ubike1_buffer$StationUID1)

temp=filter(ubike2_card_summary, StationUID2_O %in% ubike1_buffer$StationUID2,
            StationUID2_D %in% ubike1_buffer$StationUID2)%>%
  left_join(ubike1_buffer, by=c("StationUID2_O"="StationUID2"))%>%
  rename(StationUID1_O=StationUID1)%>%
  left_join(ubike1_buffer, by=c("StationUID2_D"="StationUID2"))%>%
  rename(StationUID1_D=StationUID1)%>%
  st_drop_geometry()%>%
  group_by(StationUID1_O, StationUID1_D)%>%
  summarise(count=sum(count),
            BikesCapacity2_O=sum(BikesCapacity2_O),
            BikesCapacity2_D=sum(BikesCapacity2_D))

ubike_compare=left_join(ubike_compare, temp, by=c("StationUID1_O", "StationUID1_D"))%>%
  rename(count_1=count.x, count_2=count.y)%>%
  dplyr::select(-BikesCapacity2_O, -BikesCapacity2_D)%>%
  left_join(ubike1_buffer_count[, c("StationUID1","BikesCapacity2","OPENDAY","ubike2_count")], by=c("StationUID1_O"="StationUID1"))%>%
  rename(BikesCapacity2_O=BikesCapacity2, OPENDAY_O=OPENDAY, ubike2_count_O=ubike2_count)%>%
  left_join(ubike1_buffer_count[, c("StationUID1","BikesCapacity2","OPENDAY","ubike2_count")], by=c("StationUID1_D"="StationUID1"))%>%
  rename(BikesCapacity2_D=BikesCapacity2, OPENDAY_D=OPENDAY, ubike2_count_D=ubike2_count)

ubike_compare$count_2[is.na(ubike_compare$count_2)]=0

ubike_compare$ubike2_ride_per=ubike_compare$count_2/(ubike_compare$count_1+ubike_compare$count_2)


classInt::classIntervals(ubike_compare$count_2, n=10)

png("./figure/OD_pair_percentage.png", width=840*2, height=785*2, res=200)
ggplot()+
  geom_sf(data=filter(taiwan_town, COUNTYNAME=="臺北市"), fill="#BEBEBE", color="#F0F0F0")+
  geom_sf(data=st_buffer(YouBike_Station1, 100))+
  geom_sf(data=st_intersection(taipei_mrt_route, taipei), color="#7B7B7B")+
  geom_sf(data=st_intersection(YouBike_Station2, st_buffer(YouBike_Station1, 100)), color="blue", size=0.01)+
  geom_sf(data=arrange(filter(ubike_compare, count_2>=100), ubike2_ride_per), aes(color=ubike2_ride_per, size=ubike2_ride_per))+
  scale_color_distiller(palette="YlOrRd", direction=1, name="<span style='font-family:serif;'>YouBike 2.0</span><br> 起訖對騎乘比例", breaks=seq(0, 1, 0.2))+
  scale_size_continuous(range=c(0, 1.5), name="<span style='font-family:serif;'>YouBike 2.0</span><br> 起訖對騎乘比例", breaks=seq(0, 1, 0.2))+
  guides(color=guide_legend(), size=guide_legend())+ 
  theme_void()+
  theme(legend.title=element_markdown(family="A", size=20),
        legend.text=element_text(family="B", size=15),
        legend.position=c(0.95, 0.8))
dev.off()



ubike_compare=filter(ubike_compare, count_1>=30, count_2>=30)

tm_shape(st_buffer(YouBike_Station1, 100))+
  tm_polygons()+
  tm_shape(st_intersection(YouBike_Station2, st_buffer(YouBike_Station1, 100)))+
  tm_dots(col="blue", size=0.01)+
  tm_shape(arrange(filter(ubike_compare, count_2>=100), ubike2_ride_per))+
  tm_lines(col="ubike2_ride_per", palette="-RdYlBu")

png("./figure/YouBike 2.0騎乘比例密度分布圖.png", width=930*2, height=720*2, res=200)
ggplot()+
  geom_density(data=ubike_compare, aes(x=ubike2_ride_per))+
  xlab("<span style='font-family:serif;'>YouBike 2.0</span> 騎乘比例") + ylab("密度")+
  theme_minimal()+
  theme(axis.title.x=element_markdown(size=20, family="A"),
        axis.title.y=element_text(size=20, family="A"),
        axis.text=element_text(size=18, family="B"))
dev.off()

# qplot(ubike_compare$ubike2_ride_per, geom='density')

# nrow(filter(ubike_compare, ubike2_ride_per<=0.4 | ubike2_ride_per>=0.6))/nrow(ubike_compare)

# ggplot()+
#   geom_point(data=ubike_compare_model, aes(x=count_1, y=ubike2_ride_per))
# 
# cor(ubike_compare_model$ubike2_ride_per, ubike_compare_model$count_1)
median(ubike_compare$ubike2_ride_per)




#---Construct the model---#
ubike_compare_model=ubike_compare
names(ubike_compare_model)

betalm1=betareg(ubike2_ride_per ~ ride_time+BikesCapacity1_O+BikesCapacity1_D+BikesCapacity2_O+BikesCapacity2_D+scale(POP_DENSITY1_O*POP_DENSITY1_D), data=ubike_compare_model, link="logit")
summary(betalm1)

# check p, q, mu and phi
p=4.8
q=7.2
p/(p+q)  # mu
mean(ubike_compare$ubike2_ride_per)  # mu (real)
p+q  # phi

prob_dens=data.frame(PROB=ubike_compare$ubike2_ride_per,
                     DENS=gamma(p+q)/gamma(p)/gamma(q)*(ubike_compare$ubike2_ride_per)^(p-1)*(1-ubike_compare$ubike2_ride_per)^(q-1))

# the distribution is close to the real probability density
ggplot()+
  geom_point(data=prob_dens, aes(x=PROB, y=DENS))



# logistic regression
logm1=lm(log(ubike2_ride_per/(1-ubike2_ride_per)) ~ scale(ride_time)+scale(BikesCapacity1_O*BikesCapacity1_D)+scale(BikesCapacity2_O*BikesCapacity2_D)+
           scale(POP_DENSITY1_O*POP_DENSITY1_D)+scale(MRT_DIST1_O*MRT_DIST1_D), data=ubike_compare_model)
summary(logm1)




lmm0=lmer(log(ubike2_ride_per/(1-ubike2_ride_per)) ~ 1 + (1|District_D), data=ubike_compare_model)
logLik(lmm0)

lmm1=lmer(log(ubike2_ride_per/(1-ubike2_ride_per)) ~ scale(ride_time)+scale(BikesCapacity1_O*BikesCapacity1_D)+scale(BikesCapacity2_O*BikesCapacity2_D)+
             scale(POP_DENSITY1_O*POP_DENSITY1_D)+scale(MRT_DIST1_O*MRT_DIST1_D) + (1|District1_O), data=ubike_compare_model)
summary(lmm1)

lmm2=lmer(log(ubike2_ride_per/(1-ubike2_ride_per)) ~ scale(ride_time)+scale(BikesCapacity1_O)+scale(BikesCapacity1_D)+scale(BikesCapacity2_O)+scale(BikesCapacity2_D)+
            scale(POP_DENSITY1_O*POP_DENSITY1_D)+scale(MRT_DIST1_O)+scale(MRT_DIST1_D) + (1|District1_O), data=ubike_compare_model)
summary(lmm2)

lmm3=lmer(log(ubike2_ride_per/(1-ubike2_ride_per)) ~ scale(ride_time)+scale(BikesCapacity1_O)+scale(BikesCapacity1_D)+scale(BikesCapacity2_O)+scale(BikesCapacity2_D)+
            scale(POP_DENSITY1_O+POP_DENSITY1_D)+scale(MRT_DIST1_O+MRT_DIST1_D)+scale(SHOP_NUM1_O+SHOP_NUM1_D)+scale(count_1) + (1|District1_O), data=ubike_compare_model)
summary(lmm3)

lmm4=lmer(log(ubike2_ride_per/(1-ubike2_ride_per)) ~ scale(ride_time)+scale(BikesCapacity1_O)+scale(BikesCapacity1_D)+scale(BikesCapacity2_O)+scale(BikesCapacity2_D)+
            scale(MRT_DIST1_O)+scale(MRT_DIST1_D)+scale(count_1)+scale(OPENDAY_O)+scale(OPENDAY_D) + (1|District1_O), data=ubike_compare_model)
summary(lmm4)


# count_1 -> proxy of the 依賴性(慣性)
# OPENDAY_O / OPENDAY_D -> proxy of 適應性


tidy(lmm1, effects="ran_vals")
tab_model(list(lmm0, lmm4))

stargazer(list(lmm4), type="html", out="Multilevel Logistic Regression.doc", single.row=T)


ranef(lmm4)
temp=data.frame(TOWNNAME=rownames(data.frame(ranef(lmm4)$District1_O)), data.frame(fixef(lmm4)["(Intercept)"]+ranef(lmm4)$District1_O))
temp=left_join(taipei, temp)%>%
  rename(Intercept=X.Intercept.)

png("./figure/YouBike 2.0騎乘比例隨機截矩分布圖.png", width=684*2, height=785*2, res=200)
ggplot()+
  geom_sf(data=temp, aes(fill=Intercept))+  
  scale_fill_distiller(palette="YlOrRd", direction=1, name="Intercept")+
  theme_void()+
  theme(legend.title=element_text(family="B", size=18, face="bold"),
        legend.text=element_text(family="B", size=15),
        legend.position=c(0.9, 0.8))
dev.off()


