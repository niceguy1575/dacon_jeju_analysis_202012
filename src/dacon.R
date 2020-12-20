#### 0. setup ---

# clear the variable and load my custom setup
## 1. clear
rm(list=ls(all=TRUE))
gc()

## 2. load my custom setup

options(help_type="html")

# General options

options(continue="...")
options(width = 100)
options(stringsAsFactors=FALSE)
options(max.print=100)
options(scipen=999)
library(utils)

packages= c("ggplot2", "data.table", "dplyr", "lwgeom", "sf",
            "mapview","sp","rgeos","rgdal", "gtools","rJava",
            "tidyr", 'parallel', 'foreach', 'doParallel', 'leaflet',
            'fpc', 'gridExtra', 'grid', 'BAMMtools', 'foreign')

if ( sum(!(packages %in% rownames(utils::installed.packages()))) > 0 ) {
  unInstalled = packages[!packages %in% rownames(utils::installed.packages())]
  suppressMessages(invisible(lapply(unInstalled, install.packages) ))
}
suppressMessages(invisible(lapply(packages, library, character.only = TRUE)))

## Connect to OPENDW
katech = "+proj=tmerc +lat_0=38 +lon_0=128 +k=0.9999 +x_0=400000 +y_0=600000 +ellps=bessel +towgs84=-115.8,474.99,674.11,1.16,-2.31,-1.63,6.43 +units=m +no_defs"
wgs84 = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"
utmk = "+proj=tmerc +lat_0=38 +lon_0=127.5 +k=0.9996 +x_0=1000000 +y_0=2000000 +ellps=GRS80 +units=m +no_defs"

## 내가 자주 쓰는 함수.
ul = function(vector) {
  return( length(unique(vector)) )
}

nl = function(vector) {
  return( sum(is.na(vector) ) )
}

nw = function(vector) {
  return( which(is.na(vector) ) )
}

len = function(vector) {
  return( length(vector) )
}

# data setup
data_path = "/home/niceguy1575/niceguy1575/dacon/data/"
data_files = dir(data_path)
data_files = data_files[grepl("^KRI", data_files)]

nm = c("YM", "SIDO", "SIGUNGU", "FranClass", "Type", "Time", "TotalSpent", "DisSpent", "NumofSpent", "NumofDisSpent", "POINT_X", "POINT_Y")
data_bind = lapply(1:length(data_files), function(i) {
  d = fread( paste0(data_path, data_files[i]) , select = nm)
}) %>% do.call(rbind, .)

# 우편번호 형상정보 획득
# 출처: "https://www.juso.go.kr/addrlink/addressBuildDevNew.do?menu=bsin"
# 제주도 형상정보 활용
bas = st_read("/home/niceguy1575/niceguy1575/dacon/data/우편번호/TL_KODIS_BAS_50.shp", crs = 5179)
bas_select = bas %>% dplyr::select(BAS_ID, geometry)
bas_wgs = bas_select %>% st_transform(crs = 4326)
# 지도 확인
leaflet() %>% addTiles() %>% addPolygons(data = bas_wgs)

# 업종정보 
# 출처: 수작업 수행
# git upload
sector = fread("/home/niceguy1575/niceguy1575/dacon/data/업종구분/업종분류.csv")

#### 1. 업종별 차이 존재? 상위 업종의 매출 비율
# https://www.yna.co.kr/view/GYH20200610002000044

# 데이터 집계
data_filter = data_bind %>%
  dplyr::filter(TotalSpent >= 0 & DisSpent >= 0) %>%
  left_join(sector, by = 'Type')

data_smry = data_filter %>%
  group_by(YM, Type_Upper, Type) %>% 
  dplyr::summarise(
    TotalSpent = sum(TotalSpent), NumofSpent = sum(NumofSpent),
    DisSpent = sum(DisSpent), NumofDisSpent = sum(NumofDisSpent)
  )

data_smry_by_mon = data_smry %>%
  group_by(Type_Upper, Type) %>% 
  dplyr::summarise(
    total_spent = mean(TotalSpent), total_cnt = mean(NumofSpent),
    total_disspent = mean(DisSpent), total_discnt = mean(NumofDisSpent)
  ) %>%
  ungroup %>% mutate(
            total_rt = round(total_spent / sum(total_spent), 4) * 100,
             dis_rt = round(total_disspent / sum(total_disspent), 4) * 100 )
  
# 업종분류
# 1. 재난지원금 다수 사용 업종 [건수]
# 가용 가능 금액이 적기 때문에 건수로 확인하였음.
top = 15
type_dis = data_smry_by_mon %>% arrange(desc(total_discnt)) %>% head(n=top)
type_dis_nm = type_dis %>% pull(Type)
type_upper_dis_nm = type_dis %>% pull(Type_Upper) %>% unique

type_dis_select = type_dis %>%
  dplyr::select(Type_Upper, Type, total_disspent, total_discnt, dis_rt) %>%
  arrange(desc(dis_rt)) %>%
  mutate(spent = round(total_disspent / 10000000, 1), 
         label = paste0(spent) ) #단위 천만원

gg1 = ggplot(data = type_dis_select,
             aes(x =reorder(Type, dis_rt), y = dis_rt, fill = spent)) +
  geom_bar( stat = "identity" ) + coord_flip() +
  geom_text(aes(label=label), hjust = 1.2, color = "white", fontface = "bold" ) +
  labs(fill = "사용금액(단위: 천만원)") +
  theme_light() + 
  #theme(legend.position = "bottom") + 
  ylab("재난지원금액 사용 비율(%)") +
  xlab("상위 15개 업종명") +
  ggtitle("월평균 재난지원금 결제 건 기준 상위 15개 업종별 사용금액 및 비율") +
  theme(plot.title = element_text(hjust = 0.5))

type_upper_dis_select = type_dis %>%
  group_by(Type_Upper) %>%
  dplyr::summarise(total_disspent = sum(total_disspent),
                   total_discnt = sum(total_discnt),
                   dis_rt = sum(dis_rt)) %>%
  arrange(desc(dis_rt)) %>%
  mutate(spent = round(total_disspent / 10000000), 
         label = paste0(spent) ) #단위 천만원

gg2 = ggplot(data = type_upper_dis_select,
             aes(x = reorder(Type_Upper, dis_rt), y = dis_rt, fill = spent)) +
  geom_bar( stat = "identity" ) + coord_flip() +
  geom_text(aes(label=label), hjust = 1.2, color = "white", fontface = "bold" ) +
  labs(fill = "사용금액(단위: 천만원)") +
  theme_light() + 
  #theme(legend.position = "bottom") + 
  ylab("재난지원금액 사용 비율(%)") +
  xlab("요약 업종") +
  ggtitle("월평균 재난지원금 결제 건 기준 상위 15개 요약 업종별 사용금액 및 비율") +
  theme(plot.title = element_text(hjust = 0.5))

# 2. 일반금액 많이 사용한 업종
type_norm = data_smry_by_mon %>% arrange(desc(total_spent)) %>% head(n = top) 
type_norm_nm = type_norm %>% pull(Type)
type_norm_upper = type_norm %>% pull(Type_Upper) %>% unique

type_norm_select = type_norm %>%
  dplyr::select(Type_Upper, Type, total_spent, total_cnt, total_rt) %>%
  arrange(desc(total_rt)) %>%
  mutate(spent = round(total_spent / 100000000, 1), 
         label = paste0(spent) ) #단위 억원

gg3 = ggplot(data = type_norm_select,
             aes(x =reorder(Type, total_rt), y = total_rt, fill = spent)) +
  geom_bar( stat = "identity" ) + coord_flip() +
  geom_text(aes(label=label), hjust = 1.2, color = "white", fontface = "bold" ) +
  labs(fill = "사용금액(단위: 억원)") +
  theme_light() + 
  ylab("재난지원금액 사용 비율(%)") +
  xlab("상위 15개 업종") +
  ggtitle("월평균 매출 상위 15개 업종의 사용금액 및 비율") +
  theme(plot.title = element_text(hjust = 0.5))

type_upper_nrom_select = type_norm %>%
  group_by(Type_Upper) %>%
  dplyr::summarise(total_spent = sum(total_spent),
                   total_cnt = sum(total_cnt),
                   total_rt = sum(total_rt)) %>%
  arrange(desc(total_rt)) %>%
  mutate(spent = round(total_spent / 100000000, 1), 
         label = paste0(spent) ) #단위 억원

gg4 = ggplot(data = type_upper_nrom_select,
             aes(x =reorder(Type_Upper, total_rt), y = total_rt, fill = spent)) +
  geom_bar( stat = "identity" ) + coord_flip() +
  geom_text(aes(label=label), hjust = 1.2, color = "white", fontface = "bold" ) +
  labs(fill = "사용금액(단위: 억원)") +
  theme_light() + 
  ylab("재난지원금액 사용 비율(%)") +
  xlab("요약 업종") +
  ggtitle("월평균 매출 상위 15개 업종의 사용금액 및 비율") +
  theme(plot.title = element_text(hjust = 0.5))

#### 1-3. 영세규모별 업종별 재난지원금 사용 비교 [mfrow 2,2 업종비교]
# 영세	3억원 이하
# 중소1	3억원 초과 ~ 5억원 이하
# 중소2	5억원 초과 ~ 10억원 이하
# 중소3	10억원 초과 ~ 30억원 이하
# 일반	30억원 초과
# 출처: http://web.innopay.co.kr/9979

data_clss_smry = data_filter %>%
  group_by(YM, FranClass, Type) %>% 
  dplyr::summarise(
    TotalSpent = sum(TotalSpent), NumofSpent = sum(NumofSpent),
    DisSpent = sum(DisSpent), NumofDisSpent = sum(NumofDisSpent)
  )

data_clss_smry_by_mon = data_clss_smry %>%
  group_by(FranClass, Type) %>% 
  dplyr::summarise(
    total_spent = mean(TotalSpent), total_cnt = mean(NumofSpent),
    total_disspent = mean(DisSpent), total_discnt = mean(NumofDisSpent)
  ) %>%
  ungroup %>% mutate(
    total_rt = round(total_spent / sum(total_spent), 4) * 100,
    dis_rt = round(total_disspent / sum(total_disspent), 4) * 100 )

clss = unique(data_clss_smry_by_mon$FranClass)
top_cl = 10

# 영세	3억원 이하
# 중소1	3억원 초과 ~ 5억원 이하
# 중소2	5억원 초과 ~ 10억원 이하
# 중소3	10억원 초과 ~ 30억원 이하
# 일반	30억원 초과

cl_df = data.frame(clss = clss, clss_nm = c("매출 <=3억", "매출 > 30억",
                                         "매출 10억 ~ 30억", "매출 3억 ~ 5억",
                                         "매출 5억 ~ 10억" ) )

clss_plt = lapply(1:length(clss), function(i) {
  cl = clss[i]
  typ = cl_df[cl_df$clss == cl,]$clss_nm
  
  cl_data = data_clss_smry_by_mon %>% 
    dplyr::filter(FranClass == cl) %>%
    arrange(desc(total_disspent)) %>% head(n = top_cl) 
  
  cl_gg_data = cl_data %>%
    dplyr::select(Type, total_disspent, total_discnt, dis_rt) %>%
    arrange(desc(dis_rt)) %>%
    mutate(spent = round(total_disspent / 10000000, 1), 
           label = paste0(spent) ) #단위 천만원
  
  g = ggplot(data = cl_gg_data,
         aes(x =reorder(Type, dis_rt), y = dis_rt, fill = spent)) +
    geom_bar( stat = "identity" ) + coord_flip() +
    geom_text(aes(label=label), hjust = 1, color = "white", fontface = "bold" ) +
    labs(fill = "사용금액(단위: 천만원)") +
    theme_light() + 
    ylab("재난지원금액 사용 비율") +
    xlab( paste0("상위 ", top_cl, "개 업종") )+
    ggtitle( paste0(cl, "(", typ, ")") ) +
    theme(plot.title = element_text(hjust = 0.5))
  
  return(g)
})
  
gg5 = grid.arrange(clss_plt[[1]], clss_plt[[4]], clss_plt[[5]],
             nrow=2, ncol=2,
             top = textGrob("월평균 매출기준 상위 8개 업종의 사용금액 및 비율",
                            gp=gpar(fontsize=20,font=3)) )

gg6 = grid.arrange(clss_plt[[3]], clss_plt[[2]], nrow=1, ncol=2,
                   top = textGrob("월평균 매출기준 상위 8개 업종의 사용금액 및 비율",
                                  gp=gpar(fontsize=20,font=3)) )

#### 2. 시간에 따른 흐름 확인
# http://www.safetimes.co.kr/news/articleView.html?idxno=82157

#### 2-1. 전체업종의 전체금액 & 재난 금액 (월별)

data_total_smry = data_filter %>%
  group_by(YM) %>% 
  dplyr::summarise(
    TotalSpent = sum(TotalSpent), NumofSpent = sum(NumofSpent),
    DisSpent = sum(DisSpent), NumofDisSpent = sum(NumofDisSpent)
) %>% mutate(총결제금액 = TotalSpent/NumofSpent, 재난지원금액 = DisSpent/NumofDisSpent) %>%
  ungroup %>% dplyr::select(YM, 총결제금액, 재난지원금액) %>%
  as.data.table %>%
  melt(id = "YM")

# 건당가로 표기
gg7 = ggplot(data = data_total_smry,
       aes(x= YM, y = value, group = variable, color = variable)) +
  geom_line(stat='identity') +
  labs(color = "구분") +
  theme_light() + 
  ylab("건당가(결제금액/결제건수)") +
  xlab( "월" )+
  ggtitle( "월별 결제당 거래금액 평균의 추이 (20.05 ~ 20.08)" ) +
  theme(plot.title = element_text(hjust = 0.5))

# 재난지원금 초기 지급 이후 많이 사용하였음.
# 2차재난지원금 지급 이후 반등한 모습 확인

#### 3. 일자별 재난지원금 활용 현황
# 재난지원금 건당가 [소비금액/소비건수] 캘린더 플롯
# 시간대별 분석에는 x시 제외
data_time_smry = data_filter %>%
  dplyr::filter(Time != "x시") %>% 
  group_by(YM, Time) %>% 
  dplyr::summarise(
    total_disspent = mean(DisSpent), total_discnt = mean(NumofDisSpent)
  ) %>%
  ungroup %>% mutate(
    재난지원금_건당가 = total_disspent / total_discnt,
    재난지원금_건당가_일별비율 = round(재난지원금_건당가 / sum(재난지원금_건당가), 4) * 100 )
data_time_smry$ym = as.factor(data_time_smry$YM)
data_time_smry$time = 
  as.factor(as.numeric(gsub("x", "99", gsub("시", "", data_time_smry$Time))))

gg8 = ggplot(data_time_smry,aes(x=ym,y=time))+
  geom_tile(aes(fill=재난지원금_건당가_일별비율)) +
  scale_fill_gradient(low="#ffffb3", high="#EA4510")
  theme_bw(base_size=10)+
  theme(legend.title=element_blank(),
        panel.grid=element_blank(),
        panel.border=element_blank(),
        axis.ticks=element_blank(),
        strip.background=element_blank(),
        legend.position="top",
        legend.justification="right",
        legend.direction="horizontal",
        legend.key.size=unit(0.3,"cm"),
        legend.spacing.x=unit(0.2,"cm"))

#### 4. 재난지원금 사용 밀집지역 확인
data_dis = data_filter %>% mutate(key = 1:nrow(.)) %>%
            dplyr::filter(DisSpent > 0)

data_ptr = data_dis %>% dplyr::select(POINT_X, POINT_Y) %>% unique
# 거리 기준 300m, 최소 그룹 기준 4로 그룹핑
# EPSG 5179는 유클리드 거리 활용시 meter단위로 환산됨
data_grp = dbscan(data_ptr, eps = 250, MinPts = 15)

data_ptr$grp = data_grp$cluster
data_ptr_sf = data_ptr %>% st_as_sf(coords = c("POINT_X", "POINT_Y"), crs = 5179 )

# 대표 그룹 도출 [15곳 이상 군집된 지역] 및 시각화
#grp_idx = as.numeric(which(table(data_ptr$grp) > 100))
unq_grp = unique(data_grp$cluster)
unq_grp = unq_grp[unq_grp>0]
#x = lapply(1:length(unq_grp), function(i) {
convex_hull = lapply(1:length(unq_grp), function(i) {
  grp = unq_grp[i]
  x = data_ptr_sf[data_ptr_sf$grp == grp,]
  
  res = st_convex_hull(st_union(x)) %>% st_transform(crs = 4326)
  
  return(res)
})
#convex_hull = convex_hull[-9] # 제주시, 서귀포시에서 멀리 떨어진곳 제외

center_xy = bas_wgs %>% st_centroid %>% st_coordinates %>% apply(., 2, mean)
vx = as.numeric(center_xy[1])
vy = as.numeric(center_xy[2])
zoom = 10

l = leaflet() %>% addTiles() %>%
  setView( vx, vy, zoom )
for(i in 1:length(convex_hull)) {
  l = l %>%
    addPolygons(data = convex_hull[[i]])
}

## save html to png
mapshot(l, file = "jeju_district.png")
# png를 ipynb로 띄우기


#### 4. 월별, 우편번호별 재난지원금 사용 분포
#### natural break기반 구간 생성
#### 우편번호 데이터 https://www.juso.go.kr/addrlink/addressBuildDevNew.do?menu=bsin

# 우편번호별 상권 생성
data_sf = data_dis %>%
  st_as_sf(coords = c("POINT_X", "POINT_Y"), crs = 5179)
data2bas = st_join(data_sf, bas_select)
data2bas_select = data2bas %>% as.data.table %>% dplyr::select(-geometry)
data2bas_smry = data2bas_select %>% group_by(BAS_ID, YM) %>%
  dplyr::summarise(
    DisSpent = sum(DisSpent), NumofDisSpent = sum(NumofDisSpent),
    shop_cnt = n() 
  )

grp = 8
natural_breaks_by_ym = data2bas_smry %>% group_by(YM) %>%
  dplyr::do( as.data.frame(getJenksBreaks(.$DisSpent,k = grp)) )
names(natural_breaks_by_ym) = c("YM", "val")
natural_breaks_by_ym = as.data.table(natural_breaks_by_ym)
natural_breaks_by_ym[val == 0, val:= -1]

# natural break별 컬러 생성
map_pallete = c("#f7ebe9","#f5c7bc","#f7b7a8","#f7a592","#f08369","#f25935","#f03105")
pallet_df = data.frame(cut = c(1:length(map_pallete)), color = map_pallete)

yms = unique(natural_breaks_by_ym$YM)

for(ym in yms) {
  print(ym)
  target_natural_breaks = natural_breaks_by_ym[natural_breaks_by_ym$YM == ym,]    
  target_smry = data2bas_smry[data2bas_smry$YM == ym,]
  
  target_smry$cut = as.numeric(cut(target_smry$DisSpent,
                                 target_natural_breaks$val,
                                 labels = c(1:(grp-1) ) ) )
  target_smry_join = target_smry %>%left_join(pallet_df, by = "cut")
  
  target_bas = merge(bas_select, target_smry_join, by = "BAS_ID", all = FALSE)
  target_bas = target_bas %>% st_transform(., crs = 4326)
  
  center_xy = target_bas %>% st_centroid %>% st_coordinates %>% apply(., 2, mean)
  vx = as.numeric(center_xy[1])
  vy = as.numeric(center_xy[2])
  zoom = 10
  
  bas_ids = target_bas$BAS_ID
  
  br_val = target_natural_breaks$val
  br_val = br_val[-length(br_val)]
  
  label_add = paste0(round(br_val/10000000,2),
                    " ~ ",
                    round(lead(br_val/10000000), 2))
  
  label_add[1] = gsub("0 ~ ", " <= ", label_add[1])
  label_add[length(label_add)] = gsub(" ~ NA", " >= ", label_add[length(label_add)])
  
  l_ym = leaflet() %>% addTiles() %>%
          addLegend("topright", 
              colors = map_pallete,
              labels= label_add,
              title= paste0(ym, " 재난지원금(단위: 천만원)" ),
              opacity = 1) %>%
    setView( vx, vy, zoom )
    
  for(bas_id in bas_ids) {
    bas_data = target_bas %>% dplyr::filter(BAS_ID == bas_id)
    
    l_ym = l_ym %>% addPolygons(data = bas_data,
                                fillOpacity = 0.5,
                                color = bas_data$color,
                                weight = 1)
    
  }
  
  layer = paste0("월별_재난지원금사용금액_natural_breaks_",ym, ".png")
  mapshot(l_ym, file = layer)
}


#### 5. 전통시장 활성화 됐니? => 전통시장 or not 매출 비교 시각화
# 전통시장 지오코딩 및 buffer 형상화..
# https://www.data.go.kr/data/15052837/fileData.do

# 전통시장 지오코딩 수행
# 다음 api 샘플을 통해 수동으로 좌표변환 수행
# https://apis.map.kakao.com/web/sample/addr2coord/

jeju_market_ptr = fread("/home/niceguy1575/niceguy1575/dacon/data/전통시장/제주_전통시장.csv")
jeju_market_ptr = jeju_market_ptr[,c(5, 7, 8, 9, 10, 11, 12)]
names(jeju_market_ptr) = c("nm", "addr", "magnitude", "area", "shop_cnt", "x", "y")
jeju_market_ptr$area = as.numeric(gsub(",", "", jeju_market_ptr$area))

# buffer 150m + 넓이 기준 추가
jeju_market_sf = st_as_sf(jeju_market_ptr, coords = c("x", "y"), crs = 4326) %>%
                  st_transform(crs = 5179) %>% mutate(buffer_area = 150 + sqrt(area) / pi )

jeju_market_buffer = lapply(1:nrow(jeju_market_sf), function(i) {
  md = jeju_market_sf[i,]
  buf = st_buffer(md, dist = md$buffer_area) %>% st_transform(., crs = 5179)
  return(buf)
}) %>% do.call(rbind, .)

# 전통시장 형상정보 확인
center_xy = bas_wgs %>% st_centroid %>% st_coordinates %>% apply(., 2, mean)
vx = as.numeric(center_xy[1])
vy = as.numeric(center_xy[2])
zoom = 10

jeju_market_map = leaflet() %>% addTiles() %>%
  setView( vx, vy, zoom )
for(i in 1:nrow(jeju_market_buffer)) {
  jeju_market_buffer_tmp = jeju_market_buffer[i,] %>% st_transform(., crs = 4326)
  jeju_market_map = jeju_market_map %>%
              addPolygons(data = jeju_market_buffer_tmp)
}

# 전통시장 매출 추이 확인
# 전통시장 - 매장 매칭
data_market_ptr = data_filter %>% mutate(key = 1:nrow(.)) %>% 
              dplyr::select(POINT_X, POINT_Y) %>% unique
data_market_ptr_sf = data_market_ptr %>% 
                      st_as_sf( coords = c("POINT_X", "POINT_Y"), crs = 5179)

ptr2market = st_join(data_market_ptr_sf, jeju_market_buffer)
ptr2market = ptr2market[!is.na(ptr2market$nm),]

ptr_remain = ptr2market %>% st_coordinates %>% as.data.table
names(ptr_remain) = c("POINT_X", "POINT_Y")

data_join = data_filter %>% inner_join(ptr_remain, by = c("POINT_X", "POINT_Y"))

# 전통시장 월별 재난지원금 vs 전체 추이
data_join_smry = data_join %>% group_by(YM) %>%
  dplyr::summarise(
    재난지원금_사용금액 = sum(DisSpent), 재난지원금_사용건수 = sum(NumofDisSpent)
  ) %>% mutate(재난지원금_사용금액 = round(재난지원금_사용금액/10000000, 2),
                         재난지원금_사용건수 = round(재난지원금_사용건수/ 1000, 2) ) # 천만원

gg_data = melt(as.data.table(data_join_smry), id = "YM")

# 전통시장에서 사용금액과 건수는 떨어짐.
gg99 = ggplot(data = gg_data,
       aes(x= YM, y = value, group = variable, color = variable) ) +
  geom_line(stat='identity') +
  scale_y_continuous(name="사용금액(단위: 천만원)",
                     sec.axis = sec_axis(~ (1/3)*., name="사용건수")) +
  labs(color = "") +
  theme(legend.position = "bottom") +
  ggtitle( "전통시장의 월별 재난지원금 사용 추이" ) +
  theme(plot.title = element_text(hjust = 0.5))
  
# 전체 소비 대비 전통시장 내 소비 비율 확인
data_filter_smry = data_filter %>% group_by(YM) %>%
  dplyr::summarise(
    전체_재난지원금_사용금액 = sum(DisSpent), 전체_재난지원금_사용건수 = sum(NumofDisSpent)
  ) %>% mutate(전체_재난지원금_사용금액 = round(전체_재난지원금_사용금액/10000000, 2),
                            전체_재난지원금_사용건수 = round(전체_재난지원금_사용건수/ 1000, 2) )

gg_data = data_join_smry %>% left_join(data_filter_smry, by = "YM")
gg_data$dis_rt = round(gg_data$재난지원금_사용금액 / gg_data$전체_재난지원금_사용금액, 4) *100
gg_data$cnt_rt = round(gg_data$재난지원금_사용건수 / gg_data$전체_재난지원금_사용건수, 4) * 100
gg_data2 = gg_data %>% dplyr::select(YM, dis_rt, cnt_rt)

gg_data2 = melt(as.data.table(gg_data2), id = "YM")

# 전체 대비 비율은 증가함을 확인.
gg98 = ggplot(data = gg_data2,
              aes(x= YM, y = value, group = variable, color = variable) ) +
  geom_line(stat='identity') +
  scale_y_continuous(name="사용금액 비율",
                     sec.axis = sec_axis(~ (1/3)*., name="사용건수 비율")) +
  labs(color = "") +
  theme(legend.position = "bottom") +
  ggtitle( "전통시장의 월별 전체소비 대비 재난지원금 사용 추이" ) +
  theme(plot.title = element_text(hjust = 0.5))








