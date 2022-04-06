setwd("C:/Users/lgm92/Desktop/20210421科协项目结题报告/结题报告的数据分析/promap")
library(readxl)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(sf)
library(hrbrthemes)
library(geojsonsf)
library(ggimage)
library(ggspatial)
library(rcartocolor)
Sys.setlocale(category = "LC_ALL",locale="Chinese")
df2<- read_excel("20210614finishdata.xlsx")

###画省级地图
read_sf("C:/Users/lgm92/Desktop/20210421科协项目结题报告/结题报告的数据分析/promap/2019行政区划/省.shp") %>%
  st_transform(4326)  -> pro
pro
df2 %>%
  count(省) %>%
  dplyr::filter(.$省 %in% c("河南", "湖北", "江西", "湖南","安徽","陕西")) -> df3 ###筛选出湖北临近的几个省
df3

df3$pro<-c("省") ##在数据框里面加入一列含有“省”的字
df3
df3$省<-str_c(df3$省,df3$pro) ###将原来省份数据与加入的一列字，用str_c()函数进行合并，生成“河南省”、“湖北省”等带省份的数据

df3
df3 <- df3[,-3] ###剔除添加的列，保留原始数据   
df3
df3 %>%
  left_join(pro) %>%
  st_sf() -> myprov
myprov
ggplot(myprov)+
  geom_sf(aes(fill=n)) +
  ggsflabel::geom_sf_label_repel(aes(label=省),family=cnfont)+
  # geom_sf_text(aes(label=省),family=cnfont###添加省市的名称
  #)+
  #scale_fill_viridis_c(option = "D",direction = -1)
  rcartocolor::scale_fill_carto_c(palette = "BluGrn")+###修改映射颜色
  annotation_scale(
    width_hint=0.2,
    text_family = cnfont)+
  annotation_north_arrow(
    location="tr",which_north="false",
    width = unit(1.6,"cm"),
    height = unit(2,"cm"),
    style = north_arrow_fancy_orienteering(
      text_family = cnfont
    )
  )+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())+   ###删除x，y轴纵横坐标
 # theme(axis.text.x = element_blank(),
        #axis.text.y = element_blank(),
       # panel.grid.major = element_blank(),  ##删除经纬网及横纵坐标轴文本
        #legend.position = "none")  ###去除图片的图例
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        panel.grid.major = element_blank(),  ##删除经纬网及横纵坐标轴文本
       legend.position = c(0.05,0.25))+     ###修改图例的位置，图例位置以x，y轴是1为标准
  labs(fill="调查人数")  -> p1 ###修改图例标签名称
p1
 


###画市级地图
read_sf("C:/Users/lgm92/Desktop/20210421科协项目结题报告/结题报告的数据分析/promap/2019行政区划/市.shp") %>%
  st_transform(4326)  -> pro
df2 %>%
  count(市) %>%
  dplyr::filter(.$市 %in% c("武汉", "南阳", "驻马店", "郑州","襄阳","孝感","长沙","信阳","宜昌","芜湖","抚州","西安")) -> df3 ###筛选出湖北临近的几个市
df3


df3$pro<-c("市")     ##在数据框里面加入一列含有“省”的字
df3$市<-str_c(df3$市,df3$pro) ###将原来省份数据与加入的一列字，用str_c()函数进行合并，生成“河南省”、“湖北省”等带省份的数据

df3
df3 <- df3[,-3] ###剔除添加的列，保留原始数据   

df3 %>%
  left_join(pro) %>%
  st_sf() -> myprov

myprov
# myprov%>% 
#   select("市")  -> myprov1
# myprov1
# world_df = st_drop_geometry(myprov1)  ###删除geometry对象
# world_df
# 
# library(openxlsx)
# write.xlsx(world_df,"myprov1.xlsx")   

myprov %>% 
  mutate(pinyin=case_when(
    市=="南阳市"~"Nanyang",
    市=="信阳市"~"Xinyang",
    市=="驻马店市"~"Zhumadian",
    市=="郑州市"~"Zhengzhou",
    市=="孝感市"~"Xiaogan",
    市=="襄阳市"~"Xiangyang",
    市=="宜昌市"~"Yichang",
    市=="西安市"~"Xi'an",
    市=="长沙市"~"Changsha",
    市=="芜湖市"~"Wuhu",
    市=="武汉市"~"Wuhan",
    市=="抚州市"~"Fuzhou"
  )) -> myprov

myprov




ggplot(myprov)+
  geom_sf(aes(fill=n)) +
  ggsflabel::geom_sf_label_repel(aes(label=市),family=cnfont)+
  # geom_sf_text(aes(label=省),family=cnfont    ####添加省市的名称
  #)+
  #scale_fill_viridis_c(option = "D",direction = -1)
  rcartocolor::scale_fill_carto_c(palette = "BluGrn")+###修改映射颜色
  annotation_scale(
    width_hint=0.2,
    text_family = cnfont
  )+
  annotation_north_arrow(
    location="tr",which_north="false",
    width = unit(1.6,"cm"),
    height = unit(2,"cm"),
    style = north_arrow_fancy_orienteering(
      text_family = cnfont
    )
  )+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())+   ###删除x，y轴纵横坐标
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        panel.grid.major = element_blank(),  ##删除经纬网及横纵坐标轴文本
        legend.position = c(0.05,0.25))+     ###修改图例的位置，图例位置以x，y轴是1为标准
  labs(fill="调查人数")  -> sdmp ###修改图例标签名称
sdmp




library(cowplot)

jn <- ggplot() +
      geom_line(aes(x = 1:12, y = 1),
            linetype = 2, color = "#C61B22",
            size = 0.8) +
       geom_line(aes(x = 1:12, y = 2),
            linetype = 2, color = "#C61B22",
            size = 0.8) +
       scale_y_continuous(limits = c(-1, 2)) +
      coord_polar() +
      theme_nothing()


ggdraw() +
  draw_plot(sdmp) +
  draw_plot(jn, height = 0.85, x = 0.08, y = 0.08) -> p2
p2

#devtools::install_git("https://gitee.com/tidyfriday/ggsflabel.git") #安装ggsflabel包，进行地图标签名称位置更改

rcartocolor::cartocolors %>% DT::datatable()   ##查看rcartocolor包颜色的映射

library(cowplot)
plot_grid(p1, p2,align = "h", axis = "b", nrow = 1, rel_widths = c(1, 1)
          ,labels = c("A","B"))  ###调整变量

?plot_grid  



#####画无图例的调查范围图，以及带拼音的变量名称
ggplot(myprov)+
  geom_sf(aes(fill=n)) +
  ggsflabel::geom_sf_label_repel(aes(label=pinyin),family=cnfont)+
   # geom_sf_text(aes(label=省),family=cnfont)    ####添加省市的名称
  #)+
  #scale_fill_viridis_c(option = "D",direction = -1)
  rcartocolor::scale_fill_carto_c(palette = "RedOr")+ ###修改映射颜色###修改映射颜色
  annotation_scale(
    width_hint=0.2,
    text_family = cnfont
  )+
  annotation_north_arrow(
    location="tr",which_north="false",
    width = unit(1.6,"cm"),
    height = unit(2,"cm"),
    style = north_arrow_fancy_orienteering(
      text_family = cnfont
    )
  )+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())+   ###删除x，y轴纵横坐标
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        panel.grid.major = element_blank(),  ##删除经纬网及横纵坐标轴文本
        legend.position ="none")+     ###修改图例的位置，图例位置以x，y轴是1为标准
  labs(fill="调查人数")  -> sdmp2 ###修改图例标签名称
sdmp2



??geom_sf_label_repel


library(cowplot)

jn <- ggplot() +
  geom_line(aes(x = 1:16, y = 1),
            linetype = 2, color = "#C61B22",
            size = 0.8) +
  geom_line(aes(x = 1:16, y = 2),
            linetype = 2, color = "#C61B22",
            size = 0.8) +
  scale_y_continuous(limits = c(-1,2)) +
  coord_polar() +
  theme_nothing()


ggdraw() +
  draw_plot(sdmp2) +
  draw_plot(jn, height = 0.85, x = 0.02, y = 0.1) -> p3

p3







