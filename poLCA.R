setwd("C:/Users/lgm92/Desktop/20210421科协项目结题报告/结题报告的数据分析/validity")
library(readxl)
library(tidyverse)
library(ggplot2)
library(dplyr)
Sys.setlocale(category = "LC_ALL",locale="Chinese")
bc<- read_excel("20210614finishdata.xlsx")
d1 <- data.frame(Q50=bc$Q50*39.2,Q61=bc$Q61*36.8,Q75=bc$Q75*24.00,Q51=bc$Q51*12.91,Q53=bc$Q53*19.74,Q57=bc$Q57*9.99,Q68=bc$Q68*20.13,Q73=bc$Q73*8.96,Q77=bc$Q77*20.86,Q85=bc$Q85*5.83,Q87=bc$Q87*10.49,Q52=bc$Q52*10.47,Q55=bc$Q55*9.31,Q56=bc$Q56*7.22,Q65=bc$Q65*7.08,Q69=bc$Q69*9.76,Q72=bc$Q72*13.99,Q80=bc$Q80*13.95,Q81=bc$Q81*16.21,Q86=bc$Q86*12.01,Q54=bc$Q54*22.37,Q62=bc$Q62*12.57,Q71=bc$Q71*27.26,Q78=bc$Q78*16.10,Q82=bc$Q82*21.70,Q58=bc$Q58*22.01,Q64=bc$Q64*19.36,Q70=bc$Q70*20.13,Q79=bc$Q79*22.53,Q83=bc$Q83*15.97,Q59=bc$Q59*11.54,Q60=bc$Q60*10.57,Q63=bc$Q63*21.30,Q66=bc$Q66*10.79,Q67=bc$Q67*9.30,Q74=bc$Q74*12.61,Q76=bc$Q76*11.2,Q84=bc$Q84*12.69)  #对量表变量进行赋值
d1 <- as.data.frame(letter=letters[1:879],d1)  #整合数据框
d1$el <- rowSums(d1[,c(1:3)])
d1$p <- rowSums(d1[,c(4:11)])
d1$er <- rowSums(d1[,c(12:20)])
d1$s <- rowSums(d1[,c(21:25)])
d1$si <- rowSums(d1[,c(26:30)])
d1$pa <- rowSums(d1[,c(31:38)])
d1 <- d1[,c(39:44)] 
str(d1)
library(lavaan)
library(semPlot)

str(bc)

set.seed(2021)




###将数据转化为标准的潜类别分析识别格式，1代表否，2代表是
##消毒管理模块
dm1 <- bc[,c(23,31,32,33)]
str(dm1)
dm11 <- dm1[,1]
dm12 <- dm1[,-1]
dm12[dm12==1]=2   ###批量替换，不破坏数据框结构的情况下，把数据框的全部1数据进行替换为2，把1代表的是，转化为2代表的是；
str(dm12)
dm12[dm12==0]=1   ###批量替换，不破坏数据框结构的情况下，把数据框的全部0数据进行替换为1，把0代表的否，转化为1代表的否；  
str(dm12)

dm11$Family_food <- factor(dm11$Family_food)
Family_food <- model.matrix(~Family_food,dm11)
Family_food %>% 
  data.frame() -> Family_food1
Family_food1[,-1] -> Family_food2
Family_food2

Family_food2[Family_food2==1]=2   ###批量替换，不破坏数据框结构的情况下，把数据框的全部1数据进行替换为2，把1代表的是，转化为2代表的是；

Family_food2[Family_food2==0]=1   ###批量替换，不破坏数据框结构的情况下，把数据框的全部0数据进行替换为1，把0代表的否，转化为1代表的否；  

dm1 <- bind_cols(Family_food2,dm12)
glimpse(dm1)



##家庭空间改造模块
dm2 <- bc[,c(42,50)]
str(dm2)   

dm2$jtkjRN <- factor(dm2$jtkjRN)


jtkjRN <- model.matrix(~jtkjRN,dm2)   ###进行哑变量的设置
jtkjRN
colnames(jtkjRN)    ###查看转换后哑变量的名称
jtkjRN %>% 
  data.frame() -> jtkjRN1   ###将矩阵转换为数据框
dm22 <- bind_cols(dm2,jtkjRN1)
dm22
dm23 <- dm22[,-c(2:3)]
dm23
dm23[dm23==1]=2    ###批量替换，不破坏数据框结构的情况下，把数据框的全部1数据进行替换为2，把1代表的是，转化为2代表的是；
dm23
dm23[dm23==0]=1    ###批量替换，不破坏数据框结构的情况下，把数据框的全部0数据进行替换为1，把0代表的否，转化为1代表的否； 
dm23


###家庭锻炼模块

bc %>% 
  mutate(BMI=weight/(height/100)^2) -> bc
dm3 <- bc[,c(51,52,53,69,126)]
str(dm3)
dm3 <- as_tibble(dm3)
dm3
dm3 %>% 
  mutate(
    bmi=case_when(
      BMI<18.5 ~1,      ###体重过轻
      BMI>=18.5 & BMI<24~2,   ###体重正常   
      BMI>=24 & BMI<28 ~3,    #### 超重
      BMI>=28~4)       ##肥胖
  ) -> dm31
###用case_when变量进行赋值的时候，  

dm31[,-5] -> dm31   ###去除BMI值，保留分类的BMI值
dm31    
#####批量进行因子转换
dm31 %>% 
  mutate(across(where(is.double),factor)) %>% 
  as_tibble() -> dm32

model.matrix(~diet,dm32) %>%    ####对diet变量进行哑变量处理
  data.frame() -> diet     

model.matrix(~weight_I,dm32) %>%   ####对weight_I进行亚变量
  data.frame() ->weight_I
model.matrix(~ train_N,dm32) %>% 
  data.frame() -> train_N
model.matrix(~ emotion,dm32) %>% 
  data.frame() -> emotion

model.matrix(~ bmi,dm32) %>% 
  data.frame() -> bmi    

dm33 <- bind_cols(diet,weight_I,train_N,emotion,bmi )
dm33
colnames(dm33)
dm34 <- dm33[,-c(1,6,11,16,21)]  ###去除参考变量
head(dm34)
dm34[dm34==1]=2     ##批量替换，不破坏数据框结构的情况下，把数据框的全部1数据进行替换为2，把1代表的是，转化为2代表的是；
dm34[dm34==0]=1    ###批量替换，不破坏数据框结构的情况下，把数据框的全部0数据进行替换为1，把0代表的否，转化为1代表的否； 
dm34

####将三个维度的数据进行合并     


dm4 <- bind_cols(dm1,dm23,dm34)

head(dm4)
dm4 %>% 
 dplyr::rename( trfamspa=jtkjR,famdisspa=xdkj,
              houdis=xdy ,houdisapp=xdyq,
              exerfreq2=train_N2,exerfreq3=train_N3,exerfreq4=train_N4,exerfreq5=train_N5,
                  famfood2=Family_food2,famfood3=Family_food3,famfood4=Family_food4
                ) -> dm4



glimpse(dm4)
###对合并的数据进行潜类别构造   
library(poLCA)
f4 <- cbind(trfamspa,famdisspa,houdis ,houdisapp, exerfreq2,exerfreq3,exerfreq4,exerfreq5,famfood2,famfood3,famfood4)~1
LCA4 <- poLCA(f4,dm4,nclass = 4,graphs = TRUE)   ##潜类别变量图形



library(readr)
library(readxl)
library(openxlsx)
dm4$prdclass <- LCA4$predclass  ###此步将分类后的类别写入数据框
write.xlsx(dm4,"dm4.xlsx")  ##此步将分类后的类别保存为excel文件以备后面分析使用  
fr1 <- read_excel("final_result20210714.xlsx")


f<-with(dm4, cbind(trfamspa,famdisspa,houdis ,houdisapp, exerfreq2,exerfreq3,exerfreq4,exerfreq5,famfood2,famfood3,famfood4)~1)
k=10
for(i in 1:k){
  assign(paste("lc",i,sep=""),
         poLCA(f, dm4, nclass=i, maxiter=3000,
               tol=1e-5, na.rm=FALSE, 
               nrep=10, verbose=TRUE, calc.se=TRUE))
}




plot(lc4)




# dm4$prdclass <- lc4$predclass  ###此步将分类后的类别写入数据框
# write.xlsx(dm4,"dm42.xlsx")  ##此步将分类后的类别保存为excel文件以备后面分析使用  
# # fr1 <- read_excel("final_result20210714.xlsx")



tab.modfit<-data.frame(matrix(rep(999,7),nrow=1))
names(tab.modfit)<-c("log-likelihood",
                     "resid. df","BIC",
                     "aBIC","cAIC","likelihood-ratio","Entropy")
tab.modfit









entropy.poLCA<-function (lc)
{
  K.j <- sapply(lc$probs, ncol)
  if(length(unique(K.j))==1){
    fullcell <- expand.grid(data.frame(sapply(K.j, 
                                              seq, from = 1)))
  } else{
    fullcell <- expand.grid(sapply(K.j, seq, from = 1))
  }
  P.c <- poLCA.predcell(lc, fullcell)
  return(-sum(P.c * log(P.c), na.rm = TRUE))
}

entropy.poLCA(lc2)  ###本来自带的熵

entropy<-function(lc){
  return(-sum(lc$posterior*log(lc$posterior),
              na.rm=T))
}

lc4$posterior[1,]   ###查看后验概率分布   

###相对熵函数
relative.entropy<-function(lc){        
  en<--sum(lc$posterior*
             log(lc$posterior),na.rm=T)
  e<-1-en/(nrow(lc$posterior)*log(ncol(lc$posterior)))
  return(e)
}

relative.entropy(lc4)   ###这是相对熵值 ，类似Mplus中的所有值    



for(i in 2:k){
  tab.modfit<-rbind(tab.modfit,
                    c(get(paste("lc",i,sep=""))$llik,
                      get(paste("lc",i,sep=""))$resid.df,
                      get(paste("lc",i,sep=""))$bic,
                      (-2*get(paste("lc",i,sep=""))$llik) +
                        ((log((get(paste("lc",i,sep=""))$N + 2)/24)) *
                           get(paste("lc",i,sep=""))$npar),
                      (-2*get(paste("lc",i,sep=""))$llik) +
                        get(paste("lc",i,sep=""))$npar *
                        (1 + log(get(paste("lc",i,sep=""))$N)),
                      get(paste("lc",i,sep=""))$Gsq,
                      relative.entropy(get(paste("lc",i,sep="")))
                    ))
}
tab.modfit<-round(tab.modfit[-1,],2)     ###删除第一行，一般潜类别分析是从第一行开始的
tab.modfit$Nclass <- 2:k
tab.modfit     ####BIC与调整的aBIC均显示分三类最好    

tab.modfit$Nclass <-as.factor(tab.modfit$Nclass)

results2<-tidyr::gather(tab.modfit,label,value,4:7)     ###利用gather函数，进行长宽数据的转换
library(cowplot)
results2 

ggplot(results2) +
  geom_point(aes(x=Nclass,y=value),size=3) +
  geom_line(aes(Nclass, value,group = 1)) +    ####此处的另group=1，为的就是防止报错
  theme_bw()+
  labs(x = "Number of classes", y="", title = "") +
  facet_grid(label ~. ,scales = "free") +
  theme_cowplot() + 
  labs(x=" ") +                                                               #
  theme(text=element_text(family="Times New Roman", size=12),                                   #
        legend.key.width = unit(.5, "line"),                                                    #
        legend.text = element_text(family="Times New Roman", size=12),                          #
        legend.title = element_blank(),                                                         #
        legend.position = "top"
  )+
  geom_vline(aes(xintercept=3), colour="#e41a1c", linetype="dashed",size=1)
  # theme_classic(base_size = 16, base_family = "") + 
  # theme(panel.grid.major.x = element_blank() ,
  #       panel.grid.major.y = element_line(colour="grey", 
  #                                         size=0.5),
  #       legend.title = element_text(size = 16, face = 'bold'),
  #       axis.text = element_text(size = 16),
  #       axis.title = element_text(size = 16),
  #       legend.text= element_text(size=16),
  #       axis.line = element_line(colour = "black"))


lc5$P
lc3$P   ###查看分类的概率  


lca_select <- function(f,dm4,nb_var,k,nbr_repet)
{
  N=length(t(dm4[,1]))
  tab.modfit<-data.frame(matrix(rep(999,12),nrow=1))
  names(tab.modfit)<-c("Df","Gsq","Llik","AIC",
                       "mAIC","AICc","HT",
                       "cAIC","AICc","BIC","aBIC","HQ")
  for(i in 2:k){
    assign(paste("lc",i,sep=""),
           poLCA(f, dm4, nclass=i, maxiter=3000,
                 tol=1e-5, na.rm=FALSE,
                 nrep=nbr_repet, verbose=TRUE, calc.se=TRUE))
    tab.modfit<-rbind(tab.modfit, c(
      get(paste("lc",i,sep=""))$resid.df, #df
      get(paste("lc",i,sep=""))$Gsq, #gsq
      get(paste("lc",i,sep=""))$llik, #llik
      -2*get(paste("lc",i,sep=""))$llik+
        2*get(paste("lc",i,sep=""))$npar, #AIC
      -2*get(paste("lc",i,sep=""))$llik+
        3*get(paste("lc",i,sep=""))$npar, #AIC3
      -2*get(paste("lc",i,sep=""))$llik+
        2*get(paste("lc",i,sep=""))$npar+
        (2*get(paste("lc",i,sep=""))$npar*get(paste("lc",
                                                    i,sep=""))$npar+1)/(N-get(
                                                      paste("lc",i,sep=""))$npar-1), #AICC
      -2*get(paste("lc",i,sep=""))$llik+
        2*get(paste("lc",i,sep=""))$npar+
        (2*(get(paste("lc",i,sep=""))$npar+1)*(get(paste("lc",
                                                         i,sep=""))$npar+2))/(N-get(
                                                           paste("lc",i,sep=""))$npar-2), #HT
      -2*get(paste("lc",i,sep=""))$llik+get(
        paste("lc",i,sep=""))$npar*(log(N)+1), #CAIC
      -2*get(paste("lc",i,sep=""))$llik+
        2*get(paste("lc",i,sep=""))$npar+
        (2*get(paste("lc",i,sep=""))$npar*get(paste("lc",
                                                    i,sep=""))$npar+1)/(N-get(paste("lc",i,sep=""))$
                                                                          npar-1)+
        N*log(N/(N-get(paste("lc",i,sep=""))$npar-1)), #CAIU
      -2*get(paste("lc",i,sep=""))$llik+
        get(paste("lc",i,sep=""))$npar*log(N), #BIC
      -2*get(paste("lc",i,sep=""))$llik+
        get(paste("lc",i,sep=""))$npar*log((N+2)/24), #ABIC
      -2*get(paste("lc",i,sep=""))$llik+
        2*get(paste("lc",i,sep=""))$npar*log(log(N)) #HQ
    ))
  }
  tab.modfit<-round(tab.modfit[-1,],2)
  tab.modfit$Nclass<-2:k
  print(tab.modfit)
  plot(tab.modfit$AIC,type="l",lty=2,lwd=1,
       xaxt="n",yaxt="n",bty="l",
       ylim=c(min(tab.modfit$AIC,tab.modfit$aBIC)-
                100,round(max(tab.modfit$BIC,tab.modfit$aBIC))+100),
       col="black",
       xlab="Number of classes",ylab="Information criteria",
       main="Comparison of information criteria to choose the 
number of classes")
  axis(1,at=1:length(tab.modfit$Nclass),
       labels=tab.modfit$Nclass)
  lines(tab.modfit$AIC,col="black",type="l",lty=2,lwd=2)
  lines(tab.modfit$BIC,col="red",type="l",lty=2,lwd=2)
  lines(tab.modfit$aBIC,col="green",type="l",lty=2,
        lwd=2)
  lines(tab.modfit$cAIC,col="orange",type="l",lty=2,
        lwd=2)
  lines(tab.modfit$HQ,col="blue",type="l",lty=2,lwd=2)
  #lines(dd$caiu,col="purple",type="l",lty=7,lwd=2)
  #lines(dd$bica,col="grey",type="l",lty=8,lwd=2)
  #lines(dd$hq,col="pink",type="l",lty=9,lwd=2)
  legend("topright",legend=c("AIC","BIC","aBIC","cAIC",
                             "HQ"),
         pch=21,col=c("black","red","green","orange","blue"),
         ncol=5,bty="n",cex=0.8,lty=1:9,
         text.col=c("black","red","green","orange","blue"), 
         inset=0.01)
}


lca_select(with(dm4, cbind(trfamspa,famdisspa,houdis ,houdisapp, exerfreq2,exerfreq3,exerfreq4,exerfreq5,famfood2,famfood3,famfood4)~1),dm4, k=10, nbr_repet=10)


library(openxlsx)
write.xlsx(dm4,"result.xlsx")


lc4
class(lc4)

####至此，已经筛选出最合适的三分类变量，并且每个类别变量的值均展现在图中
   

###第一种绘制条件概率图形   


###显示每个类别在各个部分所占的比例-条件概率
library(cowplot)

lcmodel <- reshape2::melt(lc4$probs, level=4)     ####此图形展示的就是，1就是“否”，2就是“是”
# lcmodel1<- round(lcmodel$value,2)
# lcmodel1
# lcmodel2 <- lcmodel[,2]
# lcmodel2
# lcmodel3 <- bind_cols(lcmodel1,lcmodel2)
# str(lcmodel3)
# # 
# write.xlsx(lcmodel3,"lcmodel3.xlsx")
# lcmodel1[,89]

lcmodel$L4 <- fct_inorder(lcmodel$L4 )    ####将L4变量无需分类变量转换为有序分类变量
str(lcmodel)
colnames(lcmodel)  
head(lcmodel)
ggplot(lcmodel, aes(L4, value, shape = Var1,                                              #
                     colour = Var2,lty=Var2)) +       ###lty是连连接的参数，这里应该是以每个指标的选项类别作为连接   ###这里以L4为x轴，以后验概率为y轴，以后验概率的分布为分组变量，以生成的分类变量也为分组变量，绘制双变量线性折线图      
  geom_point(size = 4) + geom_line(aes(as.integer(L4),group=Var2),linetype=2) +
  # scale_linetype_manual(values=c("twodash", "dotted"))+
  # # scale_color_manual(values=c('#999999','#E69F00'))+
  # scale_size_manual(values=c(1, 1.5))+
  # theme(axis.text.x = element_text(angle = 45,hjust=1,family = "Times New Roman ",colour = "black",size = rel(1.2)))+
  # scale_x_discrete(labels = c("Lie Exam", "Lie Paper", "Fraud", "Copy Exam")) +                 #
  scale_y_continuous("Probability") +                                                           #
  scale_colour_viridis_d(end =.7) +                                                            #
  theme_cowplot() + 
  labs(x=" ") +                                                               #
  theme(text=element_text(family="Times New Roman", size=12),                                   #
        legend.key.width = unit(.5, "line"),                                                    #
        legend.text = element_text(family="Times New Roman", size=12),                          #
        legend.title = element_blank(),                                                         #
        legend.position = "top",
        axis.text.x = element_text(angle = 45,hjust=1)
        )  +
  # scale_colour_discrete(    ####对图像的图例进行名称和显示的设定
  #   breaks = c("Pr(1)", "Pr(2)"),
  #   labels = c("Pr(No)", "Pr(Yes)"))+
  guides(fill = guide_legend(reverse = TRUE))+
  scale_colour_manual(values=c('#377eb8','#e41a1c'),
                      breaks = c("Pr(1)", "Pr(2)"),
                      labels = c("Pr(No)", "Pr(Yes)"))+   ####  用红与蓝进行颜色的填充，使图像更美观
  facet_grid(Var1 ~ .)


??scale_colour_discrete



lcmodel %>% 
  group_by(Var1)-> t1
library(openxlsx)
write.xlsx(t1,"t1.xlsx")



plot(lc4)

summary(lc4)
lc4$probs
lc4$P
lc4$probs.start
lc4$numiter
lc4$probs.start.ok
lc4$eflag
lc4$Chisq
lc4$time




####第二种绘制条件概率图形


zp1 <- ggplot(lcmodel,aes(x = L4, y = value, fill = 
                            Var2))+
  geom_point()+geom_line(aes(as.integer(Var1)))+
  # geom_bar(stat = "identity", position = "stack")+
  # scale_x_discrete(breaks=c("jtkjR","xdkj", "xdy","xdyq","train_N2","train_N3","train_N4","train_N5","Family_food2","Family_food3","Family_food4"),
  #                  labels=c("trfamspa","famdisspa",
  #                          "houdis","houdisapp",
  #                          "exerfreq2","exerfreq3","exerfreq4","exerfreq5",
  #                          "famfood2","famfood3","famfood4"
  #                  ))+                               ####对横坐标的名称进行替换
  facet_grid(Var1 ~ .)+
  # scale_fill_brewer(type="seq", palette="Greys") +
   scale_fill_manual(values = c("#cccccc","#636363"),        ####对图像的图例进行名称和显示的设定
                     breaks = c("Pr(1)", "Pr(2)"),
                     labels = c("Pr(No)", "Pr(Yes)"))+
  theme_classic()+
  labs(x = "Manifest variables",
       y="Share of item response categories",
       fill ="Response
 category")+
  theme(axis.text.y=element_blank(),
        axis.ticks.y=element_blank(), 
        panel.grid.major.y=element_blank(),
        axis.text.x = element_text(angle = 45,hjust=1,family = "Times New Roman ",colour = "black",size = rel(1.2)))+  ###对横轴名称进行字体、大小、颜色设定
  guides(fill = guide_legend(reverse=TRUE))
  # scale_fill_manual(breaks = c("Pr(1)", "Pr(2)"),
  #                   labels = c("Pr(No)", "Pr(Yes)"))

####生成柱状图

ggplot(lcmodel,aes(x = L4, y = value, fill = 
                     Var2))+
  geom_bar(stat = "identity", position = "stack")+
  # geom_bar(stat = "identity", position = "stack")+
  # scale_x_discrete(breaks=c("jtkjR","xdkj", "xdy","xdyq","train_N2","train_N3","train_N4","train_N5","Family_food2","Family_food3","Family_food4"),
  #                  labels=c("trfamspa","famdisspa",
  #                          "houdis","houdisapp",
  #                          "exerfreq2","exerfreq3","exerfreq4","exerfreq5",
  #                          "famfood2","famfood3","famfood4"
  #                  ))+                               ####对横坐标的名称进行替换
  facet_grid(Var1 ~ .)+
  # scale_fill_brewer(type="seq", palette="Greys") +
  scale_fill_manual(values = c("#cccccc","#636363"),        ####对图像的图例进行名称和显示的设定
                    breaks = c("Pr(1)", "Pr(2)"),
                    labels = c("Pr(No)", "Pr(Yes)"))+
  theme_classic()+
  labs(x = "Manifest variables",
       y="Share of item response categories",
       fill ="Response
 category")+
  theme(axis.text.y=element_blank(),
        axis.ticks.y=element_blank(), 
        panel.grid.major.y=element_blank(),
        axis.text.x = element_text(angle = 45,hjust=1,family = "Times New Roman ",colour = "black",size = rel(1.2)))+  ###对横轴名称进行字体、大小、颜色设定
  guides(fill = guide_legend(reverse=TRUE))
# scale_fill_manual(breaks = c("Pr(1)", "Pr(2)"),
#                   labels = c("Pr(No)", "Pr(Yes)"))   

lc4
###至此已经完成所有变量的计算  ，剩下的就是潜类别回归分析及影响因素分析  


###关于潜在类别变量在人口特征资料的分布，与潜在类别对人群分布特征的影响  

library(poLCA)
f4 <- cbind(trfamspa,famdisspa,houdis ,houdisapp, exerfreq2,exerfreq3,exerfreq4,exerfreq5,famfood2,famfood3,famfood4)~1
LCA4 <- poLCA(f4,dm4,nclass = 4)   ##潜类别变量图形

library(readr)
library(readxl)
library(openxlsx)
dm4$prdclass <- LCA4$predclass  ###此步将分类后的类别写入数据框
write.xlsx(dm4,"dm4.xlsx")  ##此步将分类后的类别保存为excel文件以备后面分析使用
fr1 <- read_excel("final_result20210714.xlsx")
fr2 <- fr1[,31]      ####取出分类后的潜在类别
fr2
dm35 <- dm31[,5]   ####取出BMI值，分类后的BMI值   
dm35   

bc$resident <- factor(bc$resident,levels = c("0","1"),labels = c("农村","城市"))
bc$gender <- factor(bc$gender,levels = c("0","1"),labels = c("男","女"))
bc$education <- factor(bc$education,levels = c("1","2","3","4","5"),
                       labels = c("初中及以下","高中|中专","本科|大专",
                                  "硕士","博士及以上"))
bc$marriage <- factor(bc$marriage,levels = c("1","2","3","4","5"),
                      labels = c("未婚","已婚","离异",
                                 "丧偶","其他"))
bc$income <- factor(bc$income,levels = c("1","2","3","4"),
                    labels = c("2000元以下","2001-5000元","5000-10000元",
                               "10000元及以下"))
bc$czMI <- factor(bc$czMI,levels = c("0","1"),
                  labels = c("无","有"))
bc$syMI <- factor(bc$syMI,levels = c("0","1"),
                  labels = c("无","有"))
bc$diagnose <- factor(bc$diagnose,levels = c("1","2","3"),
                      labels = c("是","否","不清楚"))
bc$sqHOSPITAL <- factor(bc$sqHOSPITAL,levels = c("0","1"),
                        labels = c("无","有"))
bc$online_study <- factor(bc$online_study,levels = c("0","1"),
                          labels = c("无","有"))
bc%>% 
  mutate(job=if_else(professional=="2",
                     true = "1",
                     false = "0"))  %>%      ###此部分把job转换为逻辑值，用数据框再转化回来
  data.frame()-> bc

bc$job <- factor(bc$job,levels = c("0","1"),
                 labels = c("非医务人员","医务人员"))
bc
bc$HS<- factor(bc$HS,levels = c("1","2","3"),
               labels = c("完全健康","亚健康",
                          "存在基础性疾病")) 
bc%>%    ####此年龄划分根据中国的国情进行划分
  mutate(
    Age=case_when(
      age>=18& age<40~1,   ###青年     
      age>=40 & age<65 ~2,    #### 中年
      age>=65~3)       ##老年
  ) -> bc     

bc
glimpse(bc)


bc1 <- bc[,4:17]    ###取出人口基本资料
bc1
glimpse(bc1)
bc2 <- bc1[,-c(2:4,8)]   ###剔除职业这个变量   
bc2
bc3 <- bc[,c("HS","job","Age")]   ######提取出个人健康状态指标，与个人工作种类，医务人员与非医务人员  
bc3
bc4 <- bind_cols(bc2,bc3)     ####将上午清理变量进行合并，最终生成人口基本特征资料
bc4
###将潜类别变量、BMI、与最终的人口特征资料进行合并   

fr3 <-bind_cols(fr2,dm35,bc4) 
fr3
str(fr3)
fr3$prdclass <- factor(fr3$prdclass,levels = c("2","1","3","4"),
                       labels = c("NFHM","LFHM",
                                  "MFHM","AFHM"))    ###对潜在类别进行因子化
fr3$bmi <- factor(fr3$bmi,levels = c("1","2","3","4"),
                  labels = c("underweight","normal_weight",
                             "overweight","obesity"))

fr3$Age <- factor(fr3$Age,levels = c("1","2","3"),
                  labels = c("youth","middle_age",
                             "elderly"))


library(compareGroups)

table <- compareGroups(prdclass~ ., data = fr3,method = c(waist = 3)) ####ref设定比较参数
summary(table[])    ###返回结果中所有的变量
pvals <- getResults(table, "descr")
p.adjust(pvals, method = "BH")
export_table <- createTable(table,show.ratio = TRUE)    ###展示率值

export2word(export_table, file = "table7.docx")

###各变量的统计描述

library(vcd)
summary(fr3)    ###只是简单的描述了变量，没有对行列变量进行百分比计算
library(Hmisc)
Hmisc::describe(fr3)   ####该函数描述了数据的频数百分比



##针对婚姻这个变量不满足卡方检验进行单独卡方检验进行矫正，利用Fisher test进行检验分析

library(gmodels)
dt<- xtabs(~prdclass+bmi,fr3)     ###BMI与潜类别变量的关系,经蒙特卡洛检验bmi与潜类别变量有差异
head(dt)
CrossTable(dt)
chisq.test(dt)
fisher.test(dt,simulate.p.value=TRUE,B=1e5) -> r0
r0$p.value
r0$alternative



dt1<- xtabs(~prdclass+education,fr3)     ##education与潜类别变量的关系，经蒙特卡洛检验与潜类别无差异
head(dt1)
CrossTable(dt1)
chisq.test(dt1)
fisher.test(dt1,simulate.p.value=TRUE,B=1e5)

dt2<- xtabs(~prdclass+marriage,fr3)     ##marriage与潜类别变量的关系，经蒙特卡洛检验与潜类别无差异
head(dt2)
CrossTable(dt2)
chisq.test(dt2)
fisher.test(dt2,simulate.p.value=TRUE,B=1e5) -> r1
r1$p.value



dt3<- xtabs(~prdclass+diagnose,fr3)     #diagnose与潜类别变量的关系，经蒙特卡洛检验与潜类别无差异
head(dt3)
CrossTable(dt3)
chisq.test(dt3)
fisher.test(dt3,simulate.p.value=TRUE,B=1e5) -> r2
r2$p.value

dt4<- xtabs(~prdclass+HS,fr3)     ##HS健康状态与潜类别变量的关系，经蒙特卡洛检验与潜类别有差异
head(dt4)
CrossTable(dt4)
chisq.test(dt4)
fisher.test(dt4,simulate.p.value=TRUE,B=1e5) -> r3
r3$p.value

dt5<- xtabs(~prdclass+gender,fr3)     ##gender性别与潜类别变量的关系，经蒙特卡洛检验与潜类别有差异
head(dt5)
CrossTable(dt5)
chisq.test(dt5)
fisher.test(dt5,simulate.p.value=TRUE,B=1e5) -> r4
r4$p.value



###将潜在类别与个人身体健康状态进行方差分析，探究健康管理对个人健康状态的影响
fr4 <- bind_cols(fr2,d1)
str(fr4)

fr4$prdclass <- factor(fr4$prdclass,fr3$prdclass,levels = c("2","1","3","4"),
                       labels = c("NFHM","LFHM",
                                  "MFHM","AFHM"))    ###对潜在类别进行因子化

fr4
by(fr4$el,fr4$prdclass,shapiro.test)   ####正态性检验
library(car)
leveneTest(fr4$el,fr4$prdclass)    ### 方差齐性检验
oneway.test(el~prdclass ,data=fr4,var.equal = TRUE)   #单因素方差分析

kruskal.test(fr4$el,fr4$prdclass)   ###多组独立样本比较，不满足正态分布，方差齐条件，采用Kruskal-Wallis检验

kruskal.test(el~prdclass ,data=fr4)   ###另一种表示形式，也是Kruskal-Wallis检验    

####多重比较的事后检验    


library(PMCMR)

library(PMCMRplus)

posthoc.kruskal.nemenyi.test(el~prdclass ,data=fr4)
posthoc.kruskal.nemenyi.test(el~prdclass ,data=fr4,dist = "Chisquare")

kruskalTest(el~prdclass ,data=fr4)   ###多组之间非参数比较

kwAllPairsNemenyiTest(el~prdclass ,data=fr4,dist = "Chisquare")

kwAllPairsDunnTest(el~prdclass ,data=fr4,dist="Chisquare",p.adjust.method = "bonferroni")

kwManyOneConoverTest(el~prdclass,fr4,p.adjust.method = "bonferroni")
summary(ans)




kruskalTest(el~prdclass ,data=fr4)   ###多组之间非参数比较
kwAllPairsConoverTest(el~prdclass ,data=fr4,p.adjust.method = "none")     ###有统计学差异的
# kwAllPairsConoverTest(el~prdclass ,data=fr4,p.adjust.method = "bonferroni")   


kruskalTest(p~prdclass ,data=fr4)   ###多组之间非参数比较
kwAllPairsConoverTest(p~prdclass ,data=fr4,p.adjust.method = "none")   

kruskalTest(er~prdclass ,data=fr4)   ###多组之间非参数比较
kwAllPairsConoverTest(er~prdclass ,data=fr4,p.adjust.method = "none")    ###有统计学差异


kruskalTest(s~prdclass ,data=fr4)   ###多组之间非参数比较
kwAllPairsConoverTest(s~prdclass ,data=fr4,p.adjust.method = "none")      

kruskalTest(si~prdclass ,data=fr4)   ###多组之间非参数比较
kwAllPairsConoverTest(si~prdclass ,data=fr4,p.adjust.method = "none")   


kruskalTest(pa~prdclass ,data=fr4)   ###多组之间非参数比较
kwAllPairsConoverTest(pa~prdclass ,data=fr4,p.adjust.method = "none")   







str(fr4)
library(tidyverse)
library(rstatix)
library(ggpubr)

library(cowplot)

fr4 %>% 
  dplyr::rename(energy_level=el,emotional_reaction=er,
                pain=p ,sleep=s,
                 social_isolation=si,physical_abilities=pa) -> fr41



fr41 %>% 
  pivot_longer(-prdclass,names_to = "variables",values_to = "value") -> fr42
fr42




ggboxplot(fr42, x = "prdclass", y = "value",
          color = "prdclass", palette = "jco",
          add = "jitter",
          facet.by = "variables", short.panel.labs = FALSE)+
  theme_cowplot()  +
  theme(legend.position = "none")    ####去除图例


ggboxplot(fr42, x = "prdclass", y = "value",
          color = "prdclass", palette = "jco",
          add = "jitter",
          facet.by = "variables", short.panel.labs = FALSE,
         )+
  theme_cowplot()  







# astr(fr4)
# 
# fr4
# library(tidyverse)
# library(rstatix)
# library(ggpubr)
# 
# fr4 %>% 
#   pivot_longer(-prdclass,names_to = "variables",values_to = "value") -> fr41
# fr41
# stat.test6 <- fr41 %>%
#   group_by(variables) %>%
#   mcnemar_test(value ~ prdclass) %>%             ######
# adjust_pvalue(method = "BH") %>%
#   add_significance() %>% 
#   dplyr::filter(p.adj.signif<0.05)    ####过滤出p值小于0.05的进行画图
# stat.test6   ###进行批量t检验
# 
# ##   关于stat.test非常重要的函数解释
# # # #####Error in stop_ifnot_class(stat.test, .class = names(allowed.tests)) : 
# # stat.test should be an object of class: t_test, wilcox_test, sign_test, dunn_test, emmeans_test, tukey_hsd, games_howell_test, prop_test, fisher_test, chisq_test, exact_binom_test, mcnemar_test, kruskal_test, friedman_test, anova_test, welch_anova_test, chisq_test, exact_multinom_test, exact_binom_test, cochran_qtest, chisq_trend_test
# ##?adjust_pvalue  检查“BH”的矫正意义
# ###画T检验有统计学意义的合成图
# 
# 
# 
# 
# 
# 
# 
# myplot6 <- ggboxplot(
#   fr41, x = "prdclass", y = "value",
#   fill = "prdclass", palette = "npg", legend = "none",
#   ggtheme = theme_pubr(border = TRUE)
# ) +
#   facet_wrap(~variables)
# # Add statistical test p-values
# stat.test6 <- stat.test6 %>% add_xy_position(x = "prdclass")
# myplot6 + stat_pvalue_manual(stat.test6, label = "p.adj.signif")+
#   theme(axis.text.x = element_text(angle = 25,hjust=1)) +  ###x轴标签倾斜45度
#   labs(x="prdclass",y="得分值")
# 
# 
str(bc4)


#####构建潜在类别回归分析方程
bc5 <- bc4[,c(1,2,5,9,11)]
str(bc5)
dm5 <- bind_cols(dm4,bc5,fr4)

colnames(dm5)
dm5

library(poLCA)
f5 <- cbind(trfamspa,famdisspa,houdis ,houdisapp, exerfreq2,exerfreq3,exerfreq4,exerfreq5,famfood2,famfood3,famfood4)~1
LCA5 <- poLCA(f5,dm5,nclass = 4,calc.se = TRUE)   #

lc4

LCA5$P

lc4$prdclass <-lc4$predclass  ###此步将分类后的类别写入数据框
write.xlsx(dm5,"dm54.xlsx")  ##此步将分类后的类别保存为excel文件以备后面分析使用

dm5$prdclass
dm5

# nes2a <- poLCA(f5,dm5,nclass=4,nrep=5) # log-likelihood: -16222.32
# pidmat <- cbind(1,c(1:2))
# pidmat
# exb <- exp(pidmat %*% nes2a$coeff.se)
# exb
# matplot(c(1:2),(cbind(1,exb)/(1+rowSums(exb))),ylim=c(0,1),type="l",
#         main="Party ID as a predictor of candidate affinity class",
#         xlab="Party ID: strong Democratic (1) to strong Republican (6)",
#         ylab="Probability of latent class membership",lwd=2,col=1)
# text(5.9,0.35,"Other")
# 
# text(5.4,0.7,"Bush affinity")
# 
# text(1.8,0.6,"Gore affinity")

glimpse(dm5)



mod<-glm(as.factor(prdclass)~gender+income+sqHOSPITAL+HS,data=dm5,
         family="binomial")
summary(mod)

round(summary(mod)$coefficients,3)

ggplot(dm5,mapping=aes(LCA5$predclass,pain))+
  geom_boxplot(aes(group=LCA5$predclass))
library(mgcv)


dm5$predclass<-LCA5$predclass
prop<-rbind(ctrl=prop.table(table(dat[dm5$trt=="",
]$predclass,
dat[dat$trt=="ctrl",]$outcome),1)[4:6],
trt=prop.table(table(dat[dat$trt=="trt",]$predclass,
                     dat[dat$trt=="trt",]$outcome),1)[4:6])
colnames(prop)<-c('class 1',"class 2","class 3")
barplot(prop,beside =T,
        legend.text=c('ctrl',"trt"),
        ylim = c(0,0.4))








