library(tidyverse)
library(ggplot2)
library(patchwork)


data <- read.csv(file.choose())
data1 <- data %>% filter(IsNAorNot == "FALSE")  
data1 <- data1 %>% filter(!(Rep == 5 & Trt == "75-150"))
level_set <-c("WA","25","50","75","100","150","200")


#function 
plots=list()
plot_func <- function(trt) {
 for (date in c(4,7,11,15)){
  data1_select <- data1 %>% filter(Trt %in% trt,Date==date)
  data1_select <- data.frame(data1_select)
  data1_select$Trt <- factor(data1_select$Trt,levels=trt)
  data1_select$Plug <- factor(data1_select$Plug,levels = level_set)
  p<-ggplot(data1_select,aes(x = Trt ,y= Dist_I_P.unit.mm.,color = Plug))+
   geom_jitter(position = position_dodge(0.5),
               size = 2,
               alpha = 0.8)+
   geom_hline(yintercept = 12,color="red",size=1)+
   coord_cartesian(ylim=c(0,24))+
   labs(title = date)+
   theme(plot.title = element_text(hjust=0.5))
  
  plots[[as.character(date)]] <- p 
 }
 return(wrap_plots(plots,ncol=4))
}

#不同倍率差異
plot_func(trt=c("25-50","25-75","25-100"))  # base_trt=25
plot_func(trt=c("50-75","50-100","50-150","50-200")) # base_trt=50

#相同倍率差異，但濃度不同
plot_func(trt=c("50-75","100-150")) #1.5X
plot_func(trt=c("25-50","50-100","75-150","100-200")) #2X
plot_func(trt=c("25-75","50-150")) #3X
plot_func(trt=c("25-100","50-200")) #4X

# 相同差距
plot_func(trt=c("25-100","75-150"))

# 統計檢定
# WA
data_wa_4 <- data1 %>% filter(Plug =="WA",Date =="4")
data_wa_4$Trt <- as.factor(data_wa_4$Trt)
result <- aov(Dist_I_P.unit.mm.~Trt,data = data_wa_4)
summary(result) # 在第4天時，50-100 與 100-150, 100-200, 75-150 有顯著差異，可能是人為誤差所造成
PostHocTest(result,method = "lsd")
data_wa_7 <- data1 %>% filter(Plug =="WA",Date =="7")
data_wa_7$Trt <- as.factor(data_wa_7$Trt)
result <- aov(Dist_I_P.unit.mm.~Trt,data = data_wa_7)
summary(result)                                         # 在第7天時處理間並無顯著差異

data_wa_11 <- data1 %>% filter(Plug =="WA",Date =="11")
data_wa_11$Trt <- as.factor(data_wa_11$Trt)
result <- aov(Dist_I_P.unit.mm.~Trt,data = data_wa_11)
summary(result) 
TukeyHSD(result)[[1]][TukeyHSD(result)[[1]][,4]<0.05,4] #在第11天時，50-100 與 25-100, 25-50, 50-200 處理間有無顯著差異
                                                        #50-200 又與 50-150 有 顯著差異

data_wa_15 <- data1 %>% filter(Plug =="WA",Date =="15")
data_wa_15$Trt <- as.factor(data_wa_15$Trt)
result <- aov(Dist_I_P.unit.mm.~Trt,data = data_wa_15)
summary(result)                                         # 在第14天時處理間並無顯著差異

# 比較不同時間點、不同處理組合、單一倍率處理間是否有差異
