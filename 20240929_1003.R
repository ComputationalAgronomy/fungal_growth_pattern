library(tidyverse)
library(ggplot2)
library(patchwork)
data <- read.csv(file.choose())
level_set <-c("25","50","75","100","150","200")
head(data)
plots=list()

#Plug to Food Source
plot_func <- function(Trt) {
 for (Date in unique(data$date)){
  data_select <- data %>% filter(trt %in% Trt,date==Date)
  data_select <- data.frame(data_select)
  data_select$trt <- factor(data_select$trt,levels=Trt)
  data_select$conc. <- factor(data_select$conc.,levels = level_set)
  p<-ggplot(data_select,aes(x = trt , y = extend, color = conc.))+
   geom_jitter(position = position_dodge(0.5),
               size = 2,
               alpha = 0.8)+
   geom_hline(yintercept = 12,color="red",size=1)+
   coord_cartesian(ylim=c(0,24))+
   labs(title = Date)+
   theme(plot.title = element_text(hjust=0.5))
  
  plots[[as.character(Date)]] <- p 
 }
 return(wrap_plots(plots,ncol=4))
}

plot_func(c("25-100","25-150")) # 4X , 6X
plot_func(c("50-100","50-150")) # 2X , 3X 
plot_func(c("50-100","75-100")) # 2X , 1.33X
plot_func(c("75-100","50-100","50-150","25-100","25-150"))

data$extend <- data$Innoculate_to_plug.mm.+data$plug_to_edge.mm.


data_25150_3 <- data %>% filter(trt =="25-150",date=="3")
summary(aov(extend~conc.,data = data_25150_3))
data_25150_5 <- data %>% filter(trt =="25-150",date=="5")
summary(aov(extend~conc.,data = data_25150_5))
data_25150_6 <- data %>% filter(trt =="25-150",date=="6")
summary(aov(extend~conc.,data = data_25150_6))
data_25150_10 <- data %>% filter(trt =="25-150",date=="10")
summary(aov(extend~conc.,data = data_25150_10))
