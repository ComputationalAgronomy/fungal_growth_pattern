library(dplyr)
library(survival)
library(survminer)

data <- read.csv("/Users/test/Downloads/chemotaxis  - chemotaxis_data.csv",header =  TRUE)
data_clean<- data %>% filter(IsNAorNot != TRUE) 

data_clean_wa_4 <- data %>% filter(IsNAorNot != TRUE, Trt == "WA",Date == "4")
data_clean_wa_4$Dist <- as.factor(data_clean_wa_4$Dist)
result <- aov(Dist_I_P.unit.mm.~Dist,data =data_clean_wa_4)
summary(result)
#Df Sum Sq Mean Sq F value   Pr(>F)    
#Dist         4 13.153   3.288   8.079 0.000283 ***
# Residuals   24  9.768   0.407                     
#---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

TukeyHSD(result)[[1]][TukeyHSD(result)[[1]][,4] < 0.05,4] # WA 在第4天時 12mm 的組別 與其他組的長度有顯著差異 
# 12-6         18-12        24-12        30-12 
# 0.0470529227 0.0016041503 0.0002294559 0.0046567871 (p-value)

data_clean_wa_7 <- data %>% filter(IsNAorNot != TRUE, Trt == "WA",Date == "7",Dist !="6")
data_clean_wa_7$Dist <- as.factor(data_clean_wa_7$Dist)
result <- aov(Dist_I_P.unit.mm.~Dist,data =data_clean_wa_7) 
summary(result) 
# WA 在第7天時 排除距離 6mm 處理以外，各組長度沒有顯著差異
# Df Sum Sq Mean Sq F value Pr(>F)
# Dist         3  4.312  1.4374   1.548  0.241
# Residuals   16 14.861  0.9288     

data_clean_wa_11 <- data %>% filter(IsNAorNot != TRUE, Trt == "WA",Date == "11", Dist %in% c("18","24","30"))
data_clean_wa_11$Dist <- as.factor(data_clean_wa_11$Dist)
result <- aov(Dist_I_P.unit.mm.~Dist,data =data_clean_wa_11) 
summary(result)
# WA 在第11天時，排除距離 6mm, 12mm 處理以外，各組長度沒有顯著差異
# Df Sum Sq Mean Sq F value Pr(>F)
# Dist         2  15.10   7.548   2.611  0.109
# Residuals   14  40.47   2.891    

data_clean_wa_14 <- data %>% filter(IsNAorNot != TRUE, Trt == "WA",Date == "14", Dist %in% c("24","30"))
data_clean_wa_14$Dist <- as.factor(data_clean_wa_14$Dist)
result <- aov(Dist_I_P.unit.mm.~Dist,data =data_clean_wa_14) 
summary(result)
# WA 在第14天時，排除距離 6mm, 12mm, 18mm 處理以外，各組長度沒有顯著差異


data_clean_pda_4 <- data %>% filter(IsNAorNot != TRUE, Trt == "PDA",Date == "4")
data_clean_pda_4$Dist <- as.factor(data_clean_pda_4$Dist)
result <- aov(Dist_I_P.unit.mm.~Dist,data =data_clean_pda_4)
summary(result)
TukeyHSD(result)[[1]][TukeyHSD(result)[[1]][,4] < 0.05,4] # PDA 在第4天時 12mm 的組別 與 24mm, 30mm 的長度有顯著差異 


data_clean_pda_7 <- data %>% filter(IsNAorNot != TRUE, Trt == "PDA",Date == "7",Dist !="6")
data_clean_pda_7$Dist <- as.factor(data_clean_pda_7$Dist)
result <- aov(Dist_I_P.unit.mm.~Dist,data =data_clean_pda_7) 
summary(result) 
TukeyHSD(result)[[1]][TukeyHSD(result)[[1]][,4] < 0.05,4]           # PDA 在第7天時 排除距離 6mm 處理以外, 12mm 與 18mm 具有顯著差異 
#     18-12      24-12      30-12      24-18      30-18      30-24 
# 0.03036031 0.87418848 0.13445890 0.48747795 0.85528951 0.81061914

data_clean_pda_11 <- data %>% filter(IsNAorNot != TRUE, Trt == "PDA",Date == "11",Dist %in% c("18","24","30"))
data_clean_pda_11$Dist <- as.factor(data_clean_pda_11$Dist)
result <- aov(Dist_I_P.unit.mm.~Dist,data =data_clean_pda_11) 
summary(result) 

TukeyHSD(result)[[1]][,4] # PDA 在第11天時 排除距離 6mm, 12mm 處理以外， 18mm 與其他組別具有顯著差異
#       24-18        30-18        30-24 
# 0.0009485789 0.0003642361 0.8505429040 

data_clean_pda_14 <- data %>% filter(IsNAorNot != TRUE, Trt == "PDA",Date == "14",Dist %in% c("24","30"))
data_clean_pda_14$Dist <- as.factor(data_clean_pda_14$Dist)
result <- aov(Dist_I_P.unit.mm.~Dist,data =data_clean_pda_14) 
summary(result) 
#Df Sum Sq Mean Sq F value Pr(>F)  
# Dist         1  47.56   47.56    9.54 0.0115 *
# Residuals   10  49.85    4.99                 
#---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# PDA 在第14天時 排除距離 6mm, 12mm, 18mm 處理以外，剩餘兩組具有顯著差異。

data_clean_4 <- data %>% filter(IsNAorNot != TRUE,Date == "4")
data_clean_4$Dist <- as.factor(data_clean_4$Dist)
data_clean_4$Trt <- as.factor(data_clean_4$Trt)
result <- aov(Dist_I_P.unit.mm.~Dist+Trt,data =data_clean_4) 
summary(result) 
TukeyHSD(result)

data_clean_7 <- data %>% filter(IsNAorNot != TRUE,Date == "7",Dist != "6")
data_clean_7$Dist <- as.factor(data_clean_7$Dist)
data_clean_7$Trt <- as.factor(data_clean_7$Trt)
result <- aov(Dist_I_P.unit.mm.~Dist+Trt,data =data_clean_7) 
summary(result) 
TukeyHSD(result)
