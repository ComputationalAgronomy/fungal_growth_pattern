library(dplyr)
library(ggplot2)

library(patchwork)

# 用 patchwork 組合圖表，設定 2x3 佈局


# data <- read.csv("/Users/test/Downloads/chemotaxis  - chemotaxis_data.csv")
data <- read.csv("raw_data/chemotaxis_data 240706_240809(revised).csv")
data <- data[, c("Date", "Dist", "Rep", "Trt", "Dist_I_P.unit.mm.")]
# data %>% filter(IsNAorNot != TRUE)
remove_index <- data[["Dist"]]=="24" & data[["Date"]]=="7"
data[remove_index, "Dist_I_P.unit.mm."] <- NA


data$Date <- as.factor(data$Date)
# 針對每個 Dist 繪製圖表
unique_dists <- unique(data$Dist)
#I-P
plots <- lapply(unique_dists, function(dist_value) {
  data %>%
    filter(Dist == dist_value) %>%
    ggplot(aes(x = Date, y = Dist_I_P.unit.mm., color = Trt)) +
    geom_point(size = 3, position = position_dodge(width = 0.2)) +
    geom_hline(yintercept = dist_value, linetype = "dashed", color = "red") +  # 加入水平線
    labs(
      title = paste("Distance from Plug to Food Source:", dist_value, "mm"),
      x = "Day",
      y = "Growth (mm)"
    ) +
    scale_color_manual(values = c("WA" = "#1f77b4", "PDA" = "#ff7f0e")) +
    scale_y_continuous(limits = c(0, 30), breaks = seq(0, 30, by = 6))
})

invisible(lapply(plots, print))
combined_plot <- wrap_plots(plots, nrow = 2, ncol = 3)


png("dist_plug_food.png", width = 4500, height = 3000, res = 300)
print(combined_plot)
dev.off()




data_dist <- split(data, data$Dist)

summary(data_dist[["12"]])

results_dist <- list()
results_anova <- list()
for (dd in names(data_dist)) {
  data_d <- data_dist[[dd]]
  results_anova[[dd]] <- aov(Dist_I_P.unit.mm.~Trt*Date, data = data_d)
  data_day <- split(data_d, data_d$Date)
  results_dist[[dd]] <- sapply(data_day, function(df) {
    tryCatch({
      t.test(Dist_I_P.unit.mm.~Trt, data = df)$p.value
    }, error = function(e) {
      NA
    })
  })
  results_dist[[sprintf("%s_adj", dd)]] <- p.adjust(results_dist[[dd]], method = "fdr")
}

sapply(results_anova, function(x){
  summary(x)
})

sapply(results_dist[-grep("adj",names(results_dist))], function(x){
  prettyNum(x, digits = 2, format = "f")
})

sapply(results_dist[grep("adj",names(results_dist))], function(x){
  prettyNum(x, digits = 2, format = "f")
})

dist_day_index <- list(
  "12" = c(4),
  "18" = c(4, 7),
  "24" = c(4),
  "30" = c(4, 7, 11, 14)
)
results_prior <- sapply(names(dist_day_index), function(day) {
  index <- seq_along(dist_day_index[[day]])
  results_dist[[day]][index]
})


rbind(
  # prettyNum(unlist(results_prior), digits = 3, format = "g"),
  # prettyNum(p.adjust(unlist(results_prior), method = "fdr"), digits = 3, format = "g")
  format.pval(unlist(results_prior), digits = 2, eps= 1e-3),
  format.pval(p.adjust(unlist(results_prior), method = "fdr"), digits = 2, eps= 1e-3)
)


all_p <- lapply(results_dist, function(x){
    # prettyNum(x, digits = 2, format = "f")
    x
})


TH <- 0.001
for (dd in names(data_dist)) {
  p0 <- all_p[[dd]]
  padj <- all_p[[sprintf("%s_adj", dd)]]

  p0[p0>TH] <- sprintf("%.3f", p0[p0>TH])
  p0[p0<TH] <- sprintf("<%s", TH)

  padj[padj>TH] <- sprintf("%.3f", padj[padj>TH])
  padj[padj<TH] <- sprintf("<%s", TH)

  ss <- sprintf("%.3E (adj: %.3e)", p0, padj)
  ss <- sprintf("%s (adj: %s)", p0, padj)
  paste(ss, collapse = " | ")
}




output_table <- do.call(rbind, results_dist[grep("adj",names(results_dist))])
prettyNum(output_table, digits = 2, format = "f")
                4            7           11           14
6_adj  0.04405699 0.3203385732 0.3203385732 0.5558429676
12_adj 0.19347568 0.5273752127 0.5273752127 0.5273752127
18_adj 0.02425170 0.0015452486 0.9482453373 0.9482453373
24_adj 0.01242765 0.1031164347 0.0002581105 0.3101720093
30_adj 0.17601706 0.0006109437 0.0002588198 0.0002402349



data_clean_wa_4 <- data %>% filter(IsNAorNot != TRUE, Trt == "WA",Date == "4")
data_clean_wa_4$Dist <- as.factor(data_clean_wa_4$Dist)
result <- aov(Dist_I_P.unit.mm.~Trt*Date,data =data_dist[["18"]])
summary(result)


summary(result)
#Df Sum Sq Mean Sq F value   Pr(>F)
#Dist         4 13.153   3.288   8.079 0.000283 ***
# Residuals   24  9.768   0.407
#---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

TukeyHSD(result)[[2]][TukeyHSD(result)[[2]][,4] < 0.05,4] # WA 在第4天時 12mm 的組別 與其他組的長度有顯著差異
TukeyHSD(result)[[1]][TukeyHSD(result)[[1]][,4] < 0.05,4] # WA 在第4天時 12mm 的組別 與其他組的長度有顯著差異
# 12-6         18-12        24-12        30-12
# 0.0470529227 0.0016041503 0.0002294559 0.0046567871 (p-value)



#P-E
plots <- lapply(unique_dists, function(dist_value) {
  data %>%
    filter(Dist == dist_value) %>%
    ggplot(aes(x = Date, y = Dist_P_E.unit.mm., color = Trt)) +
    geom_point(size = 3, position = position_dodge(width = 0.2)) +
    labs(
      title = paste("Distance from Fungal Colony to Food Source:", dist_value, "mm"),
      x = "Date",
      y = "Hyphal Length (mm)"
    ) +
    scale_color_manual(values = c("WA" = "#1f77b4", "PDA" = "#ff7f0e")) +
    scale_y_continuous(limits = c(0, 30), breaks = seq(0, 30, by = 6))
})

combined_plot <- wrap_plots(plots, nrow = 2, ncol = 3)
combined_plot


