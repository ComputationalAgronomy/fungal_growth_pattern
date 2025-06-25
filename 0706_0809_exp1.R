library(dplyr)
library(ggplot2)

library(patchwork)

# 用 patchwork 組合圖表，設定 2x3 佈局



data <- read.csv("/Users/test/Downloads/chemotaxis  - chemotaxis_data.csv")
data %>% filter(IsNAorNot != TRUE)

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
      x = "Date",
      y = "Hyphal Length (mm)"
    ) +
    scale_color_manual(values = c("WA" = "#1f77b4", "PDA" = "#ff7f0e")) +
    scale_y_continuous(limits = c(0, 30), breaks = seq(0, 30, by = 6))
})

invisible(lapply(plots, print))
combined_plot <- wrap_plots(plots, nrow = 2, ncol = 3)
combined_plot
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


