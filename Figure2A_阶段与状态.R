# Phase × Status 热图：读取本文件夹 阶段状态数据.xlsx
auto_dir <- function() {
  f <- commandArgs(FALSE)
  ff <- f[grepl("^--file=", f)]
  if (length(ff) > 0) {
    setwd(dirname(normalizePath(sub("^--file=", "", ff[1]))))
  } else if (interactive() && requireNamespace("rstudioapi", quietly = TRUE)) {
    try(setwd(dirname(rstudioapi::getSourceEditorContext()$path)), silent = TRUE)
  }
}
auto_dir()
suppressMessages({
  library(readxl)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
})

if (!file.exists("阶段状态数据.xlsx")) {
  stop("请先把从总表主分析提取的 阶段状态数据.xlsx 放到本文件夹")
}

dat <- read_excel("阶段状态数据.xlsx")
need <- c("试验编号", "阶段", "状态")
if (!all(need %in% names(dat))) stop("阶段状态数据.xlsx 缺少必需列")

# 左侧 Y 轴从上到下：Not applicable, Phase I, Phase I/II, Phase II, Phase II/III, Phase III, Phase IV
phase_levels <- rev(c("Not applicable", "Phase I", "Phase I/II",
                      "Phase II", "Phase II/III", "Phase III", "Phase IV"))
# 下方 X 轴从左到右：Completed, Ongoing, Planned, Closed, Terminated/withdrawn, Unknown
status_levels <- c("Completed", "Ongoing", "Planned",
                   "Closed", "Terminated/withdrawn", "Unknown")

counts <- dat %>%
  count(Phase = factor(`阶段`, levels = phase_levels),
        Status = factor(`状态`, levels = status_levels), .drop = FALSE)

# 指定颜色从多到少：多 -> 少 对应 #6F4C9F -> #B5B5B5
user_colors <- c("#6F4C9F", "#D56C9C", "#7B8CCB", "#B68ACD",
                 "#F59B5E", "#F6D55C", "#B5B5B5")

p <- ggplot(counts, aes(Status, Phase, fill = n)) +
  geom_tile(color = "white", linewidth = 0.6) +
  geom_text(aes(label = n, color = n > 20), size = 3.1) +
  scale_fill_gradientn(colors = rev(user_colors), name = "Trials") +
  scale_color_manual(values = c("FALSE" = "black", "TRUE" = "white"),
                     guide = "none") +
  labs(x = "Trial status", y = "Phase",
       title = "Phase and status distribution",
       subtitle = "Main analysis") +
  theme_minimal(base_size = 11) +
  theme(axis.text.x = element_text(angle = 20, hjust = 1),
        panel.grid = element_blank(),
        axis.text = element_text(color = "black"),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, color = "grey35"))

ggsave("阶段状态图.pdf", p, width = 9, height = 6, device = cairo_pdf)
ggsave("阶段状态图.png", p, width = 9, height = 6, device = ragg::agg_png, dpi = 300)
cat("已输出：阶段状态图.pdf / 阶段状态图.png\n")

