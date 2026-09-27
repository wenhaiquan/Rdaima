# Top 12 单抗同心进度环图
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

if (!file.exists("Top抗体数据.xlsx")) {
  stop("请先把从总表主分析提取的 Top抗体数据.xlsx 放到本文件夹")
}

dat <- read_excel("Top抗体数据.xlsx")
need <- c("试验编号", "抗体")
if (!all(need %in% names(dat))) stop("Top抗体数据.xlsx 缺少必需列")

# 原始色板，不替换颜色
user_pal <- c("#6F4C9F", "#D56C9C", "#7B8CCB", "#B68ACD",
              "#F59B5E", "#F6D55C", "#9E9E9E")

# 每项试验每种抗体计 1 次，取 Top 12
ab_df <- dat %>%
  filter(!is.na(`抗体`), trimws(`抗体`) != "") %>%
  separate_rows(`抗体`, sep = ";\\s*|；|\\r?\\n") %>%
  filter(trimws(`抗体`) != "", `抗体` != "N/A") %>%
  transmute(`试验编号`, Antibody = trimws(`抗体`)) %>%
  distinct() %>%
  count(Antibody, name = "n") %>%
  arrange(desc(n), Antibody) %>%
  slice_head(n = 12)

if (nrow(ab_df) == 0) stop("Top抗体数据.xlsx 中没有可用的抗体")

# 分段刻度：0、10、20、40、80、160，每段圆周角相等为 72°
# 160 与 0 共用同一条 12 点方向的半径线，但图中不单独显示 160
piecewise_angle <- function(x) {
  breaks <- c(0, 10, 20, 40, 80, 160)
  angles <- seq(0, 360, length.out = length(breaks))
  
  vapply(x, function(v) {
    v <- min(max(v, 0), 160)
    j <- max(which(breaks <= v))
    
    if (j >= length(breaks)) return(360)
    
    lo <- breaks[j]
    hi <- breaks[j + 1]
    a_lo <- angles[j]
    a_hi <- angles[j + 1]
    
    a_lo + (v - lo) / (hi - lo) * (a_hi - a_lo)
  }, numeric(1))
}

# 每个抗体一个同心圆环，Top1 在最外层
ab_df$Rank <- seq_len(nrow(ab_df))
ring_width <- 0.72
ring_gap <- 0.35
outer_max <- 1 + nrow(ab_df) * (ring_width + ring_gap)

# 数值标签相对圆环末端的偏移角度（度），越大越往前挪
label_offset <- 4

ab_df <- ab_df %>%
  mutate(
    r_outer = outer_max - (Rank - 1) * (ring_width + ring_gap),
    r_inner = r_outer - ring_width,
    end_angle = piecewise_angle(n),
    label_y = (r_inner + r_outer) / 2,
    label_x = pmin(end_angle + label_offset, 355)
  )

# 图例顺序：数量从多到少
ab_df$Antibody <- factor(ab_df$Antibody, levels = ab_df$Antibody)

# 完整底环：浅灰色
track_col <- "#EDEDED"
ring_base <- ab_df %>%
  transmute(
    xmin = 0,
    xmax = 360,
    ymin = r_inner,
    ymax = r_outer
  )

# 等角刻度线：0 位于 12 点方向；160 与 0 重合但不单独显示
axis_df <- data.frame(
  angle = c(0, 72, 144, 216, 288),
  label = c("0", "10", "20", "40", "80")
)

p <- ggplot() +
  # 完整底环（浅灰）
  geom_rect(
    data = ring_base,
    aes(
      xmin = xmin,
      xmax = xmax,
      ymin = ymin,
      ymax = ymax
    ),
    fill = track_col,
    color = NA
  ) +
  # 彩色进度弧段，从 12 点方向开始顺时针增长
  geom_rect(
    data = ab_df,
    aes(
      xmin = 0,
      xmax = end_angle,
      ymin = r_inner,
      ymax = r_outer,
      fill = Antibody
    ),
    color = NA
  ) +
  # 从圆心发出的等角刻度线
  geom_segment(
    data = axis_df,
    aes(
      x = angle,
      xend = angle,
      y = 0,
      yend = outer_max * 1.06
    ),
    color = "#9E9E9E",
    linewidth = 0.35,
    linetype = "dashed",
    alpha = 0.8
  ) +
  # 刻度标签，只显示 0、10、20、40、80
  geom_text(
    data = axis_df,
    aes(
      x = angle,
      y = outer_max * 1.08,
      label = label
    ),
    size = 2.7,
    color = "#555555",
    fontface = "bold"
  ) +
  # 每根圆环末端写具体数值
  geom_text(
    data = ab_df,
    aes(
      x = label_x,
      y = label_y,
      label = n
    ),
    size = 2.9,
    color = "#222222",
    fontface = "bold"
  ) +
  scale_fill_manual(
    values = setNames(
      colorRampPalette(user_pal)(nrow(ab_df)),
      levels(ab_df$Antibody)
    ),
    breaks = levels(ab_df$Antibody),
    name = "Monoclonal antibody"
  ) +
  scale_x_continuous(
    limits = c(0, 360),
    breaks = NULL,
    expand = c(0, 0)
  ) +
  scale_y_continuous(
    limits = c(0, outer_max * 1.18),
    breaks = NULL,
    expand = c(0, 0)
  ) +
  coord_polar(
    theta = "x",
    start = 0,
    direction = 1,
    clip = "off"
  ) +
  labs(
    title = "Most frequently investigated monoclonal antibodies",
    subtitle = "Main analysis; top 12 antibodies by occurrence count"
  ) +
  theme_void(base_size = 11) +
  theme(
    legend.position = "left",
    legend.title = element_text(face = "bold", size = 10),
    legend.text = element_text(size = 9),
    legend.key.height = grid::unit(0.42, "cm"),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, color = "grey35"),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    legend.background = element_rect(fill = "white", color = NA),
    legend.key = element_rect(fill = "white", color = NA),
    plot.margin = margin(10, 90, 10, 90)
  )

ggsave("Top抗体图.pdf", p, width = 10, height = 7, device = cairo_pdf)
ggsave("Top抗体图.png", p, width = 10, height = 7,
       device = ragg::agg_png, dpi = 300)

cat("已输出：Top抗体图.pdf / Top抗体图.png\n")

