# 按真实频数绘制 Era -> Target -> Phase 冲积图，不改变任何 stratum 的面积比例。
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

if (!requireNamespace("ggalluvial", quietly = TRUE)) {
  stop("请先安装 ggalluvial 包")
}
if (!requireNamespace("ggrepel", quietly = TRUE)) {
  stop("请先安装 ggrepel 包")
}

suppressMessages({
  library(readxl)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(ggalluvial)
  library(ggrepel)
})

if (!file.exists("冲积图数据.xlsx")) {
  stop("请把冲积图数据.xlsx 放到本文件夹")
}

dat <- read_excel("冲积图数据.xlsx")
need <- c("试验编号", "年份", "靶点类别", "阶段")
if (!all(need %in% names(dat))) {
  stop("冲积图数据.xlsx 缺少必需列")
}
dat$`年份` <- suppressWarnings(as.numeric(dat$`年份`))

era_fun <- function(y) {
  ifelse(
    is.na(y), "Unknown year",
    ifelse(
      y <= 2005, "1997-2005",
      ifelse(
        y <= 2010, "2006-2010",
        ifelse(y <= 2015, "2011-2015", ifelse(y <= 2020, "2016-2020", "2021-2026"))
      )
    )
  )
}

target_levels <- c("IgE", "IL-5/IL-5Rα", "IL-4Rα/IL-13",
                   "TSLP", "IL-33/ST2", "Other")
era_levels <- c("1997-2005", "2006-2010", "2011-2015",
                "2016-2020", "2021-2026", "Unknown year")
phase_levels <- c("Phase I", "Phase I/II", "Phase II", "Phase II/III",
                  "Phase III", "Phase IV", "Not applicable")
user_pal <- c("#6F4C9F", "#D56C9C", "#7B8CCB", "#B68ACD",
              "#F59B5E", "#F6D55C", "#9E9E9E")

long <- dat %>%
  filter(!is.na(`靶点类别`), trimws(`靶点类别`) != "") %>%
  mutate(Era = era_fun(`年份`)) %>%
  separate_rows(`靶点类别`, sep = ";\\s*|；|\\r?\\n") %>%
  filter(trimws(`靶点类别`) != "", `靶点类别` %in% target_levels) %>%
  transmute(
    `试验编号`,
    Era = factor(Era, levels = era_levels),
    Target = trimws(`靶点类别`),
    Phase = factor(`阶段`, levels = phase_levels)
  ) %>%
  distinct()

target_order <- long %>%
  count(Target, name = "n") %>%
  arrange(desc(n), Target) %>%
  pull(Target)

long <- long %>%
  mutate(Target = factor(Target, levels = target_order))

cols <- setNames(
  colorRampPalette(user_pal)(length(target_order)),
  target_order
)

unknown_n <- sum(is.na(dat$`年份`))

# 每条记录固定权重为 1，保证 stratum 高度和 ribbon 宽度与频数严格成比例。
p <- ggplot(
  long,
  aes(y = 1, axis1 = Era, axis2 = Target, axis3 = Phase)
) +
  geom_alluvium(
    aes(fill = Target),
    width = 0.18,
    alpha = 0.68,
    colour = NA,
    curve_type = "sine"
  ) +
  geom_stratum(
    width = 0.18,
    fill = "#F8F6FA",
    colour = "#555555",
    linewidth = 0.35
  ) +
  scale_fill_manual(
    values = cols,
    breaks = target_order,
    name = "Target"
  ) +
  scale_x_discrete(
    limits = c("Start year era", "Target", "Phase"),
    expand = c(0.08, 0.08)
  ) +
  scale_y_continuous(expand = c(0.02, 0.02)) +
  labs(
    title = "Evolution of severe asthma mAb trials by era, target, and phase",
    subtitle = paste0(
      "Main analysis; n = ", nrow(dat), " trials; ",
      nrow(long), " target-category mentions; ",
      unknown_n, " trials without a recorded start year"
    ),
    y = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    legend.text = element_text(size = 10),
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(color = "grey35", size = 10),
    plot.margin = margin(12, 95, 18, 95)
  ) +
  coord_cartesian(clip = "off")

# 从 stratum 图层读取真实位置；大格子内写标签，小格子才用引线外置。
strata <- as.data.frame(ggplot_build(p)$data[[2]])
count_col <- intersect(c("count", "n"), names(strata))[1]
if (length(count_col) == 0L || is.na(count_col)) {
  stop("无法从 ggalluvial stratum 图层读取计数")
}

strata$y_mid <- (strata$ymin + strata$ymax) / 2
strata$n_label <- as.integer(round(strata[[count_col]]))

phase_short <- c(
  "Phase I" = "I",
  "Phase I/II" = "I/II",
  "Phase II" = "II",
  "Phase II/III" = "II/III",
  "Phase III" = "III",
  "Phase IV" = "IV",
  "Not applicable" = "N/A"
)

strata$label_inside <- ifelse(
  strata$x >= 2.5,
  paste0(phase_short[as.character(strata$stratum)], " (n = ", strata$n_label, ")"),
  paste0(strata$stratum, "\n(n = ", strata$n_label, ")")
)
strata$label_outside <- ifelse(
  strata$x >= 2.5,
  paste0(as.character(strata$stratum), " (n = ", strata$n_label, ")"),
  paste0(strata$stratum, " (n = ", strata$n_label, ")")
)

inside_min_n <- 4
inside_labels <- strata[strata$n_label >= inside_min_n, ]
outside_labels <- strata[strata$n_label < inside_min_n, ]
outside_left <- outside_labels[outside_labels$x < 2.5, ]
outside_right <- outside_labels[outside_labels$x >= 2.5, ]

p <- p +
  geom_text(
    data = inside_labels,
    aes(x = x, y = y_mid, label = label_inside),
    inherit.aes = FALSE,
    size = 3.0,
    lineheight = 0.9,
    color = "#222222"
  )

if (nrow(outside_left) > 0) {
  p <- p +
    geom_text_repel(
      data = outside_left,
      aes(x = x, y = y_mid, label = label_outside),
      inherit.aes = FALSE,
      direction = "y",
      nudge_x = -0.12,
      hjust = 1,
      size = 3.0,
      lineheight = 0.9,
      color = "#222222",
      bg.color = "white",
      bg.r = 0.12,
      box.padding = 0.30,
      point.padding = 0.10,
      min.segment.length = 0,
      segment.color = "#9CA3AF",
      segment.size = 0.25,
      max.overlaps = Inf,
      seed = 20260912
    )
}

if (nrow(outside_right) > 0) {
  p <- p +
    geom_text_repel(
      data = outside_right,
      aes(x = x, y = y_mid, label = label_outside),
      inherit.aes = FALSE,
      direction = "y",
      nudge_x = 0.12,
      hjust = 0,
      size = 3.0,
      lineheight = 0.9,
      color = "#222222",
      bg.color = "white",
      bg.r = 0.12,
      box.padding = 0.30,
      point.padding = 0.10,
      min.segment.length = 0,
      segment.color = "#9CA3AF",
      segment.size = 0.25,
      max.overlaps = Inf,
      seed = 20260912
    )
}

ggsave("冲积图2.pdf", p, width = 16, height = 12, device = cairo_pdf)
ggsave("冲积图2.png", p, width = 16, height = 12,
       device = ragg::agg_png, dpi = 300)
cat("已输出：冲积图2.pdf / 冲积图2.png\n")

