# 年度已知年份趋势图：堆叠柱状图 + 总量折线
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

if (!file.exists("年度趋势数据.xlsx")) {
  stop("请先把 年度趋势数据.xlsx 放到本文件夹")
}

dat <- read_excel("年度趋势数据.xlsx")
need <- c("试验编号", "年份", "阶段")
if (!all(need %in% names(dat))) stop("年度趋势数据.xlsx 缺少必需列")

phase_order <- c("Phase I", "Phase I/II", "Phase II", "Phase II/III",
                 "Phase III", "Phase IV", "Not applicable")

# 固定颜色映射：按指定顺序一一对应
phase_cols <- c(
  "Phase IV"       = "#6F4C9F",
  "Phase III"      = "#D56C9C",
  "Phase II/III"   = "#7B8CCB",
  "Phase II"       = "#B68ACD",
  "Phase I/II"     = "#F59B5E",
  "Phase I"        = "#F6D55C",
  "Not applicable" = "#9E9E9E"
)

dat <- dat %>%
  filter(!is.na(`年份`), `阶段` %in% phase_order) %>%
  mutate(Year = as.integer(`年份`))

if (nrow(dat) == 0) stop("年度趋势数据.xlsx 中没有已知年份记录")

counts <- dat %>%
  count(Year, Phase = as.character(`阶段`))

year_grid <- seq(min(dat$Year), max(dat$Year), by = 1)
counts <- counts %>%
  complete(Year = year_grid, Phase = phase_order, fill = list(n = 0)) %>%
  filter(Phase %in% phase_order) %>%
  mutate(Phase = factor(Phase, levels = phase_order))  # 堆叠顺序按临床阶段顺序

totals <- counts %>%
  group_by(Year) %>%
  summarise(n = sum(n), .groups = "drop")

p <- ggplot(counts, aes(Year, n, fill = Phase)) +
  geom_col(position = "stack", width = 0.76,
           color = "white", linewidth = 0.2) +
  geom_line(data = totals, aes(Year, n, group = 1), inherit.aes = FALSE,
            color = "#222222", linewidth = 0.7) +
  geom_point(data = totals, aes(Year, n), inherit.aes = FALSE,
             color = "#222222", size = 1.5) +
  geom_text(data = totals, aes(Year, n, label = n), inherit.aes = FALSE,
            vjust = -0.55, size = 2.7, color = "#222222", fontface = "bold") +
  scale_fill_manual(values = phase_cols, breaks = names(phase_cols), drop = FALSE) +
  scale_x_continuous(breaks = seq(min(dat$Year), max(dat$Year), by = 2)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.12))) +
  labs(x = "Start year", y = "Trials",
       title = "Annual trial starts by phase",
       subtitle = paste0("Main analysis; start year known, n = ", nrow(dat))) +
  theme_minimal(base_size = 11) +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        axis.text = element_text(color = "black"),
        legend.position = "bottom",
        legend.key.size = grid::unit(0.4, "cm"),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, color = "grey35"))

ggsave("年度趋势图.pdf", p, width = 10, height = 6, device = cairo_pdf)
ggsave("年度趋势图.png", p, width = 10, height = 6,
       device = ragg::agg_png, dpi = 300)
cat("已输出：年度趋势图.pdf / 年度趋势图.png\n")

