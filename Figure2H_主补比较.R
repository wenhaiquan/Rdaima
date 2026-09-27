# ============================================================
# 主分析 vs 补充分析靶点趋势图
# 读取本文件夹 主补靶点数据.xlsx
# ============================================================

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

if (!file.exists("主补靶点数据.xlsx")) {
  stop("请先把 主补靶点数据.xlsx 放到本文件夹")
}

dat <- read_excel("主补靶点数据.xlsx")
if (ncol(dat) < 4) stop("主补靶点数据.xlsx 至少需要 4 列")

names(dat)[1:4] <- c("Trial_ID", "Analysis", "Target_Category", "Start_Year")
dat$Analysis <- enc2utf8(as.character(dat$Analysis))
dat$Target_Category <- gsub("\u03b1", "alpha",
                            enc2utf8(as.character(dat$Target_Category)),
                            fixed = TRUE)
dat$Group <- ifelse(dat$Analysis == "\u4e3b\u5206\u6790",
                    "Main analysis", "Supplementary analysis")
dat$Start_Year <- suppressWarnings(as.numeric(dat$Start_Year))

era_levels <- c("1997-2010", "2011-2020", "2021-2026", "Unknown start year")
dat$Era <- case_when(
  is.na(dat$Start_Year) ~ "Unknown start year",
  dat$Start_Year <= 2010 ~ "1997-2010",
  dat$Start_Year <= 2020 ~ "2011-2020",
  TRUE ~ "2021-2026"
)
dat$Era <- factor(dat$Era, levels = era_levels)

target_keys <- c("IgE", "IL-5/IL-5Ralpha", "IL-4Ralpha/IL-13",
                 "TSLP", "IL-33/ST2", "Other")
target_labels <- c("IgE", "IL-5/IL-5R\u03b1", "IL-4R\u03b1/IL-13",
                   "TSLP", "IL-33/ST2", "Other")
target_cols <- setNames(
  c("#C372A5", "#6F4C9F", "#DB948A", "#928BCB", "#9E9E9E", "#F5C95C"),
  target_keys
)

long <- dat %>%
  filter(!is.na(Target_Category), trimws(Target_Category) != "") %>%
  separate_rows(Target_Category, sep = ";\\s*|\\r?\\n") %>%
  filter(trimws(Target_Category) != "") %>%
  transmute(Trial_ID, Group, Era, Target = trimws(Target_Category)) %>%
  distinct() %>%
  filter(Target %in% target_keys)

mentions_n <- long %>%
  count(Group, Era, name = "mentions") %>%
  complete(Group, Era, fill = list(mentions = 0))

counts <- long %>%
  count(Group, Era, Target, name = "n") %>%
  complete(Group, Era, Target, fill = list(n = 0)) %>%
  left_join(mentions_n, by = c("Group", "Era")) %>%
  mutate(pct = ifelse(mentions > 0, n / mentions * 100, 0))

group_levels <- c("Main analysis", "Supplementary analysis")
counts$Group   <- factor(counts$Group,   levels = group_levels)
counts$Target  <- factor(counts$Target,  levels = target_keys)
counts$Era     <- factor(counts$Era,     levels = era_levels)
mentions_n$Group <- factor(mentions_n$Group, levels = group_levels)
mentions_n$Era   <- factor(mentions_n$Era,   levels = era_levels)

# ---------- 每段累计位置 ----------
counts <- counts %>%
  group_by(Group, Era) %>%
  arrange(Target, .by_group = TRUE) %>%
  mutate(
    total    = sum(pct),
    fraction = ifelse(total > 0, pct / total, 0),
    ymax     = cumsum(fraction),
    ymin     = lag(ymax, default = 0),
    ymid     = (ymin + ymax) / 2
  ) %>%
  ungroup()

# ---------- 位置 ----------
era_centers <- setNames(c(1, 3, 5, 7), era_levels)

counts$xcenter <- era_centers[as.character(counts$Era)]
counts$xpos    <- counts$xcenter +
  ifelse(counts$Group == "Main analysis", -0.21, 0.21)

# ---------- 顶部 n 标签：拆成两个独立标签，各自贴柱子 ----------
top_labels <- mentions_n %>%
  mutate(
    xpos = era_centers[as.character(Era)] +
      ifelse(Group == "Main analysis", -0.21, 0.21),
    lab  = paste0(ifelse(Group == "Main analysis", "M", "S"),
                  " n = ", mentions)
  )

# ---------- 所有百分比统一白色 ----------
counts$text_col <- "white"

# ---------- 绘图 ----------
p <- ggplot() +
  geom_rect(
    data = counts,
    aes(xmin = xpos - 0.18, xmax = xpos + 0.18,
        ymin = ymin, ymax = ymax, fill = Target),
    color = "white", linewidth = 0.3
  ) +
  # 所有片段都显示百分比，全部白色
  geom_text(
    data = counts,
    aes(x = xpos, y = ymid,
        label = ifelse(pct > 0, sprintf("%.1f%%", pct), ""),
        color = text_col),
    size = 2.0, fontface = "bold"
  ) +
  # 顶部 n 标签：两个独立标签，各自贴在柱子正上方
  geom_text(
    data = top_labels,
    aes(x = xpos, y = 1.02, label = lab),
    inherit.aes = FALSE, size = 2.7, color = "#222222", fontface = "bold"
  ) +
  geom_hline(yintercept = 0, color = "#777777", linewidth = 0.35) +
  geom_vline(xintercept = 6, linetype = 2,
             color = "#B5B5B5", linewidth = 0.4) +
  scale_fill_manual(values = target_cols, labels = target_labels,
                    breaks = target_keys, drop = FALSE, name = "Target") +
  scale_color_identity() +
  scale_x_continuous(
    breaks = c(1, 3, 5, 7),
    labels = c("1997-2010", "2011-2020", "2021-2026",
               "Unknown\nstart year"),
    limits = c(0.3, 7.7),
    expand = c(0, 0)
  ) +
  scale_y_continuous(
    breaks = seq(0, 1, 0.25),
    labels = function(x) paste0(x * 100, "%"),
    expand = expansion(mult = c(0, 0.12))
  ) +
  coord_cartesian(ylim = c(0, 1.10), clip = "off") +
  labs(
    x = "Start year period",
    y = "Share of target category mentions",
    title = "Target composition over start-year periods",
    subtitle = "M = Main analysis; S = Supplementary analysis"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    panel.grid.minor   = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.text = element_text(color = "black"),
    axis.text.x = element_text(color = "black"),
    plot.title    = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, color = "grey35"),
    legend.position = "right"
  )

print(p)
ggsave("主补靶点趋势图.pdf", p, width = 13, height = 6.6, device = cairo_pdf)
ggsave("主补靶点趋势图.png", p, width = 13, height = 6.6,
       device = ragg::agg_png, dpi = 300)
cat("已输出：主补靶点趋势图.pdf / 主补靶点趋势图.png\n")

