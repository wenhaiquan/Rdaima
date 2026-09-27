# Era x Target 靶点演变图：读取本文件夹 靶点演变数据.xlsx
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

if (!file.exists("靶点演变数据.xlsx")) {
  stop("请先把从总表主分析提取的 靶点演变数据.xlsx 放到本文件夹")
}

dat <- read_excel("靶点演变数据.xlsx")
need <- c("试验编号", "年份", "靶点类别")
if (!all(need %in% names(dat))) stop("靶点演变数据.xlsx 缺少必需列")

# 时间相关图只使用已知起始年份
dat <- dat %>%
  filter(!is.na(`年份`), !is.na(`靶点类别`), trimws(`靶点类别`) != "")

era_fun <- function(y) {
  ifelse(y <= 2005, "1997-2005",
         ifelse(y <= 2010, "2006-2010",
                ifelse(y <= 2015, "2011-2015",
                       ifelse(y <= 2020, "2016-2020", "2021-2026"))))
}

target_levels_all <- c("IgE", "IL-5/IL-5Rα", "IL-4Rα/IL-13",
                       "TSLP", "IL-33/ST2", "Other")
user_pal <- c("#6F4C9F", "#D56C9C", "#7B8CCB", "#B68ACD",
              "#F59B5E", "#F6D55C", "#9E9E9E")

long <- dat %>%
  mutate(Era = era_fun(`年份`)) %>%
  separate_rows(`靶点类别`, sep = ";\\s*|；|\\r?\\n") %>%
  filter(trimws(`靶点类别`) != "") %>%
  transmute(
    `试验编号`,
    # 反转顺序：ggplot 把第一个 level 放在最底部，
    # 因此把 2021-2026 放最前 → 1997-2005 会落在最上面
    Era = factor(Era, levels = c("2021-2026", "2016-2020", "2011-2015",
                                 "2006-2010", "1997-2005")),
    Target = trimws(`靶点类别`)
  ) %>%
  distinct() %>%
  filter(Target %in% target_levels_all)

counts <- long %>%
  count(Era, Target, name = "n") %>%
  filter(n > 0)

target_total <- long %>%
  count(Target, name = "total") %>%
  arrange(desc(total), Target)

target_order <- target_total$Target
counts$Target <- factor(counts$Target, levels = target_order)
cols <- setNames(colorRampPalette(user_pal)(length(target_order)), target_order)

p <- ggplot(counts, aes(n, Era, fill = Target)) +
  geom_col(position = "stack", width = 0.68,
           color = "white", linewidth = 0.25) +
  scale_fill_manual(values = cols, breaks = target_order, drop = FALSE) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.03))) +
  labs(x = "Target category mentions", y = "Start year era",
       title = "Evolution of therapeutic targets in severe asthma mAb trials",
       subtitle = paste0(
         "Main analysis; start year known, n = ", nrow(dat),
         "; target category mentions = ", nrow(long))) +
  theme_minimal(base_size = 11) +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        axis.text = element_text(color = "black"),
        legend.position = "right",
        legend.title = element_text(face = "bold"),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, color = "grey35"))

ggsave("靶点演变图.pdf", p, width = 9, height = 5.8, device = cairo_pdf)
ggsave("靶点演变图.png", p, width = 9, height = 5.8,
       device = ragg::agg_png, dpi = 300)
cat("已输出：靶点演变图.pdf / 靶点演变图.png\n")

