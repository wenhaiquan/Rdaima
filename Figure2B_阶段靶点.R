# 阶段 x 靶点水平堆叠图：读取本文件夹 阶段靶点数据.xlsx
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

if (!file.exists("阶段靶点数据.xlsx")) {
  stop("请把阶段靶点数据.xlsx 放到本文件夹")
}

dat <- read_excel("阶段靶点数据.xlsx")
need <- c("试验编号", "阶段", "靶点类别")
if (!all(need %in% names(dat))) stop("阶段靶点数据.xlsx 缺少必需列")

target_levels_all <- c("IgE", "IL-5/IL-5Rα", "IL-4Rα/IL-13",
                       "TSLP", "IL-33/ST2", "Other")
user_pal <- c("#6F4C9F", "#D56C9C", "#7B8CCB", "#B68ACD",
              "#F59B5E", "#F6D55C", "#9E9E9E")
phase_order <- c("Phase I", "Phase I/II", "Phase II", "Phase II/III",
                 "Phase III", "Phase IV", "Not applicable")

long <- dat %>%
  filter(!is.na(`靶点类别`), trimws(`靶点类别`) != "") %>%
  separate_rows(`靶点类别`, sep = ";\\s*|；|\\r?\\n") %>%
  filter(trimws(`靶点类别`) != "") %>%
  transmute(`试验编号`, Phase = `阶段`, Target = trimws(`靶点类别`)) %>%
  distinct() %>%
  filter(Target %in% target_levels_all)

target_total <- long %>%
  count(Target, name = "total") %>%
  arrange(desc(total), Target)
target_order <- target_total$Target

counts <- long %>%
  count(
    Phase = factor(Phase, levels = phase_order),
    Target = factor(Target, levels = target_order),
    name = "n"
  ) %>%
  filter(n > 0)

cols <- setNames(
  colorRampPalette(user_pal)(length(target_order)),
  target_order
)

p <- ggplot(counts, aes(n, Phase, fill = Target)) +
  geom_col(position = "stack", width = 0.72,
           color = "white", linewidth = 0.25) +
  scale_fill_manual(values = cols, breaks = target_order, drop = FALSE) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.03))) +
  labs(x = "Target category mentions", y = "Clinical phase",
       fill = "Target",
       title = "Target distribution across clinical phases",
       subtitle = paste0(
         "Main analysis, n = ", nrow(dat),
         "; one trial may contribute to multiple targets")) +
  theme_minimal(base_size = 11) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    axis.text = element_text(color = "black"),
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5, color = "grey35"),
    legend.position = "right"
  )

ggsave("阶段靶点图.pdf", p, width = 9, height = 6, device = cairo_pdf)
ggsave("阶段靶点图.png", p, width = 9, height = 6,
       device = ragg::agg_png, dpi = 300)
cat("已输出：阶段靶点图.pdf / 阶段靶点图.png\n")

