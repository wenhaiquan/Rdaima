
# 参与国世界地图：单国与跨国合并计数，读取本文件夹 国家数据.xlsx
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
  library(sf)
  library(rnaturalearth)
})

if (!file.exists("国家数据.xlsx")) {
  stop("请把国家数据.xlsx 放到本文件夹")
}

dat <- read_excel("国家数据.xlsx")
need <- c("试验编号", "单国国家", "跨国国家")
if (!all(need %in% names(dat))) stop("国家数据.xlsx 缺少必需列")

# 单国试验计 1 次，跨国试验的每个参与国各计 1 次
country_df <- bind_rows(
  dat %>%
    filter(!is.na(`单国国家`), trimws(`单国国家`) != "") %>%
    select(`试验编号`, 国家 = `单国国家`),
  dat %>%
    filter(!is.na(`跨国国家`), trimws(`跨国国家`) != "") %>%
    select(`试验编号`, 国家 = `跨国国家`) %>%
    separate_rows(国家, sep = ";\\s*|；|\\r?\\n") %>%
    filter(trimws(国家) != "")
) %>%
  mutate(国家 = trimws(国家)) %>%
  distinct(`试验编号`, 国家) %>%
  count(国家, name = "n") %>%
  arrange(desc(n))

unknown_n <- sum(
  is.na(dat$`单国国家`) & is.na(dat$`跨国国家`) |
    trimws(dat$`单国国家`) == "" & trimws(dat$`跨国国家`) == ""
)
cat("主分析国家不可用条数：", unknown_n, "\n")
cat("国家参与合计：", sum(country_df$n), "\n")

world <- ne_countries(scale = "medium", returnclass = "sf")
map_df <- left_join(world, country_df, by = c("name" = "国家"))

unmatched <- country_df$国家[!country_df$国家 %in% world$name]
if (length(unmatched) > 0) {
  cat("地图未匹配国家：", paste(unmatched, collapse = "; "), "\n")
}

# ---------- 前10名国家名称（按参与次数降序） ----------
top10_names <- head(country_df$国家, 10)
cat("前10名国家：", paste(top10_names, collapse = "; "), "\n")

# ---------- 手动指定每个前十国家的标签位置（避免欧洲区重叠） ----------
# lon 东正西负，lat 北正南负。想改哪个国家的位置，直接改数字。
label_pos <- data.frame(
  name = c("United States of America", "Canada", "France", "Germany",
           "United Kingdom", "Italy", "Japan", "China", "Poland", "Spain"),
  lon  = c(-100, -110,   2,  10,  -5,  13, 138, 105,  20,  -4),
  lat  = c(  40,   60,  46,  51,  57,  42,  37,  35,  53,  40),
  stringsAsFactors = FALSE
)

# 只保留实际出现在前十的国家（防止某个国家的名字对不上地图）
label_pos <- label_pos[label_pos$name %in% top10_names, ]

# 指定颜色由深到浅：深紫 -> 浅灰
user_colors <- c("#6F4C9F", "#D56C9C", "#7B8CCB", "#B68ACD",
                 "#F59B5E", "#F6D55C", "#B5B5B5")

p <- ggplot(map_df) +
  geom_sf(aes(fill = n), color = "white", linewidth = 0.12) +
  # 连续渐变：低值浅色，高值深色（将颜色向量反转）
  scale_fill_gradientn(colors = rev(user_colors),
                       na.value = "#F0ECF5",
                       name = "Trial-country participations") +
  # 标注前10名国家名称：用 geom_text 精确钉在指定经纬度上，字号 2.0
  geom_text(data = label_pos,
            aes(x = lon, y = lat, label = name),
            size = 2.0,
            color = "black",
            fontface = "plain") +
  labs(title = "Countries involved in severe asthma monoclonal antibody trials",
       subtitle = paste0("Main analysis; country unavailable n = ", unknown_n)) +
  theme_void() +
  theme(legend.position = "bottom",
        legend.key.size = grid::unit(0.45, "cm"),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, color = "grey35"))

ggsave("参与国地图.pdf", p, width = 12, height = 7, device = cairo_pdf)
ggsave("参与国地图.png", p, width = 12, height = 7, device = ragg::agg_png, dpi = 300)
cat("已输出：参与国地图.pdf / 参与国地图.png\n")

