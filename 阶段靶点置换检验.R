Sys.setlocale("LC_CTYPE", "zh_CN.UTF-8")
suppressPackageStartupMessages(library(readxl))

取得项目根目录 <- function() {
  参数 <- commandArgs(trailingOnly = FALSE)
  脚本参数 <- grep("^--file=", 参数, value = TRUE)
  if (length(脚本参数) == 0L) {
    return(getwd())
  }
  脚本路径 <- sub("^--file=", "", 脚本参数[[1]])
  dirname(dirname(normalizePath(脚本路径, mustWork = TRUE)))
}

项目根目录 <- 取得项目根目录()
总表路径 <- file.path(项目根目录, "数据", "总表_终版.xlsx")
结果路径 <- file.path(项目根目录, "代码", "阶段靶点置换检验结果.csv")

靶点顺序 <- c(
  "IgE",
  "IL-5/IL-5Rα",
  "IL-4Rα/IL-13",
  "TSLP",
  "IL-33/ST2",
  "Multiple targets",
  "Other"
)
阶段顺序 <- c(
  "Phase I/I-II",
  "Phase II",
  "Phase II/III-III",
  "Phase III/IV-IV"
)
允许阶段 <- c(
  "Phase I",
  "Phase I/II",
  "Phase II",
  "Phase II/III",
  "Phase III",
  "Phase III/IV",
  "Phase IV",
  "Not applicable"
)

映射阶段 <- function(阶段) {
  映射表 <- c(
    "Phase I" = "Phase I/I-II",
    "Phase I/II" = "Phase I/I-II",
    "Phase II" = "Phase II",
    "Phase II/III" = "Phase II/III-III",
    "Phase III" = "Phase II/III-III",
    "Phase III/IV" = "Phase III/IV-IV",
    "Phase IV" = "Phase III/IV-IV"
  )
  unname(映射表[阶段])
}

分类靶点 <- function(靶点类别) {
  结果 <- ifelse(
    grepl(";", 靶点类别, fixed = TRUE),
    "Multiple targets",
    trimws(靶点类别)
  )
  异常值 <- setdiff(unique(结果), 靶点顺序)
  if (length(异常值) > 0L) {
    stop("发现未预期的靶点类别：", paste(异常值, collapse = "、"))
  }
  结果
}

构建列联表 <- function(数据, 分析类型) {
  必需列 <- c("Phase", "Target_Category")
  缺少列 <- setdiff(必需列, names(数据))
  if (length(缺少列) > 0L) {
    stop(分析类型, "缺少必需字段：", paste(缺少列, collapse = "、"))
  }

  异常阶段 <- setdiff(unique(数据[["Phase"]]), 允许阶段)
  if (length(异常阶段) > 0L) {
    stop(分析类型, "发现未预期的 Phase：", paste(异常阶段, collapse = "、"))
  }

  分析阶段 <- 映射阶段(数据[["Phase"]])
  保留记录 <- !is.na(分析阶段)
  分析靶点 <- 分类靶点(数据[["Target_Category"]][保留记录])
  分析阶段 <- 分析阶段[保留记录]

  计数表 <- table(
    factor(分析靶点, levels = 靶点顺序),
    factor(分析阶段, levels = 阶段顺序)
  )
  matrix(
    as.integer(计数表),
    nrow = length(靶点顺序),
    ncol = length(阶段顺序),
    dimnames = list(靶点顺序, 阶段顺序)
  )
}

检查列联表 <- function(列联表, 预期行合计, 预期列合计, 分析类型) {
  if (!identical(as.integer(rowSums(列联表)), 预期行合计)) {
    stop(
      分析类型,
      "行合计不符，实际为：",
      paste(rowSums(列联表), collapse = "、")
    )
  }
  if (!identical(as.integer(colSums(列联表)), 预期列合计)) {
    stop(
      分析类型,
      "列合计不符，实际为：",
      paste(colSums(列联表), collapse = "、")
    )
  }
  message(分析类型, "列联表验证通过，有效试验数：", sum(列联表))
}

检查列联表内容 <- function(实际列联表, 预期列联表, 分析类型) {
  if (!identical(dimnames(实际列联表), dimnames(预期列联表))) {
    stop(分析类型, "列联表的行名或列名与 table4-6.docx 不一致")
  }
  if (any(as.numeric(实际列联表) != as.numeric(预期列联表))) {
    stop(分析类型, "列联表的单元格与 table4-6.docx 不一致")
  }
}

计算卡方值 <- function(列联表) {
  期望频数 <- outer(rowSums(列联表), colSums(列联表)) / sum(列联表)
  sum((列联表 - 期望频数)^2 / 期望频数)
}

置换卡方检验 <- function(
  列联表,
  置换次数 = 100000L,
  随机种子 = 20260924L
) {
  观察卡方值 <- 计算卡方值(列联表)
  自由度 <- (nrow(列联表) - 1L) * (ncol(列联表) - 1L)
  克拉默V值 <- sqrt(
    观察卡方值 / (sum(列联表) * (min(dim(列联表)) - 1L))
  )

  行数 <- nrow(列联表)
  列数 <- ncol(列联表)
  行编号 <- rep.int(seq_len(行数), times = as.integer(rowSums(列联表)))
  列编号 <- rep.int(seq_len(列数), times = as.integer(colSums(列联表)))
  记录数 <- length(列编号)

  set.seed(随机种子)
  极端结果数 <- 0L
  for (第次置换 in seq_len(置换次数)) {
    置换列编号 <- sample(列编号, size = 记录数, replace = FALSE)
    置换频数 <- tabulate(
      (行编号 - 1L) * 列数 + 置换列编号,
      nbins = 行数 * 列数
    )
    置换列联表 <- matrix(
      置换频数,
      nrow = 行数,
      ncol = 列数,
      byrow = TRUE
    )
    if (计算卡方值(置换列联表) >= 观察卡方值 - 1e-12) {
      极端结果数 <- 极端结果数 + 1L
    }
  }

  精确P值 <- (1 + 极端结果数) / (置换次数 + 1)
  报告P值 <- if (精确P值 < 0.001) {
    "P < 0.001"
  } else {
    sprintf("P = %.3f", 精确P值)
  }

  list(
    有效试验数 = sum(列联表),
    卡方值 = 观察卡方值,
    自由度 = 自由度,
    克拉默V值 = 克拉默V值,
    置换次数 = 置换次数,
    随机种子 = 随机种子,
    精确P值 = 精确P值,
    报告P值 = 报告P值
  )
}

主分析数据 <- read_excel(总表路径, sheet = "主分析")
补充分析数据 <- read_excel(总表路径, sheet = "补充分析")

if (nrow(主分析数据) != 169L) {
  stop("主分析应为169行，实际为", nrow(主分析数据), "行")
}
if (nrow(补充分析数据) != 132L) {
  stop("补充分析应为132行，实际为", nrow(补充分析数据), "行")
}

主分析列联表 <- 构建列联表(主分析数据, "主分析")
补充分析列联表 <- 构建列联表(补充分析数据, "补充分析")

主分析预期列联表 <- matrix(
  c(
    0, 9, 11, 62,
    2, 10, 15, 14,
    0, 2, 4, 1,
    2, 4, 13, 1,
    0, 2, 0, 0,
    0, 2, 0, 2,
    0, 9, 0, 0
  ),
  nrow = 7,
  byrow = TRUE,
  dimnames = list(靶点顺序, 阶段顺序)
)
补充分析预期列联表 <- matrix(
  c(
    0, 10, 31, 23,
    0, 2, 7, 0,
    2, 11, 4, 1,
    1, 6, 2, 0,
    0, 9, 0, 0,
    4, 6, 0, 1,
    1, 10, 0, 0
  ),
  nrow = 7,
  byrow = TRUE,
  dimnames = list(靶点顺序, 阶段顺序)
)

检查列联表内容(主分析列联表, 主分析预期列联表, "主分析")
检查列联表内容(补充分析列联表, 补充分析预期列联表, "补充分析")

检查列联表(
  主分析列联表,
  预期行合计 = c(82L, 41L, 7L, 20L, 2L, 4L, 9L),
  预期列合计 = c(4L, 38L, 43L, 80L),
  分析类型 = "主分析"
)
检查列联表(
  补充分析列联表,
  预期行合计 = c(64L, 9L, 18L, 9L, 9L, 11L, 11L),
  预期列合计 = c(8L, 54L, 44L, 25L),
  分析类型 = "补充分析"
)

主分析结果 <- 置换卡方检验(
  主分析列联表,
  置换次数 = 100000L,
  随机种子 = 20260924L
)
补充分析结果 <- 置换卡方检验(
  补充分析列联表,
  置换次数 = 100000L,
  随机种子 = 20260924L
)

主分析结果值 <- unlist(主分析结果, use.names = TRUE)
补充分析结果值 <- unlist(补充分析结果, use.names = TRUE)

结果表 <- data.frame(
  分析类型 = c("主分析", "补充分析"),
  有效试验数 = c(
    主分析结果值[["有效试验数"]],
    补充分析结果值[["有效试验数"]]
  ),
  卡方值 = c(
    主分析结果值[["卡方值"]],
    补充分析结果值[["卡方值"]]
  ),
  自由度 = c(
    主分析结果值[["自由度"]],
    补充分析结果值[["自由度"]]
  ),
  克拉默V值 = c(
    主分析结果值[["克拉默V值"]],
    补充分析结果值[["克拉默V值"]]
  ),
  置换次数 = c(
    主分析结果值[["置换次数"]],
    补充分析结果值[["置换次数"]]
  ),
  随机种子 = c(
    主分析结果值[["随机种子"]],
    补充分析结果值[["随机种子"]]
  ),
  精确P值 = c(
    主分析结果值[["精确P值"]],
    补充分析结果值[["精确P值"]]
  ),
  报告P值 = c(
    主分析结果值[["报告P值"]],
    补充分析结果值[["报告P值"]]
  ),
  R版本 = c(R.version.string, R.version.string),
  check.names = FALSE
)

write.csv(
  结果表,
  file = 结果路径,
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

message("结果已写入：", 结果路径)
print(结果表, row.names = FALSE, digits = 15)
