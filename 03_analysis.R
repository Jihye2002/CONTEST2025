# 03_analysis.R
source(here::here("01_setup.R"))
source(here::here("02_import_clean.R"))
library(ggplot2); library(ggrepel)

message("▶ 03_analysis.R 시작")

df <- readr::read_csv(p("_merged_by_gu.csv"), show_col_types = FALSE)

# 1) 회귀분석: 아동위험률 ~ 유흥주점_밀도 + CCTV_아동비 + 가로등_아동비 + 보호구역_아동10만명
if ("아동위험률_10만명" %in% names(df) && !all(is.na(df$아동위험률_10만명))) {
  model_vars <- c("유흥주점_밀도","CCTV_아동비","가로등_아동비","보호구역_아동10만명")
  used <- intersect(model_vars, names(df))
  used <- used[!sapply(df[used], function(x) all(is.na(x)))]
  if (length(used) >= 1) {
    form <- as.formula(paste("아동위험률_10만명 ~", paste(used, collapse = " + ")))
    fit <- lm(form, data = df)
    print(broom::tidy(fit))
    sink(p(here::here("data","regression_summary.txt")))
    print(summary(fit))
    sink()
    message(" - 회귀 결과 저장: data/regression_summary.txt")
  } else message("⚠ 회귀 실행 불가: 설명변수 부족")
} else message("⚠ 아동위험률 컬럼 없음 -> 회귀 생략")

# 2) 시각화 예시: 안전지수 지도 대신 막대(자치구별)
if ("안전지수_raw" %in% names(df)) {
  g1 <- ggplot(df, aes(x = reorder(자치구, 안전지수_raw), y = 안전지수_raw, fill = 안전등급)) +
    geom_col() + coord_flip() +
    labs(title = "자치구별 합성 안전지수 (높을수록 위험)", x = NULL, y = "안전지수_raw") +
    theme_minimal()
  ggsave(filename = p("plot_safety_index_bar.png"), plot = g1, width = 9, height = 7)
  message(" - 시각화 저장: data/plot_safety_index_bar.png")
}

# 3) 상관/산점도: 유흥주점 밀도 vs 아동위험률
if ("유흥주점_밀도" %in% names(df) && "아동위험률_10만명" %in% names(df)) {
  g2 <- ggplot(df, aes(x = 유흥주점_밀도, y = 아동위험률_10만명, label=자치구)) +
    geom_point(size=3) + geom_smooth(method="lm", se=FALSE) +
    ggrepel::geom_text_repel(size=3) +
    labs(title="유흥주점 밀도 vs 아동위험률(10만명)", x="유흥주점 밀도(건/10만아동)", y="아동위험률(건/10만명)") +
    theme_minimal()
  ggsave(filename = p("plot_bar_vs_risk.png"), plot = g2, width = 8, height = 6)
  message(" - 시각화 저장: data/plot_bar_vs_risk.png")
}

message("03_analysis.R 완료 (결과: data/_merged_by_gu.csv, regression_summary.txt, plot_*.png)")
