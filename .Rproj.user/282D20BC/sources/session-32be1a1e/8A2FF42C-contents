# 02_import_clean.R
source(here::here("01_setup.R"))
options(readr.show_col_types = FALSE)
library(dplyr); library(stringr); library(readr); library(tidyr)

message("▶ 02_import_clean.R 시작")

# ---------- 1) 5대 범죄 ----------
crime_raw <- read_kor_csv(p("2023년 서울시 5대 범죄 발생현황.csv"))
cn <- names(crime_raw)

# 자치구 컬럼 자동 탐색(여러 포맷 대비)
gu_col_crime <- case_when(
  "자치구별(2)" %in% cn ~ "자치구별(2)",
  "자치구별(1)" %in% cn ~ "자치구별(1)",
  TRUE ~ cn[stringr::str_detect(cn, "자치구|구별|행정구|시군구") & !stringr::str_detect(cn, "구분")][1]
)
if (is.na(gu_col_crime)) gu_col_crime <- cn[1]

crime <- crime_raw %>%
  rename(자치구 = all_of(gu_col_crime)) %>%
  mutate(자치구 = str_replace_all(as.character(자치구), "\\s", ""))

# 숫자 변환 시도 (콤마/문자 제거)
val_cols_crime <- setdiff(names(crime), "자치구")
crime[val_cols_crime] <- lapply(crime[val_cols_crime], function(x) parse_number(as.character(x)))

# 합계/소계/전국 제거 및 총범죄 집계
crime_by_gu <- crime %>%
  filter(!자치구 %in% c("계","합계","소계","전국","서울특별시"), !is.na(자치구), 자치구 != "") %>%
  mutate(총범죄 = rowSums(across(all_of(val_cols_crime)), na.rm = TRUE)) %>%
  group_by(자치구) %>%
  summarise(총범죄 = sum(총범죄, na.rm = TRUE), .groups = "drop")

message(" - 범죄 처리 완료: rows=", nrow(crime_by_gu))

# ---------- 2) 유흥주점 ----------
bars <- read_kor_csv(p("서울시 유흥주점영업 인허가 정보.csv"))
addr_col <- case_when(
  "지번주소" %in% names(bars) ~ "지번주소",
  "도로명주소" %in% names(bars) ~ "도로명주소",
  TRUE ~ NA_character_
)
if (!is.na(addr_col)) {
  bars_by_gu <- bars %>%
    mutate(자치구 = extract_gu(.data[[addr_col]])) %>%
    filter(!is.na(자치구), 자치구 != "") %>%
    count(자치구, name = "유흥주점수") %>%
    mutate(자치구 = str_replace_all(자치구, "\\s", ""))
  message(" - 유흥주점 처리 완료")
} else {
  message("⚠ 유흥주점: 주소 컬럼 없음 -> 빈 테이블로 대체")
  bars_by_gu <- tibble(자치구=character(), 유흥주점수=integer())
}

# ---------- 3) CCTV ----------
cctv <- read_kor_csv(p("서울시 자치구 (범죄예방 수사용) CCTV 설치현황('25.6.30 기준).csv"))
# 컬럼명 다양성 처리
gu_cctv_col <- if ("구분" %in% names(cctv)) "구분" else names(cctv)[str_detect(names(cctv), "구분|자치구")][1]
val_cols_cctv <- names(cctv)[str_detect(names(cctv), "^[0-9]{4}년|년\\s*\\d+월|\\d{4}")]
latest_cctv_col <- if (length(val_cols_cctv)>0) tail(val_cols_cctv, 1) else NA_character_

if (!is.na(gu_cctv_col) && !is.na(latest_cctv_col)) {
  cctv_by_gu <- cctv %>%
    rename(자치구 = all_of(gu_cctv_col)) %>%
    mutate(자치구 = str_replace_all(as.character(자치구), "\\s", ""),
           cctv수 = parse_number(as.character(.data[[latest_cctv_col]]))) %>%
    filter(!is.na(자치구), !자치구 %in% c("계","합계","소계")) %>%
    group_by(자치구) %>%
    summarise(cctv수 = sum(cctv수, na.rm = TRUE), .groups = "drop")
  message(" - CCTV 처리 완료")
} else {
  message("⚠ CCTV: 컬럼 인식 실패 -> 빈 테이블")
  cctv_by_gu <- tibble(자치구=character(), cctv수=integer())
}

# ---------- 4) 어린이 보호구역 ----------
sz <- read_kor_csv(p("어린이 보호구역 지정현황(2025.6월말 기준).csv"))
if ("관할\n자치구" %in% names(sz)) {
  sz_by_gu <- sz %>%
    transmute(자치구 = `관할\n자치구`) %>%
    mutate(자치구 = str_replace_all(as.character(자치구),"\\s",""),
           자치구 = str_squish(자치구)) %>%
    filter(!is.na(자치구), 자치구 != "") %>%
    count(자치구, name = "어린이보호구역수")
  message(" - 보호구역(원본 컬럼) 처리 완료")
} else {
  backup_addr <- if ("소재지" %in% names(sz)) "소재지" else if ("도로명 주소" %in% names(sz)) "도로명 주소" else NA_character_
  if (!is.na(backup_addr)) {
    sz_by_gu <- sz %>%
      mutate(자치구 = extract_gu(.data[[backup_addr]])) %>%
      filter(!is.na(자치구), 자치구 != "") %>%
      count(자치구, name = "어린이보호구역수") %>%
      mutate(자치구 = str_replace_all(자치구, "\\s",""))
    message(" - 보호구역(주소 추출) 처리 완료")
  } else {
    message("⚠ 보호구역: 컬럼 인식 실패 -> 빈 테이블")
    sz_by_gu <- tibble(자치구=character(), 어린이보호구역수=integer())
  }
}

# ---------- 5) 가로등 ----------
lights <- read_kor_csv(p("서울시 가로등 위치 정보.csv"))
lights_by_gu <- tibble(자치구=character(), 가로등수=integer())
if ("자치구" %in% names(lights) || "관할" %in% names(lights)) {
  gu_col_l <- names(lights)[str_detect(names(lights), "자치구|관할")][1]
  lights_by_gu <- lights %>%
    mutate(자치구 = extract_gu(.data[[gu_col_l]])) %>%
    filter(!is.na(자치구), 자치구 != "") %>%
    count(자치구, name = "가로등수") %>%
    mutate(자치구 = str_replace_all(자치구, "\\s",""))
  message(" - 가로등(자치구 컬럼) 처리 완료")
} else if (all(c("위도","경도") %in% names(lights))) {
  message("ℹ 가로등에 좌표 있음: 공간조인 권장(추가 코드 필요). 현재는 빈 테이블로 둠.")
} else if ("소재지" %in% names(lights) || "도로명주소" %in% names(lights)) {
  addr_col_l <- if ("소재지" %in% names(lights)) "소재지" else "도로명주소"
  lights_by_gu <- lights %>%
    mutate(자치구 = extract_gu(.data[[addr_col_l]])) %>%
    filter(!is.na(자치구), 자치구 != "") %>%
    count(자치구, name = "가로등수") %>%
    mutate(자치구 = str_replace_all(자치구, "\\s",""))
  message(" - 가로등(주소 추출) 처리 완료")
} else {
  message("가로등 데이터: 자치구/주소/좌표 모두 없음 -> 스킵")
}

# ---------- 6) 인구 (옵션: 2023-2025_data.csv) ----------
pop_by_gu <- NULL
if (file.exists(p("2023-2025_data.csv"))) {
  pop <- read_kor_csv(p("2023-2025_data.csv"))
  
  # 자치구 컬럼 후보
  gu_pop_col <- names(pop)[str_detect(names(pop), "자치구|구명|^구$|구분")][1]
  
  # 1) 우선 내가 원하는 정확 이름(있으면 무조건 채택)
  preferred_child_cols <- c(
    "2023평균인구(아동_청소년_0-19세)",
    "2024평균인구(아동_청소년_0-19세)",
    "2025평균인구(아동_청소년_0-19세)"
  )
  child_pop_col <- preferred_child_cols[preferred_child_cols %in% names(pop)][1]
  
  # 2) 없으면 자동 탐색(이때 보호구역/가로등/CCTV/유흥 같은 지표는 제외)
  if (is.na(child_pop_col)) {
    auto_candidates <- names(pop)[
      str_detect(names(pop), "인구|population|people") &
        str_detect(names(pop), "0\\s*[-~_]\\s*19|아동|청소년") &
        !str_detect(names(pop), "보호구역|가로등|CCTV|유흥|주점|면적|km|km2|면적_km")
    ]
    child_pop_col <- if (length(auto_candidates)) auto_candidates[1] else NA_character_
  }
  
  if (!is.na(gu_pop_col) && !is.na(child_pop_col)) {
    pop_by_gu <- pop %>%
      rename(자치구 = all_of(gu_pop_col)) %>%
      mutate(
        자치구 = str_replace_all(as.character(자치구), "\\s", ""),
        아동인구 = readr::parse_number(as.character(.data[[child_pop_col]]))
      ) %>%
      filter(!is.na(자치구), 자치구 != "", !is.na(아동인구)) %>%
      select(자치구, 아동인구)
    message(" - 인구(아동) 처리 완료: 컬럼 -> ", child_pop_col)
  } else {
    message("인구 파일 존재하지만 아동 인구 컬럼을 못 찾음 names(pop)로 확인 필요")
  }
} else {
  message("ℹ 인구 파일 없음 -> 인구기준 지표는 생략")
}

# ---------- 7) 병합 및 파생지표 ----------
df <- crime_by_gu %>%
  full_join(bars_by_gu, by="자치구") %>%
  full_join(cctv_by_gu, by="자치구") %>%
  full_join(sz_by_gu,  by="자치구") %>%
  full_join(lights_by_gu, by="자치구") %>%
  mutate(across(-자치구, ~ replace_na(., 0)))

if (!is.null(pop_by_gu)) df <- df %>% left_join(pop_by_gu, by="자치구")

df <- df %>%
  mutate(
    # 없으면 0으로 보정
    유흥주점수 = ifelse(!"유흥주점수" %in% names(.), 0, 유흥주점수),
    cctv수      = ifelse(!"cctv수" %in% names(.), 0, cctv수),
    어린이보호구역수 = ifelse(!"어린이보호구역수" %in% names(.), 0, 어린이보호구역수),
    가로등수   = ifelse(!"가로등수" %in% names(.), 0, 가로등수)
  ) %>%
  mutate(
    아동유효 = if_else(!is.na(아동인구) & 아동인구 > 0, TRUE, FALSE, missing = FALSE),
    아동분모 = if_else(아동유효, 아동인구 / 1e5, NA_real_),
    
    아동위험률_10만명   = if_else(아동유효, 총범죄 / 아동분모, NA_real_),
    유흥주점_밀도       = if_else(아동유효, 유흥주점수 / 아동분모, NA_real_),
    CCTV_아동비         = if_else(아동유효, cctv수 / 아동분모, NA_real_),
    가로등_아동비       = if_else(아동유효, 가로등수 / 아동분모, NA_real_),
    보호구역_아동10만명 = if_else(아동유효, 어린이보호구역수 / 아동분모, NA_real_)
  )

# 표준화 + 합성 안전지수
safe_scale <- function(x) {
  if (all(is.na(x))) return(rep(0, length(x)))
  xs <- scale(x)
  as.numeric(ifelse(is.nan(xs), 0, xs))
}

w <- list(bar=0.5, cctv=-0.2, lights=-0.2, protection=-0.1)
df <- df %>%
  mutate(
    z_bar  = safe_scale(유흥주점_밀도),
    z_cctv = safe_scale(-CCTV_아동비),
    z_lgt  = safe_scale(-가로등_아동비),
    z_prt  = safe_scale(-보호구역_아동10만명),
    안전지수_raw = w$bar*z_bar + w$cctv*z_cctv + w$lights*z_lgt + w$protection*z_prt,
    안전등급 = dplyr::ntile(-안전지수_raw, 3)  # 1 가장 안전 ~ 3 위험
  )

# 저장
write_csv(df, p("_merged_by_gu.csv"))
message("데이터 병합 및 지표 생성 완료 -> data/_merged_by_gu.csv (rows=", nrow(df), ")")