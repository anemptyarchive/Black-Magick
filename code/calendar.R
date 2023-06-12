
# ggplot2でカレンダーを作成したい -------------------------------------------------------------

# 利用パッケージ
library(tidyverse)
library(lubridate)
library(zipangu)

# 追加パッケージ
library(rtweet)

# チェック用
library(ggplot2)


### ・ひと月 -----

# 年を指定
year <- 2024

# 月を指定
month <- 2

# 週初めの曜日を指定:(月: 1, ..., 日: 7)
dow_start_idx <- 1

# 土曜・日曜の曜日インデックスを設定
dow_sat_idx <- ifelse(dow_start_idx == 7, yes = 7, no = 7 - dow_start_idx)
dow_sun_idx <- 8 - dow_start_idx


# ひと月の暦データを作成
date_df <- tibble::tibble(
  # 1日間隔の日付を作成
  date = seq(
    from = paste0(year, "-", month, "-01") |> 
      lubridate::as_date(), # 初日
    to   = paste0(year, "-", month, "-01") |> 
      lubridate::as_date() |> 
      lubridate::rollforward(), # 末日
    by = "day"
  ), 
) |> 
  dplyr::mutate(
    day = lubridate::day(date), # 日にちラベル
    # 作図用の値を作成
    dow_idx   = lubridate::wday(date, week_start = dow_start_idx), # 曜日番号(列インデックス)
    dow_label = lubridate::wday(date, week_start = dow_start_idx, label = TRUE), # 曜日ラベル
    cell_idx  = day + head(dow_idx, n = 1) - 1, # セルインデックス
    week_idx  = (cell_idx - 1) %/% 7 + 1 # 週番号(行インデックス)
  )

# 祝日情報を取得
holiday_vec <- zipangu::jholiday(year = year, lang = "jp") |> 
  unlist() |> 
  lubridate::as_date()

# 祝日の暦データを作成
tmp_holiday_df <- tibble::tibble(
  date = holiday_vec, # 祝日の日付
  holiday_label = names(holiday_vec), # 祝日ラベル
  dow_idx = lubridate::wday(date, week_start = dow_start_idx) # 振替休日の作成用
) |> 
  dplyr::filter(lubridate::month(date) == month) # 指定した月の祝日を抽出

# 振替休日の暦データを作成
holiday_df <- tmp_holiday_df |> 
  dplyr::filter(dow_idx == dow_sun_idx) |> # 日曜日の祝日を抽出
  dplyr::mutate(
    date = dplyr::case_when(
      holiday_label == "憲法記念日" ~ date + lubridate::days(3), 
      holiday_label == "みどりの日" ~ date + lubridate::days(2), 
      TRUE ~ date + lubridate::days(1)
    ), # 日にち
    holiday_label = "振替休日", # 祝日名
    dow_idx = lubridate::wday(date, week_start = dow_start_idx) # 祝日データの結合用
  ) |> 
  dplyr::bind_rows(tmp_holiday_df) |> # 祝日の暦データを結合
  dplyr::select(date, holiday_label, holiday_label) |> # 日付データの結合用
  dplyr::arrange(date)

# 装飾用の暦データを作成
calendar_df <- date_df |> 
  dplyr::left_join(holiday_df, by = "date") |> # 祝日データを結合
  dplyr::mutate(
    date_type = dplyr::case_when(
      !is.na(holiday_label) ~ "holiday", 
      dow_idx == dow_sun_idx ~ "holiday", 
      dow_idx == dow_sat_idx ~ "weekend", 
      TRUE ~ "weekday"
    ) # 日付カテゴリ
  )


# 予定日の暦データを作成
schedule_df <- tibble::tibble(
  date = c("2", "19", "22") |> # 日にちを指定
    (\(.){paste0(year, "-", month, "-", .)})() |> # 日付を作成
    lubridate::as_date(), 
  symbol = c("⚾", "🍈", "🎷") # 予定ラベルを指定
) |> 
  dplyr::left_join(
    calendar_df |> 
      dplyr::select(date, dow_idx, week_idx), 
    by = "date"
  ) # 作図用の値を結合


# 横軸ラベルを作成
dow_label_vec <- lubridate::wday(1:7, week_start = dow_start_idx, label = TRUE) |> # 日本語名
  sort()
#dow_label_vec <- lubridate::wday(1:7, week_start = dow_start_idx, label = TRUE, abbr = TRUE, locale = "en_US") |> # 英語名の省略形
#  sort()

# ひと月のカレンダーを作図:(休日のセル色を変更)
ggplot() + 
  # geom_tile(data = calendar_df,
  #           mapping = aes(x = dow_idx, y = week_idx),
  #           fill = "white", color = "black") + # 平日セル
  geom_tile(data = calendar_df, 
            mapping = aes(x = dow_idx, y = week_idx, fill = date_type), 
            color = "black", alpha = 0.1) + # 休日セル
  geom_text(data = calendar_df, 
            mapping = aes(x = dow_idx-0.4, y = week_idx-0.4, label = day), 
            size = 10, hjust = 0, vjust = 1) + # 日付ラベル
  geom_text(data = calendar_df, 
            mapping = aes(x = dow_idx+0.45, y = week_idx-0.4, label = holiday_label), 
            size = 4, hjust = 1, vjust = 1, na.rm = TRUE) + # 祝日ラベル
  geom_text(data = schedule_df, 
            mapping = aes(x = dow_idx, y = week_idx, label = symbol), 
            size = 15) + # 予定ラベル
  scale_x_continuous(breaks = 1:7, labels = NULL, 
                     sec.axis = dup_axis(trans = ~., labels = dow_label_vec)) + # 曜日軸
  scale_y_reverse(breaks = 1:6) + # 週軸
  scale_fill_manual(breaks = c("weekday", "holiday", "weekend"), 
                    values = c("white", "red", "blue")) + # 休日用の塗りつぶし色
  coord_fixed(ratio = 1, expand = FALSE) + # 描画領域
  theme(
    axis.title = element_blank(), # 軸ラベル
    axis.text.x = element_text(size = 30), # 横軸目盛ラベル
    axis.text.y = element_blank(), # 縦軸目盛ラベル
    axis.ticks = element_blank(), # 軸目盛指示線
    panel.grid.major = element_blank(), # 主グリッド線
    panel.grid.minor = element_blank(), # 副グリッド線
    panel.border = element_rect(fill = NA), # グラフ領域の枠線
    panel.background = element_blank(), # グラフ領域の背景
    plot.title = element_text(size = 25, face = "bold"), # タイトル
    plot.subtitle = element_text(size = 50, face = "bold", hjust = 0.5), # サブタイトル
    legend.position = "none" # 凡例の位置
  ) + # 図の体裁
  labs(title = paste0(year, "年"), 
       subtitle = paste0(month, "月"), 
       x = "曜日", y = "週")

# ひと月のカレンダーを作図:(休日のラベル色を変更)
ggplot() + 
  geom_tile(data = calendar_df, 
            mapping = aes(x = dow_idx, y = week_idx), 
            fill = "white", color = "black") + # 日付セル
  geom_text(data = calendar_df, 
            mapping = aes(x = dow_idx-0.4, y = week_idx-0.4, label = day, color = date_type), 
            size = 10, hjust = 0, vjust = 1) + # 日付ラベル
  geom_text(data = calendar_df, 
            mapping = aes(x = dow_idx+0.45, y = week_idx-0.4, label = holiday_label), 
            size = 5, color = "red", hjust = 1, vjust = 1, na.rm = TRUE) + # 祝日ラベル
  geom_text(data = schedule_df, 
            mapping = aes(x = dow_idx, y = week_idx, label = symbol), 
            size = 15) + # 予定ラベル
  scale_x_continuous(breaks = 1:7, labels = NULL, 
                     sec.axis = dup_axis(trans = ~., labels = dow_label_vec)) + # 曜日軸
  scale_y_reverse(breaks = 1:6) + # 週軸
  scale_color_manual(breaks = c("weekday", "holiday", "weekend"), 
                     values = c("black", "red", "blue")) + # 休日用文字色
  coord_fixed(ratio = 1, expand = FALSE) + # 描画領域
  theme(
    axis.title = element_blank(), # 軸ラベル
    axis.text.x = element_text(size = 30), # 横軸目盛ラベル
    axis.text.y = element_blank(), # 縦軸目盛ラベル
    axis.ticks = element_blank(), # 軸目盛指示線
    panel.grid.major = element_blank(), # 主グリッド線
    panel.grid.minor = element_blank(), # 副グリッド線
    panel.border = element_rect(fill = NA), # グラフ領域の枠線
    #panel.background = element_blank(), # グラフ領域の背景
    plot.title = element_text(size = 25, face = "bold"), # タイトル
    plot.subtitle = element_text(size = 50, face = "bold", hjust = 0.5), # サブタイトル
    legend.position = "none" # 凡例の位置
  ) + # 図の体裁
  labs(title = paste0(year, "年"), 
       subtitle = paste0(month, "月"), 
       x = "曜日", y = "週")


### ・ひと年 -----

# 年を指定
year <- 2023

# 週初めの曜日を指定:(月: 1, ..., 日: 7)
dow_start_idx <- 7

# 土曜・日曜の曜日インデックスを設定
dow_sat_idx <- ifelse(dow_start_idx == 7, yes = 7, no = 7 - dow_start_idx)
dow_sun_idx <- 8 - dow_start_idx


# ひと年の暦データを作成
date_df <- tibble::tibble(
  # 1日間隔の日付を作成
  date = seq(
    from = paste0(year, "-1-1") |> 
      lubridate::as_date(), # 正月
    to   = paste0(year, "-12-31") |> 
      lubridate::as_date(), # 大晦日
    by = "day"
  )
) |> 
  dplyr::mutate(
    year  = lubridate::year(date),  # 年ラベル
    month = lubridate::month(date), # 月ラベル
    day   = lubridate::day(date),   # 日ラベル
    # 作図用の値を作成
    dow_idx   = lubridate::wday(date, week_start = dow_start_idx), # 曜日番号(列インデックス)
    dow_label = lubridate::wday(date, week_start = dow_start_idx, label = TRUE) # 曜日ラベル
  ) |> 
  dplyr::group_by(year, month) |> # インデックスの作成用
  dplyr::mutate(
    cell_idx = day + head(dow_idx, n = 1) - 1, # セルインデックス
    week_idx = (cell_idx - 1) %/% 7 + 1 # 週番号(行インデックス)
  ) |> 
  dplyr::ungroup()

# 祝日情報を取得
holiday_vec <- zipangu::jholiday(year = year, lang = "jp") |> 
  unlist() |> 
  lubridate::as_date()

# 祝日の暦データを作成
tmp_holiday_df <- tibble::tibble(
  date = holiday_vec, # 祝日の日付
  holiday_label = names(holiday_vec), # 祝日ラベル
  dow_idx = lubridate::wday(date, week_start = dow_start_idx) # 振替休日の作成用
)

# 祝日ラベルの文字数を指定
threshold <- 5

# 振替休日の暦データを作成
holiday_df <- tmp_holiday_df |> 
  dplyr::filter(dow_idx == dow_sun_idx) |> # 日曜日の祝日を抽出
  dplyr::mutate(
    date = dplyr::case_when(
      holiday_label == "憲法記念日" ~ date + lubridate::days(3), 
      holiday_label == "みどりの日" ~ date + lubridate::days(2), 
      TRUE ~ date + lubridate::days(1)
    ), # 日にち
    holiday_label = "振替休日", # 祝日名
    dow_idx = lubridate::wday(date, week_start = dow_start_idx) # 祝日データの結合用
  ) |> 
  dplyr::bind_rows(tmp_holiday_df) |> # 祝日の暦データを結合
  dplyr::mutate(
    label_size = dplyr::if_else(
      condition = nchar(holiday_label) <= threshold, 
      true  = "normal", 
      false = "small"
    ) # ラベルサイズカテゴリ:(日付ラベルと祝日ラベルが重なる対策)
  ) |> 
  dplyr::select(date, holiday_label, label_size) |> # 日付データの結合用
  dplyr::arrange(date)

# 装飾用の暦データを作成
calendar_df <- date_df |> 
  dplyr::left_join(holiday_df, by = "date") |> # 祝日データを結合
  dplyr::mutate(
    date_type = dplyr::case_when(
      !is.na(holiday_label) ~ "holiday", 
      dow_idx == dow_sun_idx ~ "holiday", 
      dow_idx == dow_sat_idx ~ "weekend", 
      TRUE ~ "weekday"
    ) # 日付カテゴリ
  )


## Hello.RprojのReadData.Rを参照

# フォルダパスを指定
dir_path <- "../Hello/ChartRace/data/HP_DB-main/"

# 加入・卒業日一覧を読み込み
join_df <- readr::read_csv(
  file = paste0(dir_path, "join.csv"), 
  col_types = readr::cols(
    memberID = "i", 
    groupID = "i", 
    joinDate = readr::col_date(format = "%Y/%m/%d"), 
    gradDate = readr::col_date(format = "%Y/%m/%d")
  )
) |> 
  dplyr::arrange(joinDate, memberID, groupID)

# メンバー一覧を読み込み
member_df <- readr::read_csv(
  file = paste0(dir_path, "member.csv"), 
  col_types = readr::cols(
    memberID = "i", 
    memberName = "c", 
    HPjoinDate = readr::col_date(format = "%Y/%m/%d"), 
    debutDate = readr::col_date(format = "%Y/%m/%d"), 
    HPgradDate = readr::col_date(format = "%Y/%m/%d"), 
    memberKana = "c", 
    birthDate = readr::col_date(format = "%Y/%m/%d")
  )
) |> 
  dplyr::select(memberID, memberName, birthDate) |> 
  dplyr::distinct() |> 
  dplyr::arrange(memberID)

# グループを指定
group_id <- 1

# 予定の暦データを作成
schedule_df <- join_df |> 
  dplyr::filter(groupID == group_id) |> # 指定グループの加入情報を抽出
  dplyr::left_join(member_df, by = "memberID") |> # 誕生日情報を結合
  dplyr::mutate(
    month = lubridate::month(birthDate), # 誕生月
    day   = lubridate::day(birthDate),   # 誕生日(日にち)
    date  = paste0(year, "-", as.character(month), "-", as.character(day)) |> 
      lubridate::as_date(), # 指定した年の誕生日
    symbol = "🎂" # 記号を指定
  ) |> 
  dplyr::select(date, symbol, memberName, month) |> 
  dplyr::left_join(
    calendar_df |> 
      dplyr::select(date, dow_idx, week_idx), 
    by = "date"
  ) |> # 作図用の値を結合
  dplyr::arrange(date)


# ラベル用の関数を作成
str_month <- function(string) {
  paste0(string, "月") # 月の日本語名を出力
  # paste0("2000-", string, "-1") |> # 指定した月の適当な日付を作成
  #   as.Date() |> # 日付型に変換
  #   lubridate::month(label = TRUE, abbr = FALSE, locale = "en_US") |>  # 月の英語名に変換
  #   as.character() # 文字列型に変換して出力
}
str_month(5)

# 横軸ラベルを作成
dow_label_vec <- lubridate::wday(1:7, week_start = dow_start_idx, label = TRUE) |> # 日本語名
  sort()

# ひと年のカレンダーを作図:(休日のセル色を変更)
ggplot() + 
  # geom_tile(data = calendar_df, 
  #           mapping = aes(x = dow_idx, y = week_idx), 
  #           fill = "white", color = "black") + # 平日セル
  geom_tile(data = calendar_df, 
            mapping = aes(x = dow_idx, y = week_idx, fill = date_type), 
            color = "black", alpha = 0.1) + # 休日セル
  geom_text(data = calendar_df, 
            mapping = aes(x = dow_idx-0.4, y = week_idx-0.4, label = day), 
            size = 5, hjust = 0, vjust = 1) + # 日付ラベル
  geom_text(data = calendar_df, 
            mapping = aes(x = dow_idx+0.45, y = week_idx-0.4, label = holiday_label, size = label_size), 
            hjust = 1, vjust = 1, na.rm = TRUE) + # 祝日ラベル
  # geom_text(data = schedule_df, 
  #           mapping = aes(x = dow_idx, y = week_idx, label = symbol), 
  #           size = 5) + # 予定マーク
  # geom_text(data = schedule_df, 
  #           mapping = aes(x = dow_idx, y = week_idx+0.45, label = memberName), 
  #           size = 2, vjust = 0) + # 予定ラベル
  scale_x_continuous(breaks = 1:7, labels = NULL, 
                     sec.axis = dup_axis(trans = ~., labels = dow_label_vec)) + # 曜日軸
  scale_y_reverse(breaks = 1:6) + # 週軸
  scale_fill_manual(breaks = c("weekday", "holiday", "weekend"), 
                    values = c("white", "red", "blue")) + # 休日用塗りつぶし色
  scale_size_manual(breaks = c("normal", "small"), 
                    values = c(2.5, 1.5)) + # (日付ラベルと祝日ラベルが重なる対策)
  facet_wrap(month ~ ., nrow = 3, ncol = 4, 
             labeller = labeller(month = str_month), scales = "free_x") + # 年・月ごとに分割
  coord_cartesian(expand = FALSE) + # 描画領域
  theme(
    axis.title = element_blank(), # 軸ラベル
    axis.text.x = element_text(size = 10), # 横軸目盛ラベル
    axis.text.y = element_blank(), # 縦軸目盛ラベル
    axis.ticks = element_blank(), # 軸目盛指示線
    panel.grid.major = element_blank(), # 主グリッド線
    panel.grid.minor = element_blank(), # 副グリッド線
    panel.border = element_rect(fill = NA), # グラフ領域の枠線
    panel.background = element_blank(), # グラフ領域の背景
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5), # タイトル
    strip.text = element_text(size = 20, face = "bold"), # ファセットラベルの文字
    strip.background = element_blank(), # ファセットラベル領域の背景
    strip.placement = "outside", # ファセットラベルの位置
    legend.position = "none" # 凡例の位置
  ) + # 図の体裁
  labs(title = paste0(year, "年"), 
       x = "曜日", y = "週")

# ひと年のカレンダーを作図:(休日のラベル色を変更)
ggplot() + 
  geom_tile(data = calendar_df, 
            mapping = aes(x = dow_idx, y = week_idx), 
            fill = "white", color = "black") + # 日付セル
  geom_text(data = calendar_df, 
            mapping = aes(x = dow_idx-0.4, y = week_idx-0.4, label = day, color = date_type), 
            size = 5, hjust = 0, vjust = 1) + # 日付ラベル
  geom_text(data = calendar_df, 
            mapping = aes(x = dow_idx+0.45, y = week_idx-0.4, label = holiday_label, size = label_size), 
            color = "red", hjust = 1, vjust = 1, na.rm = TRUE) + # 祝日ラベル
  # geom_text(data = schedule_df, 
  #           mapping = aes(x = dow_idx, y = week_idx, label = symbol), 
  #           size = 5) + # 予定マーク
  # geom_text(data = schedule_df, 
  #           mapping = aes(x = dow_idx, y = week_idx+0.45, label = memberName), 
  #           size = 2, vjust = 0) + # 予定ラベル
  scale_x_continuous(breaks = 1:7, labels = NULL, 
                     sec.axis = dup_axis(trans = ~., labels = dow_label_vec)) + # 曜日軸
  scale_y_reverse(breaks = 1:6) + # 週軸
  scale_color_manual(breaks = c("weekday", "holiday", "weekend"), 
                     values = c("black", "red", "blue")) + # 休日用文字色
  scale_size_manual(breaks = c("normal", "small"), 
                    values = c(2.5, 1.5)) + # (日付ラベルと祝日ラベルが重なる対策)
  facet_wrap(month ~ ., nrow = 3, ncol = 4, 
             labeller = labeller(month = str_month), scales = "free_x") + # 年・月ごとに分割
  coord_cartesian(expand = FALSE) + # 描画領域
  theme(
    axis.title = element_blank(), # 軸ラベル
    axis.text.x = element_text(size = 10), # 横軸目盛ラベル
    axis.text.y = element_blank(), # 縦軸目盛ラベル
    axis.ticks = element_blank(), # 軸目盛指示線
    panel.grid.major = element_blank(), # 主グリッド線
    panel.grid.minor = element_blank(), # 副グリッド線
    panel.border = element_rect(fill = NA), # グラフ領域の枠線
    #panel.background = element_blank(), # グラフ領域の背景
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5), # タイトル
    strip.text = element_text(size = 20, face = "bold"), # ファセットラベルの文字
    strip.background = element_blank(), # ファセットラベル領域の背景
    strip.placement = "outside", # ファセットラベルの位置
    legend.position = "none" # 凡例の位置
  ) + # 図の体裁
  labs(title = paste0(year, "年"), 
       x = "曜日", y = "週")


### ・任意の期間 -----

# 開始日を指定
date_from <- "2021-05-05" |> 
  lubridate::as_date()

# 終了日を指定
date_to <- "2023-05-04" |> 
  lubridate::as_date()

# 週初めの曜日を指定:(月: 1, ..., 日: 7)
dow_start_idx <- 7

# 土曜・日曜の曜日インデックスを設定
dow_sat_idx <- ifelse(dow_start_idx == 7, yes = 7, no = 7 - dow_start_idx)
dow_sun_idx <- 8 - dow_start_idx


# 任意期間の暦データを作成
date_df <- tibble::tibble(
  # 1日間隔の日付を作成
  date = seq(
    from = date_from |> 
      lubridate::floor_date(unit = "month"), # 開始日の月の初日
    to   = date_to, # 終了日
    by = "day"
  )
) |> 
  dplyr::mutate(
    year  = lubridate::year(date),  # 年ラベル
    month = lubridate::month(date), # 月ラベル
    day   = lubridate::day(date),   # 日ラベル
    # 作図用の値を作成
    dow_idx   = lubridate::wday(date, week_start = dow_start_idx), # 曜日番号(列インデックス)
    dow_label = lubridate::wday(date, week_start = dow_start_idx, label = TRUE) # 曜日ラベル
  ) |> 
  dplyr::group_by(year, month) |> # インデックスの作成用
  dplyr::mutate(
    cell_idx = dplyr::if_else(
      condition = date >= date_from, # 指定期間の場合
      true  = day + head(dow_idx, n = 1) - 1, 
      false = NA_real_
    ), # セルインデックス
    week_idx = dplyr::if_else(
      condition = date >= date_from, # 指定期間の場合
      true  = (cell_idx - 1) %/% 7 + 1, 
      false = NA_real_
    ) # 週番号(行インデックス)
  ) |> 
  dplyr::ungroup() |> 
  dplyr::filter(date >= date_from) # 期間外のデータを除去

# 祝日情報を取得
holiday_vec <- zipangu::jholiday(
  year = lubridate::year(date_from):lubridate::year(date_to), lang = "jp"
) |> 
  unlist() |> 
  lubridate::as_date()

# 祝日の暦データを作成
tmp_holiday_df <- tibble::tibble(
  date = holiday_vec, # 祝日の日付
  holiday_label = holiday_vec |> 
    names() |> 
    stringr::str_remove(pattern = "\\d"), # 祝日ラベル
  dow_idx = lubridate::wday(date, week_start = dow_start_idx) # 振替休日の作成用
) |> 
  dplyr::filter(dplyr::between(date, left = date_from, right = date_to)) |>  # 期間内のデータを抽出
  dplyr::arrange(date)

# 「ひと月のカレンダー」のときのコードで作成
holiday_df
calendar_df


# ラベル用の関数を作成
str_year <- function(string) {
  paste0(string, "年")
}
str_month <- function(string) {
  paste0(string, "月")
}

# タイトル用の文字列を作成
title_label <- paste0(
  format(date_from, format = "%Y年%m月%d日"), "から", 
  format(date_to, format = "%Y年%m月%d日"), "までのカレンダー"
)

# 横軸ラベルを作成
dow_label_vec <- lubridate::wday(1:7, week_start = dow_start_idx, label = TRUE) |> # 日本語名
  sort()

# 任意期間のカレンダーを作図:(休日のラベル色を変更)
ggplot() + 
  geom_tile(data = calendar_df,
            mapping = aes(x = dow_idx, y = week_idx),
            fill = "white", color = "black") + # 平日セル
  geom_tile(data = calendar_df, 
            mapping = aes(x = dow_idx, y = week_idx, fill = date_type), 
            color = "black", alpha = 0.1) + # 休日セル
  geom_text(data = calendar_df, 
            mapping = aes(x = dow_idx-0.4, y = week_idx-0.4, label = day), 
            size = 3, hjust = 0, vjust = 1) + # 日付ラベル
  scale_x_continuous(breaks = 1:7, labels = dow_label_vec) + # 曜日軸
  scale_y_reverse(breaks = 1:6) + # 週軸
  scale_fill_manual(breaks = c("weekday", "holiday", "weekend"), 
                    values = c("white", "red", "blue")) + # 休日用塗りつぶし色
  # facet_wrap(year ~ month, labeller = labeller(year = str_year, month = str_month), scales = "free_x") + # 月ごとに分割
  # coord_cartesian(expand = FALSE) + # 描画領域:(facet_wrap用)
  facet_grid(year ~ month, labeller = labeller(year = str_year, month = str_month), switch = "y") + # 年・月ごとに分割
  coord_fixed(ratio = 1, expand = FALSE) + # 描画領域:(facet_grid用)
  theme(
    axis.title = element_blank(), # 軸ラベル
    axis.text.x = element_text(size = 10), # 横軸目盛ラベル
    axis.text.y = element_blank(), # 縦軸目盛ラベル
    axis.ticks = element_blank(), # 軸目盛指示線
    #panel.grid.major = element_blank(), # 主グリッド線
    panel.grid.minor = element_blank(), # 副グリッド線
    panel.border = element_rect(fill = NA), # グラフ領域の枠線
    #panel.background = element_blank(), # グラフ領域の背景
    strip.text = element_text(size = 15, face = "bold"), # ファセットラベルの文字
    strip.background = element_blank(), # ファセットラベル領域の背景
    strip.placement = "outside", # ファセットラベルの位置
    legend.position = "none" # 凡例の位置
  ) + # 図の体裁
  labs(x = "曜日", y = "週")

# 任意期間のカレンダーを作図:(休日のラベル色を変更)
ggplot() + 
  geom_tile(data = calendar_df, 
            mapping = aes(x = dow_idx, y = week_idx), 
            fill = "white", color = "black") + # 日付セル
  geom_text(data = calendar_df, 
            mapping = aes(x = dow_idx-0.4, y = week_idx-0.4, label = day, color = date_type), 
            size = 3, hjust = 0, vjust = 1) + # 日付ラベル
  scale_x_continuous(breaks = 1:7, labels = dow_label_vec) + # 曜日軸
  scale_y_reverse(breaks = 1:6) + # 週軸
  scale_color_manual(breaks = c("weekday", "holiday", "weekend"), 
                     values = c("black", "red", "blue")) + # 休日用文字色
  # facet_wrap(year ~ month, labeller = labeller(year = str_year, month = str_month), scales = "free_x") + # 月ごとに分割
  # coord_cartesian(expand = FALSE) + # 描画領域:(facet_wrap用)
  facet_grid(year ~ month, labeller = labeller(year = str_year, month = str_month), switch = "y") + # 年・月ごとに分割
  coord_fixed(ratio = 1, expand = FALSE) + # 描画領域:(facet_grid用)
  theme(
    axis.title = element_blank(), # 軸ラベル
    axis.text.x = element_text(size = 10), # 横軸目盛ラベル
    axis.text.y = element_blank(), # 縦軸目盛ラベル
    axis.ticks = element_blank(), # 軸目盛指示線
    #panel.grid.major = element_blank(), # 主グリッド線
    panel.grid.minor = element_blank(), # 副グリッド線
    panel.border = element_rect(fill = NA), # グラフ領域の枠線
    #panel.background = element_blank(), # グラフ領域の背景
    strip.text = element_text(size = 15, face = "bold"), # ファセットラベルの文字
    strip.background = element_blank(), # ファセットラベル領域の背景
    strip.placement = "outside", # ファセットラベルの位置
    legend.position = "none" # 凡例の位置
  ) + # 図の体裁
  labs(x = "曜日", y = "週")


### ・任意の期間：ツイート数のヒートマップ -----

# アカウントを指定
screen_name <- "anemptyarchive"

# アカウントでツイートを収集
tweet_df <- rtweet::get_timeline(screen_name, n = 30000, include_rts = TRUE)
tweet_df |> 
  dplyr::select(created_at)


# ツイート数を集計
tmp_freq_df <- tweet_df |> 
  dplyr::mutate(
    date = created_at |> 
      lubridate::with_tz(tzone = "Etc/GMT") |> # 協定世界時を明示
      lubridate::with_tz(tzone = "Asia/Tokyo") |> # 日本標準時に変換
      lubridate::floor_date(unit = "day") |> 
      lubridate::as_date()
  ) |> 
  dplyr::count(date, name = "n") |> # ツイート数をカウント
  dplyr::arrange(date)


# 期間を指定
date_from <- min(tmp_freq_df[["date"]])
date_to   <- max(tmp_freq_df[["date"]])
date_from; date_to

# 「任意の期間」のコードで作成
calendar_df

# ツイート日の暦データを作成
freq_df <- tmp_freq_df |> 
  dplyr::mutate(
    year  = lubridate::year(date), 
    month = lubridate::month(date), 
  ) |> 
  dplyr::left_join(
    calendar_df |> 
      dplyr::select(date, dow_idx, week_idx), 
    by = "date"
  ) # 作図用の値を結合


# タイトル用の文字列を作成
title_label <- paste0(
  "@", screen_name, "のツイート数"
)
subtitle_label <- paste0(
  format(date_from, format = "%Y年%m月%d日"), "から", 
  format(date_to, format = "%Y年%m月%d日"), "までの", 
  "総ツイート数：", sum(freq_df[["n"]])
)

# ツイート数のヒートマップ
ggplot() + 
  geom_tile(data = calendar_df, 
            mapping = aes(x = dow_idx, y = week_idx), 
            fill = "white", color = "black") + # 日付セル
  geom_tile(data = freq_df, 
            mapping = aes(x = dow_idx, y = week_idx, fill = n), 
            color = "black") + # ツイート数ヒートマップ
  geom_label(data = calendar_df, 
             mapping = aes(x = dow_idx-0.5, y = week_idx-0.5, 
                           label = stringr::str_pad(day, side = "left", width = 2, pad = " "), color = date_type), 
             size = 3.5, hjust = 0, vjust = 1, label.padding = unit(0.1, units = "line"), 
             na.rm = TRUE, show.legend = FALSE) + # 日付ラベル
  geom_text(data = freq_df, 
            mapping = aes(x = dow_idx, y = week_idx, label = n), 
            size = 4) + # ツイート数ラベル
  scale_x_continuous(breaks = 1:7, labels = dow_label_vec) + # 曜日軸
  scale_y_reverse(breaks = 1:6) + # 週軸
  scale_color_manual(breaks = c("weekday", "holiday", "weekend"), 
                     values = c("black", "red", "blue")) + # 休日用文字色
  scale_fill_gradient2(low = "gray", mid = "white", high = "green") + # 投稿数グラデーション
  facet_grid(year ~ month, labeller = labeller(year = str_year, month = str_month), switch = "y") + # 年・月ごとに分割
  coord_fixed(ratio = 1, expand = FALSE) + # 描画領域
  theme(
    axis.title = element_blank(), # 軸ラベル
    axis.text.x = element_text(size = 30), # 横軸目盛ラベル
    axis.text.y = element_blank(), # 縦軸目盛ラベル
    axis.ticks = element_blank(), # 軸目盛指示線
    #panel.grid.major = element_blank(), # 主グリッド線
    panel.grid.minor = element_blank(), # 副グリッド線
    panel.border = element_rect(fill = NA), # グラフ領域の枠線
    #panel.background = element_blank(), # グラフ領域の背景
    plot.title = element_text(size = 50, face = "bold", hjust = 0.5), # タイトル
    plot.subtitle = element_text(size = 30, face = "bold", hjust = 1), # サブタイトル
    strip.text = element_text(size = 30, face = "bold"), # ファセットラベルの文字
    strip.background = element_blank(), # ファセットラベル領域の背景
    strip.placement = "outside" # ファセットラベルの位置
  ) + # 図の体裁
  labs(title = title_label, 
       subtitle = subtitle_label, 
       fill = "ツイート数", 
       x = "曜日", y = "週")


### ・任意の期間：ブログ投稿数のヒートマップ -----

## tozan_book.Rprojのch6.Rを参照

# ファイルパスを指定
file_path <- "data/url.rds"
file_path <- "../data/url.rds"

# URLデータを読み込み
url_vec <- readRDS(file = file_path)
head(url_vec)

# ブログのURLを指定
blog_url <- "https://www.anarchive-beta.com/"

# 投稿日を抽出
date_vec <- url_vec |> 
  stringr::str_remove(pattern = paste0(blog_url, "entry/")) |> # 日時を示す文字列を抽出
  lubridate::as_datetime(tz = "Asia/Tokyo") |> # タイムゾーンを設定
  lubridate::as_date() |> 
  sort()


# 期間を設定
date_from <- min(date_vec)
date_to   <- max(date_vec)
date_to   <- lubridate::today()
date_from; date_to

# 「任意の期間」のコードで作成
calendar_df

# 記事投稿数を集計
post_df <- tibble::tibble(
  date = date_vec
) |> 
  dplyr::count(date, name = "post") |> # 投稿数をカウント |> 
  dplyr::left_join(
    calendar_df |> 
      dplyr::select(date, year, month, dow_idx, week_idx), 
    by = "date"
  ) |> # 作図用の値を結合
  dplyr::arrange(date)


# 横軸ラベルを作成
dow_label_vec <- lubridate::wday(1:7, week_start = dow_start_idx, label = TRUE) |> # 日本語名
  sort()

# タイトル用の文字列を作成
title_label <- paste0(
  format(date_from, format = "%Y年%m月%d日"), "から", 
  format(date_to, format = "%Y年%m月%d日"), "のブログ記事投稿数"
)

# 任意期間のカレンダーを作図:(休日のラベル色を変更)
ggplot() + 
  geom_tile(data = calendar_df, 
            mapping = aes(x = dow_idx, y = week_idx), 
            fill = "white", color = "black") + # 日付セル
  geom_tile(data = post_df, 
            mapping = aes(x = dow_idx, y = week_idx, fill = post), 
            color = "black") + # 投稿数ヒートマップ
  geom_text(data = post_df, 
            mapping = aes(x = dow_idx, y = week_idx, label = post), 
            size = 5) + # 投稿数ラベル
  geom_label(data = calendar_df, 
             mapping = aes(x = dow_idx-0.5, y = week_idx-0.5, 
                           label = stringr::str_pad(day, side = "left", width = 2, pad = " "), color = date_type), 
             size = 3.5, hjust = 0, vjust = 1, label.padding = unit(0.1, units = "line"), 
             na.rm = TRUE, show.legend = FALSE) + # 日付ラベル
  scale_x_continuous(breaks = 1:7, labels = dow_label_vec) + # 曜日軸
  scale_y_reverse(breaks = 1:6) + # 週軸
  scale_color_manual(breaks = c("weekday", "holiday", "weekend"), 
                     values = c("black", "red", "blue")) + # 休日用文字色
  scale_fill_gradient2(low = "gray", mid = "white", high = "green") + # 投稿数グラデーション
  facet_grid(year ~ month, labeller = labeller(year = str_year, month = str_month), switch = "y") + # 年・月ごとに分割
  coord_fixed(ratio = 1, expand = FALSE) + # 描画領域
  theme(
    axis.title = element_blank(), # 軸ラベル
    axis.text.x = element_text(size = 30), # 横軸目盛ラベル
    axis.text.y = element_blank(), # 縦軸目盛ラベル
    axis.ticks = element_blank(), # 軸目盛指示線
    #panel.grid.major = element_blank(), # 主グリッド線
    panel.grid.minor = element_blank(), # 副グリッド線
    panel.border = element_rect(fill = NA), # グラフ領域の枠線
    #panel.background = element_blank(), # グラフ領域の背景
    plot.title = element_text(size = 50, face = "bold", hjust = 0.5), # タイトル
    plot.subtitle = element_text(size = 30, face = "bold", hjust = 1), # サブタイトル
    strip.text = element_text(size = 30, face = "bold"), # ファセットラベルの文字
    strip.background = element_blank(), # ファセットラベル領域の背景
    strip.placement = "outside" # ファセットラベルの位置
  ) + # 図の体裁
  labs(title = title_label, 
       subtitle = paste0("総記事数:", sum(post_df[["post"]])), 
       fill = "投稿数", 
       x = "曜日", y = "週")



# 2つの日付の差から経過期間(n年mか月とl日)を求めたい --------------------------------------------------

# 利用パッケージ
library(tidyverse)
library(lubridate)


### ・月ごとの経過期間 -----

# 基準日を指定
date_from <- "2022-05-15" |> 
  lubridate::as_date()

# 期間の最終日を指定
date_max <- "2023-06-15" |> 
  lubridate::as_date()


# 基準日以降の月ごとに経過期間を計算
month_df <- tibble::tibble(
  # 1か月間隔の日付を作成
  date = seq(
    from = date_from |> 
      lubridate::rollforward(roll_to_first = TRUE), # 基準日の翌月の初日
    to = date_max |> 
      lubridate::floor_date(unit = "month"), # 期間の最終日の月の初日
    by = "month"
  )
) |> 
  dplyr::mutate(
    # 期間計算用の値を作成
    date_day = date |> 
      lubridate::day() |> 
      as.numeric(), # 経過日の日にち
    from_day = date_from |> 
      lubridate::day() |> 
      as.numeric(), # 基準日の日にち
    pre_last_day = date |> 
      lubridate::rollback() |> 
      lubridate::day() |> 
      as.numeric(), # 経過日の前月の末日
    # 経過期間を計算
    y = lubridate::interval(start = date_from, end = date) |> 
      lubridate::time_length(unit = "year") |> 
      floor(), # 経過年数
    m = lubridate::interval(start = date_from, end = date) |> 
      lubridate::time_length(unit = "month") |> 
      floor() %% 12, # 経過月数
    d = dplyr::case_when(
      from_day >= pre_last_day ~ 1, # 基準日の日にちが無い月の場合は「1にち」
      from_day == 1 ~ 0, # 基準日が月初の場合は「0にち」
      from_day <= pre_last_day ~ pre_last_day - from_day + 1 # 基準日の日にちが有る月の場合は「前月における基準日からの日数」
    ), # 経過日数
    days = lubridate::interval(start = date_from, end = date) |> 
      lubridate::time_length(unit = "day"), # 総経過日数
    ymd_label = paste0(y, "年", m, "か月", d, "日") # 経過期間ラベル
  )
month_df


### ・日ごとの経過期間 -----

# 基準日を指定
date_from <- "2022-05-15" |> 
  lubridate::as_date()

# 期間の最終日を指定
date_max <- "2023-06-20" |> 
  lubridate::as_date()


# 基準日以降の日ごとに経過期間を計算
date_df <- tibble::tibble(
  # 1日間隔の日付を作成
  date = seq(from = date_from , to = date_max, by = "day")
) |> 
  dplyr::mutate(
    # 期間計算用の値を作成
    date_day = date |> 
      lubridate::day() |> 
      as.numeric(), # 経過日の日にち
    from_day = date_from |> 
      lubridate::day() |> 
      as.numeric(), # 基準日の日にち
    pre_last_day = date |> 
      lubridate::rollback() |> 
      lubridate::day() |> 
      as.numeric(), # 経過日の前月の末日
    # 経過期間を計算
    y = lubridate::interval(start = date_from, end = date) |> 
      lubridate::time_length(unit = "year") |> 
      floor(), # 経過年数
    m = lubridate::interval(start = date_from, end = date) |> 
      lubridate::time_length(unit = "month") |> 
      floor() %% 12, # 経過月数
    d = dplyr::case_when(
      (from_day > date_day & from_day <  pre_last_day) ~ pre_last_day - from_day + date_day, # 前月の途中から
      (from_day > date_day & from_day >= pre_last_day) ~ date_day, # 当月の頭から
      from_day <= date_day ~ date_day - from_day # 当月の途中から
    ), # 経過日数
    days = lubridate::interval(start = date_from, end = date) |> 
      lubridate::time_length(unit = "day"), # 総経過日数
    ymd_label = paste0(y, "年", m, "か月", d, "日") # 経過期間ラベル
  )
date_df


### ・日ごとの事前・事後の経過期間 -----

# 基準日を指定
date_from <- "2022-05-30" |> 
  lubridate::as_date()

# 期間を指定
date_min <- "2021-05-10" |> 
  lubridate::as_date()
date_max <- "2022-06-20" |> 
  lubridate::as_date()


# 基準日以前・以降の日ごとに経過期間を計算
date_df <- tibble::tibble(
  # 1日間隔の日付を作成
  date = seq(from = date_min, to = date_max, by = "day")
) |> 
  dplyr::mutate(
    # 期間計算用の値を作成
    date_day = date |> 
      lubridate::day() |> 
      as.numeric(), # 経過日の日にち
    from_day = date_from |> 
      lubridate::day() |> 
      as.numeric(), # 基準日の日にち
    last_day = date |> 
      lubridate::rollforward() |> 
      lubridate::day() |> 
      as.numeric(),# 基準日の月の末日
    pre_last_day = date |> 
      lubridate::rollback() |> 
      lubridate::day() |> 
      as.numeric(), # 経過日の前月の末日
    # 経過期間を計算
    y = dplyr::case_when(
      date >= date_from ~ lubridate::interval(start = date_from, end = date) |> 
        lubridate::time_length(unit = "year") |> 
        floor(), 
      date < date_from ~ lubridate::interval(start = date, end = date_from) |> 
        lubridate::time_length(unit = "year") |> 
        floor() * (-1), 
    ), # 経過年数
    m = dplyr::case_when(
      date >= date_from ~ lubridate::interval(start = date_from, end = date) |> 
        lubridate::time_length(unit = "month") |> 
        floor() %% 12, 
      date < date_from ~ lubridate::interval(start = date, end = date_from) |> 
        lubridate::time_length(unit = "month") |> 
        floor() %% 12 * (-1)
    ), # 経過月数
    d = dplyr::case_when(
      (date >= date_from & from_day >  date_day & from_day <  pre_last_day) ~ pre_last_day - from_day + date_day, # 前月の途中から当月の途中まで
      (date >= date_from & from_day >  date_day & from_day >= pre_last_day) ~ date_day, # 当月の頭から当月の途中まで
      (date >= date_from & from_day <= date_day) ~ date_day - from_day, # 当月の途中から当月の途中まで
      (date <  date_from & from_day >= date_day) ~ date_day - from_day, # 当月の途中から当月の途中まで
      (date <  date_from & from_day <  date_day) ~ date_day - last_day - from_day # 当月の途中から翌月の途中まで
    ), # 経過日数
    days = lubridate::interval(start = date_from, end = date) |> 
      lubridate::time_length(unit = "day"), # 総経過日数
    ymd_label = paste0(y, "年", m, "か月", d, "日") # 経過期間ラベル
  )
date_df


### ・ヒートマップによる可視化 -----

# ヒートマップ用に経過期間データを整形
heatmap_df <- date_df |> 
  dplyr::select(date, d, ymd_label, day = date_day) |> 
  dplyr::mutate(
    year_month = date |> 
      format(format = "%Y-%m") |> 
      factor() # 年月ラベル
  )


# 経過期間をヒートマップで可視化
ggplot() + 
  geom_tile(data = heatmap_df, 
            mapping = aes(x = year_month, y = day, fill = d)) + # 日付セル
  geom_tile(mapping = aes(x = format(date_from, format = "%Y-%m"), y = lubridate::day(date_from)), 
            fill = "blue", color = "blue", alpha = 0.1) + # 基準日セル
  geom_text(data = heatmap_df, 
            mapping = aes(x = year_month, y = day, label = ymd_label)) + # 経過期間ラベル
  scale_y_reverse(breaks = 1:31) + # 日軸
  #scale_fill_gradient(low = "white" , high = "#00A968") + # 経過日数のグラデーション:(正の値のみの場合)
  scale_fill_gradient2(low = "hotpink", mid = "white" , high = "#00A968") + # 経過日数のグラデーション:(負の値を含む場合)
  theme(panel.grid.minor = element_blank()) + # 図の体裁
  labs(title = paste0(format(date_from, format = "%Y年%m月%d日"), "からの経過期間"), 
       fill = "日数の差", 
       x = "年-月", y = "日")


### ・カレンダーによる可視化 -----

# 期間の開始日を設定
date_min <- min(date_df[["date"]])

# カレンダー用に経過期間データを整形
calendar_df <- date_df |> 
  dplyr::select(date, day = date_day, d, ymd_label) |> 
  dplyr::bind_rows(
    tibble::tibble(
      # 1日間隔の日付を作成
      date = seq(
        from = (date_min - lubridate::days(1)) |> # (-1は期間の開始日が月初の場合の簡易的対策)
          lubridate::floor_date(unit = "month"), # 期間の開始日の月の初日
        to   = date_min - lubridate::days(1), # 期間の開始の前日
        by = "day"
      )
    )
  ) |> # 基準日の月のインデックスの計算用
  dplyr::arrange(date) |> # インデックスの計算用
  dplyr::mutate(
    year = date |> 
      lubridate::year() |> 
      as.numeric(), # 経過日の年
    month = date |> 
      lubridate::month() |> 
      as.numeric(), # 経過日の月
    dow_idx   = lubridate::wday(date), # 曜日番号(列インデックス)
    dow_label = lubridate::wday(date, label = TRUE) # 曜日ラベル
  ) |> 
  dplyr::group_by(year, month) |> # インデックスの作成用
  dplyr::mutate(
    cell_idx = dplyr::if_else(
      condition = date >= date_min, 
      true = dplyr::row_number() + head(dow_idx, n = 1) - 1, 
      false = NA_real_
    ), # セルインデックス
    week_idx = dplyr::if_else(
      condition = date >= date_min, 
      true = (cell_idx - 1) %/% 7 + 1, 
      false = NA_real_
    ) # 週番号(行インデックス)
  ) |> 
  dplyr::ungroup() |> 
  dplyr::filter(date >= date_min) # 基準日以前の日付を除去

# 基準日を格納
date_from_df <- calendar_df |> 
  dplyr::filter(date == date_from) # 基準日のデータを抽出


# ラベル用の関数を作成
str_year <- function(string) {
  paste0(string, "年")
}
str_month <- function(string) {
  paste0(string, "月")
}

# 経過期間をカレンダー上のヒートマップで可視化
ggplot() + 
  geom_tile(data = calendar_df, 
            mapping = aes(x = dow_idx, y = week_idx, fill = d), 
            color = "black") + # 日付セル
  geom_tile(data = date_from_df, 
            mapping = aes(x = dow_idx, y = week_idx), 
            fill = "blue", color = "blue", alpha = 0.1) + # 基準日セル
  geom_label(data = calendar_df, 
             mapping = aes(x = dow_idx-0.5, y = week_idx-0.5, label = day), 
             hjust = 0, vjust = 1, label.padding = unit(0.1, units = "line"), 
             size = 1.5) + # 日付ラベル
  geom_text(data = calendar_df, 
            mapping = aes(x = dow_idx, y = week_idx, label = ymd_label), 
            size = 1.5) + # 経過日数ラベル
  scale_x_continuous(breaks = 1:7, labels = lubridate::wday(1:7, label = TRUE)) + # 曜日軸
  scale_y_reverse(breaks = 1:6) + # 週軸
  #scale_fill_gradient(low = "white" , high = "#00A968") + # 経過日数のグラデーション:(正の値のみの場合)
  scale_fill_gradient2(low = "hotpink", mid = "white" , high = "#00A968") + # 経過日数のグラデーション:(負の値を含む場合)
  facet_wrap(year ~ month, labeller = labeller(year = str_year, month = str_month)) + # 月ごとに分割
  theme(panel.grid.minor = element_blank()) + # 図の体裁
  labs(title = paste0(format(date_from, format = "%Y年%m月%d日"), "からの経過期間"), 
       fill = "経過日数", 
       x = "曜日", y = "週")


