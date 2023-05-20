
# ggplot2でカレンダーを作成したい -------------------------------------------------------------

# 利用パッケージ
library(tidyverse)
library(lubridate)
library(zipangu)

# 追加パッケージ
library(rtweet)


### ・セルインデックスの確認 -----

# セルインデックスを作成
index_df <- tibble::tibble(
  week_idx  = rep(1:6, each = 7), 
  dow_idx   = rep(1:7, times = 6), 
  dow_label = rep(lubridate::wday(1:7, label = TRUE), times = 6), 
  cell_idx  = 1:(6*7)
)
index_df

# セルインデックスを作図
ggplot() + 
  geom_tile(data = index_df, 
            mapping = aes(x = dow_idx, y = week_idx), 
            fill = "white", color = "black") + # セル
  geom_text(data = index_df, 
            mapping = aes(x = dow_idx, y = week_idx, label = cell_idx), 
            size = 10) + # セルインデックスラベル
  scale_x_continuous(breaks = 1:max(index_df[["dow_idx"]])) + # 曜日軸
  scale_y_reverse(breaks = 1:max(index_df[["week_idx"]])) + # 週軸
  coord_fixed(ratio = 1) + # アスペクト比
  theme(panel.grid.minor = element_blank()) + # 図の体裁
  labs(title = "セルインデックス (cell_idx)", 
       x = "曜日インデックス (dow_idx)", y = "週インデックス (week_idx)")


### ・ひと月：(基本形) -----

# 年を指定
year <- 2023

# 月を指定
month <- 5

# 初日の日付を作成
date_from <- paste0(year, "-", month, "-1") |> 
  lubridate::as_date()


# ひと月の暦データを作成
calendar_df <- tibble::tibble(
  # 1日間隔の日付を作成
  date = seq(
    from = date_from, # 初日
    to   = date_from |> 
      lubridate::rollforward(), # 末日
    by = "day"
  )
) |> 
  dplyr::mutate(
    # 暦用の値を作成
    year  = lubridate::year(date), 
    month = lubridate::month(date), 
    day   = lubridate::day(date), 
    dow_idx   = lubridate::wday(date), # 曜日(列)インデックス
    dow_label = lubridate::wday(date, label = TRUE) # 曜日ラベル
  ) |> 
  dplyr::mutate(
    cell_idx = dplyr::row_number() + head(dow_idx, n = 1) - 1, # セルインデックス
    week_idx = (cell_idx - 1) %/% 7 + 1 # 週(行)インデックス
  )
calendar_df


# ひと月のカレンダーを作図:(基本形)
ggplot() + 
  geom_tile(data = calendar_df, 
            mapping = aes(x = dow_idx, y = week_idx), 
            fill = "white", color = "black") + # 日付セル
  geom_text(data = calendar_df, 
            mapping = aes(x = dow_idx, y = week_idx, label = day), 
            size = 10) + # 日付ラベル
  scale_x_continuous(breaks = 1:7, labels = lubridate::wday(1:7, label = TRUE)) + # 曜日軸
  scale_y_reverse(breaks = 1:max(calendar_df[["week_idx"]])) + # 週軸
  coord_fixed(ratio = 1) + # アスペクト比
  theme(panel.grid.minor = element_blank()) + # 図の体裁
  labs(title = paste0(year, "年", month, "月のカレンダー"), 
       x = "曜日", y = "週")


### ・ひと月：(装飾版) -----

# 祝日情報を取得
holiday_vec <- zipangu::jholiday(year = year, lang = "jp") |> 
  unlist() |> 
  lubridate::as_date()

# 祝日の暦データを作成
tmp_holiday_df <- tibble::tibble(
  date = holiday_vec, 
  holiday_label = names(holiday_vec) # 祝日ラベル
) |> 
  dplyr::mutate(
    y = lubridate::year(date), 
    m = lubridate::month(date)
  ) |> 
  dplyr::filter(y == year, m == month) |> # 指定した月の祝日を抽出
  dplyr::left_join(
    calendar_df |> 
      dplyr::select(date, dow_idx, week_idx), 
    by = "date"
  ) |> # 作図用の値を結合
  dplyr::select(date, holiday_label, dow_idx, week_idx)
tmp_holiday_df

# 振替休日の暦データを作成
holiday_df <- tmp_holiday_df |> 
  dplyr::filter(dow_idx == 1) |> # 日曜日の祝日を抽出
  dplyr::mutate(
    # 元の祝日の1日後の値を作成
    date = date + lubridate::days(1), 
    holiday_label = "振替休日",
    dow_idx  = dow_idx + 1, 
    week_idx = week_idx
  ) |> 
  dplyr::bind_rows(tmp_holiday_df) |> # 祝日の暦データを結合
  dplyr::arrange(date)
holiday_df

# 週末の暦データを作成
weekend_df <- calendar_df |> 
  dplyr::select(date, dow_idx, dow_label, week_idx) |> 
  dplyr::filter(dow_idx %in% c(1, 7), !(date %in% holiday_df[["date"]])) # 祝日でない土・日を抽出
weekend_df


# 今日の日付を作成
tmp_date <- lubridate::today() |> 
  as.character()

# 予定日を指定
schedule_vec <- c(tmp_date, "2023-05-07") |> 
  lubridate::as_date()

# 予定用の記号を指定
symbol_vec <- c("🚶", "🌳")


# 予定日の暦データを作成
schedule_df <- tibble::tibble(
  date = schedule_vec, 
  symbol = symbol_vec
) |> 
  dplyr::left_join(
    calendar_df |> 
      dplyr::select(date, dow_idx, week_idx), 
    by = "date"
  ) # 作図用の値を結合
schedule_df


# ひと月のカレンダーを作図:(装飾版)
ggplot() + 
  geom_tile(data = calendar_df, 
            mapping = aes(x = dow_idx, y = week_idx), 
            fill = "white", color = "black") + # 日付セル
  geom_tile(data = weekend_df, 
            mapping = aes(x = dow_idx, y = week_idx, width = 1, height = 1, fill = factor(dow_idx)), 
            alpha = 0.1) + # 週末セル
  geom_tile(data = holiday_df, 
            mapping = aes(x = dow_idx, y = week_idx, width = 1, height = 1), 
            fill = "red", alpha = 0.1) + # 祝日セル
  geom_text(data = calendar_df, 
            mapping = aes(x = dow_idx-0.4, y = week_idx-0.4, label = day), 
            hjust = 0, vjust = 1, size = 10) + # 日付ラベル
  geom_text(data = holiday_df, 
            mapping = aes(x = dow_idx+0.4, y = week_idx-0.4, label = holiday_label), 
            hjust = 1, vjust = 1, size = 5) + # 祝日ラベル
  geom_text(data = schedule_df, 
            mapping = aes(x = dow_idx, y = week_idx, label = symbol), 
            size = 10) + # 予定ラベル
  scale_x_continuous(breaks = 1:7, labels = NULL, 
                     sec.axis = dup_axis(trans = ~., labels = lubridate::wday(1:7, label = TRUE))) + # 曜日軸
  scale_y_reverse(breaks = 1:max(calendar_df[["week_idx"]])) + # 週軸
  scale_fill_manual(breaks = c(1, 7), values = c("red", "blue")) + # 祝日用の塗りつぶし
  coord_fixed(ratio = 1, expand = FALSE) + # 描画領域
  theme(
    axis.title = element_blank(), # 軸ラベル
    axis.text.x = element_text(size = 25), # 横軸目盛ラベル
    axis.text.y = element_blank(), # 縦軸目盛ラベル
    axis.ticks = element_blank(), # 軸目盛指示線
    panel.grid.major = element_blank(), # 主グリッド線
    panel.grid.minor = element_blank(), # 副グリッド線
    panel.border = element_rect(fill = NA), # グラフ領域の枠線
    panel.background = element_blank(), # グラフ領域の背景
    plot.title = element_text(size = 20, face = "bold"), # タイトル
    plot.subtitle = element_text(size = 30, face = "bold", hjust = 0.5), # サブタイトル
    legend.position = "none" # 凡例の位置
  ) + # 図の体裁
  labs(title = paste0(year, "年"), 
       subtitle = paste0(month, "月"), 
       x = "曜日", y = "週")


### ・ひと年：(基本形) -----

# 年を指定
year <- 2023


# ひと年の暦データを作成
calendar_df <- tibble::tibble(
  # 1日間隔の日付を作成
  date = seq(
    from = paste0(year, "-1-1") |> # 正月
      lubridate::as_date(), 
    to   = paste0(year, "-12-31") |> # 大晦日
      lubridate::as_date(), 
    by = "day"
  )
) |> 
  dplyr::mutate(
    # 暦用の値を作成
    year  = lubridate::year(date), 
    month = lubridate::month(date), 
    day   = lubridate::day(date), 
    dow_idx   = lubridate::wday(date), # 曜日(列)インデックス
    dow_label = lubridate::wday(date, label = TRUE) # 曜日ラベル
  ) |> 
  dplyr::group_by(year, month) |> # インデックスの作成用
  dplyr::mutate(
    cell_idx = dplyr::row_number() + head(dow_idx, n = 1) - 1, # セルインデックス
    week_idx = (cell_idx - 1) %/% 7 + 1 # 週(行)インデックス
  ) |> 
  dplyr::ungroup()
calendar_df


# ラベル用の関数を作成
str_month <- function(string) {
  paste0(string, "月")
}

# ひと年のカレンダーを作図:(基本形)
ggplot() + 
  geom_tile(data = calendar_df, 
            mapping = aes(x = dow_idx, y = week_idx), 
            fill = "white", color = "black") + # 日付セル
  geom_text(data = calendar_df, 
            mapping = aes(x = dow_idx, y = week_idx, label = day), 
            size = 5) + # 日付ラベル
  scale_x_continuous(breaks = 1:7, labels = lubridate::wday(1:7, label = TRUE)) + # 曜日軸
  scale_y_reverse(breaks = 1:max(calendar_df[["week_idx"]])) + # 週軸
  facet_wrap(month ~ ., labeller = "label_both") + # 月ごとに分割
  theme(panel.grid.minor = element_blank()) + # 図の体裁
  labs(title = paste0(year, "年のカレンダー"), 
       x = "曜日", y = "週")


### ・ひと年：(装飾版) -----

# 祝日情報を取得
holiday_vec <- zipangu::jholiday(year = year, lang = "jp") |> 
  unlist() |> 
  lubridate::as_date()

# 祝日の暦データを作成
tmp_holiday_df <- tibble::tibble(
  date = holiday_vec, 
  holiday_label = names(holiday_vec), 
  month = lubridate::month(date)
) |> # 作図用の値を結合
  dplyr::left_join(
    calendar_df |> 
      dplyr::select(date, dow_idx, week_idx), 
    by = "date"
  )
tmp_holiday_df

# 振替休日の暦データを作成
holiday_df <- tmp_holiday_df |> 
  dplyr::filter(dow_idx == 1) |> # 日曜日の祝日を抽出
  dplyr::mutate(
    # 元の祝日の1日後の値を作成
    date = date + lubridate::days(1), 
    holiday_label = "振替休日", 
    month = lubridate::month(date),
    dow_idx  = dow_idx + 1, 
    week_idx = week_idx
  ) |> 
  dplyr::bind_rows(tmp_holiday_df) |> # 祝日の暦データを結合
  dplyr::arrange(date)
holiday_df

# 週末の暦データを作成
weekend_df <- calendar_df |> 
  dplyr::select(date, month, dow_idx, dow_label, week_idx) |> 
  dplyr::filter(dow_idx %in% c(1, 7), !(date %in% holiday_df[["date"]])) # 祝日でない土・日を抽出
weekend_df


## Hello.RprojのReadData.Rを参照

# グループを指定
group_id <- 1

# 予定の暦データを作成
schedule_df <- join_df |> 
  dplyr::filter(groupID == group_id) |> # 指定グループの加入情報を抽出
  dplyr::left_join(
    member_df |> 
      dplyr::select(memberID, memberName, birthDate) |> 
      dplyr::distinct(), # 重複を削除
    by = "memberID"
  ) |> # 誕生日情報を結合
  dplyr::mutate(
    month  = lubridate::month(birthDate), 
    day    = lubridate::day(birthDate), 
    date   = paste0(year, "-", as.character(month), "-", as.character(day)) |> 
      lubridate::as_date(), 
    symbol = "🎂" # 記号を指定
  ) |> 
  dplyr::select(date, symbol, memberName, month) |> 
  dplyr::left_join(
    calendar_df |> 
      dplyr::select(date, dow_idx, week_idx), 
    by = "date"
  ) |> # 作図用の値を結合
  dplyr::arrange(date)
schedule_df


# ひと年のカレンダーを作図:(装飾版)
ggplot() + 
  geom_tile(data = calendar_df, 
            mapping = aes(x = dow_idx, y = week_idx), 
            fill = "white", color = "black") + # 日付セル
  geom_tile(data = weekend_df, 
            mapping = aes(x = dow_idx, y = week_idx, width = 1, height = 1, fill = factor(dow_idx)), 
            alpha = 0.1) + # 週末セル
  geom_tile(data = holiday_df, 
            mapping = aes(x = dow_idx, y = week_idx, width = 1, height = 1), 
            fill = "red", alpha = 0.1) + # 祝日セル
  geom_text(data = calendar_df, 
            mapping = aes(x = dow_idx-0.4, y = week_idx-0.4, label = day), 
            hjust = 0, vjust = 1, size = 5) + # 日付ラベル
  geom_text(data = holiday_df, 
            mapping = aes(x = dow_idx+0.45, y = week_idx-0.4, label = holiday_label), 
            hjust = 1, vjust = 1, size = 3) + # 祝日ラベル
  geom_text(data = schedule_df, 
            mapping = aes(x = dow_idx, y = week_idx, label = symbol), 
            size = 6) + # 予定ラベル
  scale_x_continuous(breaks = 1:7, labels = NULL, 
                     sec.axis = dup_axis(trans = ~., labels = lubridate::wday(1:7, label = TRUE))) + # 曜日軸
  scale_y_reverse(breaks = 1:max(calendar_df[["week_idx"]])) + # 週軸
  scale_fill_manual(breaks = c("1", "7"), values = c("red", "blue")) + # 祝日用の塗りつぶし
  facet_wrap(month ~ ., nrow = 3, ncol = 4, 
             labeller = labeller(month = str_month), scales = "free_x") + # 年・月ごとに分割
  coord_cartesian(expand = FALSE) + # 描画領域
  theme(
    axis.title = element_blank(), # 軸ラベル
    axis.text.x = element_text(size = 15), # 横軸目盛ラベル
    axis.text.y = element_blank(), # 縦軸目盛ラベル
    axis.ticks = element_blank(), # 軸目盛指示線
    panel.grid.major = element_blank(), # 主グリッド線
    panel.grid.minor = element_blank(), # 副グリッド線
    panel.border = element_rect(fill = NA), # グラフ領域の枠線
    #panel.background = element_blank(), # グラフ領域の背景
    plot.title = element_text(size = 30, face = "bold", hjust = 0.5), # タイトル
    strip.text = element_text(size = 20, face = "bold"), # ファセットラベルの文字
    strip.background = element_blank(), # ファセットラベル領域の背景
    strip.placement = "outside", # ファセットラベルの位置
    legend.position = "none" # 凡例の位置
  ) + # 図の体裁
  labs(title = paste0(year, "年"), 
       x = "曜日", y = "週")



### ・任意の期間：(基本形) -----

# 開始日を指定
date_from <- "2022-06-29" |> 
  lubridate::as_date()

# 終了日を指定
date_to <- "2023-12-10" |> 
  lubridate::as_date()


# 任意期間の暦データを作成
calendar_df <- tibble::tibble(
  # 1日間隔の日付を作成
  date = seq(
    from = date_from |> 
      lubridate::floor_date(unit = "month"), # 開始日の月の初日
    to = date_to, # 終了日
    by = "day"
  )
) |> 
  dplyr::mutate(
    # 暦用の値を作成
    year  = lubridate::year(date), 
    month = lubridate::month(date), 
    day   = lubridate::day(date), 
    dow_idx   = lubridate::wday(date), # 曜日(列)インデックス
    dow_label = lubridate::wday(date, label = TRUE) # 曜日ラベル
  ) |> 
  dplyr::group_by(year, month) |> # インデックスの作成用
  dplyr::mutate(
    cell_idx = dplyr::if_else(
      condition = date >= date_from, 
      true = dplyr::row_number() + head(dow_idx, n = 1) - 1, 
      false = NA_real_
    ), # セルインデックス
    week_idx = dplyr::if_else(
      condition = date >= date_from, 
      true = (cell_idx - 1) %/% 7 + 1, 
      false = NA_real_
    ) # 週(行)インデックス
  ) |> 
  dplyr::ungroup()
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

# 任意期間のカレンダーを作図:(基本形)
ggplot() + 
  geom_tile(data = calendar_df, 
            mapping = aes(x = dow_idx, y = week_idx), 
            fill = "white", color = "black", na.rm = TRUE) + # 日付セル
  geom_text(data = calendar_df, 
            mapping = aes(x = dow_idx, y = week_idx, label = day), 
            size = 5, na.rm = TRUE) + # 日付ラベル
  scale_x_continuous(breaks = 1:7, labels = lubridate::wday(1:7, label = TRUE)) + # 曜日軸
  scale_y_reverse(breaks = 1:max(calendar_df[["week_idx"]], na.rm = TRUE)) + # 週軸
  #facet_wrap(year ~ month, labeller = labeller(year = str_year, month = str_month), scales = "free_x") + # 年・月ごとに分割
  facet_grid(year ~ month, labeller = labeller(year = str_year, month = str_month), switch = "y") + # 年・月ごとに分割
  theme(panel.grid.minor = element_blank()) + # 図の体裁
  labs(title = title_label, 
       x = "曜日", y = "週")


### ・任意の期間：(装飾版) -----

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
tmp_freq_df

# 期間を指定
date_from <- min(tmp_freq_df[["date"]])
date_to   <- max(tmp_freq_df[["date"]])
date_from; date_to

# 「任意の期間：(基本形)」のコードで、任意期間の暦データを作成
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

# 任意期間のカレンダーを作図:(装飾版)
ggplot() + 
  geom_tile(data = calendar_df, 
            mapping = aes(x = dow_idx, y = week_idx), 
            fill = "white", color = "black", na.rm = TRUE) + # 日付セル
  geom_tile(data = freq_df, 
            mapping = aes(x = dow_idx, y = week_idx, fill = n), 
            color = "black") + # ツイート数ヒートマップ
  geom_label(data = calendar_df, 
             mapping = aes(x = dow_idx-0.5, y = week_idx-0.5, 
                           label = stringr::str_pad(day, side = "left", width = 2, pad = " ")), 
             size = 3, hjust = 0, vjust = 1, label.padding = unit(0.1, units = "line"), na.rm = TRUE) + # 日付ラベル
  geom_text(data = freq_df, 
            mapping = aes(x = dow_idx, y = week_idx, label = n), 
            size = 4) + # ツイート数ラベル
  scale_x_continuous(breaks = 1:7, labels = lubridate::wday(1:7, label = TRUE)) + # 曜日軸
  scale_y_reverse(breaks = 1:max(calendar_df[["week_idx"]], na.rm = TRUE)) + # 週軸
  scale_fill_gradient(low = "white" , high = "dodgerblue1") + # ツイート数グラデーション
  facet_grid(year ~ month, labeller = labeller(year = str_year, month = str_month), switch = "y") + # 年・月ごとに分割
  coord_cartesian(expand = FALSE) + # 描画領域
  theme(
    axis.title = element_blank(), # 軸ラベル
    axis.text.x = element_text(size = 10), # 横軸目盛ラベル
    axis.text.y = element_blank(), # 縦軸目盛ラベル
    axis.ticks = element_blank(), # 軸目盛指示線
    #panel.grid.major = element_blank(), # 主グリッド線
    panel.grid.minor = element_blank(), # 副グリッド線
    panel.border = element_rect(fill = NA), # グラフ領域の枠線
    #panel.background = element_blank(), # グラフ領域の背景
    plot.title = element_text(size = 20, face = "bold"), # タイトル
    plot.subtitle = element_text(size = 15, face = "bold"), # サブタイトル
    strip.text = element_text(size = 15, face = "bold"), # ファセットラベルの文字
    strip.background = element_blank(), # ファセットラベル領域の背景
    strip.placement = "outside" # ファセットラベルの位置
  ) + # 図の体裁
  labs(title = title_label, 
       subtitle = subtitle_label, 
       fill = "ツイート数", 
       x = "曜日", y = "週")



# 2つの日付の差から経過期間(n年mか月とl日)を求めたい --------------------------------------------------

# 利用パッケージ
library(tidyverse)
library(lubridate)


### ・月ごとの経過期間 -----

# 基準日を指定
date_from <- "2020-05-05" |> 
  lubridate::as_date()

# 年数を指定
year_len <- 3


# 基準日以降の月ごとに経過期間を計算
month_df <- tibble::tibble(
  # 1か月間隔の日付を作成
  date = seq(
    from = date_from |> 
      lubridate::rollforward(roll_to_first = TRUE), # 基準日の翌月の初日
    to = (date_from + lubridate::years(year_len)) |> 
      lubridate::floor_date(unit = "month"), # 指定した年数後の基準日の月の初日
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
      as.numeric(), # 基準日の前月の末日
    # 経過期間を計算
    n = lubridate::interval(start = date_from, end = date) |> 
      lubridate::time_length(unit = "year") |> 
      floor(), # 経過年数
    m = lubridate::interval(start = date_from, end = date) |> 
      lubridate::time_length(unit = "month") |> 
      floor() %% 12, # 経過月数 - 経過年数
    l = dplyr::case_when(
      from_day >= pre_last_day ~ 1, # 基準日の日にちが無い月の場合は「1にち」
      from_day == 1 ~ 0, # 基準日が月初の場合は「0にち」
      from_day <= pre_last_day ~ pre_last_day - from_day + 1 # 基準日の日にちが有る月の場合は「前月における基準日からの日数」
    ), # 経過日数 - 経過年月数
    days = lubridate::interval(start = date_from, end = date) |> 
      lubridate::time_length(unit = "day"), # 総経過日数
    nml_label = paste0(n, "年", m, "か月", l, "日") # 経過期間ラベル
  )
month_df


### ・日ごとの経過期間：ヒートマップ -----

# 基準日を指定
date_from <- "2020-05-15" |> 
  lubridate::as_date()


# 基準日以降の日ごとに経過期間を計算
date_df <- tibble::tibble(
  # 1日間隔の日付を作成
  date = seq(
    from = date_from , # 基準日
    to = (date_from + lubridate::years(1)) |> 
      lubridate::rollforward(), # 1年後の基準日の月の末日
    by = "day"
  )
) |> 
  dplyr::mutate(
    # 期間計算用の値を作成
    year = date |> 
      lubridate::year() |> 
      as.numeric(), # 経過日の年
    month = date |> 
      lubridate::month() |> 
      as.numeric(), # 経過日の月
    day = date |> 
      lubridate::day() |> 
      as.numeric(), # 経過日の日にち
    from_day = date_from |> 
      lubridate::day() |> 
      as.numeric(), # 基準日の日にち
    pre_last_day = date |> 
      lubridate::rollback() |> 
      lubridate::day() |> 
      as.numeric(), # 基準日の前月の末日
    # 経過期間を計算
    n = lubridate::interval(start = date_from, end = date) |> 
      lubridate::time_length(unit = "year") |> 
      floor(), # 経過年数
    m = lubridate::interval(start = date_from, end = date) |> 
      lubridate::time_length(unit = "month") |> 
      floor() %% 12, # 経過月数 - 経過年数
    l = dplyr::case_when(
      (day < from_day & pre_last_day >  from_day) ~ pre_last_day - from_day + day, # 前月の途中から
      (day < from_day & pre_last_day <= from_day) ~ day, # 当月の頭から
      day >= from_day ~ day - from_day # 当月の途中から
    ), # 経過日数 - 経過年月数
    days = lubridate::interval(start = date_from, end = date) |> 
      lubridate::time_length(unit = "day"), # 総経過日数
    nml_label = paste0(n, "年", m, "か月", l, "日") # 経過期間ラベル
  )
date_df


# ヒートマップ用に経過期間データを整形
heatmap_df <- date_df |> 
  dplyr::select(date, n, m, l, days, nml_label) |> 
  dplyr::mutate(
    year_month = date |> 
      format(format = "%Y-%m") |> 
      factor(), # 年月ラベル
    day = date |> 
      lubridate::day() # 日ラベル
  )
heatmap_df

# 経過期間をヒートマップで可視化
ggplot() + 
  geom_tile(data = heatmap_df, 
            mapping = aes(x = year_month, y = day, fill = l)) + # 日付セル
  geom_tile(mapping = aes(x = format(date_from, format = "%Y-%m"), y = lubridate::day(date_from)), 
            fill = "blue", color = "blue", alpha = 0.1) + # 基準日セル
  geom_text(data = heatmap_df, 
            mapping = aes(x = year_month, y = day, label = nml_label)) + # 経過期間ラベル
  scale_y_reverse(breaks = 1:31) + # 日軸
  scale_fill_gradient(low = "white" , high = "#00A968") + # 経過日数のグラデーション
  theme(panel.grid.minor = element_blank()) + # 図の体裁
  labs(title = paste0(format(date_from, format = "%Y年%m月%d日"), "からの経過期間"), 
       fill = "日数の差", 
       x = "年-月", y = "日")


### ・日ごとの経過期間：カレンダー -----

# 基準日を指定
date_from <- "2020-05-15" |> 
  lubridate::as_date()


# 基準日以降の日ごとに経過期間を計算
date_df <- tibble::tibble(
  # 1日間隔の日付を作成
  date = seq(
    from = date_from |> 
      lubridate::floor_date(unit = "month"), # 基準日の月の初日
    to = (date_from + lubridate::years(1)) |> 
      lubridate::rollforward(), # 1年後の基準日の月の末日
    by = "day"
  )
) |> 
  dplyr::mutate(
    # 期間計算用の値を作成
    year = date |> 
      lubridate::year() |> 
      as.numeric(), # 経過日の年
    month = date |> 
      lubridate::month() |> 
      as.numeric(), # 経過日の月
    day = date |> 
      lubridate::day() |> 
      as.numeric(), # 経過日の日にち
    from_day = date_from |> 
      lubridate::day() |> 
      as.numeric(), # 基準日の日にち
    pre_last_day = date |> 
      lubridate::rollback() |> 
      lubridate::day() |> 
      as.numeric(), # 基準日の前月の末日
    # 経過期間を計算
    n = dplyr::if_else(
      condition = date >= date_from, 
      true = lubridate::interval(start = date_from, end = date) |> 
        lubridate::time_length(unit = "year") |> 
        floor(), 
      false = NA_real_
    ), # 経過年数
    m = dplyr::if_else(
      condition = date >= date_from, 
      true = lubridate::interval(start = date_from, end = date) |> 
        lubridate::time_length(unit = "month") |> 
        floor() %% 12, 
      false = NA_real_
    ), # 経過月数 - 経過年数
    l = dplyr::case_when(
      date < date_from ~ NA_real_, # 基準日前の場合
      (day < from_day & pre_last_day >  from_day) ~ pre_last_day - from_day + day, # 前月の途中から
      (day < from_day & pre_last_day <= from_day) ~ day, # 当月の頭から
      day >= from_day ~ day - from_day # 当月の途中から
    ), # 経過日数 - 経過年月数
    days = lubridate::interval(start = date_from, end = date) |> 
      lubridate::time_length(unit = "day"), # 総経過日数
    nml_label = dplyr::if_else(
      condition = date >= date_from, 
      true = paste0(n, "年", m, "か月", l, "日"), 
      false = NA_character_
    ) # 経過期間ラベル
  )
date_df


# カレンダー用に経過期間データを整形
calendar_df <- date_df |> 
  dplyr::select(date, year, month, day, l, days, nml_label) |> 
  dplyr::mutate(
    dow_idx   = lubridate::wday(date), # 曜日(列)インデックス
    dow_label = lubridate::wday(date, label = TRUE) # 曜日ラベル
  ) |> 
  dplyr::group_by(year, month) |> # インデックスの作成用
  dplyr::mutate(
    cell_idx = dplyr::if_else(
      condition = date >= date_from, 
      true = dplyr::row_number() + head(dow_idx, n = 1) - 1, 
      false = NA_real_
    ), # セルインデックス
    week_idx = dplyr::if_else(
      condition = date >= date_from, 
      true = (cell_idx - 1) %/% 7 + 1, 
      false = NA_real_
    ) # 週(行)インデックス
  ) |> 
  dplyr::ungroup()
calendar_df

# 基準日を格納
date_from_df <- calendar_df |> 
  dplyr::filter(date == date_from) # 基準日のデータを抽出
date_from_df


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
            mapping = aes(x = dow_idx, y = week_idx, fill = l), 
            color = "black", na.rm = TRUE) + # 日付セル
  geom_tile(data = date_from_df, 
            mapping = aes(x = dow_idx, y = week_idx), 
            fill = "blue", color = "blue", alpha = 0.1) + # 基準日セル
  geom_label(data = calendar_df, 
             mapping = aes(x = dow_idx-0.5, y = week_idx-0.5, label = day), 
             hjust = 0, vjust = 1, label.padding = unit(0.1, units = "line"), 
             size = 2, na.rm = TRUE) + # 日付ラベル
  geom_text(data = calendar_df, 
            mapping = aes(x = dow_idx, y = week_idx, label = nml_label), 
            size = 2, vjust = 1, na.rm = TRUE) + # 経過日数ラベル
  scale_x_continuous(breaks = 1:7, labels = lubridate::wday(1:7, label = TRUE)) + # 曜日軸
  scale_y_reverse(breaks = 1:max(calendar_df[["week_idx"]], na.rm = TRUE)) + # 週軸
  scale_fill_gradient(low = "white" , high = "#00A968") + # 経過日数のグラデーション
  facet_wrap(year ~ month, labeller = labeller(year = str_year, month = str_month), scales = "free_x") + # 月ごとに分割
  theme(panel.grid.minor = element_blank()) + # 図の体裁
  labs(title = paste0(format(date_from, format = "%Y年%m月%d日"), "からの経過期間"), 
       fill = "経過日数", 
       x = "曜日", y = "週") # ラベル

# 画像を保存
#ggsave(filename = "calendar.png", plot = last_plot(), width = 20, height = 16, dpi = 200)


### ・日ごとの事前・事後の経過期間 -----

# 基準日を指定
date_from <- "2020-05-30" |> 
  lubridate::as_date()


# 基準日以前・以降の日ごとに経過期間を計算
date_df <- tibble::tibble(
  # 1日間隔の日付を作成
  date = seq(
    from = (date_from - lubridate::years(1)) |> 
      lubridate::rollback(roll_to_first = TRUE), # 1年前の基準日の前月の初日
    to = (date_from + lubridate::days(40)) |> 
      lubridate::rollforward(), # 基準日の40日後の月の末日
    by = "day"
  )
) |> 
  dplyr::mutate(
    # 期間計算用の値を作成
    year = date |> 
      lubridate::year() |> 
      as.numeric(), # 経過日の年
    month = date |> 
      lubridate::month() |> 
      as.numeric(), # 経過日の月
    day = date |> 
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
      as.numeric(), # 基準日の前月の末日
    # 経過期間を計算
    n = dplyr::case_when(
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
    ), # 経過月数 - 経過年数
    l = dplyr::case_when(
      (date >= date_from & day <  from_day & pre_last_day >  from_day) ~ pre_last_day - from_day + day, # 前月の途中から当月の途中まで
      (date >= date_from & day <  from_day & pre_last_day <= from_day) ~ day, # 当月の頭から当月の途中まで
      (date >= date_from & day >= from_day) ~ day - from_day, # 当月の途中から当月の途中まで
      (date <  date_from & day >  from_day) ~ day - last_day - from_day, # 当月の途中から前月の途中まで
      (date <  date_from & day <= from_day) ~ day - from_day # 前月の途中から前月の頭まで
    ), # 経過日数 - 経過年月数
    days = lubridate::interval(start = date_from, end = date) |> 
      lubridate::time_length(unit = "day"), # 総経過日数
    nml_label = paste0(n, "年", m, "か月", l, "日") # 経過期間ラベル
  )
date_df


# ヒートマップ用に経過期間データを整形
heatmap_df <- date_df |> 
  dplyr::select(date, n, m, l, days, nml_label) |> 
  dplyr::mutate(
    year_month = date |> 
      format(format = "%Y-%m") |> 
      factor(), # 年月ラベル
    day = date |> 
      lubridate::day() # 日ラベル
  )
heatmap_df

# 経過期間をヒートマップで可視化
ggplot() + 
  geom_tile(data = heatmap_df, 
            mapping = aes(x = year_month, y = day, fill = l)) + # 日付セル
  geom_tile(mapping = aes(x = format(date_from, format = "%Y-%m"), y = lubridate::day(date_from)), 
            fill = "blue", color = "blue", alpha = 0.1) + # 基準日セル
  geom_text(data = heatmap_df, 
            mapping = aes(x = year_month, y = day, label = nml_label)) + # 経過期間ラベル
  scale_y_reverse(breaks = 1:31) + # 日軸
  scale_fill_gradient2(low = "hotpink", mid = "white" , high = "#00A968") + # 経過日数グラデーション:(正負)
  theme(panel.grid.minor = element_blank()) + # 図の体裁
  labs(title = paste0(format(date_from, format = "%Y年%m月%d日"), "からの経過期間"), 
       fill = "日数の差", 
       x = "年-月", y = "日")


# カレンダー用に経過期間データを整形
calendar_df <- date_df |> 
  dplyr::select(date, year, month, day, l, days, nml_label) |> 
  dplyr::mutate(
    dow_idx   = lubridate::wday(date), # 曜日(列)インデックス
    dow_label = lubridate::wday(date, label = TRUE) # 曜日ラベル
  ) |> 
  dplyr::group_by(year, month) |> # インデックスの作成用
  dplyr::mutate(
    cell_idx = dplyr::row_number() + head(dow_idx, n = 1) - 1, # セルインデックス
    week_idx = (cell_idx - 1) %/% 7 + 1 # 週(行)インデックス
  ) |> 
  dplyr::ungroup()
calendar_df

# 基準日を格納
date_from_df <- calendar_df |> 
  dplyr::filter(date == date_from) # 基準日のデータを抽出
date_from_df


# 「日ごとの経過期間：カレンダー」のコードで、ラベル用の関数を作成
str_year
str_month

# 経過期間をカレンダー上のヒートマップで可視化
ggplot() + 
  geom_tile(data = calendar_df, 
            mapping = aes(x = dow_idx, y = week_idx, fill = l), 
            color = "black", na.rm = TRUE) + # 日付セル
  geom_tile(data = date_from_df, 
            mapping = aes(x = dow_idx, y = week_idx), 
            fill = "blue", color = "blue", alpha = 0.1) + # 基準日セル
  geom_label(data = calendar_df, 
             mapping = aes(x = dow_idx-0.5, y = week_idx-0.5, label = day), # stringr::str_pad(day, side = "left", width = 2, pad = " ")
             hjust = 0, vjust = 1, label.padding = unit(0.1, units = "line"), 
             size = 2, na.rm = TRUE) + # 日付ラベル
  geom_text(data = calendar_df, 
            mapping = aes(x = dow_idx, y = week_idx, label = nml_label), 
            size = 2, vjust = 1, na.rm = TRUE) + # 経過日数ラベル
  scale_x_continuous(breaks = 1:7, labels = lubridate::wday(1:7, label = TRUE)) + # 曜日軸
  scale_y_reverse(breaks = 1:max(calendar_df[["week_idx"]], na.rm = TRUE)) + # 週軸
  scale_fill_gradient2(low = "hotpink", mid = "white" , high = "#00A968") + # 経過日数のグラデーション
  facet_wrap(year ~ month, labeller = labeller(year = str_year, month = str_month), scales = "free_x") + # 月ごとに分割
  theme(panel.grid.minor = element_blank()) + # 図の体裁
  labs(title = paste0(format(date_from, format = "%Y年%m月%d日"), "からの経過期間"), 
       fill = "経過日数", 
       x = "曜日", y = "週") # ラベル


