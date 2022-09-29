
# 三角グラフの作図 ----------------------------------------------------------------

# 利用パッケージ
library(tidyverse)
library(ggrepel)
library(gganimate)
library(MCMCpack)

# チェック用
library(ggplot2)


# 三角図の座標 -----------------------------------------------------------------

# 軸目盛の位置を指定
axis_vals <- seq(from = 0, to = 1, by = 0.1)

# 枠線用の値を作成
ternary_axis_df <- tibble::tibble(
  y_1_start = c(0.5, 0, 1),         # 始点のx軸の値
  y_2_start = c(0.5*sqrt(3), 0, 0), # 始点のy軸の値
  y_1_end = c(0, 1, 0.5),           # 終点のx軸の値
  y_2_end = c(0, 0, 0.5*sqrt(3)),   # 終点のy軸の値
  axis = c("x_1", "x_2", "x_3")     # 元の軸
)

# グリッド線用の値を作成
ternary_grid_df <- tibble::tibble(
  y_1_start = c(
    0.5 * axis_vals, 
    axis_vals, 
    0.5 * axis_vals + 0.5
  ), # 始点のx軸の値
  y_2_start = c(
    sqrt(3) * 0.5 * axis_vals, 
    rep(0, times = length(axis_vals)), 
    sqrt(3) * 0.5 * (1 - axis_vals)
  ), # 始点のy軸の値
  y_1_end = c(
    axis_vals, 
    0.5 * axis_vals + 0.5, 
    0.5 * rev(axis_vals)
  ), # 終点のx軸の値
  y_2_end = c(
    rep(0, times = length(axis_vals)), 
    sqrt(3) * 0.5 * (1 - axis_vals), 
    sqrt(3) * 0.5 * rev(axis_vals)
  ), # 終点のy軸の値
  axis = c("x_1", "x_2", "x_3") |> 
    rep(each = length(axis_vals)) # 元の軸
)

# 軸ラベル用の値を作成
ternary_axislabel_df <- tibble::tibble(
  y_1 = c(0.25, 0.5, 0.75),               # x軸の値
  y_2 = c(0.25*sqrt(3), 0, 0.25*sqrt(3)), # y軸の値
  label = c("x[1]", "x[2]", "x[3]"),      # 軸ラベル
  h = c(3, 0.5, -2),  # 水平方向の調整用の値
  v = c(0.5, 3, 0.5), # 垂直方向の調整用の値
  axis = c("x_1", "x_2", "x_3") # 元の軸
)

# 軸目盛ラベル用の値を作成
ternary_ticklabel_df <- tibble::tibble(
  y_1 = c(
    0.5 * axis_vals, 
    axis_vals, 
    0.5 * axis_vals + 0.5
  ), # x軸の値
  y_2 = c(
    sqrt(3) * 0.5 * axis_vals, 
    rep(0, times = length(axis_vals)), 
    sqrt(3) * 0.5 * (1 - axis_vals)
  ), # y軸の値
  label = c(
    rev(axis_vals), 
    axis_vals, 
    rev(axis_vals)
  ), # 軸目盛ラベル
  h = c(
    rep(1.5, times = length(axis_vals)), 
    rep(1.5, times = length(axis_vals)), 
    rep(-0.5, times = length(axis_vals))
  ), # 水平方向の調整用の値
  v = c(
    rep(0.5, times = length(axis_vals)), 
    rep(0.5, times = length(axis_vals)), 
    rep(0.5, times = length(axis_vals))
  ), # 垂直方向の調整用の値
  angle = c(
    rep(-60, times = length(axis_vals)), 
    rep(60, times = length(axis_vals)), 
    rep(0, times = length(axis_vals))
  ), # ラベルの表示角度
  axis = c("x_1", "x_2", "x_3") |> 
    rep(each = length(axis_vals)) # 元の軸
)

# 三角図の枠を作成
ggplot() + 
  geom_segment(data = ternary_grid_df, 
               mapping = aes(x = y_1_start, y = y_2_start, xend = y_1_end, yend = y_2_end), 
               color = "gray50", linetype = "dashed") + # 三角図のグリッド線
  geom_segment(data = ternary_axis_df, 
               mapping = aes(x = y_1_start, y = y_2_start, xend = y_1_end, yend = y_2_end), 
               color = "gray50") + # 三角図の枠線
  geom_text(data = ternary_ticklabel_df, 
            mapping = aes(x = y_1, y = y_2, label = label, hjust = h, vjust = v, angle = angle)) + # 三角図の軸目盛ラベル
  geom_text(data = ternary_axislabel_df, 
            mapping = aes(x = y_1, y = y_2, label = label, hjust = h, vjust = v), 
            parse = TRUE, size = 6) + # 三角図の軸ラベル
  scale_x_continuous(breaks = c(0, 0.5, 1), labels = NULL) + # x軸
  scale_y_continuous(breaks = c(0, 0.25*sqrt(3), 0.5*sqrt(3)), labels = NULL) + # y軸
  coord_fixed(ratio = 1, clip = "off") + # アスペクト比
  theme(
    axis.ticks = element_blank(), # 目盛の指示線
    panel.grid.minor = element_blank() # 補助目盛のグリッド線
  ) + # 図の体裁
  labs(title = "Ternary Plot", 
       subtitle = parse(text = "x==(list(x[1], x[2], x[3]))"), 
       x = "", y = "")

# 三角図の各軸を可視化
ggplot() + 
  geom_segment(data = ternary_axis_df, 
               mapping = aes(x = y_1_start, y = y_2_start, xend = y_1_end, yend = y_2_end), 
               color = "gray50") + # 三角図の枠線
  geom_segment(data = ternary_grid_df, 
               mapping = aes(x = y_1_start, y = y_2_start, xend = y_1_end, yend = y_2_end, color = axis), 
               linetype = "dashed") + # 三角図のグリッド線
  geom_text(data = ternary_ticklabel_df, 
            mapping = aes(x = y_1, y = y_2, label = label, hjust = h, vjust = v, angle = angle, color = axis)) + # 三角図の軸目盛ラベル
  geom_text(data = ternary_axislabel_df, 
            mapping = aes(x = y_1, y = y_2, label = label, hjust = h, vjust = v, color = axis), 
            parse = TRUE, size = 6) + # 三角図の軸ラベル
  scale_x_continuous(breaks = c(0, 0.5, 1), labels = NULL) + # x軸
  scale_y_continuous(breaks = c(0, 0.25*sqrt(3), 0.5*sqrt(3)), labels = NULL) + # y軸
  scale_color_manual(breaks = c("x_1", "x_2", "x_3"), values = c("red", "green4", "blue")) + # 線と文字の色
  coord_fixed(ratio = 1, clip = "off") + # アスペクト比
  theme(
    axis.ticks = element_blank(), # 目盛の指示線
    panel.grid.minor = element_blank(), # 補助目盛のグリッド線
    legend.position = "none" # 凡例の位置
  ) + # 図の体裁
  labs(title = "Ternary Plot", 
       subtitle = parse(text = "x==(list(x[1], x[2], x[3]))"), 
       x = "", y = "")


# 散布図 ---------------------------------------------------------------------

# データ数を指定
N <- 9

# ディリクレ分布のパラメータを指定
alpha_k <- c(1, 1, 1)

# ディリクレ分布の乱数を生成
x_nk <- MCMCpack::rdirichlet(n = N, alpha = alpha_k)

# 三角座標に変換して格納
data_df <- tibble::tibble(
  y_1 = x_nk[, 2] + 0.5 * x_nk[, 3], # 三角座標のx軸の値
  y_2 = sqrt(3) * 0.5 * x_nk[, 3],   # 三角座標のy軸の値
  label = paste0(
    "(", round(x_nk[, 1], 2), ", ", round(x_nk[, 2], 2), ", ", round(x_nk[, 3], 2), ")"
  ) # データラベル
)

# パラメータラベル用の文字列を作成
param_text <- paste0(
  "list(", 
  "alpha==(list(", paste0(alpha_k, collapse = ", "), "))", 
  ", N==", N, 
  ", x==(list(x[1], x[2], x[3]))", 
  ")"
)

# 散布図を作成
ggplot() + 
  geom_segment(data = ternary_axis_df, 
               mapping = aes(x = y_1_start, y = y_2_start, xend = y_1_end, yend = y_2_end), 
               color = "gray50") + # 三角図の枠線
  geom_segment(data = ternary_grid_df, 
               mapping = aes(x = y_1_start, y = y_2_start, xend = y_1_end, yend = y_2_end, color = axis), 
               linetype = "dashed") + # 三角図のグリッド線
  geom_text(data = ternary_ticklabel_df, 
            mapping = aes(x = y_1, y = y_2, label = label, hjust = h, vjust = v, angle = angle, color = axis)) + # 三角図の軸目盛ラベル
  geom_text(data = ternary_axislabel_df, 
            mapping = aes(x = y_1, y = y_2, label = label, hjust = h, vjust = v, color = axis), 
            parse = TRUE, size = 6) + # 三角図の軸ラベル
  geom_point(data = data_df, 
             mapping = aes(x = y_1, y = y_2), 
             color = "orange", size = 3) + # 観測データ
  ggrepel::geom_label_repel(data = data_df, 
                            mapping = aes(x = y_1, y = y_2, label = label), 
                            color = "orange", alpha = 0.9, size = 3) + # データラベル
  scale_x_continuous(breaks = c(0, 0.5, 1), labels = NULL) + # x軸
  scale_y_continuous(breaks = c(0, 0.25*sqrt(3), 0.5*sqrt(3)), labels = NULL) + # y軸
  scale_color_manual(breaks = c("x_1", "x_2", "x_3"), values = c("red", "green4", "blue")) + # 線と文字の色
  coord_fixed(ratio = 1, clip = "off") + # アスペクト比
  theme(
    axis.ticks = element_blank(), # 軸目盛の指示線
    panel.grid.minor = element_blank(), # 軸目盛の補助線
    legend.position = "none" # 凡例の位置
  ) + # 図の体裁
  labs(title = "Scatter Ternary Plot", 
       subtitle = parse(text = param_text), 
       x = "", y = "")


# 散布図のアニメーション ------------------------------------------------------------

# 3次元変数の値を指定
x_1_vals <- c(
  seq(0.2, 0.4, by = 0.01), 
  rev(seq(0.2, 0.39, by = 0.01)), 
  rep(0.2, times = 19)
)
x_2_vals <- c(
  rep(0.2, times = 21), 
  seq(0.21, 0.4, by = 0.01), 
  rev(seq(0.21, 0.39, by = 0.01))
)
x_3_vals <- c(
  rev(seq(0.4, 0.6, by = 0.01)), 
  rep(0.4, times = 20), 
  seq(from = 0.41, to = 0.59, by = 0.01)
)

# 三角座標に変換して格納
anime_data_df <- tibble::tibble(
  y_1 = x_2_vals + 0.5 * x_3_vals, # x軸の値
  y_2 = sqrt(3) * 0.5 * x_3_vals, # y軸の値
  frame = paste0("(", round(x_1_vals, 2), ", ", round(x_2_vals, 2), ", ", round(x_3_vals, 2), ")") |> 
    factor(levels = paste0("(", round(x_1_vals, 2), ", ", round(x_2_vals, 2), ", ", round(x_3_vals, 2), ")")) # フレーム切替用ラベル
)

# フレーム数を設定
frame_num <- nrow(anime_data_df)

# 三角座標に変換して格納
anime_data_df <- tibble::tibble(
  y_1 = x_2_vals + 0.5 * x_3_vals, # x軸の値
  y_2 = sqrt(3) * 0.5 * x_3_vals, # y軸の値
  frame = paste0("(", round(x_1_vals, 2), ", ", round(x_2_vals, 2), ", ", round(x_3_vals, 2), ")") |> 
    factor(levels = paste0("(", round(x_1_vals, 2), ", ", round(x_2_vals, 2), ", ", round(x_3_vals, 2), ")")) # フレーム切替用ラベル
)

# 散布図のアニメーションを作図
anime_graph <- ggplot() + 
  geom_segment(data = ternary_axis_df, 
               mapping = aes(x = y_1_start, y = y_2_start, xend = y_1_end, yend = y_2_end), 
               color = "gray50") + # 三角図の枠線
  geom_segment(data = ternary_grid_df, 
               mapping = aes(x = y_1_start, y = y_2_start, xend = y_1_end, yend = y_2_end, color = axis), 
               linetype = "dashed") + # 三角図のグリッド線
  geom_text(data = ternary_ticklabel_df, 
            mapping = aes(x = y_1, y = y_2, label = label, hjust = h, vjust = v, angle = angle, color = axis)) + # 三角図の軸目盛ラベル
  geom_text(data = ternary_axislabel_df, 
            mapping = aes(x = y_1, y = y_2, label = label, hjust = h, vjust = v, color = axis), 
            parse = TRUE, size = 6) + # 三角図の軸ラベル
  geom_point(data = anime_data_df, 
             mapping = aes(x = y_1, y = y_2), 
             color = "orange", size = 6) + 
  geom_label(data = anime_data_df, 
             mapping = aes(x = y_1, y = y_2, label = frame), 
             hjust = 0, vjust = -1, color = "orange", alpha = 0.8, size = 5) + 
  gganimate::transition_manual(frame) + # フレーム
  scale_x_continuous(breaks = c(0, 0.5, 1), labels = NULL) + # x軸
  scale_y_continuous(breaks = c(0, 0.25*sqrt(3), 0.5*sqrt(3)), labels = NULL) + # y軸
  scale_color_manual(breaks = c("x_1", "x_2", "x_3"), values = c("red", "green4", "blue")) + # 線と文字の色
  coord_fixed(ratio = 1, clip = "off") + # アスペクト比
  theme(axis.ticks = element_blank(), 
        panel.grid.minor = element_blank(), 
        legend.position = "none") + # 図の体裁
  labs(title = "Ternary Plot", 
       subtitle = parse(text = "x==(list(x[1], x[2], x[3]))"), 
       x = "", y = "")

# gif画像を作成
gganimate::animate(plot = anime_graph, nframes = frame_num, fps = 10, width = 600, height = 600)


# 等高線図 --------------------------------------------------------------------

# 三角座標の値を作成
y_1_vals <- seq(from = 0, to = 1, length.out = 301)
y_2_vals <- seq(from = 0, to = 0.5*sqrt(3), length.out = 300)

# 格子点を作成
y_mat <- tidyr::expand_grid(
  y_1 = y_1_vals, 
  y_2 = y_2_vals
) |> # 格子点を作成
  as.matrix() # マトリクスに変換

# 3次元変数に変換
x_mat <- tibble::tibble(
  x_2 = y_mat[, 1] - y_mat[, 2] / sqrt(3), 
  x_3 = 2 * y_mat[, 2] / sqrt(3)
) |> # 元の座標に変換
  dplyr::mutate(
    x_2 = dplyr::if_else(x_2 >= 0 & x_2 <= 1, true = x_2, false = as.numeric(NA)), 
    x_3 = dplyr::if_else(x_3 >= 0 & x_3 <= 1 & !is.na(x_2), true = x_3, false = as.numeric(NA)), 
    x_1 = 1 - x_2 - x_3, 
    x_1 = dplyr::if_else(x_1 >= 0 & x_1 <= 1, true = x_1, false = as.numeric(NA))
  ) |> # 範囲外の値をNAに置換
  dplyr::select(x_1, x_2, x_3) |> # 順番を変更
  as.matrix() # マトリクスに変換


# ディリクレ分布のパラメータを指定
alpha_k <- c(4, 2, 3)

# ディリクレ分布の確率密度を計算
dens_df <- tibble::tibble(
  y_1 = y_mat[, 1], # x軸の値
  y_2 = y_mat[, 2], # y軸の値
  density = MCMCpack::ddirichlet(x = x_mat, alpha = alpha_k), # 確率密度
) |> 
  dplyr::mutate(
    fill_flg = !is.na(rowSums(x_mat)), 
    density = dplyr::if_else(fill_flg, true = density, false = as.numeric(NA))
  ) # 範囲外の値をNAに置換


# パラメータラベル用の文字列を作成
param_text <- paste0(
  "list(", 
  "alpha==(list(", paste0(alpha_k, collapse = ", "), "))", 
  ", x==(list(x[1], x[2], x[3]))", 
  ")"
)

# 等高線図を作成
ggplot() + 
  geom_segment(data = ternary_grid_df, 
               mapping = aes(x = y_1_start, y = y_2_start, xend = y_1_end, yend = y_2_end), 
               color = "gray50", linetype = "dashed") + # 三角図のグリッド線
  geom_segment(data = ternary_axis_df, 
               mapping = aes(x = y_1_start, y = y_2_start, xend = y_1_end, yend = y_2_end), 
               color = "gray50") + # 三角図の枠線
  geom_text(data = ternary_ticklabel_df, 
            mapping = aes(x = y_1, y = y_2, label = label, hjust = h, vjust = v, angle = angle)) + # 三角図の軸目盛ラベル
  geom_text(data = ternary_axislabel_df, 
            mapping = aes(x = y_1, y = y_2, label = label, hjust = h, vjust = v), 
            parse = TRUE, size = 6) + # 三角図の軸ラベル
  geom_contour_filled(data = dens_df, 
                      mapping = aes(x = y_1, y = y_2, z = density, fill = ..level..), 
                      alpha = 0.8) + # 確率密度の等高線
  scale_x_continuous(breaks = c(0, 0.5, 1), labels = NULL) + # x軸
  scale_y_continuous(breaks = c(0, 0.25*sqrt(3), 0.5*sqrt(3)), labels = NULL) + # y軸
  coord_fixed(ratio = 1, clip = "off") + # アスペクト比
  theme(axis.ticks = element_blank(), 
        panel.grid.minor = element_blank()) + # 図の体裁
  labs(title = "Ternary Contour Plot", 
       subtitle = parse(text = param_text), 
       fill = "density", 
       x = "", y = "")

# ヒートマップを作成
ggplot() + 
  geom_segment(data = ternary_grid_df, 
               mapping = aes(x = y_1_start, y = y_2_start, xend = y_1_end, yend = y_2_end), 
               color = "gray50", linetype = "dashed") + # 三角図のグリッド線
  geom_segment(data = ternary_axis_df, 
               mapping = aes(x = y_1_start, y = y_2_start, xend = y_1_end, yend = y_2_end), 
               color = "gray50") + # 三角図の枠線
  geom_text(data = ternary_ticklabel_df, 
            mapping = aes(x = y_1, y = y_2, label = label, hjust = h, vjust = v, angle = angle)) + # 三角図の軸目盛ラベル
  geom_text(data = ternary_axislabel_df, 
            mapping = aes(x = y_1, y = y_2, label = label, hjust = h, vjust = v), 
            parse = TRUE, size = 6) + # 三角図の軸ラベル
  geom_tile(data = dens_df, 
            mapping = aes(x = y_1, y = y_2, fill = density, alpha = fill_flg)) + # 確率密度のヒートマップ
  scale_alpha_manual(breaks = c(TRUE, FALSE), values = c(0.8, 0), guide = "none") + # 透過
  scale_fill_viridis_c() + # グラデーション
  #scale_fill_gradientn(colors = c("blue", "green", "yellow", "orange")) + # グラデーション
  scale_x_continuous(breaks = c(0, 0.5, 1), labels = NULL) + # x軸
  scale_y_continuous(breaks = c(0, 0.25*sqrt(3), 0.5*sqrt(3)), labels = NULL) + # y軸
  coord_fixed(ratio = 1, clip = "off") + # アスペクト比
  theme(axis.ticks = element_blank(), 
        panel.grid.minor = element_blank()) + # 図の体裁
  labs(title = "Ternary Heatmap", 
       subtitle = parse(text = param_text), 
       fill = "density", 
       x = "", y = "")

# 散布図によるヒートマップを作成
ggplot() + 
  geom_point(data = dens_df, 
             mapping = aes(x = y_1, y = y_2, color = density, alpha = fill_flg)) + # 確率密度の散布図
  scale_alpha_manual(breaks = c(TRUE, FALSE), values = c(0.5, 0), guide = "none") + # 透過
  scale_color_viridis_c() + # グラデーション
  #scale_color_gradientn(colors = c("blue", "green", "yellow", "orange")) + # グラデーション
  geom_segment(data = ternary_grid_df, 
               mapping = aes(x = y_1_start, y = y_2_start, xend = y_1_end, yend = y_2_end), 
               color = "gray50", alpha = 0.5, linetype = "dashed") + # 三角図のグリッド線
  geom_segment(data = ternary_axis_df, 
               mapping = aes(x = y_1_start, y = y_2_start, xend = y_1_end, yend = y_2_end), 
               color = "gray50") + # 三角図の枠線
  geom_text(data = ternary_ticklabel_df, 
            mapping = aes(x = y_1, y = y_2, label = label, hjust = h, vjust = v, angle = angle)) + # 三角図の軸目盛ラベル
  geom_text(data = ternary_axislabel_df, 
            mapping = aes(x = y_1, y = y_2, label = label, hjust = h, vjust = v), 
            parse = TRUE, size = 6) + # 三角図の軸ラベル
  scale_x_continuous(breaks = c(0, 0.5, 1), labels = NULL) + # x軸
  scale_y_continuous(breaks = c(0, 0.25*sqrt(3), 0.5*sqrt(3)), labels = NULL) + # y軸
  coord_fixed(ratio = 1, clip = "off") + # アスペクト比
  theme(axis.ticks = element_blank(), 
        panel.grid.minor = element_blank()) + # 図の体裁
  labs(title = "Ternary Heatmap", 
       subtitle = parse(text = param_text), 
       fill = "density", 
       x = "", y = "")


