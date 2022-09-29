
# 任意の値で等高線を動かしたい ----------------------------------------------------------

# 利用パッケージ
library(tidyverse)
library(mvnfast)

# チェック用
library(ggplot2)


### ・等高線図の作成 -----

# x軸とy軸の値を作成
x_vals <- seq(from = -3, to = 3, length.out = 301) |> 
  round(digits = 2)

# 格子点を作成
x_mat <- tidyr::expand_grid(
  x1 = x_vals, 
  x2 = x_vals
) |> # 全ての組み合わせを作成
  as.matrix() # マトリクスに変換


# 次元数を設定:(固定)
D <- 2

# 平均ベクトルを指定
mu_d <- rep(0, times = D)

# 分散共分散行列を指定
sigma_dd <- diag(D) # 単位行列


# ガウス分布を計算
dens_df <- tibble::tibble(
  x1 = x_mat[, 1], # x軸の値
  x2 = x_mat[, 2], # y軸の値
  dens = mvnfast::dmvn(X = x_mat, mu = mu_d, sigma = sigma_dd) # 確率密度
)

# 等高線図を作成
ggplot() + 
  geom_contour(data = dens_df, mapping = aes(x = x1, y = x2, z = dens, color = ..level..)) + # 等高線
  coord_fixed(ratio = 1, xlim = c(min(x_vals), max(x_vals)), ylim = c(min(x_vals), max(x_vals))) + # アスペクト比
  labs(x = expression(x[1]), y = expression(x[2]))


### ・等高線図のアニメーションの作成 -----

# x軸の分散を指定
sigma2_1 <- 1

# y軸の分散を指定
sigma2_2 <- 1

# 共分散として利用する値を指定
sigma_12_vals <- seq(from = -0.8, to = 0.8, by = 0.1) |> 
  round(digits = 1)


# 共分散の値ごとにガウス分布を計算
anime_dens_df <- tidyr::expand_grid(
  sigma_12 = sigma_12_vals, # 共分散
  x1 = x_vals, # x軸の値
  x2 = x_vals  # y軸の値
) |> # 格子点を複製
  dplyr::group_by(sigma_12) |> # 分布の計算用にグループ化
  dplyr::mutate(
    dens = mvnfast::dmvn(
      X = x_mat, 
      mu = mu_d, 
      sigma = matrix(c(sigma2_1, unique(sigma_12), unique(sigma_12), sigma2_2), nrow = D, ncol = D)
    ) # 確率密度
  ) |> 
  dplyr::ungroup() # グループ化を解除

# 等高線図のアニメーションを作成
anime_graph <- ggplot() + 
  geom_contour(data = anime_dens_df, mapping = aes(x = x1, y = x2, z = dens, color = ..level..)) + # 等高線
  gganimate::transition_manual(sigma_12) + # フレーム
  coord_fixed(ratio = 1, xlim = c(min(x_vals), max(x_vals)), ylim = c(min(x_vals), max(x_vals))) + # アスペクト比
  labs(title = paste0("covariance = ", "{current_frame}"), 
       x = expression(x[1]), y = expression(x[2]))

# gif画像を作成
gganimate::animate(
  plot = anime_graph, nframes = length(sigma_12_vals), fps = 10, 
  width = 800, height = 800
)


### ・値を指定して等高線図のアニメーションの作成 -----

# 倍率を指定
rate <- 0.5

# ガウス分布の断面を計算
anime_ellipse_df <- anime_dens_df |> 
  dplyr::group_by(sigma_12) |> # 断面の計算用にグループ化
  dplyr::mutate(
    max_dens = mvnfast::dmvn(
      X = mu_d, 
      mu = mu_d, 
      sigma = matrix(c(sigma2_1, unique(sigma_12), unique(sigma_12), sigma2_2), nrow = D, ncol = D)
    ), # 確率密度の最大値
    dens = dplyr::if_else(
      dens >= max_dens * rate, 
      true = max_dens * rate,  
      false = 0 # (true引数の値から離れた値を設定)
    ) # 断面を作成
  ) |> 
  dplyr::ungroup() # グループ化を解除

# 値を指定して等高線図のアニメーションを作成
anime_graph <- ggplot() + 
  geom_contour(data = anime_ellipse_df, mapping = aes(x = x1, y = x2, z = dens, color = ..level..), 
               bins = 2) + # 等高線
  gganimate::transition_manual(sigma_12) + # フレーム
  coord_fixed(ratio = 1, xlim = c(min(x_vals), max(x_vals)), ylim = c(min(x_vals), max(x_vals))) + # アスペクト比
  labs(title = "covariance = {current_frame}", 
       x = expression(x[1]), y = expression(x[2]))

# gif画像を作成
gganimate::animate(
  plot = anime_graph, nframes = length(sigma_12_vals), fps = 10, 
  width = 800, height = 800
)


# 閾値を指定
threshold <- 0.001

# 等高線の点を抽出
anime_point_df <- anime_dens_df |> 
  dplyr::group_by(sigma_12) |> # 点の抽出用にグループ化
  dplyr::mutate(
    max_dens = mvnfast::dmvn(
      X = mu_d, 
      mu = mu_d, 
      sigma = matrix(c(sigma2_1, unique(sigma_12), unique(sigma_12), sigma2_2), nrow = D, ncol = D)
    ) # 確率密度の最大値
  ) |> 
  dplyr::ungroup() |> # グループ化を解除
  dplyr::filter(abs(dens - max_dens*rate) <= threshold) # 等高線の点を抽出

# 散布図による等高線のアニメーションを作成
anime_graph <- ggplot() + 
  geom_point(data = anime_point_df, mapping = aes(x = x1, y = x2, color = dens), 
             size = 2) + # 散布図
  gganimate::transition_manual(sigma_12) + # フレーム
  coord_fixed(ratio = 1, xlim = c(min(x_vals), max(x_vals)), ylim = c(min(x_vals), max(x_vals))) + # アスペクト比
  labs(title = "covariance = {current_frame}", 
       color = "density", 
       x = expression(x[1]), y = expression(x[2]))

# gif画像を作成
gganimate::animate(
  plot = anime_graph, nframes = length(sigma_12_vals), fps = 10, 
  width = 800, height = 800
)


### ・完成例 -----

# 等高線の値のラベルを作成
anime_label_df <- anime_ellipse_df |> 
  dplyr::distinct(sigma_12, max_dens) |> # 各共分散と確率密度の最大値を抽出
  dplyr::mutate(
    dens_label = paste0(
      "max density     : ", round(max_dens, digits = 5), "\n",
      "drawn density : ", round(max_dens * rate, digits = 5)
    )
  ) # ラベルを作成

# 2次元ガウス分布の等高線図のアニメーションを作成
anime_graph <- ggplot() + 
  geom_contour_filled(data = anime_dens_df, mapping = aes(x = x1, y = x2, z = dens, fill = ..level..), 
                      alpha = 0.8) + # 塗りつぶし等高線
  geom_contour(data = anime_ellipse_df, mapping = aes(x = x1, y = x2, z = dens, color = ..level..), 
               bins = 2, color = "red", size = 1, linetype = "dashed") + # 等高線
  geom_label(data = anime_label_df, mapping = aes(x = min(x_vals), y = max(x_vals), label = dens_label), 
             hjust = "inward", vjust = "inward", color = "red") + # 等高線ラベル
  gganimate::transition_manual(sigma_12) + # フレーム
  coord_fixed(ratio = 1) + # アスペクト比
  labs(title = "Bivariate Normal Distribution", 
       subtitle = "covariance = {current_frame}", 
       fill = "density", 
       x = expression(x[1]), y = expression(x[2]))

# gif画像を作成
gganimate::animate(
  plot = anime_graph, nframes = length(sigma_12_vals), fps = 10, 
  width = 800, height = 800
)


