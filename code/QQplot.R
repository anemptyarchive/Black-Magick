
# Q-Qプロット -----------------------------------------------------------------

# 利用パッケージ
library(tidyverse)
library(patchwork)
library(magick)

# パッケージ名の省略用
library(ggplot2)


# サンプリング ------------------------------------------------------------------

# サンプルサイズを指定
N <- 100


# 正規分布のパラメータを指定
mu    <- 10
sigma <- 5

# 正規分布の乱数を生成
sample_vec <- rnorm(n = N, mean = mu, sd = sigma)


# t分布のパラメータを指定
nu <- 4

# t分布の統計量を計算
mu    <- 0
sigma <- sqrt(nu / (nu - 2))

# t分布の乱数を生成
sample_vec <- rt(n = N, df = nu)


# Q-Qプロットの作図 --------------------------------------------------------------

# 理論値の計算用のパラメータを指定
mu_z    <- 0
sigma_z <- 1

# サンプルごとの理論値を作成
sample_df <- tibble::tibble(
  i   = 1:N, # 昇順のサンプル番号(分位番号)
  x   = sort(sample_vec), # サンプル(確率変数)
  p   = (i - 0.5) / N, # 分位番号 → パーセンタイル:(両端は0.5)
  #p   = i / (N + 1), # 分位番号 → パーセンタイル:(両端も1)
  q_x = qnorm(p = p, mean = mu, sd = sigma), # パーセンタイル → クォンタイル
  q_z = qnorm(p = p, mean = mu_z, sd = sigma_z) # パーセンタイル → クォンタイル
)

# 理論値線の傾き・切片を計算
a <- (sample_df[["q_x"]][N] - sample_df[["q_x"]][1]) / (sample_df[["q_z"]][N] - sample_df[["q_z"]][1])
b <- mu - a * mu_z


# グラフサイズを設定
axis_x_size <- (sample_df[["x"]] - mu) |> 
  abs() |> 
  max() |> 
  ceiling()
axis_z_size <- (sample_df[["q_z"]] - mu_z) |> 
  abs() |> 
  max() |> 
  ceiling()

# ラベル用の文字列を作成
param_label <- paste0(
  "list(", 
  "N == ", N, ", mu == ", mu, ", sigma == ", sigma, 
  ")"
)

# Q-Qプロットを作図
ggplot() + 
  geom_abline(slope = a, intercept = b) + # 理論値の線
  geom_point(data = sample_df, 
             mapping = aes(x = q_z, y = x)) + # サンプル値と理論値の対応点
  coord_cartesian(xlim = c(mu_z-axis_z_size, mu_z+axis_z_size), 
                  ylim = c(mu-axis_x_size, mu+axis_x_size)) + # 描画領域
  labs(title = "Normal Q-Q Plot", 
       subtitle = parse(text = param_label), 
       x = "Theoretical Quantiles", 
       y = "Sample Quantiles")


# サンプル番号とクォンタイルの変換 --------------------------------------------------------

# 一時保存フォルダを指定
dir_path <- "output/tmp"

# 理論値の計算用のパラメータを指定
mu_z    <- 0
sigma_z <- 1

# サンプルごとの理論値を作成
sample_df <- tibble::tibble(
  i   = 1:N, # 昇順のサンプル番号(分位番号)
  x   = sort(sample_vec), # サンプル(確率変数)
  p   = (i - 0.5) / N, # 分位番号 → パーセンタイル:(両端は0.5)
  #p   = i / (N + 1), # 分位番号 → パーセンタイル:(両端も1)
  q_x = qnorm(p = p, mean = mu, sd = sigma), # パーセンタイル → クォンタイル
  q_z = qnorm(p = p, mean = mu_z, sd = sigma_z) # パーセンタイル → クォンタイル
)

# 理論値線の傾き・切片を計算
a <- (sample_df[["q_x"]][N] - sample_df[["q_x"]][1]) / (sample_df[["q_z"]][N] - sample_df[["q_z"]][1])
b <- mu - a * mu_z

# グラフサイズを設定
axis_x_size <- (sample_df[["x"]] - mu) |> 
  abs() |> 
  max() |> 
  ceiling()

# ラベル用の文字列を作成
dist_label <- dist_label <- paste0(
  "x[i] %~% N(list(", "mu == ", mu, ", sigma == ", sigma, "))"
)
dist_label <- dist_label <- paste0(
  "x[i] %~% t(nu == ", nu, ")"
)

# サンプルごとに作図
for(i_val in 1:N) {
  
  # サンプルを抽出
  point_df <- sample_df |> 
    dplyr::filter(i == i_val)
  x_val <- point_df[["x"]]
  p_val <- point_df[["p"]]
  q_val <- point_df[["q_z"]]
  
  # ラベル用の文字列を作成
  coord_label <- paste0(
    "(list(i, x)) == ", 
    "(list(", i_val, ", ", round(x_val, digits = 2), "))"
  )
  
  # 昇順のサンプルを作図
  graph_ix <- ggplot() + 
    geom_point(data = point_df, 
               mapping = aes(x = i, y = x), 
               color = "orange", size = 6) + # i番目のサンプル
    geom_vline(xintercept = i_val, linetype = "dotted") + # 横軸の補助線
    geom_hline(yintercept = x_val, linetype = "dotted") + # 縦軸の補助線
    geom_point(data = sample_df, 
               mapping = aes(x = i, y = x)) + # 全てのサンプル
    coord_cartesian(ylim = c(mu-axis_x_size, mu+axis_x_size)) + # サイズの統一用
    labs(title = parse(text = dist_label), 
         subtitle = parse(text = coord_label), 
         x = "sample number", 
         y = "sample value")
  
  # ラベル用の文字列を作成
  coord_label <- paste0(
    "(list(i, p)) == ", 
    "(list(", i_val, ", ", round(p_val, digits = 2), "))"
  )
  
  # サンプル番号と累積確率の変換を作図
  graph_ip <- ggplot() + 
    geom_point(data = point_df, 
               mapping = aes(x = i, y = p), 
               color = "orange", size = 6) + # i番目のサンプル
    geom_vline(xintercept = i_val, linetype = "dotted") + # 横軸の補助線
    geom_hline(yintercept = p_val, linetype = "dotted") + # 縦軸の補助線
    geom_abline(slope = 1/N, intercept = 0) + # 理論値
    geom_point(data = sample_df, 
               mapping = aes(x = i, y = p)) + # 全てのサンプル
    labs(subtitle = parse(text = coord_label), 
         x = "quantile number", 
         y = "percentile value")
  
  # ラベル用の文字列を作成
  coord_label <- paste0(
    "(list(q, p)) == ", 
    "(list(", round(q_val, digits = 2), ", ", round(p_val, digits = 2), "))"
  )
  
  # 累積分布確率を作図
  graph_qp <- ggplot() + 
    geom_point(data = point_df, 
               mapping = aes(x = q_z, y = p), 
               color = "orange", size = 6) + # i番目のサンプル
    geom_vline(xintercept = q_val, linetype = "dotted") + # 横軸の補助線
    geom_hline(yintercept = p_val, linetype = "dotted") + # 縦軸の補助線
    geom_line(data = sample_df, 
              mapping = aes(x = q_z, y = p)) + # 理論値
    geom_point(data = sample_df, 
               mapping = aes(x = q_z, y = p)) + # 全てのサンプル
    labs(subtitle = parse(text = coord_label), 
         x = "quantile value", 
         y = "probability")
  
  # ラベル用の文字列を作成
  coord_label <- paste0(
    "(list(q, x)) == ", 
    "(list(", round(q_val, digits = 2), ", ", round(x_val, digits = 2), "))"
  )
  
  # Q-Qプロットを作図
  graph_qx <- ggplot() + 
    geom_point(data = point_df, 
               mapping = aes(x = q_z, y = x), 
               color = "orange", size = 6) + # i番目のサンプル
    geom_vline(xintercept = q_val, linetype = "dotted") + # 横軸の補助線
    geom_hline(yintercept = x_val, linetype = "dotted") + # 縦軸の補助線
    geom_abline(slope = a, intercept = b) + # 理論値
    geom_point(data = sample_df, 
               mapping = aes(x = q_z, y = x)) + # 全てのサンプル
    coord_cartesian(ylim = c(mu-axis_x_size, mu+axis_x_size)) + # サイズの統一用
    labs(title = "Q-Q Plot", 
         subtitle = parse(text = coord_label), 
         x = "quantile value", 
         y = "sample value")
  
  # 並べて作図
  wrap_graph <- patchwork::wrap_plots(
    graph_ix, graph_qx, 
    graph_ip, graph_qp, 
    nrow = 2, ncol = 2
  )
  
  # ファイルを書き出し
  file_path <- paste0(dir_path, "/", stringr::str_pad(i_val, width = nchar(N), pad = "0"), ".png")
  ggplot2::ggsave(filename = file_path, plot = wrap_graph, width = 1200, height = 1200, units = "px", dpi = 100)
  
  # 途中経過を表示
  message("\r", i_val, " / ", N, appendLF = FALSE)
}

# gif画像を作成
paste0(dir_path, "/", stringr::str_pad(1:N, width = nchar(N), pad = "0"), ".png") |> # ファイルパスを作成
  magick::image_read() |> # 画像ファイルを読込
  magick::image_animate(fps = 1, dispose = "previous") |> # gif画像を作成
  magick::image_write_gif(path = "output/QQplot/sampleID_to_quantile.gif", delay = 0.1) -> tmp_path # gifファイルを書出


# stat_qq_line関数のline.p引数 -------------------------------------------------

# 一時保存フォルダを指定
dir_path <- "output/tmp"

# 理論値の計算用のパラメータを指定
mu_z    <- 0
sigma_z <- 1

# サンプルごとの理論値を作成
sample_df <- tibble::tibble(
  i   = 1:N, # 昇順のサンプル番号(分位番号)
  x   = sort(sample_vec), # サンプル(確率変数)
  p   = (i - 0.5) / N, # 分位番号 → パーセンタイル:(両端は0.5)
  #p   = i / (N + 1), # 分位番号 → パーセンタイル:(両端も1)
  q_x = qnorm(p = p, mean = mu, sd = sigma), # パーセンタイル → クォンタイル
  q_z = qnorm(p = p, mean = mu_z, sd = sigma_z) # パーセンタイル → クォンタイル
)

# 基準とする下限の点のサンプル番号を指定
i_k <- 10
p_k <- sample_df[["p"]][i_k]

# グラフサイズを設定
axis_x_size <- (sample_df[["x"]] - mu) |> 
  abs() |> 
  max() |> 
  ceiling()
axis_z_size <- (sample_df[["q_z"]] - mu_z) |> 
  abs() |> 
  max() |> 
  ceiling()

# ラベル用の文字列を作成
param_label <- paste0(
  "list(", 
  "mu[x] == ", mu, ", sigma[x] == ", round(sigma, digits = 2), ", ", 
  "mu[z] == ", mu_z, ", sigma[z] == ", sigma_z, 
  ")"
)

# 基準点ごとに作図
i_vals <- N:(i_k+1)
for(i_val in rev(i_vals)) {
  
  # 基準とする上限の点のパーセンタイルを取得
  i_l <- i_val
  p_l <- sample_df[["p"]][i_l]
  
  # 疑似理論値線の傾き・切片を計算
  a <- (sample_df[["x"]][i_l] - sample_df[["x"]][i_k]) / (sample_df[["q_z"]][i_l] - sample_df[["q_z"]][i_k])
  b <- sample_df[["x"]][i_k] - a * sample_df[["q_z"]][i_k]
  
  # 基準とす2点の理論値を作成
  point_df <- sample_df |> 
    dplyr::filter(i %in% c(i_k, i_l)) # 基準とする2点を抽出
  
  # Q-Qプロットを作図
  graph <- ggplot() + 
    geom_point(data = point_df, 
               mapping = aes(x = q_z, y = x), 
               color = "orange", size = 6) + # 基準とする2点
    geom_vline(data = point_df, 
               mapping = aes(xintercept = q_z), 
               linetype = "dotted") + # パーセンタイル軸の補助線
    stat_qq(data = sample_df, 
            mapping = aes(sample = x), 
            distribution = qnorm, dparams = list(mean = mu_z, sd = sigma_z), 
            color = "blue", size = 2) + # サンプル値と理論値の対応点
    geom_point(data = sample_df, 
               mapping = aes(x = q_z, y = x), 
               color = "red", size = 1) + # サンプル値と理論値の対応点
    stat_qq_line(data = sample_df, 
                 mapping = aes(sample = x, color = "stat_qq_line"), 
                 distribution = qnorm, dparams = list(mean = mu_z, sd = sigma_z), 
                 line.p = c(p_k, p_l), fullrange = FALSE, 
                 linewidth = 2) + # 理論値の線
    geom_abline(mapping = aes(slope = a, intercept = b, color = "geom_abline"), 
                linewidth = 1, show.legend = FALSE) + # 理論値の線
    scale_color_manual(breaks = c("stat_qq_line", "geom_abline"), 
                       values = c("blue", "red"), 
                       name = "function") + # 凡例表示用
    scale_x_continuous(sec.axis = sec_axis(trans = ~ ., 
                                           breaks = point_df[["q_z"]], 
                                           labels = round(point_df[["p"]], digits = 3), 
                                           name = "Theoretical Percentile")) + # パーセンタイル軸目盛
    guides(color = guide_legend(override.aes = list(linewidth = 1))) + 
    coord_cartesian(xlim = c(mu_z-axis_z_size, mu_z+axis_z_size), 
                    ylim = c(mu-axis_x_size, mu+axis_x_size)) + 
    labs(title = "Normal Q-Q Plot", 
         subtitle = parse(text = param_label), 
         x = "Theoretical Quantiles (z)", 
         y = "Sample Quantiles (x)")
  
  # ファイルを書き出し
  file_path <- paste0(dir_path, "/", stringr::str_pad(i_val, width = nchar(N), pad = "0"), ".png")
  ggplot2::ggsave(filename = file_path, plot = graph, width = 800, height = 800, units = "px", dpi = 100)
  
  # 途中経過を表示
  message("\r", i_val, " / ", N, appendLF = FALSE)
}

# gif画像を作成
paste0(dir_path, "/", stringr::str_pad(i_vals, width = nchar(N), pad = "0"), ".png") |> # ファイルパスを作成
  magick::image_read() |> # 画像ファイルを読込
  magick::image_animate(fps = 1, dispose = "previous") |> # gif画像を作成
  magick::image_write_gif(path = "output/QQplot/linep_arg.gif", delay = 0.1) -> tmp_path # gifファイルを書出


