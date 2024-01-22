
# 軸の変換 --------------------------------------------------------------------

# 利用パッケージ
library(tidyverse)
library(patchwork)
library(magick)

# パッケージ名の省略用
library(ggplot2)


# 恒等関数による変換：円 ---------------------------------------------------------------

# 一時書き出しフォルダを指定
dir_path <- "output/tmp"

# フレーム数を指定
frame_num <- 300

# 点用のラジアンの範囲を指定
t_vals <- seq(from = 0, to = 2*pi, length.out = frame_num+1)[1:frame_num]

# 円の半径を指定
r <- 1

# 円周の座標を作成
circle_df <- tibble::tibble(
  t = seq(from = 0, to = 2*pi, length.out = 361), # ラジアン
  x = r * cos(t), 
  y = r * sin(t)
)

# グラフサイズを設定
axis_size <- ceiling(abs(r)) + 0.5

# 変数(角度)ごとに作図
for(i in 1:frame_num) {
  
  # 点用のラジアンを取得
  t_val <- t_vals[i]
  
  # 円周上の点の座標を作成
  point_df <- tibble::tibble(
    t = t_val, 
    x = r * cos(t), 
    y = r * sin(t)
  )
  
  # ラベル用の文字列を作成
  coord_label <- paste0(
    "list(", 
      "r == ", r, ", ", 
      "(list(x, y)) == (list(", 
        round(point_df[["x"]], digits = 2), ", ", 
        round(point_df[["y"]], digits = 2), 
      "))", 
    ")"
  )
  
  # 円周上の点を作図
  circle_graph <- ggplot() + 
    geom_segment(mapping = aes(x = c(-Inf, 0), y = c(0, -Inf), 
                               xend = c(Inf, 0), yend = c(0, Inf)), 
                 arrow = arrow(length = unit(10, units = "pt"), ends = "last")) + # x・y軸線
    geom_path(data = circle_df, 
              mapping = aes(x = x, y = y)) + # 円周
    geom_segment(data = point_df, 
                 mapping = aes(x = 0, y = 0, xend = x, yend = y)) + # 動径
    geom_segment(data = point_df, 
                 mapping = aes(x = x, y = y, xend = x, yend = -Inf), 
                 color= "blue", linewidth = 1, linetype = "dotted") + # x軸の目盛線
    geom_segment(data = point_df, 
                 mapping = aes(x = x, y = y, xend = Inf, yend = y), 
                 color= "red", linewidth = 1, linetype = "dotted") + # y軸の目盛線
    geom_point(data = point_df, 
               mapping = aes(x = x, y = -Inf), 
               color= "blue", size = 4) + # x軸線上の点
    geom_point(data = point_df, 
               mapping = aes(x = Inf, y = y), 
               color= "red", size = 4) + # y軸線上の点
    geom_point(data = point_df, 
               mapping = aes(x = x, y = y), 
               size = 4) + # 円周上の点
    coord_fixed(ratio = 1, clip = "off", 
                xlim = c(-axis_size, axis_size), 
                ylim = c(-axis_size, axis_size)) + 
    labs(title = "circle", 
         subtitle = parse(text = coord_label), 
         x = expression(x == r ~ cos~theta), 
         y = expression(y == r ~ sin~theta))
  
  # x軸の変換を作図
  axis_x_graph <- ggplot() + 
    geom_segment(mapping = aes(x = c(-Inf, 0), y = c(0, -Inf), 
                               xend = c(Inf, 0), yend = c(0, Inf)), 
                 arrow = arrow(length = unit(10, units = "pt"), ends = "last")) + # x・x軸線
    geom_abline(slope = 1, intercept = 0) + # 恒等関数
    geom_segment(data = point_df, 
                 mapping = aes(x = x, y = x, xend = x, yend = Inf), 
                 color= "blue", linewidth = 1, linetype = "dotted") + # x軸の目盛線(横)
    geom_segment(data = point_df, 
                 mapping = aes(x = x, y = x, xend = Inf, yend = x), 
                 color= "blue", linewidth = 1, linetype = "dotted") + # x軸の目盛線(縦)
    geom_point(data = point_df, 
               mapping = aes(x = x, y = Inf), 
               color= "blue", size = 4) + # x軸線上の点(横)
    geom_point(data = point_df, 
               mapping = aes(x = Inf, y = x), 
               color= "blue", size = 4) + # x軸線上の点(縦)
    geom_point(data = point_df, 
               mapping = aes(x = x, y = x), 
               color = "blue", size = 4) + # 直線上の点
    coord_fixed(ratio = 1, clip = "off", 
                xlim = c(-axis_size, axis_size), 
                ylim = c(-axis_size, axis_size)) + 
    labs(title = "x-axis", 
         x = expression(x), 
         y = expression(x))
  
  # y軸の変換を作図
  axis_y_graph <- ggplot() + 
    geom_segment(mapping = aes(x = c(-Inf, 0), y = c(0, -Inf), 
                               xend = c(Inf, 0), yend = c(0, Inf)), 
                 arrow = arrow(length = unit(10, units = "pt"), ends = "last")) + # y・y軸線
    geom_abline(slope = 1, intercept = 0) + # 恒等関数
    geom_segment(data = point_df, 
                 mapping = aes(x = y, y = y, xend = y, yend = -Inf), 
                 color= "red", linewidth = 1, linetype = "dotted") + # y軸の目盛線(横)
    geom_segment(data = point_df, 
                 mapping = aes(x = y, y = y, xend = -Inf, yend = y), 
                 color= "red", linewidth = 1, linetype = "dotted") + # y軸の目盛線(縦)
    geom_point(data = point_df, 
               mapping = aes(x = y, y = -Inf), 
               color= "red", size = 4) + # y軸線上の点(横)
    geom_point(data = point_df, 
               mapping = aes(x = -Inf, y = y), 
               color= "red", size = 4) + # y軸線上の点(縦)
    geom_point(data = point_df, 
               mapping = aes(x = y, y = y), 
               color = "red", size = 4) + # 直線上の点
    coord_fixed(ratio = 1, clip = "off", 
                xlim = c(-axis_size, axis_size), 
                ylim = c(-axis_size, axis_size)) + 
    labs(title = "y-axis", 
         x = expression(x), 
         y = expression(x))
  
  # 円周上の点を作図
  convert_graph <- ggplot() + 
    geom_segment(mapping = aes(x = c(-Inf, 0), y = c(0, -Inf), 
                               xend = c(Inf, 0), yend = c(0, Inf)), 
                 arrow = arrow(length = unit(10, units = "pt"), ends = "last")) + # y・x軸線
    geom_path(data = circle_df, 
              mapping = aes(x = y, y = x)) + # 円周
    geom_segment(data = point_df, 
                 mapping = aes(x = 0, y = 0, xend = y, yend = x)) + # 動径
    geom_segment(data = point_df, 
                 mapping = aes(x = y, y = x, xend = -Inf, yend = x), 
                 color= "blue", linewidth = 1, linetype = "dotted") + # x軸の目盛線
    geom_segment(data = point_df, 
                 mapping = aes(x = y, y = x, xend = y, yend = Inf), 
                 color= "red", linewidth = 1, linetype = "dotted") + # y軸の目盛線
    geom_point(data = point_df, 
               mapping = aes(x = -Inf, y = x), 
               color= "blue", size = 4) + # x軸線上の点
    geom_point(data = point_df, 
               mapping = aes(x = y, y = Inf), 
               color= "red", size = 4) + # y軸線上の点
    geom_point(data = point_df, 
               mapping = aes(x = y, y = x), 
               size = 4) + # 円周上の点
    coord_fixed(ratio = 1, clip = "off", 
                xlim = c(-axis_size, axis_size), 
                ylim = c(-axis_size, axis_size)) + 
    labs(title = "converted axes", 
         x = expression(y == r ~ sin~theta), 
         y = expression(x == r ~ cos~theta))
  
  # 並べて描画
  wrap_graph <- patchwork::wrap_plots(
    circle_graph, axis_y_graph, 
    axis_x_graph, convert_graph, 
    nrow = 2, ncol = 2
  )
  
  # 画像ファイルを書出
  file_path <- paste0(dir_path, "/", stringr::str_pad(i, width = nchar(frame_num), pad = "0"), ".png")
  ggplot2::ggsave(filename = file_path, plot = wrap_graph, width = 1000, height = 1000, units = "px", dpi = 100)
  
  # 途中経過を表示
  message("\r", i, " / ", frame_num, appendLF = FALSE)
}

# gif画像を作成
paste0(dir_path, "/", stringr::str_pad(1:frame_num, width = nchar(frame_num), pad = "0"), ".png") |> # ファイルパスを作成
  magick::image_read() |> # 画像ファイルを読込
  magick::image_animate(fps = 1, dispose = "previous") |> # gif画像を作成
  magick::image_write_gif(path = "output/convert_axis/circle_diag.gif", delay = 1/40) -> tmp_path # gifファイルを書出


# 円弧による変換：円 ---------------------------------------------------------------

# 一時書き出しフォルダを指定
dir_path <- "output/tmp"

# フレーム数を指定
frame_num <- 300

# 点用のラジアンの範囲を指定
t_vals <- seq(from = 0, to = 2*pi, length.out = frame_num+1)[1:frame_num]

# 円の半径を指定
r <- 1

# 円周の座標を作成
circle_df <- tibble::tibble(
  t = seq(from = 0, to = 2*pi, length.out = 361), # ラジアン
  x = r * cos(t), 
  y = r * sin(t)
)

# グラフサイズを設定
axis_size <- ceiling(abs(r)) + 0.5

# 変換軸の目盛間隔を設定
tick_major_val <- 1
tick_minor_val <- 0.5 * tick_major_val

# 変換軸のサイズを設定:(目盛間隔で切り上げ)
grid_size <- ceiling(axis_size / tick_minor_val) * tick_minor_val
grid_size <- ceiling(abs(r) / tick_minor_val) * tick_minor_val

# 変換軸のグリッド線の座標を作成
grid_x_df <- tidyr::expand_grid(
  x = seq(from = -grid_size, to = grid_size-tick_minor_val, by = tick_minor_val), # 直交座標における目盛値
  u = seq(from = pi, to = 1.5*pi, length.out = 91) # ラジアン
) |> # 目盛線ごとにラジアンを複製
  dplyr::mutate(
    x0    = grid_size, 
    y0    = grid_size, 
    arc_r = grid_size - x, 
    arc_x = x0 + arc_r * cos(u), 
    arc_y = y0 + arc_r * sin(u), 
    grid  = dplyr::if_else(
      x%%tick_major_val == 0, true = "major", false = "minor"
    ) # 主・補助目盛の書き分け用
  )
grid_y_df <- tidyr::expand_grid(
  y = seq(from = -grid_size+tick_minor_val, to = grid_size, by = tick_minor_val), # 直交座標における目盛値
  u = seq(from = 0, to = 0.5*pi, length.out = 91) # ラジアン
) |> # 目盛線ごとにラジアンを複製
  dplyr::mutate(
    x0    = -grid_size, 
    y0    = -grid_size, 
    arc_r = grid_size + y, 
    arc_x = x0 + arc_r * cos(u), 
    arc_y = y0 + arc_r * sin(u), 
    grid  = dplyr::if_else(
      y%%tick_major_val == 0, true = "major", false = "minor"
    ) # 主・補助目盛の書き分け用
  )

# 変数(角度)ごとに作図
for(i in 1:frame_num) {
  
  # 点用のラジアンを取得
  t_val <- t_vals[i]
  
  # 円周上の点の座標を作成
  point_df <- tibble::tibble(
    t = t_val, 
    x = r * cos(t), 
    y = r * sin(t)
  )
  
  # ラベル用の文字列を作成
  coord_label <- paste0(
    "list(", 
      "r == ", r, ", ", 
      "(list(x, y)) == (list(", 
        round(point_df[["x"]], digits = 2), ", ", 
        round(point_df[["y"]], digits = 2), 
      "))", 
    ")"
  )
  
  # 円周上の点を作図
  circle_graph <- ggplot() + 
    geom_segment(mapping = aes(x = c(-Inf, 0), y = c(0, -Inf), 
                               xend = c(Inf, 0), yend = c(0, Inf)), 
                 arrow = arrow(length = unit(10, units = "pt"), ends = "last")) + # x・y軸線
    geom_path(data = circle_df, 
              mapping = aes(x = x, y = y)) + # 円周
    geom_segment(data = point_df, 
                 mapping = aes(x = 0, y = 0, xend = x, yend = y)) + # 動径
    geom_segment(data = point_df, 
                 mapping = aes(x = x, y = y, xend = x, yend = -Inf), 
                 color= "blue", linewidth = 1, linetype = "dotted") + # x軸の目盛線
    geom_segment(data = point_df, 
                 mapping = aes(x = x, y = y, xend = Inf, yend = y), 
                 color= "red", linewidth = 1, linetype = "dotted") + # y軸の目盛線
    geom_point(data = point_df, 
               mapping = aes(x = x, y = -Inf), 
               color= "blue", size = 4) + # x軸線上の点
    geom_point(data = point_df, 
               mapping = aes(x = Inf, y = y), 
               color= "red", size = 4) + # y軸線上の点
    geom_point(data = point_df, 
               mapping = aes(x = x, y = y), 
               size = 4) + # 円周上の点
    coord_fixed(ratio = 1, clip = "off", 
                xlim = c(-axis_size, axis_size), 
                ylim = c(-axis_size, axis_size)) + 
    labs(title = "circle", 
         subtitle = parse(text = coord_label), 
         x = expression(x == r ~ cos~theta), 
         y = expression(y == r ~ sin~theta))
  
  # 変換曲線の座標を作成
  arc_x_df <- tibble::tibble(
    u     = seq(from = pi, to = 1.5*pi, length.out = 91), # ラジアン
    x0    = grid_size, 
    y0    = grid_size, 
    arc_r = -(r*cos(t_val) - grid_size), 
    arc_x = x0 + arc_r * cos(u), 
    arc_y = y0 + arc_r * sin(u)
  )
  
  # x軸の変換を作図
  axis_x_graph <- ggplot() + 
    geom_path(data = grid_x_df, 
              mapping = aes(x = arc_x, y = arc_y, group = arc_r, linewidth = grid), 
              color = "white") + # グリッド線
    geom_segment(mapping = aes(x = c(-Inf, 0), y = c(0, -Inf), 
                               xend = c(Inf, 0), yend = c(0, Inf)), 
                 arrow = arrow(length = unit(10, units = "pt"), ends = "last")) + # x・x軸線
    geom_line(data = arc_x_df, 
              mapping = aes(x = arc_x, y = arc_y), 
              color = "blue", linewidth = 1, linetype = "dotted") + # 変換曲線
    geom_segment(data = point_df, 
                 mapping = aes(x = x, y = grid_size, xend = x, yend = Inf), 
                 color= "blue", linewidth = 1, linetype = "dotted") + # x軸の目盛線(横)
    geom_segment(data = point_df, 
                 mapping = aes(x = grid_size, y = x, xend = Inf, yend = x), 
                 color= "blue", linewidth = 1, linetype = "dotted") + # x軸の目盛線(縦)
    geom_point(data = point_df, 
               mapping = aes(x = x, y = Inf), 
               color= "blue", size = 4) + # x軸線上の点(横)
    geom_point(data = point_df, 
               mapping = aes(x = Inf, y = x), 
               color= "blue", size = 4) + # x軸線上の点(縦)
    scale_linewidth_manual(breaks = c("major", "minor"), 
                           values = c(0.5, 0.25), guide = "none") + # 主・補助目盛線用
    coord_fixed(ratio = 1, clip = "off", 
                xlim = c(-axis_size, axis_size), 
                ylim = c(-axis_size, axis_size)) + 
    labs(title = "x-axis", 
         x = expression(x), 
         y = expression(x))
  
  # 変換曲線の座標を作成
  arc_y_df <- tibble::tibble(
    u     = seq(from = 0, to = 0.5*pi, length.out = 91), # ラジアン
    x0    = -grid_size, 
    y0    = -grid_size, 
    arc_r = r*sin(t_val) + grid_size, 
    arc_x = x0 + arc_r * cos(u), 
    arc_y = y0 + arc_r * sin(u)
  )
  
  # y軸の変換を作図
  axis_y_graph <- ggplot() + 
    geom_path(data = grid_y_df, 
              mapping = aes(x = arc_x, y = arc_y, group = arc_r, linewidth = grid), 
              color = "white") + # グリッド線
    geom_segment(mapping = aes(x = c(-Inf, 0), y = c(0, -Inf), 
                               xend = c(Inf, 0), yend = c(0, Inf)), 
                 arrow = arrow(length = unit(10, units = "pt"), ends = "last")) + # y・y軸線
    geom_line(data = arc_y_df, 
              mapping = aes(x = arc_x, y = arc_y), 
              color = "red", linewidth = 1, linetype = "dotted") + # 変換曲線
    geom_segment(data = point_df, 
                 mapping = aes(x = y, y = -grid_size, xend = y, yend = -Inf), 
                 color= "red", linewidth = 1, linetype = "dotted") + # y軸の目盛線(横)
    geom_segment(data = point_df, 
                 mapping = aes(x = -grid_size, y = y, xend = -Inf, yend = y), 
                 color= "red", linewidth = 1, linetype = "dotted") + # y軸の目盛線(縦)
    geom_point(data = point_df, 
               mapping = aes(x = y, y = -Inf), 
               color= "red", size = 4) + # y軸線上の点(横)
    geom_point(data = point_df, 
               mapping = aes(x = -Inf, y = y), 
               color= "red", size = 4) + # y軸線上の点(縦)
    scale_linewidth_manual(breaks = c("major", "minor"), 
                           values = c(0.5, 0.25), guide = "none") + # 主・補助目盛線用
    coord_fixed(ratio = 1, clip = "off", 
                xlim = c(-axis_size, axis_size), 
                ylim = c(-axis_size, axis_size)) + 
    labs(title = "y-axis", 
         x = expression(x), 
         y = expression(x))
  
  # 円周上の点を作図
  convert_graph <- ggplot() + 
    geom_segment(mapping = aes(x = c(-Inf, 0), y = c(0, -Inf), 
                               xend = c(Inf, 0), yend = c(0, Inf)), 
                 arrow = arrow(length = unit(10, units = "pt"), ends = "last")) + # y・x軸線
    geom_path(data = circle_df, 
              mapping = aes(x = y, y = x)) + # 円周
    geom_segment(data = point_df, 
                 mapping = aes(x = 0, y = 0, xend = y, yend = x)) + # 動径
    geom_segment(data = point_df, 
                 mapping = aes(x = y, y = x, xend = -Inf, yend = x), 
                 color= "blue", linewidth = 1, linetype = "dotted") + # x軸の目盛線
    geom_segment(data = point_df, 
                 mapping = aes(x = y, y = x, xend = y, yend = Inf), 
                 color= "red", linewidth = 1, linetype = "dotted") + # y軸の目盛線
    geom_point(data = point_df, 
               mapping = aes(x = -Inf, y = x), 
               color= "blue", size = 4) + # x軸線上の点
    geom_point(data = point_df, 
               mapping = aes(x = y, y = Inf), 
               color= "red", size = 4) + # y軸線上の点
    geom_point(data = point_df, 
               mapping = aes(x = y, y = x), 
               size = 4) + # 円周上の点
    coord_fixed(ratio = 1, clip = "off", 
                xlim = c(-axis_size, axis_size), 
                ylim = c(-axis_size, axis_size)) + 
    labs(title = "converted axes", 
         x = expression(y == r ~ sin~theta), 
         y = expression(x == r ~ cos~theta))
  
  # 並べて描画
  wrap_graph <- patchwork::wrap_plots(
    circle_graph, axis_y_graph, 
    axis_x_graph, convert_graph, 
    nrow = 2, ncol = 2
  )
  
  # 画像ファイルを書出
  file_path <- paste0(dir_path, "/", stringr::str_pad(i, width = nchar(frame_num), pad = "0"), ".png")
  ggplot2::ggsave(filename = file_path, plot = wrap_graph, width = 1000, height = 1000, units = "px", dpi = 100)
  
  # 途中経過を表示
  message("\r", i, " / ", frame_num, appendLF = FALSE)
}

# gif画像を作成
paste0(dir_path, "/", stringr::str_pad(1:frame_num, width = nchar(frame_num), pad = "0"), ".png") |> # ファイルパスを作成
  magick::image_read() |> # 画像ファイルを読込
  magick::image_animate(fps = 1, dispose = "previous") |> # gif画像を作成
  magick::image_write_gif(path = "output/convert_axis/circle_arc.gif", delay = 1/40) -> tmp_path # gifファイルを書出


# 恒等関数による変換：楕円 ---------------------------------------------------------------

# 一時書き出しフォルダを指定
dir_path <- "output/tmp"

# フレーム数を指定
frame_num <- 300

# 点用のラジアンの範囲を指定
t_vals <- seq(from = 0, to = 2*pi, length.out = frame_num+1)[1:frame_num]

# 楕円の長・短半径を指定
a <- 2
b <- 1

# 楕円周の座標を作成
ellipse_df <- tibble::tibble(
  t = seq(from = 0, to = 2*pi, length.out = 361), # ラジアン
  x = a * cos(t), 
  y = b * sin(t)
)

# 円の半径を設定
r     <- max(abs(a), abs(b))
sgn_a <- ifelse(test = a >= 0, yes = 1, no = -1)
sgn_b <- ifelse(test = b >= 0, yes = 1, no = -1)

# 円周の座標を作成
circle_df <- tibble::tibble(
  t = seq(from = 0, to = 2*pi, length.out = 361), # ラジアン
  x = r * cos(t), 
  y = r * sin(t)
)

# グラフサイズを設定
axis_x_size <- ceiling(r) + 0.5
axis_y_size <- ceiling(r) + 0.5

# 垂線の向きを設定
vertical_flag <- abs(a) >= abs(b)

# 変数(角度)ごとに作図
for(i in 1:frame_num) {
  
  # 点用のラジアンを取得
  t_val <- t_vals[i]
  
  # 楕円周上の点の座標を作成
  point_df <- tibble::tibble(
    t = t_val, 
    x = a * cos(t), 
    y = b * sin(t)
  )
  
  # 動径の座標を作成
  radius_df <- tibble::tibble(
    t = t_val, 
    x = sgn_a*r * cos(t), 
    y = sgn_b*r * sin(t), 
    # 垂線(補助線)用
    x_to = dplyr::if_else(vertical_flag, true = x, false = 0), 
    y_to = dplyr::if_else(vertical_flag, true = 0, false = y)
  )
  
  # ラベル用の文字列を作成
  coord_label <- paste0(
    "list(", 
      "a == ", a, ", ", 
      "b == ", b, ", ", 
      "(list(x, y)) == (list(", 
        round(point_df[["x"]], digits = 2), ", ", 
        round(point_df[["y"]], digits = 2), 
      "))", 
    ")"
  )
  
  # 楕円周上の点を作図
  ellipse_graph <- ggplot() + 
    geom_segment(mapping = aes(x = c(-Inf, 0), y = c(0, -Inf), 
                               xend = c(Inf, 0), yend = c(0, Inf)), 
                 arrow = arrow(length = unit(10, units = "pt"), ends = "last")) + # x・y軸線
    geom_path(data = circle_df, 
              mapping = aes(x = x, y = y), 
              linetype = "dotted") + # 円周
    geom_segment(data = radius_df, 
                 mapping = aes(x = 0, y = 0, xend = x, yend = y)) + # 動径
    geom_segment(data = radius_df, 
                 mapping = aes(x = x, y = y, xend = x_to, yend = y_to), 
                 linetype = "dashed") + # 動径からの垂線
    geom_path(data = ellipse_df, 
              mapping = aes(x = x, y = y)) + # 楕円周
    geom_segment(data = point_df, 
                 mapping = aes(x = x, y = y, xend = x, yend = -Inf), 
                 color= "blue", linewidth = 1, linetype = "dotted") + # x軸の目盛線
    geom_segment(data = point_df, 
                 mapping = aes(x = x, y = y, xend = Inf, yend = y), 
                 color= "red", linewidth = 1, linetype = "dotted") + # y軸の目盛線
    geom_point(data = point_df, 
               mapping = aes(x = x, y = -Inf), 
               color= "blue", size = 4) + # x軸線上の点
    geom_point(data = point_df, 
               mapping = aes(x = Inf, y = y), 
               color= "red", size = 4) + # y軸線上の点
    geom_point(data = point_df, 
               mapping = aes(x = x, y = y), 
               size = 4) + # 楕円周上の点
    coord_fixed(ratio = 1, clip = "off", 
                xlim = c(-axis_x_size, axis_x_size), 
                ylim = c(-axis_y_size, axis_y_size)) + 
    labs(title = "ellipse", 
         subtitle = parse(text = coord_label), 
         x = expression(x == a ~ cos~theta), 
         y = expression(y == b ~ sin~theta))
  
  # x軸の変換を作図
  axis_x_graph <- ggplot() + 
    geom_segment(mapping = aes(x = c(-Inf, 0), y = c(0, -Inf), 
                               xend = c(Inf, 0), yend = c(0, Inf)), 
                 arrow = arrow(length = unit(10, units = "pt"), ends = "last")) + # x・x軸線
    geom_abline(slope = 1, intercept = 0) + # 恒等関数
    geom_segment(data = point_df, 
                 mapping = aes(x = x, y = x, xend = x, yend = Inf), 
                 color= "blue", linewidth = 1, linetype = "dotted") + # x軸の目盛線(横)
    geom_segment(data = point_df, 
                 mapping = aes(x = x, y = x, xend = Inf, yend = x), 
                 color= "blue", linewidth = 1, linetype = "dotted") + # x軸の目盛線(縦)
    geom_point(data = point_df, 
               mapping = aes(x = x, y = Inf), 
               color= "blue", size = 4) + # x軸線上の点(横)
    geom_point(data = point_df, 
               mapping = aes(x = Inf, y = x), 
               color= "blue", size = 4) + # x軸線上の点(縦)
    geom_point(data = point_df, 
               mapping = aes(x = x, y = x), 
               color = "blue", size = 4) + # 直線上の点
    coord_fixed(ratio = 1, clip = "off", 
                xlim = c(-axis_x_size, axis_x_size), 
                ylim = c(-axis_x_size, axis_x_size)) + 
    labs(title = "x-axis", 
         x = expression(x), 
         y = expression(x))
  
  # y軸の変換を作図
  axis_y_graph <- ggplot() + 
    geom_segment(mapping = aes(x = c(-Inf, 0), y = c(0, -Inf), 
                               xend = c(Inf, 0), yend = c(0, Inf)), 
                 arrow = arrow(length = unit(10, units = "pt"), ends = "last")) + # y・y軸線
    geom_abline(slope = 1, intercept = 0) + # 恒等関数
    geom_segment(data = point_df, 
                 mapping = aes(x = y, y = y, xend = y, yend = -Inf), 
                 color= "red", linewidth = 1, linetype = "dotted") + # y軸の目盛線(横)
    geom_segment(data = point_df, 
                 mapping = aes(x = y, y = y, xend = -Inf, yend = y), 
                 color= "red", linewidth = 1, linetype = "dotted") + # y軸の目盛線(縦)
    geom_point(data = point_df, 
               mapping = aes(x = y, y = -Inf), 
               color= "red", size = 4) + # y軸線上の点(横)
    geom_point(data = point_df, 
               mapping = aes(x = -Inf, y = y), 
               color= "red", size = 4) + # y軸線上の点(縦)
    geom_point(data = point_df, 
               mapping = aes(x = y, y = y), 
               color = "red", size = 4) + # 直線上の点
    coord_fixed(ratio = 1, clip = "off", 
                xlim = c(-axis_y_size, axis_y_size), 
                ylim = c(-axis_y_size, axis_y_size)) + 
    labs(title = "y-axis", 
         x = expression(x), 
         y = expression(x))
  
  # 楕円周上の点を作図
  convert_graph <- ggplot() + 
    geom_segment(mapping = aes(x = c(-Inf, 0), y = c(0, -Inf), 
                               xend = c(Inf, 0), yend = c(0, Inf)), 
                 arrow = arrow(length = unit(10, units = "pt"), ends = "last")) + # y・x軸線
    geom_path(data = circle_df, 
              mapping = aes(x = x, y = y), 
              linetype = "dotted") + # 円周
    geom_segment(data = radius_df, 
                 mapping = aes(x = 0, y = 0, xend = y, yend = x)) + # 動径
    geom_segment(data = radius_df, 
                 mapping = aes(x = y, y = x, xend = 0, yend = x), 
                 linetype = "dotted") + # 円周上の点からの垂線
    geom_path(data = ellipse_df, 
              mapping = aes(x = y, y = x)) + # 楕円周
    geom_segment(data = point_df, 
                 mapping = aes(x = y, y = x, xend = -Inf, yend = x), 
                 color= "blue", linewidth = 1, linetype = "dotted") + # x軸の目盛線
    geom_segment(data = point_df, 
                 mapping = aes(x = y, y = x, xend = y, yend = Inf), 
                 color= "red", linewidth = 1, linetype = "dotted") + # y軸の目盛線
    geom_point(data = point_df, 
               mapping = aes(x = -Inf, y = x), 
               color= "blue", size = 4) + # x軸線上の点
    geom_point(data = point_df, 
               mapping = aes(x = y, y = Inf), 
               color= "red", size = 4) + # y軸線上の点
    geom_point(data = point_df, 
               mapping = aes(x = y, y = x), 
               size = 4) + # 楕円周上の点
    coord_fixed(ratio = 1, clip = "off", 
                xlim = c(-axis_y_size, axis_y_size), 
                ylim = c(-axis_x_size, axis_x_size)) + 
    labs(title = "converted axes", 
         x = expression(y == b ~ sin~theta), 
         y = expression(x == a ~ cos~theta))
  
  # 並べて描画
  wrap_graph <- patchwork::wrap_plots(
    ellipse_graph, axis_y_graph, 
    axis_x_graph, convert_graph, 
    nrow = 2, ncol = 2
  )
  
  # 画像ファイルを書出
  file_path <- paste0(dir_path, "/", stringr::str_pad(i, width = nchar(frame_num), pad = "0"), ".png")
  ggplot2::ggsave(filename = file_path, plot = wrap_graph, width = 1000, height = 1000, units = "px", dpi = 100)
  
  # 途中経過を表示
  message("\r", i, " / ", frame_num, appendLF = FALSE)
}

# gif画像を作成
paste0(dir_path, "/", stringr::str_pad(1:frame_num, width = nchar(frame_num), pad = "0"), ".png") |> # ファイルパスを作成
  magick::image_read() |> # 画像ファイルを読込
  magick::image_animate(fps = 1, dispose = "previous") |> # gif画像を作成
  magick::image_write_gif(path = "output/convert_axis/ellipse_diag.gif", delay = 1/40) -> tmp_path # gifファイルを書出


# 円弧による変換：楕円 ---------------------------------------------------------------

# 一時書き出しフォルダを指定
dir_path <- "output/tmp"

# フレーム数を指定
frame_num <- 300

# 点用のラジアンの範囲を指定
t_vals <- seq(from = 0, to = 2*pi, length.out = frame_num+1)[1:frame_num]

# 楕円の長・短半径を指定
a <- 2
b <- 1

# 楕円周の座標を作成
ellipse_df <- tibble::tibble(
  t = seq(from = 0, to = 2*pi, length.out = 361), # ラジアン
  x = a * cos(t), 
  y = b * sin(t)
)

# 円の半径を設定
r     <- max(abs(a), abs(b))
sgn_a <- ifelse(test = a >= 0, yes = 1, no = -1)
sgn_b <- ifelse(test = b >= 0, yes = 1, no = -1)

# 円周の座標を作成
circle_df <- tibble::tibble(
  t = seq(from = 0, to = 2*pi, length.out = 361), # ラジアン
  x = r * cos(t), 
  y = r * sin(t)
)

# グラフサイズを設定
axis_x_size <- ceiling(r) + 0.5
axis_y_size <- ceiling(r) + 0.5

# 変換軸の目盛間隔を設定
tick_x_major_val <- 1
tick_y_major_val <- 1
tick_x_minor_val <- 0.5 * tick_x_major_val
tick_y_minor_val <- 0.5 * tick_y_major_val

# 変換軸のサイズを設定:(目盛間隔で切り上げ)
grid_x_size <- ceiling(axis_x_size / tick_x_minor_val) * tick_x_minor_val
grid_y_size <- ceiling(axis_y_size / tick_y_minor_val) * tick_y_minor_val
grid_x_size <- ceiling(abs(a) / tick_x_minor_val) * tick_x_minor_val
grid_y_size <- ceiling(abs(b) / tick_y_minor_val) * tick_y_minor_val

# 変換軸のグリッド線の座標を作成
grid_x_df <- tidyr::expand_grid(
  x = seq(from = -grid_x_size, to = grid_x_size-tick_x_minor_val, by = tick_x_minor_val), # 直交座標における目盛値
  u = seq(from = pi, to = 1.5*pi, length.out = 91) # ラジアン
) |> # 目盛線ごとにラジアンを複製
  dplyr::mutate(
    x0    = grid_x_size, 
    y0    = grid_x_size, 
    arc_r = grid_x_size - x, 
    arc_x = x0 + arc_r * cos(u), 
    arc_y = y0 + arc_r * sin(u), 
    grid  = dplyr::if_else(
      x%%tick_x_major_val == 0, true = "major", false = "minor"
    ) # 主・補助目盛の書き分け用
  )
grid_y_df <- tidyr::expand_grid(
  y = seq(from = -grid_y_size+tick_y_minor_val, to = grid_y_size, by = tick_y_minor_val), # 直交座標における目盛値
  u = seq(from = 0, to = 0.5*pi, length.out = 91) # ラジアン
) |> # 目盛線ごとにラジアンを複製
  dplyr::mutate(
    x0    = -grid_y_size, 
    y0    = -grid_y_size, 
    arc_r = grid_y_size + y, 
    arc_x = x0 + arc_r * cos(u), 
    arc_y = y0 + arc_r * sin(u), 
    grid  = dplyr::if_else(
      y%%tick_y_major_val == 0, true = "major", false = "minor"
    ) # 主・補助目盛の書き分け用
  )

# 垂線の向きを設定
vertical_flag <- abs(a) >= abs(b)

# 変数(角度)ごとに作図
for(i in 1:frame_num) {
  
  # 点用のラジアンを取得
  t_val <- t_vals[i]
  
  # 楕円周上の点の座標を作成
  point_df <- tibble::tibble(
    t = t_val, 
    x = a * cos(t), 
    y = b * sin(t)
  )
  
  # 動径の座標を作成
  radius_df <- tibble::tibble(
    t = t_val, 
    x = sgn_a*r * cos(t), 
    y = sgn_b*r * sin(t), 
    # 垂線(補助線)用
    x_to = dplyr::if_else(vertical_flag, true = x, false = 0), 
    y_to = dplyr::if_else(vertical_flag, true = 0, false = y)
  )
  
  # ラベル用の文字列を作成
  coord_label <- paste0(
    "list(", 
      "a == ", a, ", ", 
      "b == ", b, ", ", 
      "(list(x, y)) == (list(", 
        round(point_df[["x"]], digits = 2), ", ", 
        round(point_df[["y"]], digits = 2), 
      "))", 
    ")"
  )
  
  # 楕円周上の点を作図
  ellipse_graph <- ggplot() + 
    geom_segment(mapping = aes(x = c(-Inf, 0), y = c(0, -Inf), 
                               xend = c(Inf, 0), yend = c(0, Inf)), 
                 arrow = arrow(length = unit(10, units = "pt"), ends = "last")) + # x・y軸線
    geom_path(data = circle_df, 
              mapping = aes(x = x, y = y), 
              linetype = "dotted") + # 円周
    geom_segment(data = radius_df, 
                 mapping = aes(x = 0, y = 0, xend = x, yend = y)) + # 動径
    geom_segment(data = radius_df, 
                 mapping = aes(x = x, y = y, xend = x_to, yend = y_to), 
                 linetype = "dashed") + # 動径からの垂線
    geom_path(data = ellipse_df, 
              mapping = aes(x = x, y = y)) + # 楕円周
    geom_segment(data = point_df, 
                 mapping = aes(x = x, y = y, xend = x, yend = -Inf), 
                 color= "blue", linewidth = 1, linetype = "dotted") + # x軸の目盛線
    geom_segment(data = point_df, 
                 mapping = aes(x = x, y = y, xend = Inf, yend = y), 
                 color= "red", linewidth = 1, linetype = "dotted") + # y軸の目盛線
    geom_point(data = point_df, 
               mapping = aes(x = x, y = -Inf), 
               color= "blue", size = 4) + # x軸線上の点
    geom_point(data = point_df, 
               mapping = aes(x = Inf, y = y), 
               color= "red", size = 4) + # y軸線上の点
    geom_point(data = point_df, 
               mapping = aes(x = x, y = y), 
               size = 4) + # 楕円周上の点
    coord_fixed(ratio = 1, clip = "off", 
                xlim = c(-axis_x_size, axis_x_size), 
                ylim = c(-axis_y_size, axis_y_size)) + 
    labs(title = "ellipse", 
         subtitle = parse(text = coord_label), 
         x = expression(x == a ~ cos~theta), 
         y = expression(y == b ~ sin~theta))
  
  # 変換曲線の座標を作成
  arc_x_df <- tibble::tibble(
    u     = seq(from = pi, to = 1.5*pi, length.out = 91), # ラジアン
    x0    = grid_x_size, 
    y0    = grid_x_size, 
    arc_r = -(a*cos(t_val) - grid_x_size), 
    arc_x = x0 + arc_r * cos(u), 
    arc_y = y0 + arc_r * sin(u)
  )
  
  # x軸の変換を作図
  axis_x_graph <- ggplot() + 
    geom_path(data = grid_x_df, 
              mapping = aes(x = arc_x, y = arc_y, group = arc_r, linewidth = grid), 
              color = "white") + # グリッド線
    geom_segment(mapping = aes(x = c(-Inf, 0), y = c(0, -Inf), 
                               xend = c(Inf, 0), yend = c(0, Inf)), 
                 arrow = arrow(length = unit(10, units = "pt"), ends = "last")) + # x・x軸線
    geom_line(data = arc_x_df, 
              mapping = aes(x = arc_x, y = arc_y), 
              color = "blue", linewidth = 1, linetype = "dotted") + # 変換曲線
    geom_segment(data = point_df, 
                 mapping = aes(x = x, y = grid_x_size, xend = x, yend = Inf), 
                 color= "blue", linewidth = 1, linetype = "dotted") + # x軸の目盛線(横)
    geom_segment(data = point_df, 
                 mapping = aes(x = grid_x_size, y = x, xend = Inf, yend = x), 
                 color= "blue", linewidth = 1, linetype = "dotted") + # x軸の目盛線(縦)
    geom_point(data = point_df, 
               mapping = aes(x = x, y = Inf), 
               color= "blue", size = 4) + # x軸線上の点(横)
    geom_point(data = point_df, 
               mapping = aes(x = Inf, y = x), 
               color= "blue", size = 4) + # x軸線上の点(縦)
    scale_linewidth_manual(breaks = c("major", "minor"), 
                           values = c(0.5, 0.25), guide = "none") + # 主・補助目盛線用
    coord_fixed(ratio = 1, clip = "off", 
                xlim = c(-axis_x_size, axis_x_size), 
                ylim = c(-axis_x_size, axis_x_size)) + 
    labs(title = "x-axis", 
         x = expression(x), 
         y = expression(x))
  
  # 変換曲線の座標を作成
  arc_y_df <- tibble::tibble(
    u     = seq(from = 0, to = 0.5*pi, length.out = 91), # ラジアン
    x0    = -grid_y_size, 
    y0    = -grid_y_size, 
    arc_r = b*sin(t_val) + grid_y_size, 
    arc_x = x0 + arc_r * cos(u), 
    arc_y = y0 + arc_r * sin(u)
  )
  
  # y軸の変換を作図
  axis_y_graph <- ggplot() + 
    geom_path(data = grid_y_df, 
              mapping = aes(x = arc_x, y = arc_y, group = arc_r, linewidth = grid), 
              color = "white") + # グリッド線
    geom_segment(mapping = aes(x = c(-Inf, 0), y = c(0, -Inf), 
                               xend = c(Inf, 0), yend = c(0, Inf)), 
                 arrow = arrow(length = unit(10, units = "pt"), ends = "last")) + # y・y軸線
    geom_line(data = arc_y_df, 
              mapping = aes(x = arc_x, y = arc_y), 
              color = "red", linewidth = 1, linetype = "dotted") + # 変換曲線
    geom_segment(data = point_df, 
                 mapping = aes(x = y, y = -grid_y_size, xend = y, yend = -Inf), 
                 color= "red", linewidth = 1, linetype = "dotted") + # y軸の目盛線(横)
    geom_segment(data = point_df, 
                 mapping = aes(x = -grid_y_size, y = y, xend = -Inf, yend = y), 
                 color= "red", linewidth = 1, linetype = "dotted") + # y軸の目盛線(縦)
    geom_point(data = point_df, 
               mapping = aes(x = y, y = -Inf), 
               color= "red", size = 4) + # y軸線上の点(横)
    geom_point(data = point_df, 
               mapping = aes(x = -Inf, y = y), 
               color= "red", size = 4) + # y軸線上の点(縦)
    scale_linewidth_manual(breaks = c("major", "minor"), 
                           values = c(0.5, 0.25), guide = "none") + # 主・補助目盛線用
    coord_fixed(ratio = 1, clip = "off", 
                xlim = c(-axis_y_size, axis_y_size), 
                ylim = c(-axis_y_size, axis_y_size)) + 
    labs(title = "y-axis", 
         x = expression(x), 
         y = expression(x))
  
  # 楕円周上の点を作図
  convert_graph <- ggplot() + 
    geom_segment(mapping = aes(x = c(-Inf, 0), y = c(0, -Inf), 
                               xend = c(Inf, 0), yend = c(0, Inf)), 
                 arrow = arrow(length = unit(10, units = "pt"), ends = "last")) + # y・x軸線
    geom_path(data = circle_df, 
              mapping = aes(x = x, y = y), 
              linetype = "dotted") + # 円周
    geom_segment(data = radius_df, 
                 mapping = aes(x = 0, y = 0, xend = y, yend = x)) + # 動径
    geom_segment(data = radius_df, 
                 mapping = aes(x = y, y = x, xend = y_to, yend = x_to), 
                 linetype = "dashed") + # 動径からの垂線
    geom_path(data = ellipse_df, 
              mapping = aes(x = y, y = x)) + # 楕円周
    geom_segment(data = point_df, 
                 mapping = aes(x = y, y = x, xend = -Inf, yend = x), 
                 color= "blue", linewidth = 1, linetype = "dotted") + # x軸の目盛線
    geom_segment(data = point_df, 
                 mapping = aes(x = y, y = x, xend = y, yend = Inf), 
                 color= "red", linewidth = 1, linetype = "dotted") + # y軸の目盛線
    geom_point(data = point_df, 
               mapping = aes(x = -Inf, y = x), 
               color= "blue", size = 4) + # x軸線上の点
    geom_point(data = point_df, 
               mapping = aes(x = y, y = Inf), 
               color= "red", size = 4) + # y軸線上の点
    geom_point(data = point_df, 
               mapping = aes(x = y, y = x), 
               size = 4) + # 楕円周上の点
    coord_fixed(ratio = 1, clip = "off", 
                xlim = c(-axis_y_size, axis_y_size), 
                ylim = c(-axis_x_size, axis_x_size)) + 
    labs(title = "converted axes", 
         x = expression(y == b ~ sin~theta), 
         y = expression(x == a ~ cos~theta))
  
  # 並べて描画
  wrap_graph <- patchwork::wrap_plots(
    ellipse_graph, axis_y_graph, 
    axis_x_graph, convert_graph, 
    nrow = 2, ncol = 2
  )
  
  # 画像ファイルを書出
  file_path <- paste0(dir_path, "/", stringr::str_pad(i, width = nchar(frame_num), pad = "0"), ".png")
  ggplot2::ggsave(filename = file_path, plot = wrap_graph, width = 1000, height = 1000, units = "px", dpi = 100)
  
  # 途中経過を表示
  message("\r", i, " / ", frame_num, appendLF = FALSE)
}

# gif画像を作成
paste0(dir_path, "/", stringr::str_pad(1:frame_num, width = nchar(frame_num), pad = "0"), ".png") |> # ファイルパスを作成
  magick::image_read() |> # 画像ファイルを読込
  magick::image_animate(fps = 1, dispose = "previous") |> # gif画像を作成
  magick::image_write_gif(path = "output/convert_axis/ellipse_arc.gif", delay = 1/40) -> tmp_path # gifファイルを書出


