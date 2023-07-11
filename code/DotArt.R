
# ggplot2でドット調の画像を作りたい -------------------------------------------------------

# 利用パッケージ
library(tidyverse)
library(raster)
library(magick)


# ドット調の画像の作成 ----------------------------------------------------------------

# 画像ファイルパスを指定
file_path <- "data/picture/icon.jpg"

# ラスタデータを読込
scale_val <- 5
raster_dat <- raster::stack(x = file_path) |> # RGBデータを取得
  raster::aggregate(fact = scale_val) # 解像度を下げる

# 画像サイズを取得
width_size  <- raster::ncol(raster_dat)
height_size <- raster::nrow(raster_dat)
width_size; height_size

# 色データを作成
raster_df <- raster_dat |> 
  raster::as.data.frame(xy = TRUE) |> 
  tibble::as_tibble() |> 
  dplyr::rename(R = 3, G = 4, B = 5) |> 
  dplyr::mutate(RGB = rgb(red = R/255, green = G/255, blue = B/255)) # 正規化してカラーコードを取得

# ドット絵を作図
size_val <- 0.8
dot_graph <- ggplot2::ggplot() + 
  ggplot2::geom_point(data = raster_df,
                      mapping = ggplot2::aes(x = x, y = y, color = RGB),
                      size = size_val) + # ドット
  ggplot2::scale_color_identity() + # カラーコードによる色付け:(ドット用)
  # ggplot2::geom_tile(data = raster_df, 
  #                    mapping = ggplot2::aes(x = x, y = y, fill = RGB), 
  #                    color = "black") + # ピクセル
  # ggplot2::scale_fill_identity() + # カラーコードによる色付け:(ピクセル用)
  ggplot2::coord_equal(ratio = 1, expand = FALSE) + # 描画領域
  ggplot2::theme(
    axis.title       = ggplot2::element_blank(), # 軸ラベル
    axis.text        = ggplot2::element_blank(), # 軸目盛ラベル
    axis.ticks       = ggplot2::element_blank(), # 軸目盛指示線
    panel.grid       = ggplot2::element_blank(), # グリッド線
    panel.background = ggplot2::element_blank(), # 描画領域の背景
    plot.background  = ggplot2::element_rect(fill = "white", color = NA), # 全体の背景
    legend.position  = "none" # 凡例の位置
  )

# 画像データを書出
scale_val <- 20
ggplot2::ggsave(
  plot = dot_graph, filename = "output/dot_art/dot_art.png", 
  width = width_size/scale_val, height = height_size/scale_val, units = "i", dpi = 300
)


# ドット調のアニメーションの作成 --------------------------------------------------------------

# 元画像のフォルダパスを指定
dir_path <- "data/picture/DeLorean"

# 画像ファイル名を取得
file_name_vec <- list.files(path = dir_path)

# 画像枚数(フレーム数)を設定
frame_num <- length(file_name_vec)

# 画像を加工
for(i in 1:frame_num) {
  
  # ラスタデータを読込
  scale_val <- 20
  raster_dat <- paste0(dir_path, "/", file_name_vec[i]) |> # ファイルパスを作成
    raster::stack() |> # RGBデータを取得
    raster::aggregate(fact = scale_val) # 解像度を下げる
  
  # 画像サイズを取得
  width_size  <- raster::ncol(raster_dat)
  height_size <- raster::nrow(raster_dat)
  
  # 色データを作成
  raster_df <- raster_dat |> 
    raster::as.data.frame(xy = TRUE) |> 
    tibble::as_tibble() |> 
    dplyr::rename(R = 3, G = 4, B = 5) |> 
    dplyr::mutate(RGB = rgb(red = R/255, green = G/255, blue = B/255)) # カラーコードを取得
  
  # ドット絵を作図
  size_val <- 0.75
  dot_graph <- ggplot2::ggplot() + 
    ggplot2::geom_point(data = raster_df, 
                        mapping = ggplot2::aes(x = x, y = y, color = RGB), 
                        size = size_val) + # ドット
    ggplot2::scale_color_identity() + # カラーコードによる色付け
    ggplot2::coord_equal(ratio = 1, expand = FALSE) + # 描画領域
    ggplot2::theme(
      axis.title       = ggplot2::element_blank(), # 軸ラベル
      axis.text        = ggplot2::element_blank(), # 軸目盛ラベル
      axis.ticks       = ggplot2::element_blank(), # 軸目盛指示線
      panel.grid       = ggplot2::element_blank(), # グリッド線
      panel.background = ggplot2::element_blank(), # 描画領域の背景
      plot.background  = ggplot2::element_rect(fill = "white", color = NA), # 全体の背景
      legend.position  = "none" # 凡例の位置
    )
  
  # 画像データを書出
  scale_val <- 20
  ggplot2::ggsave(
    plot = dot_graph, filename = paste0("tmp_data/dot_art_", i, ".jpg"), 
    width = width_size/scale_val, height = height_size/scale_val, units = "i", dpi = 150
  )
  
  # 途中経過を表示
  message("\r", i, " / ", frame_num, appendLF = FALSE)
}
warnings()

# gif画像を作成
paste0("tmp_data/dot_art_", 1:frame_num, ".jpg") |> # ファイルパスを作成
  magick::image_read() |> # 画像ファイルを読込
  magick::image_animate(fps = 1, dispose = "previous") |> # gif画像を作成
  magick::image_write_gif(path = "output/dot_art/DeLorean.gif", delay = 0.1) -> tmp_path # gifファイル書出


