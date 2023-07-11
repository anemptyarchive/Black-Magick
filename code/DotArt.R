
# ggplot2でドット絵を作りたい -------------------------------------------------------

# 利用パッケージ
library(tidyverse)
library(raster)
library(magick)

# チェック用
library(ggplot2)


# 画像のドット加工 ----------------------------------------------------------------

# 画像ファイルパスを指定
file_path <- "data/picture/gu.webp"

# ラスタデータを読込
s <- 20
raster_dat <- raster::stack(x = file_path) |> 
  raster::aggregate(fact = s) # 点を間引き

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
s <- 0.8
dot_graph <- ggplot() + 
  geom_point(data = raster_df, 
             mapping = aes(x = x, y = y, color = RGB), 
             size = s) + # ドット
  scale_color_identity() + # カラーコードによる色付け
  coord_equal(ratio = 1, expand = FALSE) + # 描画領域
  theme(
    axis.title = element_blank(), # 軸ラベル
    axis.text = element_blank(), # 軸目盛ラベル
    axis.ticks = element_blank(), # 軸目盛指示線
    panel.grid = element_blank(), # グリッド線
    panel.background = element_blank(), # 描画領域の背景
    plot.background = element_rect(fill = "white", color = NA), # 全体の背景
    legend.position = "none" # 凡例の位置
  )

# 画像データを書出
s <- 20
ggplot2::ggsave(
  plot = dot_graph, filename = "output/dot_art/dot_art.jpg", 
  width = width_size/s, height = height_size/s, units = "i", dpi = 300
)


# ドット加工アニメーションの作成 --------------------------------------------------------------

# 元画像のフォルダパスを指定
dir_path <- "data/picture/DeLorean"

# 画像ファイル名を取得
file_name_vec <- list.files(path = dir_path)

# 画像枚数(フレーム数)を設定
frame_num <- length(file_name_vec)

# 画像を加工
for(i in 1:frame_num) {
  
  # ラスタデータを読込
  s <- 20
  raster_dat <- paste0(dir_path, "/", file_name_vec[i]) |> # ファイルパスを作成
    raster::stack() |> # データを読込
    raster::aggregate(fact = s) # 点を間引き
  
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
  s <- 0.75
  dot_graph <- ggplot() + 
    geom_point(data = raster_df, 
               mapping = aes(x = x, y = y, color = RGB), 
               size = s) + # ドット
    scale_color_identity() + # カラーコードによる色付け
    coord_equal(ratio = 1, expand = FALSE) + # 描画領域
    theme(
      axis.title = element_blank(), # 軸ラベル
      axis.text = element_blank(), # 軸目盛ラベル
      axis.ticks = element_blank(), # x軸の目盛指示線
      panel.grid = element_blank(), # グリッド線
      panel.background = element_blank(), # 描画領域の背景
      plot.background = element_rect(fill = "white", color = NA), # 描画領域外の背景
      legend.position = "none" # 凡例の位置
    )
  
  # 画像データを書出
  s <- 20
  ggplot2::ggsave(
    plot = dot_graph, filename = paste0("tmp_data/dot_art_", i, ".jpg"), 
    width = width_size/s, height = height_size/s, units = "i", dpi = 150
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


