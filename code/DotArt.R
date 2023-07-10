
# ggplot2でドット絵を作りたい -------------------------------------------------------

# 利用パッケージ
library(tidyverse)
library(sf)
library(raster)
library(ggtext)
library(showtext)

# チェック用
library(ggplot2)


# 画像のドット加工 ----------------------------------------------------------------

### ・元画像の前処理 -----

# 元の画像ファイルパスを指定
file_path <- "data/picture/tanabata.jpg"

# 画像データを読込
image_original <- magick::image_read(path = file_path)

# 画像サイズを確認
magick::image_info(image = image_original)

# 画像サイズの上限を指定
width_upper  <- 200
height_upper <- 200

# 画像サイズを変更
image_rescale <- magick::image_scale(image = image_original, geometry = paste0(width_upper, "x", height_upper))

# 画像サイズを取得
image_info  <- magick::image_info(image = image_rescale)
width_size  <- image_info[["width"]]
height_size <- image_info[["height"]]
width_size; height_size

# 画像データを書出
magick::image_write(image = image_rescale, path = "tmp_data/tmp_image.jpg")


### ・加工画像の作成 -----

# ラスタデータを読込
raster_dat <- raster::stack(x = "tmp_data/tmp_image.jpg")

# 色データを作成
raster_df <- raster_dat |> 
  raster::as.data.frame(xy = TRUE) |> 
  tibble::as_tibble() |> 
  dplyr::rename(
    R = tmp_image_1, 
    G = tmp_image_2, 
    B = tmp_image_3
  ) |> 
  dplyr::mutate(
    RGB = rgb(red = R/255, green = G/255, blue = B/255) # 正規化してカラーコードを取得
  )

# ドット絵を作図
dot_graph <- ggplot() + 
  geom_point(data = raster_df, 
             mapping = aes(x = x, y = y, color = RGB), 
             size = 0.8) + # ドット
  scale_color_identity() + 
  coord_equal(ratio = 1, expand = FALSE) + # 描画領域
  theme(
    axis.title = element_blank(), # 軸ラベル
    axis.text = element_blank(), # 軸目盛ラベル
    axis.ticks = element_blank(), # 軸目盛指示線
    panel.grid = element_blank(), # グリッド線
    panel.background = element_blank(), # 描画領域の背景
    plot.background = element_rect(fill = "white", color = NA), # 描画領域外の背景
    legend.position = "none" # 凡例の位置
  )

# 画像データを書出
ggsave(
  plot = dot_graph, filename = "output/dot_art/dot_art.jpg", 
  width = width_size/20, height = height_size/20, units = "i", dpi = 300
)


# ドット加工アニメーションの作成 --------------------------------------------------------------

### ・元画像の前処理 -----

# 元画像のフォルダパスを指定
dir_path <- "data/picture/DeLorean"

# 画像ファイル名を取得
file_name_vec <- list.files(path = dir_path)

# 画像サイズの上限を指定
width_upper  <- 120
height_upper <- 180

# 画像サイズを変更
image_rescale_vec <- paste0(dir_path, "/", file_name_vec) |> # 画像ファイルパスを作成
  magick::image_read() |> # 画像データを読込
  magick::image_scale(geometry = paste0(width_upper, "x", height_upper)) # 画像サイズを変更

# 画像サイズを取得
image_info  <- magick::image_info(image = image_rescale_vec)
width_size  <- max(image_info[["width"]])
height_size <- max(image_info[["height"]])
width_size; height_size

# 画像枚数(フレーム数)を設定
frame_num <- length(image_rescale_vec)

# 画像データを書出
for(i in 1:frame_num) {
  magick::image_write(image = image_rescale_vec[i], path = paste0("tmp_data/tmp_image_", i, ".jpg"))
}


### ・加工画像の作成 -----

# 画像を加工
for(i in 1:frame_num) {
  
  # 色データを作成
  raster_df <- paste0("tmp_data/tmp_image_", i, ".jpg") |> # ファイルパスを作成
    raster::stack() |> # ラスタデータを読込
    raster::as.data.frame(xy = TRUE) |> 
    tibble::as_tibble() |> 
    dplyr::rename(
      R = paste0("tmp_image_", i, "_1"), 
      G = paste0("tmp_image_", i, "_2"), 
      B = paste0("tmp_image_", i, "_3")
    ) |> 
    dplyr::mutate(
      RGB = rgb(red = R/255, green = G/255, blue = B/255) # 正規化してカラーコードを取得
    )
  
  # ドット絵を作図
  dot_graph <- ggplot() + 
    geom_point(data = raster_df, 
               mapping = aes(x = x, y = y, color = RGB), 
               size = 0.75) + # ドット
    scale_color_identity() + 
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
  ggsave(
    plot = dot_graph, filename = paste0("tmp_data/dot_art_", i, ".jpg"), 
    width = width_size/20, height = height_size/20, units = "i", dpi = 150
  )
  
  # 途中経過を表示
  message("\r", i, " / ", frame_num, appendLF = FALSE)
}

warnings()


### ・gif画像の作成 -----

# gif画像を作成
paste0("tmp_data/dot_art_", 1:frame_num, ".jpg") |> # ファイルパスを作成
  magick::image_read() |> # 画像ファイルを読込
  magick::image_animate(fps = 1, dispose = "previous") |> # gif画像を作成
  magick::image_write_gif(path = "output/dot_art/DeLorean.gif", delay = 0.1) -> tmp_path # gifファイル書出


