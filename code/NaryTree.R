
# N-aryツリーの作図 -------------------------------------------------------------

# 利用パッケージ
library(tidyverse)

# チェック用
library(ggplot2)


# n分木の作成 ------------------------------------------------------------------

### ・関数の作成 -----

# 深さカウント関数を作成
count_depth <- function(index, base, depth_cnt = 0) {
  
  # 親インデックスを計算
  parent_index <- ceiling(index / base) - 1
  
  # 深さをカウント
  depth_cnt <- depth_cnt + 1
  
  # 親が根なら終了
  if(parent_index <= 0) return(depth_cnt)
  
  # 深さを計算
  depth_cnt <- count_depth(parent_index, base, depth_cnt)
  
  # 深さを出力
  return(depth_cnt)
}

# 深さ計算関数(スカラ)を作成
index_to_depth <- function(index, base) {
  
  # 深さを作成
  depth_cnt <- 0
  
  # 根なら終了
  if(index == 0) return(depth_cnt)
  
  # 深さを計算
  depth_cnt <- count_depth(index, base, depth_cnt)
  
  # 深さを出力
  return(depth_cnt)
}

# 深さ計算関数(ベクトル)を作成
depth <- function(index, base) {
  
  # 深さの受け皿を初期化
  depth <- rep(0, times = length(index))
  
  # 要素ごとに処理
  for(i in seq_along(index)) {
    
    # 深さを計算
    depth[i] <- index_to_depth(index[i], base)
  }
  
  # 深さを出力
  return(depth)
}

# 最小インデックス計算関数(スカラ)を作成
depth_to_min_index <- function(depth, base) {
  
  # インデックスの最小値を初期化
  index <- 0
  
  # 根なら終了
  if(depth == 0) return(index)
  
  # 深さごとに処理
  for(h in 1:depth) {
    
    # インデックスの最小値を計算
    index <- index * base + 1
  }
  
  # 最小インデックスを出力
  return(index)
}

# 最小インデックス計算関数(ベクトル)を作成
min_index <- function(depth, base) {
  
  # インデックスの受け皿を作成
  index <- rep(0, times = length(depth))
  
  # 要素ごとに処理
  for(i in seq_along(depth)) {
    
    # 最小インデックスを計算
    index[i] <- depth_to_min_index(depth[i], base)
  }
  
  # 最小インデックスを出力
  return(index)
}


### ・作図 -----

# 分岐数を指定
b <- 5

# ノード数を指定
N <- 61

# (簡易的に)値を作成
a <- 1:N

# ラベル位置の調整値を指定
d <- 0.6

# ノードの座標を作成
node_df <- tibble::tibble(
  value   = a, 
  index   = 0:(N-1), 
  depth   = depth(index, b), 
  col_idx = index - min_index(depth, b) + 1, # 深さごとのノード番号
  coord_x = (col_idx * 2 - 1) * 1/(min_index(depth+1, b) - min_index(depth, b)), 
  label_offset = dplyr::case_when(
    depth >= 3 ~ 0.5 - d, # 指定した深さ以降は全て指定した位置に配置:(深さと符号を指定)
    (index-1)%%b <  0.5*b ~ 0.5 + d, # 中より左の分岐は左に配置
    (index-1)%%b >= 0.5*b ~ 0.5 - d  # 中と右の分岐は右に配置
  ) # ラベル位置を左右にズラす
)

# エッジの座標を作成
edge_df <- dplyr::bind_rows(
  # 子ノードの座標
  node_df |> 
    dplyr::filter(depth > 0) |> # 根を除去
    dplyr::mutate(
      edge_id   = index, 
      node_type = "childe" # (確認用)
    ), 
  # 親ノードの座標
  node_df |> 
    dplyr::filter(depth > 0) |> # 根を除去
    dplyr::mutate(
      edge_id   = index, 
      node_type = "parent", # (確認用)
      depth   = depth - 1, 
      index   = ceiling(index / b) - 1, 
      col_idx = index - min_index(depth, b) + 1, # 深さごとのノード番号
      coord_x = (col_idx * 2 - 1) * 1/(min_index(depth+1, b) - min_index(depth, b)), 
    )
) |> 
  dplyr::select(!label_offset) |> 
  dplyr::arrange(edge_id, depth)


# ツリーの高さを取得
max_h <- depth(N-1, b)

# 縦方向の余白の調整値を指定
d <- 0.1

# 二分木を作図
ggplot() + 
  geom_path(data = edge_df, 
            mapping = aes(x = coord_x, y = depth, group = edge_id), 
            linewidth = 1) + # エッジ
  geom_point(data = node_df, 
             mapping = aes(x = coord_x, y = depth), 
             size = 10, shape = "circle filled", fill = "white", stroke = 1) + # ノード
  # geom_text(data = node_df, 
  #           mapping = aes(x = coord_x, y = depth, label = value), 
  #           size = 5) + # 値ラベル
  geom_text(data = node_df, 
            mapping = aes(x = coord_x, y = depth, label = paste0("a[", index, "]")), parse = TRUE, 
            size = 4) + # 要素ラベル
  geom_text(data = node_df, 
            mapping = aes(x = coord_x, y = depth, label = index, hjust = label_offset), 
            size = 4, vjust = -1.5, color = "blue") + # 位置ラベル
  scale_x_continuous(labels = NULL, name = "") + 
  scale_y_reverse(breaks = 0:max_h, minor_breaks = FALSE) + 
  coord_cartesian(xlim = c(0, 2), ylim = c(max_h+d, -d)) + # 表示範囲
  labs(title = paste0(b, "-ary tree"), 
       subtitle = paste0("height = ", max_h), 
       y = "depth")


