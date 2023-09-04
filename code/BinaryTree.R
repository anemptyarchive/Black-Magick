
# バイナリツリーの作図 ------------------------------------------------------------------

# 利用パッケージ
library(tidyverse)

# チェック用
library(ggplot2)


# 二分木の作成 ------------------------------------------------------------------

# ノード数を指定
N <- 24

# (簡易的に)値を作成
a <- 1:N


# インデックスラベルの調整値を指定
d <- 0.6

# 頂点の座標を作成
node_df <- tibble::tibble(
  value = a, 
  index = 1:N, 
  depth = floor(log2(index)), 
  col_idx = index - 2^depth + 1, # 深さごとの頂点数
  coord_x = (col_idx * 2 - 1) * 1/(2^depth * 2), 
  label_offset = dplyr::if_else(
    condition = index%%2 == 0, true = 0.5+d, false = 0.5-d
  ) # ラベル位置を左右にズラす
)

# 辺の座標を作成
edge_df <- dplyr::bind_rows(
  # 子ノードの座標
  node_df |> 
    dplyr::filter(depth > 0) |> # 根を除去
    dplyr::mutate(
      edge_id   = index, # 線分描画用
      node_type = "childe" # 目視確認用
    ), 
  # 親ノードの座標
  node_df |> 
    dplyr::filter(depth > 0) |> # 根を除去
    dplyr::mutate(
      edge_id   = index, # 線分描画用
      node_type = "parent", # 目視確認用
      depth   = depth - 1, 
      index   = index %/% 2, 
      col_idx = index - 2^depth + 1, 
      coord_x = (col_idx * 2 - 1) * 1/(2^depth * 2)
    )
) |> 
  dplyr::select(!label_offset) |> 
  dplyr::arrange(edge_id, depth)


# ツリーの高さを取得
max_h <- floor(log2(N))
max_h <- max(node_df[["depth"]])

# 縦方向の余白の調整値を指定
d <- 0.1

# 二分木を作図
ggplot() + 
  geom_path(data = edge_df, 
            mapping = aes(x = coord_x, y = depth, group = edge_id)) + # 辺
  geom_point(data = node_df, 
             mapping = aes(x = coord_x, y = depth), 
             size = 10, shape = "circle filled", fill = "white", stroke = 1) + # 頂点
  geom_text(data = node_df, 
            mapping = aes(x = coord_x, y = depth, label = value), 
            size = 5) + # 値ラベル
  geom_text(data = node_df, 
            mapping = aes(x = coord_x, y = depth, label = index, hjust = label_offset), 
            size = 4, vjust = -2, color = "red") + # 位置ラベル
  scale_x_continuous(labels = NULL, name = "") + 
  scale_y_reverse(breaks = 0:max_h, limits = c(max_h+d, -d), minor_breaks = FALSE) + 
  coord_cartesian(xlim = c(0, 1)) + 
  labs(title = "binary tree", 
       subtitle = paste0("height = ", max_h), 
       y = "depth")


# 部分木インデックスの計算 ------------------------------------------------------------------

# 部分木インデックスの定義
subtree <- function(root_index, max_index) {
  
  # インデックスの最大値を取得
  N <- max_index
  
  # 部分木の根を取得
  parent_idx <- root_index[root_index <= N] |> 
    sort()
  
  # 親ごとに処理
  childe_idx <- NULL
  for(i in parent_idx) {
    
    # 兄弟インデックスを作成
    tmp_idx <- c(2*i, 2*i+1)
    
    # 子インデックスを格納
    childe_idx <- c(childe_idx, tmp_idx[tmp_idx <= N]) # 最大位置を超えたら除去
  }
  
  # 子がなければ再帰処理を終了
  if(length(childe_idx) == 0) return(parent_idx)
  
  # 孫インデックスを作成
  tmp_idx <- subtree(childe_idx, N)
  
  # 子孫インデックスを格納
  parent_idx <- c(parent_idx, tmp_idx) |> 
    sort()
  
  # 子孫インデックスを出力
  return(parent_idx)
}


# 部分木の作成 ------------------------------------------------------------------

# ノード数を指定
N <- 31

# 部分木の根を指定
sub_root_idx <- c(7, 9, 22)

# 部分木のインデックスを作成
sub_node_idx <- subtree(sub_root_idx, N)

# 値を作成
a <- sample(x = 1:N, size = N, replace = TRUE)

# 部分木以外のノードを剪定
a[!(1:N %in% sub_node_idx)] <- NA


# 頂点の座標を作成
d <- 0.6
node_df <- tibble::tibble(
  value   = a, 
  node_id = 1:N, 
  index   = dplyr::if_else(
    condition = !is.na(value), true = node_id, false = NA_real_
  ), 
  depth   = floor(log2(index)), 
  col_idx = index - 2^depth + 1, # 深さごとの頂点数
  coord_x = (col_idx * 2 - 1) * 1/(2^depth * 2), 
  label_offset = dplyr::if_else(
    condition = index%%2 == 0, true = 0.5+d, false = 0.5-d
  ) # ラベル位置を左右にズラす
)

# 辺の座標を作成
edge_df <- dplyr::bind_rows(
  # 子ノードの座標
  node_df |> 
    dplyr::filter(depth > 0, !node_id %in% sub_root_idx) |> # 根を除去
    dplyr::mutate(
      edge_id   = index, # 線分描画用
      node_type = "childe" # 目視確認用
    ), 
  # 親ノードの座標
  node_df |> 
    dplyr::filter(depth > 0, !node_id %in% sub_root_idx) |> # 根を除去
    dplyr::mutate(
      edge_id   = index, # 線分描画用
      node_type = "parent", # 目視確認用
      depth   = depth - 1, 
      index   = index %/% 2, 
      col_idx = index - 2^depth + 1, 
      coord_x = (col_idx * 2 - 1) * 1/(2^depth * 2)
    )
) |> 
  dplyr::select(!label_offset) |> 
  dplyr::arrange(edge_id, depth)


# ツリーの高さを取得
max_h <- floor(log2(N))

# 部分二分木を作図
d <- 0.1
ggplot() + 
  geom_path(data = edge_df, 
            mapping = aes(x = coord_x, y = depth, group = edge_id)) + # 辺
  geom_point(data = node_df, 
             mapping = aes(x = coord_x, y = depth), 
             size = 12, shape = "circle filled", fill = "white", stroke = 1, na.rm = TRUE) + # 頂点
  geom_text(data = node_df, 
            mapping = aes(x = coord_x, y = depth, label = value), 
            size = 5, na.rm = TRUE) + # 値ラベル
  geom_text(data = node_df, 
            mapping = aes(x = coord_x, y = depth, label = index, hjust = label_offset), 
            size = 4, vjust = -2, color = "red", na.rm = TRUE) + # 位置ラベル
  scale_x_continuous(labels = NULL, name = "") + 
  scale_y_reverse(breaks = 0:max_h, limits = c(max_h+d, -d), minor_breaks = FALSE) + 
  coord_cartesian(xlim = c(0, 1)) + 
  labs(title = "binary tree", 
       subtitle = "subtree", 
       y = "depth")


# 欠損の作成 -------------------------------------------------------------------

# ノード数を指定
N <- 31

# (削除する)部分木の根を指定
sub_root_idx <- c(7, 9, 22)

# 部分木のインデックスを作成
del_node_idx <- subtree(sub_root_idx, N)

# 値を作成
a <- sample(x = 1:N, size = N, replace = TRUE)

# 部分木のノードを剪定
a[del_node_idx] <- NA


# 頂点の座標を作成
d <- 0.6
node_df <- tibble::tibble(
  value   = a, 
  node_id = 1:N, 
  index   = dplyr::if_else(
    condition = !is.na(value), true = node_id, false = NA_real_
  ), 
  depth   = floor(log2(index)), 
  col_idx = index - 2^depth + 1, # 深さごとの頂点数
  coord_x = (col_idx * 2 - 1) * 1/(2^depth * 2), 
  label_offset = dplyr::if_else(
    condition = index%%2 == 0, true = 0.5+d, false = 0.5-d
  ) # ラベル位置を左右にズラす
)

# 辺の座標を作成
edge_df <- dplyr::bind_rows(
  # 子ノードの座標
  node_df |> 
    dplyr::filter(depth > 0) |> # 根を除去
    dplyr::mutate(
      edge_id   = index, # 線分描画用
      node_type = "childe" # 目視確認用
    ), 
  # 親ノードの座標
  node_df |> 
    dplyr::filter(depth > 0) |> # 根を除去
    dplyr::mutate(
      edge_id   = index, # 線分描画用
      node_type = "parent", # 目視確認用
      depth   = depth - 1, 
      index   = index %/% 2, 
      col_idx = index - 2^depth + 1, 
      coord_x = (col_idx * 2 - 1) * 1/(2^depth * 2)
    )
) |> 
  dplyr::select(!label_offset) |> 
  dplyr::arrange(edge_id, depth)


# ツリーの高さを取得
max_h <- max(node_df[["depth"]], na.rm = TRUE)

# 部分二分木を作図
d <- 0.1
ggplot() + 
  geom_path(data = edge_df, 
            mapping = aes(x = coord_x, y = depth, group = edge_id)) + # 辺
  geom_point(data = node_df, 
             mapping = aes(x = coord_x, y = depth), 
             size = 12, shape = "circle filled", fill = "white", stroke = 1, na.rm = TRUE) + # 頂点
  geom_text(data = node_df, 
            mapping = aes(x = coord_x, y = depth, label = value), 
            size = 5, na.rm = TRUE) + # 値ラベル
  geom_text(data = node_df, 
            mapping = aes(x = coord_x, y = depth, label = index, hjust = label_offset), 
            size = 4, vjust = -2, color = "red", na.rm = TRUE) + # 位置ラベル
  scale_x_continuous(labels = NULL, name = "") + 
  scale_y_reverse(breaks = 0:max_h, limits = c(max_h+d, -d), minor_breaks = FALSE) + 
  coord_cartesian(xlim = c(0, 1)) + 
  labs(title = "binary tree", 
       subtitle = paste0("height = ", max_h), 
       y = "depth")


