
# KL情報量が最小となる組み合わせの探索 -----------------------------------------

# 利用パッケージ
library(tidyverse)
library(gtools)

# チェック用
library(ggplot2)


### ・真の分布の設定 -----

# 真の確率を指定
true_prob_k <- c(0.2, 0.1, 0.4, 0.3)

# 次元数を設定
K <- length(true_prob_k)

# 真の確率分布を格納
true_dist_df <- tibble::tibble(
  k = factor(1:K), 
  prob = true_prob_k
)

# 真の確率分布を作図
ggplot() + 
  geom_bar(data = true_dist_df, mapping = aes(x = k, y = prob, fill = k), 
           stat = "identity", show.legend = FALSE) + 
  coord_cartesian(ylim = c(0, 1)) + # 描画範囲
  labs(title = "true distribution", 
       x = "k", y = "probability")


### ・分布の生成 -----

# 確率(カテゴリ分布)を生成
sample_prob_j <- MCMCpack::rdirichlet(n = 1, alpha = rep(1, times = K)) |> 
  as.vector()

# 確率分布のサンプルを格納
sample_dist_df <- tibble::tibble(
  j = factor(1:K), 
  prob = sample_prob_j
)

# 確率分布のサンプルを作図
ggplot() + 
  geom_bar(data = sample_dist_df, mapping = aes(x = j, y = prob, fill = j), 
           stat = "identity", show.legend = FALSE) + 
  coord_cartesian(ylim = c(0, 1)) + # 描画範囲
  labs(title = "sampled distribution", 
       x = "j", y = "probability")


### ・KL情報量の計算 -----

# インデックスの組み合わせを作成してKL情報量を計算
kl_df <- tidyr::expand_grid(
  group = 1:gamma(K+1), # 組み合わせ番号
  k = factor(1:K) # 真の分布・並べ替え後のインデックス
) |> 
  tibble::add_column(
    j = gtools::permutations(n = K, r = K, v = 1:K) |> # インデックスの順列を作成
      t() |> 
      as.vector() |> 
      factor() # 元のインデックス・並べ替え用の値
  ) |> 
  dplyr::group_by(group) |> # 組み合わせごとの計算用
  dplyr::mutate(
    kl = sum(true_prob_k * (log(true_prob_k) - log(sample_prob_j[j]))), # KL情報量
    true_prob = true_prob_k, # 真の確率
    sample_prob = sample_prob_j[j] # 確率のサンプル
  ) |> 
  dplyr::ungroup() |> 
  dplyr::arrange(kl, k) # 当て嵌まりが良い順に並べ替え
kl_df

# インデックスの組み合わせごとに確率分布のサンプルを作図
ggplot() + 
  geom_bar(data = kl_df, mapping = aes(x = k, y = sample_prob, fill = j), 
           stat = "identity", alpha = 0.5) + # 分布のサンプル
  geom_bar(data = kl_df, mapping = aes(x = k, y = true_prob, color = k), 
           stat = "identity", fill = NA, size = 1, linetype = "dashed", show.legend = FALSE) + # 真の分布
  facet_wrap(kl ~ ., labeller = label_bquote(kl==.(kl))) + # 分割
  coord_cartesian(ylim = c(0, 1)) + # 描画範囲
  labs(title = "sampled distribution with changed index", 
       x = "k", y = "probability")


### ・インデックスの入替 -----

# インデックスの対応ベクトルを作成
adapt_idx <- kl_df |> 
  dplyr::slice_head(n = K) |> # 当て嵌まりが良い組み合わせを抽出
  dplyr::pull(j) |> # 列をベクトルに変換
  as.numeric() # 数値に変換
adapt_idx

# インデックスを入れ替えた確率分布のサンプルを格納
adapt_dist_df <- tibble::tibble(
  j = factor(adapt_idx), # 元のインデックス
  k = factor(1:K), # KL情報量が最小になるインデックス
  prob = sample_prob_j[adapt_idx] # 確率を入れ替え
)

# KL情報量が最小になる確率分布を作図
ggplot() + 
  geom_bar(data = adapt_dist_df, mapping = aes(x = k, y = prob, fill = j), 
           stat = "identity", alpha = 0.5) + # 分布のサンプル
  geom_bar(data = true_dist_df, mapping = aes(x = k, y = prob, color = k), 
           stat = "identity", fill = NA, size = 1, linetype = "dashed", show.legend = FALSE) + # 真の分布
  coord_cartesian(ylim = c(0, 1)) + # 描画範囲
  labs(title = "sampled distribution with adapted index", 
       x = "k", y = "probability")


