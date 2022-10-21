# %% # 三角グラフの作図

# 利用ライブラリ
import numpy as np
import matplotlib.pyplot as plt
from matplotlib.animation import FuncAnimation
from scipy.stats import dirichlet


# %% # 三角座標の作成

# 軸目盛の位置を指定
axis_vals = np.arange(start=0.0, stop=1.1, step=0.1)

# 軸線用の値を作成
axis_x = np.array([0.5, 0.0, 1.0])
axis_y = np.array([0.5*np.sqrt(3.0), 0.0, 0.0])
axis_u = np.array([-0.5, 1.0, -0.5])
axis_v = np.array([-0.5*np.sqrt(3.0), 0.0, 0.5*np.sqrt(3.0)])

# グリッド線用の値を作成
grid_x = np.hstack([
    0.5 * axis_vals, 
    axis_vals, 
    0.5 * axis_vals + 0.5
])
grid_y = np.hstack([
    0.5 * axis_vals * np.sqrt(3.0), 
    np.zeros_like(axis_vals), 
    0.5 * (1.0 - axis_vals) * np.sqrt(3.0)
])
grid_u = np.hstack([
    0.5 * axis_vals, 
    0.5 * (1.0 - axis_vals), 
    -axis_vals
])
grid_v = np.hstack([
    -0.5 * axis_vals * np.sqrt(3.0), 
    0.5 * (1.0 - axis_vals) * np.sqrt(3.0), 
    np.zeros_like(axis_vals)
])

# %%

# 三角座標を作成
plt.figure(figsize=(10, 10), facecolor='white') # 図の設定
plt.quiver(grid_x, grid_y, grid_u, grid_v, 
           scale_units='xy', scale=1, units='dots', width=0.1, headwidth=0.1, 
           fc='none', ec='gray', linewidth=1.5, linestyle=':') # 三角座標のグリッド線
plt.quiver(axis_x, axis_y, axis_u, axis_v, 
           scale_units='xy', scale=1, units='dots', width=1.5, headwidth=1.5, 
           fc='black', linestyle='-') # 三角座標の枠線
for val in axis_vals:
    plt.text(x=0.5*val, y=0.5*val*np.sqrt(3.0), s=str(np.round(1.0-val, 1))+' '*2, 
             ha='right', va='bottom', rotation=-60) # 三角座標のx軸目盛
    plt.text(x=val, y=0.0, s=str(np.round(val, 1))+' '*10, 
             ha='center', va='center', rotation=60) # 三角座標のy軸目盛
    plt.text(x=0.5*val+0.5, y=0.5*(1.0-val)*np.sqrt(3.0), s=' '*3+str(np.round(1.0-val, 1)), 
             ha='left', va='center') # 三角座標のz軸目盛
plt.text(x=0.25, y=0.25*np.sqrt(3.0), s='$x_0$'+' '*5, 
         ha='right', va='center', size=25) # 三角座標のx軸ラベル
plt.text(x=0.5, y=0.0, s='\n'+'$x_1$', 
         ha='center', va='top', size=25) # 三角座標のy軸ラベル
plt.text(x=0.75, y=0.25*np.sqrt(3.0), s=' '*4+'$x_2$', 
         ha='left', va='center', size=25) # 三角図のz軸ラベル
plt.xticks(ticks=[0.0, 0.5, 1.0], labels='') # 2次元座標のx軸目盛
plt.yticks(ticks=[0.0, 0.25*np.sqrt(3.0), 0.5*np.sqrt(3.0)], labels='') # 2次元座標のy軸目盛
plt.grid() # 2次元座標のグリッド線
plt.axis('equal') # アスペクト比
plt.suptitle(t='Ternary Plot', fontsize=20) # 全体のタイトル
plt.title(label='$x=(x_0, x_1, x_2)$', loc='left') # タイトル
plt.show() # 描画

# %%

# 三角座標を作成:(軸の可視化)
plt.figure(figsize=(10, 10), facecolor='white') # 図の設定
plt.quiver(grid_x, grid_y, grid_u, grid_v, 
           scale_units='xy', scale=1, units='dots', width=0.1, headwidth=0.1, 
           fc='none', ec=np.repeat(['red', 'green', 'blue'], len(axis_vals)), linewidth=1.5, linestyle=':') # 三角座標のグリッド線
plt.quiver(axis_x, axis_y, axis_u, axis_v, 
           scale_units='xy', scale=1, units='dots', width=1.5, headwidth=1.5, 
           fc=['red', 'green', 'blue'], linestyle='-') # 三角座標の枠線
for val in axis_vals:
    plt.text(x=0.5*val, y=0.5*val*np.sqrt(3.0), s=str(np.round(1.0-val, 1))+' '*2, 
             ha='right', va='bottom', c='red', rotation=-60) # 三角座標のx軸目盛
    plt.text(x=val, y=0.0, s=str(np.round(val, 1))+' '*10, 
             ha='center', va='center', c='green', rotation=60) # 三角座標のy軸目盛
    plt.text(x=0.5*val+0.5, y=0.5*(1.0-val)*np.sqrt(3.0), s=' '*3+str(np.round(1.0-val, 1)), 
             ha='left', va='center', c='blue') # 三角座標のz軸目盛
plt.text(x=0.25, y=0.25*np.sqrt(3.0), s='$x_0$'+' '*5, 
         ha='right', va='center', c='red', size=25) # 三角座標のx軸ラベル
plt.text(x=0.5, y=0.0, s='\n'+'$x_1$', 
         ha='center', va='top', c='green', size=25) # 三角座標のy軸ラベル
plt.text(x=0.75, y=0.25*np.sqrt(3.0), s=' '*4+'$x_2$', 
         ha='left', va='center', c='blue', size=25) # 三角図のz軸ラベル
plt.xticks(ticks=[0.0, 0.5, 1.0], labels='') # 2次元座標のx軸目盛
plt.yticks(ticks=[0.0, 0.25*np.sqrt(3.0), 0.5*np.sqrt(3.0)], labels='') # 2次元座標のy軸目盛
plt.grid() # 2次元座標のグリッド線
plt.axis('equal') # アスペクト比
plt.suptitle(t='Ternary Plot', fontsize=20) # 全体のタイトル
plt.title(label='$x=(x_0, x_1, x_2)$', loc='left') # タイトル
plt.show() # 描画


# %% # 散布図の作成

# データ数を指定
N = 9

# ディリクレ分布のパラメータを指定
alpha_k = np.array([1.0, 1.0, 1.0])

# ディリクレ分布の乱数を生成
x_nk = np.random.dirichlet(alpha=alpha_k, size=N)

# サンプルを三角座標に変換
y_n0 = x_nk[:, 1] + 0.5 * x_nk[:, 2]
y_n1 = 0.5 * x_nk[:, 2] * np.sqrt(3.0)

# パラメータラベル用の文字列を作成
param_text = '$' + '\\alpha=('+', '.join([str(val) for val in alpha_k])+')' + ', x=(x_0, x_1, x_2)' + '$'

# 三角座標上の散布図を作成
plt.figure(figsize=(10, 10), facecolor='white') # 図の設定
plt.quiver(grid_x, grid_y, grid_u, grid_v, 
           scale_units='xy', scale=1, units='dots', width=0.1, headwidth=0.1, 
           fc='none', ec=np.repeat(['red', 'green', 'blue'], len(axis_vals)), linewidth=1.5, linestyle=':') # 三角座標のグリッド線
plt.quiver(axis_x, axis_y, axis_u, axis_v, 
           scale_units='xy', scale=1, units='dots', width=1.5, headwidth=1.5, 
           fc=['red', 'green', 'blue'], linestyle='-') # 三角座標の枠線
for val in axis_vals:
    plt.text(x=0.5*val, y=0.5*val*np.sqrt(3.0), s=str(np.round(1.0-val, 1))+' '*2, 
             ha='right', va='bottom', c='red', rotation=-60) # 三角座標のx軸目盛
    plt.text(x=val, y=0.0, s=str(np.round(val, 1))+' '*10, 
             ha='center', va='center', c='green', rotation=60) # 三角座標のy軸目盛
    plt.text(x=0.5*val+0.5, y=0.5*(1.0-val)*np.sqrt(3.0), s=' '*3+str(np.round(1.0-val, 1)), 
             ha='left', va='center', c='blue') # 三角座標のz軸目盛
plt.text(x=0.25, y=0.25*np.sqrt(3.0), s='$x_0$'+' '*5, 
         ha='right', va='center', c='red', size=25) # 三角座標のx軸ラベル
plt.text(x=0.5, y=0.0, s='\n'+'$x_1$', 
         ha='center', va='top', c='green', size=25) # 三角座標のy軸ラベル
plt.text(x=0.75, y=0.25*np.sqrt(3.0), s=' '*4+'$x_2$', 
         ha='left', va='center', c='blue', size=25) # 三角図のz軸ラベル
plt.scatter(x=y_n0, y=y_n1, c='orange', s=100) # サンプルの点
for n in range(N):
    plt.annotate(text='('+', '.join([str(np.round(val, 2)) for val in x_nk[n]])+')', xy=(y_n0[n], y_n1[n]+0.02), 
                 ha='center', va='bottom', c='orange', size=10, 
                 bbox=dict(boxstyle='round', fc='white', ec='orange', alpha=0.8)) # サンプルラベル
plt.xticks(ticks=[0.0, 0.5, 1.0], labels='') # 2次元座標のx軸目盛
plt.yticks(ticks=[0.0, 0.25*np.sqrt(3.0), 0.5*np.sqrt(3.0)], labels='') # 2次元座標のy軸目盛
plt.grid() # 2次元座標のグリッド線
plt.axis('equal') # アスペクト比
plt.suptitle(t='Scatter Ternary Plot', fontsize=20) # タイトル
plt.title(label=param_text, loc='left') # パラメータラベル
plt.show() # 描画


# %% # 散布図のアニメーションの作成

# 3次元変数の値を指定
x_0_vals = np.hstack([
  np.arange(0.2, 0.401, step = 0.01), 
  np.arange(0.2, 0.391, step = 0.01)[::-1], 
  np.repeat(0.2, repeats = 19)
]).round(decimals=2)
x_1_vals = np.hstack([
  np.repeat(0.2, repeats = 21), 
  np.arange(0.21, 0.401, step = 0.01), 
  np.arange(0.21, 0.391, step = 0.01)[::-1]
]).round(decimals=2)
x_2_vals = np.hstack([
  np.arange(0.4, 0.601, step = 0.01)[::-1], 
  np.repeat(0.4, repeats = 20), 
  np.arange(0.41, 0.591, step = 0.01)
]).round(decimals=2)

# 三角座標に変換
y_0_vals = x_1_vals + 0.5 * x_2_vals
y_1_vals = 0.5 * x_2_vals * np.sqrt(3.0)

# フレーム数を設定
frame_num = len(x_0_vals)

# 図を初期化
fig = plt.figure(figsize=(10, 10), facecolor='white') # 図の設定
fig.suptitle(t='Ternary Plot', fontsize=20) # タイトル

# 作図処理を関数として定義
def update(i):
    # 前フレームのグラフを初期化
    plt.cla()
    
    # i番目の値を取得
    y_0 = y_0_vals[i]
    y_1 = y_1_vals[i]
    
    # 三角座標上の散布図を作成
    plt.quiver(grid_x, grid_y, grid_u, grid_v, 
               scale_units='xy', scale=1, units='dots', width=0.1, headwidth=0.1, 
               fc='none', ec=np.repeat(['red', 'green', 'blue'], len(axis_vals)), linewidth=1.5, linestyle=':') # 三角座標のグリッド線
    plt.quiver(axis_x, axis_y, axis_u, axis_v, 
               scale_units='xy', scale=1, units='dots', width=1.5, headwidth=1.5, 
               fc=['red', 'green', 'blue'], linestyle='-') # 三角座標の枠線
    for val in axis_vals:
        plt.text(x=0.5*val, y=0.5*val*np.sqrt(3.0), s=str(np.round(1.0-val, 1))+' '*2, 
                ha='right', va='bottom', c='red', rotation=-60) # 三角座標のx軸目盛
        plt.text(x=val, y=0.0, s=str(np.round(val, 1))+' '*10, 
                ha='center', va='center', c='green', rotation=60) # 三角座標のy軸目盛
        plt.text(x=0.5*val+0.5, y=0.5*(1.0-val)*np.sqrt(3.0), s=' '*3+str(np.round(1.0-val, 1)), 
                ha='left', va='center', c='blue') # 三角座標のz軸目盛
    plt.text(x=0.25, y=0.25*np.sqrt(3.0), s='$x_0$'+' '*5, 
             ha='right', va='center', c='red', size=25) # 三角座標のx軸ラベル
    plt.text(x=0.5, y=0.0, s='\n'+'$x_1$', 
             ha='center', va='top', c='green', size=25) # 三角座標のy軸ラベル
    plt.text(x=0.75, y=0.25*np.sqrt(3.0), s=' '*4+'$x_2$', 
             ha='left', va='center', c='blue', size=25) # 三角図のz軸ラベル
    plt.scatter(x=y_0, y=y_1, c='orange', s=150) # サンプルの点
    plt.annotate(text='('+str(x_0_vals[i])+', '+str(x_1_vals[i])+', '+str(x_2_vals[i])+')', xy=(y_0, y_1+0.03), 
                 ha='center', va='bottom', c='orange', size=15, 
                 bbox=dict(boxstyle='round', fc='white', ec='orange', alpha=0.8)) # サンプルラベル
    plt.xticks(ticks=[0.0, 0.5, 1.0], labels='') # 2次元座標のx軸目盛
    plt.yticks(ticks=[0.0, 0.25*np.sqrt(3.0), 0.5*np.sqrt(3.0)], labels='') # 2次元座標のy軸目盛
    plt.grid() # 2次元座標のグリッド線
    plt.axis('equal') # アスペクト比
    plt.title(label='$x=(x_0, x_1, x_2)$', loc='left')

# gif画像を作成
ani = FuncAnimation(fig=fig, func=update, frames=frame_num, interval=100)

# gif画像を保存
ani.save('../figure/Python/ternary_scatter.gif')


# %% # 三角座標上の格子点の作成

## 作図用と計算用の変数の作成

# 2次元座標の値を作成
y_0_vals = np.linspace(start=0.0, stop=1.0, num=201)
y_1_vals = np.linspace(start=0.0, stop=0.5*np.sqrt(3.0), num=200)

# 2次元座標の格子点を作成
y_0_grid, y_1_grid = np.meshgrid(y_0_vals, y_1_vals)

# 格子点の形状を保存
y_shape = y_0_grid.shape

# 3次元座標の値に変換
x_1_vals = y_0_grid.flatten() - y_1_grid.flatten() / np.sqrt(3.0)
x_2_vals = 2.0 * y_1_grid.flatten() / np.sqrt(3.0)

# 範囲外の点を欠損値に置換
x_1_vals = np.where(
    (x_1_vals >= 0.0) & (x_1_vals <= 1.0), 
    x_1_vals, 
    np.nan
)
x_2_vals = np.where(
    (x_2_vals >= 0.0) & (x_2_vals <= 1.0), 
    x_2_vals, 
    np.nan
)

# 3次元座標の値に変換
x_0_vals = 1.0 - x_1_vals - x_2_vals

# 範囲外の点を欠損値に置換
x_0_vals = np.where(
    (x_0_vals >= 0.0) & (x_0_vals <= 1.0), 
    x_0_vals, 
    np.nan
)

# 計算用の3次元座標の点を作成
x_points = np.stack([x_0_vals, x_1_vals, x_2_vals], axis=1)


# %% # 等高線図の作成

# ディリクレ分布のパラメータを指定
alpha_k = np.array([1.5, 2.5, 3.5])

# ディリクレ分布の確率密度を計算
dens_vals = np.array(
    [dirichlet.pdf(x=x_k, alpha=alpha_k) if all(x_k != np.nan) else np.nan for x_k in x_points]
)


# パラメータラベル用の文字列を作成
param_text = '$' + '\\alpha=('+', '.join([str(val) for val in alpha_k])+')' + ', x=(x_0, x_1, x_2)' + '$'

# 三角座標上の等高線図を作成
plt.figure(figsize=(12, 10), facecolor='white') # 図の設定
plt.quiver(grid_x, grid_y, grid_u, grid_v, 
           scale_units='xy', scale=1, units='dots', width=0.1, headwidth=0.1, 
           fc='none', ec='gray', linewidth=1.5, linestyle=':') # 三角座標のグリッド線
plt.quiver(axis_x, axis_y, axis_u, axis_v, 
           scale_units='xy', scale=1, units='dots', width=1.5, headwidth=1.5, 
           fc='black', linestyle='-') # 三角座標の枠線
for val in axis_vals:
    plt.text(x=0.5*val, y=0.5*val*np.sqrt(3.0), s=str(np.round(1.0-val, 1))+' '*2, 
             ha='right', va='bottom', rotation=-60) # 三角座標のx軸目盛
    plt.text(x=val, y=0.0, s=str(np.round(val, 1))+' '*10, 
             ha='center', va='center', rotation=60) # 三角座標のy軸目盛
    plt.text(x=0.5*val+0.5, y=0.5*(1.0-val)*np.sqrt(3.0), s=' '*3+str(np.round(1.0-val, 1)), 
             ha='left', va='center') # 三角座標のz軸目盛
plt.text(x=0.25, y=0.25*np.sqrt(3.0), s='$x_0$'+' '*5, 
         ha='right', va='center', size=25) # 三角座標のx軸ラベル
plt.text(x=0.5, y=0.0, s='\n'+'$x_1$', 
         ha='center', va='top', size=25) # 三角座標のy軸ラベル
plt.text(x=0.75, y=0.25*np.sqrt(3.0), s=' '*4+'$x_2$', 
         ha='left', va='center', size=25) # 三角図のz軸ラベル
cnf = plt.contourf(y_0_grid, y_1_grid, dens_vals.reshape(y_shape), 
                   alpha = 0.8) # 確率密度の等高線
plt.xticks(ticks=[0.0, 0.5, 1.0], labels='') # 2次元座標のx軸目盛
plt.yticks(ticks=[0.0, 0.25*np.sqrt(3.0), 0.5*np.sqrt(3.0)], labels='') # 2次元座標のy軸目盛
plt.grid() # 2次元座標のグリッド線
plt.axis('equal') # アスペクト比
plt.colorbar(cnf, label='density') # カラーバー
plt.suptitle(t='Ternary Contour Plot', fontsize=20) # タイトル
plt.title(label=param_text, loc='left') # パラメータラベル
plt.show() # 描画


# %% 等高線図のアニメーションの作成

# ディリクレ分布のパラメータとして利用する値を指定
alpha_0_vals = np.arange(start=1.0, stop=10.1, step=0.1).round(decimals=1)
alpha_1_vals = np.arange(start=2.0, stop=11.1, step=0.1).round(decimals=1)
alpha_2_vals = np.arange(start=3.0, stop=12.1, step=0.1).round(decimals=1)

# フレーム数を設定
frame_num = len(alpha_0_vals)

# z軸の最小値と最大値を設定
dens_min = 0.0
alpha_max_k = np.array([alpha_0_vals.max(), alpha_1_vals.max(), alpha_2_vals.max()])
dens_max = np.ceil(
    dirichlet.pdf(x=(alpha_max_k+1.0)/(np.sum(alpha_max_k)+3.0), alpha=alpha_max_k)
)

# 等高線を引く値を設定
dens_levels = np.linspace(dens_min, dens_max, num=10)

# 図を初期化
fig = plt.figure(figsize=(12, 10), facecolor='white') # 図の設定
fig.suptitle(t='Ternary Contour Plot', fontsize=20) # タイトル
tmp = plt.contourf(y_0_grid, y_1_grid, np.zeros(y_shape), 
                   vmin=dens_min, vmax=dens_max, levels=dens_levels, alpha = 0.8) # カラーバー用のダミー
fig.colorbar(tmp, label='density') # カラーバー

# 作図処理を関数として定義
def update(i):
    # 前フレームのグラフを初期化
    plt.cla()
    
    # i番目のパラメータを取得
    alpha_k = np.array([alpha_0_vals[i], alpha_1_vals[i], alpha_2_vals[i]])
    
    # ディリクレ分布の確率密度を計算
    dens_vals = np.array(
        [dirichlet.pdf(x=x_k, alpha=alpha_k) if all(x_k != np.nan) else np.nan for x_k in x_points]
    )
    
    # パラメータラベル用の文字列を作成
    param_text = '$' + '\\alpha=('+', '.join([str(val) for val in alpha_k])+')' + ', x=(x_0, x_1, x_2)' + '$'
    
    # 三角座標上の等高線図を作成
    plt.quiver(grid_x, grid_y, grid_u, grid_v, 
               scale_units='xy', scale=1, units='dots', width=0.1, headwidth=0.1, 
               fc='none', ec='gray', linewidth=1.5, linestyle=':') # 三角座標のグリッド線
    plt.quiver(axis_x, axis_y, axis_u, axis_v, 
               scale_units='xy', scale=1, units='dots', width=1.5, headwidth=1.5, 
               fc='black', linestyle='-') # 三角座標の枠線
    for val in axis_vals:
        plt.text(x=0.5*val, y=0.5*val*np.sqrt(3.0), s=str(np.round(1.0-val, 1))+' '*2, 
                ha='right', va='bottom', rotation=-60) # 三角座標のx軸目盛
        plt.text(x=val, y=0.0, s=str(np.round(val, 1))+' '*10, 
                ha='center', va='center', rotation=60) # 三角座標のy軸目盛
        plt.text(x=0.5*val+0.5, y=0.5*(1.0-val)*np.sqrt(3.0), s=' '*3+str(np.round(1.0-val, 1)), 
                ha='left', va='center') # 三角座標のz軸目盛
    plt.text(x=0.25, y=0.25*np.sqrt(3.0), s='$x_0$'+' '*5, 
             ha='right', va='center', size=25) # 三角座標のx軸ラベル
    plt.text(x=0.5, y=0.0, s='\n'+'$x_1$', 
             ha='center', va='top', size=25) # 三角座標のy軸ラベル
    plt.text(x=0.75, y=0.25*np.sqrt(3.0), s=' '*4+'$x_2$', 
             ha='left', va='center', size=25) # 三角図のz軸ラベル
    plt.contourf(y_0_grid, y_1_grid, dens_vals.reshape(y_shape), 
                 vmin=dens_min, vmax=dens_max, levels=dens_levels, alpha = 0.8) # 確率密度の等高線
    plt.xticks(ticks=[0.0, 0.5, 1.0], labels='') # 2次元座標のx軸目盛
    plt.yticks(ticks=[0.0, 0.25*np.sqrt(3.0), 0.5*np.sqrt(3.0)], labels='') # 2次元座標のy軸目盛
    plt.grid() # 2次元座標のグリッド線
    plt.axis('equal') # アスペクト比
    plt.title(label=param_text, loc='left') # パラメータラベル

# gif画像を作成
ani = FuncAnimation(fig=fig, func=update, frames=frame_num, interval=100)

# gif画像を保存
ani.save('../figure/Python/ternary_contour.gif')


# %% # 3Dプロットの作成

# ディリクレ分布のパラメータを指定
alpha_k = np.array([1.5, 2.5, 3.5])

# ディリクレ分布の確率密度を計算
dens_vals = np.array(
    [dirichlet.pdf(x=x_k, alpha=alpha_k) if all(x_k != np.nan) else np.nan for x_k in x_points]
)


# パラメータラベル用の文字列を作成
param_text = '$' + '\\alpha=('+', '.join([str(val) for val in alpha_k])+')' + ', x=(x_0, x_1, x_2)' + '$'

# 三角座標上の曲面図を作成
fig = plt.figure(figsize=(12, 10), facecolor='white') # 図の設定
ax = fig.add_subplot(projection='3d') # 3D用の設定
ax.quiver(grid_x, grid_y, np.zeros_like(grid_x), grid_u, grid_v, np.zeros_like(grid_x), 
          arrow_length_ratio=0.0, ec='gray', linewidth=1.5, linestyle=':') # 三角座標のグリッド線
ax.quiver(axis_x, axis_y, np.zeros_like(axis_x), axis_u, axis_v, np.zeros_like(axis_x), 
          arrow_length_ratio=0.0, ec='black', linestyle='-') # 三角座標の枠線
for val in axis_vals:
    ax.text(x=0.5*val-0.05, y=0.5*val*np.sqrt(3.0), z=0.0, s=str(np.round(1.0-val, 1)), 
            ha='center', va='center') # 三角座標のx軸目盛
    ax.text(x=val, y=0.0-0.05, z=0.0, s=str(np.round(val, 1)), 
            ha='center', va='center') # 三角座標のy軸目盛
    ax.text(x=0.5*val+0.5+0.05, y=0.5*(1.0-val)*np.sqrt(3.0), z=0.0, s=str(np.round(1.0-val, 1)), 
            ha='center', va='center') # 三角座標のz軸目盛
ax.text(x=0.25-0.1, y=0.25*np.sqrt(3.0), z=0.0, s='$x_0$', 
        ha='right', va='center', size=25) # 三角座標のx軸ラベル
ax.text(x=0.5, y=0.0-0.1, z=0.0-0.1, s='$x_1$', 
        ha='center', va='top', size=25) # 三角座標のy軸ラベル
ax.text(x=0.75+0.1, y=0.25*np.sqrt(3.0), z=0.0, s='$x_2$', 
        ha='left', va='center', size=25) # 三角図のz軸ラベル
ax.contour(y_0_grid, y_1_grid, dens_vals.reshape(y_shape), 
           offset=0.0) # 確率密度の等高線
ax.plot_surface(y_0_grid, y_1_grid, dens_vals.reshape(y_shape), 
                cmap='viridis', alpha=0.8) # 確率密度の曲面
ax.set_xticks(ticks=[0.0, 0.5, 1.0], labels='') # 2次元座標のx軸目盛
ax.set_yticks(ticks=[0.0, 0.25*np.sqrt(3.0), 0.5*np.sqrt(3.0)], labels='') # 2次元座標のy軸目盛
ax.set_zlabel(zlabel='density') # z軸ラベル
ax.set_box_aspect(aspect=(1, 1, 1)) # アスペクト比
fig.suptitle(t='3D Ternary Plot', fontsize=20) # タイトル
ax.set_title(label=param_text, loc='left') # パラメータラベル
#ax.view_init(elev=90, azim=-90) # 表示角度
plt.show() # 描画

# %% # 3Dプロットのアニメーションの作図：座標の確認

# ディリクレ分布のパラメータを指定
alpha_k = np.array([1.5, 2.5, 3.5])

# ディリクレ分布の確率密度を計算
dens_vals = np.array(
    [dirichlet.pdf(x=x_k, alpha=alpha_k) if all(x_k != np.nan) else np.nan for x_k in x_points]
)


# 水平方向の角度として利用する値を指定
h_vals = np.arange(0.0, 360.0, step=5.0)

# フレーム数を設定
frame_num = len(h_vals)


# パラメータラベル用の文字列を作成
param_text = '$' + '\\alpha=('+', '.join([str(val) for val in alpha_k])+')' + ', x=(x_0, x_1, x_2)' + '$'

# 三角座標上の等高線図を作成
fig = plt.figure(figsize=(12, 10), facecolor='white') # 図の設定
ax = fig.add_subplot(projection='3d') # 3D用の設定
fig.suptitle(t='3D Ternary Plot', fontsize=20) # タイトル
    
# 作図処理を関数として定義
def update(i):
    # 前フレームのグラフを初期化
    plt.cla()

    # i番目の角度を取得
    h = h_vals[i]

    # 三角座標上の曲面図を作成
    ax.quiver(grid_x, grid_y, np.zeros_like(grid_x), grid_u, grid_v, np.zeros_like(grid_x), 
              arrow_length_ratio=0.0, ec='gray', linewidth=1.5, linestyle=':') # 三角座標のグリッド線
    ax.quiver(axis_x, axis_y, np.zeros_like(axis_x), axis_u, axis_v, np.zeros_like(axis_x), 
              arrow_length_ratio=0.0, ec='black', linestyle='-') # 三角座標の枠線
    for val in axis_vals:
        ax.text(x=0.5*val-0.05, y=0.5*val*np.sqrt(3.0), z=0.0, s=str(np.round(1.0-val, 1)), 
                ha='center', va='center') # 三角座標のx軸目盛
        ax.text(x=val, y=0.0-0.05, z=0.0, s=str(np.round(val, 1)), 
                ha='center', va='center') # 三角座標のy軸目盛
        ax.text(x=0.5*val+0.5+0.05, y=0.5*(1.0-val)*np.sqrt(3.0), z=0.0, s=str(np.round(1.0-val, 1)), 
                ha='center', va='center') # 三角座標のz軸目盛
    ax.text(x=0.25-0.1, y=0.25*np.sqrt(3.0), z=0.0, s='$x_0$', 
            ha='right', va='center', size=25) # 三角座標のx軸ラベル
    ax.text(x=0.5, y=0.0-0.1, z=0.0-0.1, s='$x_1$', 
            ha='center', va='top', size=25) # 三角座標のy軸ラベル
    ax.text(x=0.75+0.1, y=0.25*np.sqrt(3.0), z=0.0, s='$x_2$', 
            ha='left', va='center', size=25) # 三角図のz軸ラベル
    ax.contour(y_0_grid, y_1_grid, dens_vals.reshape(y_shape), 
               offset=0.0) # 確率密度の等高線
    ax.plot_surface(y_0_grid, y_1_grid, dens_vals.reshape(y_shape), 
                    cmap='viridis', alpha=0.8) # 確率密度の曲面
    ax.set_xticks(ticks=[0.0, 0.5, 1.0], labels='') # 2次元座標のx軸目盛
    ax.set_yticks(ticks=[0.0, 0.25*np.sqrt(3.0), 0.5*np.sqrt(3.0)], labels='') # 2次元座標のy軸目盛
    ax.set_zlabel('density') # z軸ラベル
    ax.set_box_aspect(aspect=(1, 1, 1)) # アスペクト比
    ax.set_title(label=param_text, loc='left') # パラメータラベル
    ax.view_init(elev=40, azim=h) # 表示角度

# gif画像を作成
ani = FuncAnimation(fig=fig, func=update, frames=frame_num, interval=100)

# gif画像を保存
ani.save('../figure/Python/ternary_3d_turn.gif')


# %% # 3Dプロットのアニメーションの作図

# ディリクレ分布のパラメータとして利用する値を指定
alpha_0_vals = np.arange(start=1.0, stop=10.1, step=0.1).round(decimals=1)
alpha_1_vals = np.arange(start=2.0, stop=11.1, step=0.1).round(decimals=1)
alpha_2_vals = np.arange(start=3.0, stop=12.1, step=0.1).round(decimals=1)

# フレーム数を設定
frame_num = len(alpha_0_vals)

# グラデーション用に確率密度の最小値と最大値を設定
dens_min = 0.0
alpha_max_k = np.array([alpha_0_vals.max(), alpha_1_vals.max(), alpha_2_vals.max()])
dens_max = np.ceil(
    dirichlet.pdf(x=(alpha_max_k+1.0)/(np.sum(alpha_max_k)+3.0), alpha=alpha_max_k)
)

# 等高線を引く値を設定
dens_levels = np.linspace(dens_min, dens_max, num=10)

# 三角座標上の等高線図を作成
fig = plt.figure(figsize=(12, 12), facecolor='white') # 図の設定
ax = fig.add_subplot(projection='3d') # 3D用の設定
fig.suptitle(t='3D Ternary Plot', fontsize=20) # タイトル
    
# 作図処理を関数として定義
def update(i):
    # 前フレームのグラフを初期化
    plt.cla()

    # i番目のパラメータを取得
    alpha_k = np.array([alpha_0_vals[i], alpha_1_vals[i], alpha_2_vals[i]])
    
    # ディリクレ分布の確率密度を計算
    dens_vals = np.array(
        [dirichlet.pdf(x=x_k, alpha=alpha_k) if all(x_k != np.nan) else np.nan for x_k in x_points]
    )
    
    # パラメータラベル用の文字列を作成
    param_text = '$' + '\\alpha=('+', '.join([str(val) for val in alpha_k])+')' + ', x=(x_0, x_1, x_2)' + '$'
    
    # 三角座標上の曲面図を作成
    ax.quiver(grid_x, grid_y, np.zeros_like(grid_x), grid_u, grid_v, np.zeros_like(grid_x), 
              arrow_length_ratio=0.0, ec='gray', linewidth=1.5, linestyle=':') # 三角座標のグリッド線
    ax.quiver(axis_x, axis_y, np.zeros_like(axis_x), axis_u, axis_v, np.zeros_like(axis_x), 
              arrow_length_ratio=0.0, ec='black', linestyle='-') # 三角座標の枠線
    for val in axis_vals:
        ax.text(x=0.5*val-0.05, y=0.5*val*np.sqrt(3.0), z=0.0, s=str(np.round(1.0-val, 1)), 
                ha='center', va='center') # 三角座標のx軸目盛
        ax.text(x=val, y=0.0-0.05, z=0.0, s=str(np.round(val, 1)), 
                ha='center', va='center') # 三角座標のy軸目盛
        ax.text(x=0.5*val+0.5+0.05, y=0.5*(1.0-val)*np.sqrt(3.0), z=0.0, s=str(np.round(1.0-val, 1)), 
                ha='center', va='center') # 三角座標のz軸目盛
    ax.text(x=0.25-0.1, y=0.25*np.sqrt(3.0), z=0.0, s='$x_0$', 
            ha='right', va='center', size=25) # 三角座標のx軸ラベル
    ax.text(x=0.5, y=0.0-0.1, z=0.0-0.1, s='$x_1$', 
            ha='center', va='top', size=25) # 三角座標のy軸ラベル
    ax.text(x=0.75+0.1, y=0.25*np.sqrt(3.0), z=0.0, s='$x_2$', 
            ha='left', va='center', size=25) # 三角図のz軸ラベル
    ax.contour(y_0_grid, y_1_grid, dens_vals.reshape(y_shape), 
               vmin=dens_min, vmax=dens_max, levels=dens_levels, offset=0.0) # 確率密度の等高線
    ax.plot_surface(y_0_grid, y_1_grid, dens_vals.reshape(y_shape), 
                    cmap='viridis', alpha=0.8) # 確率密度の曲面
    ax.set_xticks(ticks=[0.0, 0.5, 1.0], labels='') # 2次元座標のx軸目盛
    ax.set_yticks(ticks=[0.0, 0.25*np.sqrt(3.0), 0.5*np.sqrt(3.0)], labels='') # 2次元座標のy軸目盛
    ax.set_zlabel('density') # z軸ラベル
    ax.set_zlim(bottom=dens_min, top=dens_max) # z軸の表示範囲
    ax.set_box_aspect(aspect=(1, 1, 1)) # アスペクト比
    ax.set_title(label=param_text, loc='left') # パラメータラベル

# gif画像を作成
ani = FuncAnimation(fig=fig, func=update, frames=frame_num, interval=100)

# gif画像を保存
ani.save('../figure/Python/ternary_3d_dens.gif')


# %%

