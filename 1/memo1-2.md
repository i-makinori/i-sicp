
# 1.2 手続きと処理が生成するもの

https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book-Z-H-10.html#%_sec_1.2  

考察を経ての結果の可視化は、真のプログラマにとって重要な事である.
何処で何の手続きが、望む結果を受け継ぐか。

# 1.2.1 反復と再帰

nの階乗.
```
n! x :: let x :
  1 => 1
  n => n * (n-1)!
```

漸化モデル: 単純な漸化式をそのまま表現することも出来るが、同時に、  
置換モデル: 1から数え始めてxに達するまでの積として考えることも出来る。  

2つの手続きの過程を比較すると大いに異なる。
漸化モデルでは鎖のように定義された演算子の記号が鎖のように積み重なっていて(`recursive process`)、解釈するものに再び元の場所に戻る事を要求する。
それぞれの行の長さは、掛け算は何回目なのかに比例しており、線の長さが回数に比例する時、線形再帰手続き(`linear recursive process`)と 呼ばれる。

対して置換モデルでは伸びも縮みもしない。それぞれの手順毎にすべてにn関して,現在の計算された値,手順の回数を数えるもの,手順の回数を数えるものの最大値, が示されている。 これを、反復処理(`iterative process`) と命名し、現状ではこれは同時に値の状態を示している。


scehme の場合では積み重なるように書かれた手続きであろうと最適化される。


# 1.2.2 木構造の再帰

 





