Monadic Parser Example
======================

Happy と Alex をつかった Monadic Parser を書く。なるべくコンパクトなサンプルコードを目指します。

## Example 1

まずは、Monadic ではない Lexer & Parser の例を、おなじみの四則演算式で。


## Example 2

Monadic Parser をつかって、内部状態を持つ構文解析器をつくります。
Example 1 のように加減乗除の中置演算子を predefine するのではなく、あとから定義できるようにします。

定義されるまで、どういう文字列が演算子として扱われるべきなのか、とか、優先度はわからないため、
構文解析結果によって Lexer の動作を変える必要があります。このために Parser Monad のユーザ状態を用いています。

こうすることによって、定義された優先度にしたがった正しい構文木をつくることができます
（後段で変形する必要はない）。

## Example 3

ネストしたコメントを扱う。また、コメントの途中で EOF に達した場合はその旨も報告する。

コメントのレベルを記憶するために、Parser Monad のユーザ状態を用いる。
また、コメントの途中で EOF に達した場合には alexEOF がエラーを検出する。

## Example 4 (予定)

レイアウト規則にしたがって、（仮想）ブレースやセミコロンを挿入する lexer をつくります。
ここでも、ユーザ状態を用います。

ユーザ状態を用いる練習と、Haskell のレイアウト規則の確認のための例題という位置づけ。

