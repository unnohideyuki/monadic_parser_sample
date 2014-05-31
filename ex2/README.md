Monadic Parser の例
===================

Happy と Alex を用いた Monadic Perser の例として、以下のような小さな言語を処理するパーサを作ってみました。
Parser/Lexer が状態を持っていて、構文解析の結果に応じて状態が変わります。Haskell で状態を扱うには、モナドというわけです。

## 今回作るもの

Example 1 では、中置演算子があらかじめ定義されている前提で、四則演算を含む式の構文解析器を作りました。

今度は、四則演算のための関数 plus, minus, times, div があらかじめ定義されていて、
中置演算子はあとから定義するような言語を考えます。

中置演算子は、次のように定義できるものとします。優先度は 0, 1 のいずれかを指定します。

    infixl 0 + plus;
    infixl 0 - minus;
    infixl 1 * times;
    infixl 1 / div;

この言語では、最初の状態では中置演算子は定義されていません。

    $ echo "1 + 2" | ./Parser
    parseError: Name: "+" at line 1, col 3

plus, minus などは定義されています。

    $ echo "plus 1 2" | ./Parser
    3

infixl を用いて定義した後であれば、中置演算子が使えます。

    $ echo "infixl 0 + plus; infixl 1 * times; 2 + 3 * 5" | ./Parser
    17

上の例では、乗算の優先度を高くしたので乗算が先に処理されていますが、同じ優先度に設定してやれば、次のようになります。

    $ echo "infixl 0 + plus; infixl 0 * times; 2 + 3 * 5" | ./Parser
    25
