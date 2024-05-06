# wasm2u#

## 使い方

```
Usage: wasm2usharp.exe [OPTIONS] <INPUT>

Arguments:
  <INPUT>  入力ファイル

Options:
  -o <OUTPUT>      出力をファイルへ書き込み
      --test       テスト用のC#へと変換する
  -h, --help       ヘルプを出力
```

* U#では初期化のために最初に`w2us_init`メソッドを呼ぶ必要がある
* Wasmでエクスポートされたグローバル変数・メモリ・テーブルはU#ではpublicフィールドとなり、
  Wasmでエクスポートされた関数はU#ではpublicメソッドとなる
* Wasmでのインポートは、U#ではモジュール名ごとに`class_モジュール名`型のpublicフィールドが作成され、
  変換されたコード内では`モジュール名.インポート名`でアクセスされる
* 関数の再帰呼び出しは、Wasm内で完結する再帰呼び出しは正しく処理されるが、
  Wasmから外部の関数を呼び出し、
  外部から再度Wasmの関数を呼び出すことで再帰呼び出しされる場合は、
  再帰呼び出しされる関数のローカル変数の値が正しくならない可能性がある
* Wasmでの整数型はU#の符号付き整数型と対応する
* コマンドで出力ファイル名を指定した場合は、そのファイル名から拡張子を除いた名前がU#のクラス名となる
* Wasmのエクスポート・インポートの名前は、英数字以外の文字は`_`に置換され、
  先頭が数字の場合とC#のキーワードと同じ名前の場合は先頭に`_`が付与される

## 対応している[Wasm proposal](https://github.com/WebAssembly/proposals/blob/main/finished-proposals.md "proposals/finished-proposals.md at main · WebAssembly/proposals")

* Import/Export of Mutable Globals
* Sign-extension operators
* Bulk memory operations (memory.copy命令とmemory.fill命令のみ対応)

## TODO

* 出力されるU#の最適化
* UdonSharpでのWASIの実装
* 他のproposalへの対応
