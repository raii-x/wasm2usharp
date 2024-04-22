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

## Wasm型とU#型の対応

| Wasm | U#     |
| ---- | ------ |
| i32  | int    |
| i64  | long   |
| f32  | float  |
| f64  | double |

## 対応しているWasm proposal

* Sign-extension operators

## TODO

* 出力されるU#の最適化
* UdonSharpでのWASIの実装
* 他のproposalへの対応
