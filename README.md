# wasm2usharp

[日本語はこちら](#日本語)

A tool for converting WebAssembly to UdonSharp

## How to use

```text
Usage: wasm2usharp [OPTIONS] [INPUT]

Arguments:
  [INPUT]  Input file. If not provided or if this is `-` then stdin is used

Options:
  -o <OUTPUT>      Where to place output. If not provided then stdout is used
      --test       Convert to C# instead of UdonSharp for testing
  -h, --help       Print help
  -V, --version    Print version
```

See [examples](examples) for usage in a VRChat project.

* In U#, you need to call `w2us_init` method first for initialization
* Global variables, memory, and table exported by Wasm become public fields in U#,
  and functions exported by Wasm become public methods in U#
* Imports in Wasm create public fields of type <code>class_*module_name*</code> for each module name in U#,
  and they are accessed by <code>*module_name*.*import_name*</code> in the converted code
* The identifier `_start` in Wasm will be replaced by `w2us_start` after conversion to U#.
  This is to prevent a function named `_start`, which is an entry point in WASI, from being called unexpectedly,
  since it is treated as a Start event function in Udon.
* Recursive calls to functions that are completed within Wasm are handled correctly,
  but if a function is recursively called by calling an external function from Wasm and then calling the Wasm function again from outside,
  the value of the local variable of the recursively called function may not be correct
* Integer types in Wasm correspond to signed integer types in U#
* If an output file name is specified in the command, the file name minus the extension is the class name in U#
* In Wasm export/import names, non-alphanumeric characters are replaced by `_`,
  and if the first letter is a number or the same name as a C# keyword, it is prefixed with `_`

## Supported [Wasm proposals][WasmProposals]

* Import/Export of Mutable Globals
* Non-trapping float-to-int conversions
* Sign-extension operators
* Bulk memory operations (Only `memory.copy` and `memory.fill` instructions are supported)

## 日本語

WebAssemblyからUdonSharpへの変換ツール

## 使い方

```text
Usage: wasm2usharp [OPTIONS] [INPUT]

Arguments:
  [INPUT]  入力ファイル。指定されない、または`-`の場合は標準入力が使用される

Options:
  -o <OUTPUT>      出力先の場所。指定されない場合は標準出力が使用される
      --test       テストのためにUdonSharpの代わりにC#へと変換する
  -h, --help       ヘルプを出力
  -V, --version    バージョンを出力
```

VRChatプロジェクト内での使用例は[examples](examples)を参照してください。

* U#では初期化のために最初に`w2us_init`メソッドを呼ぶ必要がある
* Wasmでエクスポートされたグローバル変数・メモリ・テーブルはU#ではpublicフィールドとなり、
  Wasmでエクスポートされた関数はU#ではpublicメソッドとなる
* Wasmでのインポートは、U#ではモジュール名ごとに`class_モジュール名`型のpublicフィールドが作成され、
  変換されたコード内では`モジュール名.インポート名`でアクセスされる
* Wasmでの`_start`という識別子は、U#への変換後は`w2us_start`に置き換えられる。
  これは、WASIのエントリーポイントである`_start`という名前の関数は
  UdonではStartイベント関数として扱われるので、予期せず呼ばれるのを防ぐためである。
* 関数の再帰呼び出しは、Wasm内で完結する再帰呼び出しは正しく処理されるが、
  Wasmから外部の関数を呼び出し、
  外部から再度Wasmの関数を呼び出すことで再帰呼び出しされる場合は、
  再帰呼び出しされる関数のローカル変数の値が正しくならない可能性がある
* Wasmでの整数型はU#の符号付き整数型と対応する
* コマンドで出力ファイル名を指定した場合は、そのファイル名から拡張子を除いた名前がU#のクラス名となる
* Wasmのエクスポート・インポートの名前は、英数字以外の文字は`_`に置換され、
  先頭が数字の場合とC#のキーワードと同じ名前の場合は先頭に`_`が付与される

## 対応している[Wasm proposal][WasmProposals]

* Import/Export of Mutable Globals
* Non-trapping float-to-int conversions
* Sign-extension operators
* Bulk memory operations (`memory.copy`命令と`memory.fill`命令のみ対応)

[WasmProposals]: https://github.com/WebAssembly/proposals/blob/main/finished-proposals.md "proposals/finished-proposals.md at main · WebAssembly/proposals"
