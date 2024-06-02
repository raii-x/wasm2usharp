# Example

[日本語はこちら](#日本語)

This is an exapmle of a use of wasm2usharp.

## Description of each file

* `hello.wat`: A Wasm program in text format
* `hello.wasm`: A Wasm program converted from `hello.wat` to binary format
* `Assets/class_hello_env.cs`: A class that implement functions imported from the Wasm program
* `Assets/ExecuteHello.cs`: A class that describes the process to execute the Wasm program
* `Assets/ExecuteHello.prefab`: A prefab with dependency fields for each class
* `Assets/Hello.cs`: A class converted from `hello.wasm` with wasm2usharp

## Tested environment

* wasm2usharp 0.1.0
* wasm-tools 1.209.1
* Unity 2022.3.22f1
* VRChat SDK 3.6.1

## How to convert the Wasm program

You need [wasm-tools][wasm-tools] to convert `.wat` to `.wasm`.

```bash
wasm-tools parse hello.wat -o hello.wasm
wasm2usharp hello.wasm -o Assets/Hello.cs
```

## How to run in Unity

1. Copy all files under `Assets` in this directory to any location in the `Assets` folder in the Unity project of VRChat world.
1. Add `ExecuteHello` Prefab in the scene.
1. Play the scene and you will see the output <code>Hello, *PlayerName*</code> in the Console window.

## 日本語

wasm2usharpの使用例です。

## 各ファイルの説明

* `hello.wat`: テキスト形式のWasmプログラム
* `hello.wasm`: `hello.wat`からバイナリ形式に変換されたWasmプログラム
* `Assets/class_hello_env.cs`: Wasmプログラムからインポートされる関数を実装したクラス
* `Assets/ExecuteHello.cs`: Wasmプログラムを実行するための処理を記載したクラス
* `Assets/ExecuteHello.prefab`: 各クラスの依存関係のフィールドを設定したPrefab
* `Assets/Hello.cs`: `hello.wasm`からwasm2usharpで変換されたクラス

## 動作確認済み環境

* wasm2usharp 0.1.0
* wasm-tools 1.209.1
* Unity 2022.3.22f1
* VRChat SDK 3.6.1

## Wasmプログラムの変換方法

`.wat`を`.wasm`に変換するために[wasm-tools][wasm-tools]が必要です。

```bash
wasm-tools parse hello.wat -o hello.wasm
wasm2usharp hello.wasm -o Assets/Hello.cs
```

## Unityでの実行方法

1. このディレクトリの`Assets`以下の全てのファイルを、VRChatワールドのUnityプロジェクト内の`Assets`フォルダーの中の好きな場所へコピーします。
1. `ExecuteHello` Prefabをシーン内に追加します。
1. シーンを再生すると、Consoleウィンドウ内に`Hello, プレイヤー名`と出力されます。

[wasm-tools]: https://github.com/bytecodealliance/wasm-tools "bytecodealliance/wasm-tools: CLI and Rust libraries for low-level manipulation of WebAssembly modules"
