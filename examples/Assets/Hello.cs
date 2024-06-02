// Converted from WebAssembly with wasm2usharp 0.1.0
using System;
using UdonSharp;
using UnityEngine;
#pragma warning disable
public class Hello : UdonSharpBehaviour {
public void hello() {
int w2us_v0;
w2us_v0 = str_end;
hello_env.debug_log(0, w2us_v0);
}
public void w2us_init() {
w2us_stack_top = 0;
str_end = 7;
memory = new byte[65536];
w2us_data0.CopyTo(memory, 0);
}
byte[] w2us_data0 = new byte[] { 72,101,108,108,111,44,32, };
public class_hello_env hello_env;
[NonSerialized] public int str_end;
[NonSerialized] public byte[] memory;
object[] w2us_stack = new object[65536];
int w2us_stack_top = 0;
UdonSharpBehaviour w2us_null = null;
}
