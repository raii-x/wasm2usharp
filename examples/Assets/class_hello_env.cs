using System.Text;
using UdonSharp;
using UnityEngine;

public class class_hello_env : UdonSharpBehaviour
{
    public Hello wasm;

    /// <summary>
    /// Prints a sequence of bytes in Wasm memory as a UTF8 string.
    /// </summary>
    /// <param name="ptr">The index of the first byte to print.</param>
    /// <param name="len">The number of bytes to print.</param>
    public void debug_log(int ptr, int len)
    {
        string str = Encoding.UTF8.GetString(wasm.memory, ptr, len);
        Debug.Log(str);
    }
}
