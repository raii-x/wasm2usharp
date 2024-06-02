using System.Text;
using UdonSharp;
using VRC.SDKBase;

public class ExecuteHello : UdonSharpBehaviour
{
    public Hello wasm;

    void Start()
    {
        wasm.w2us_init();

        // Add the player name after "Hello, " in the Wasm memory
        byte[] playerName = Encoding.UTF8.GetBytes(Networking.LocalPlayer.displayName);
        playerName.CopyTo(wasm.memory, wasm.str_end);
        wasm.str_end += playerName.Length;

        wasm.hello();
    }
}
