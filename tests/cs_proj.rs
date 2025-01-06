use std::{
    collections::HashMap,
    fs::File,
    io::{Read, Write},
    process::Command,
};

use tempfile::{tempdir, TempDir};
use wasm2usharp::{convert, ir::INIT, parse::convert_to_ident};
use wast::token::Id;

pub struct CsProj<'input> {
    dir: TempDir,
    /// このVecのインデックスがモジュールインデックスとなる
    modules: Vec<CsModule>,
    /// モジュールIDに対して設定されたモジュールインデックス
    id_to_module: HashMap<Id<'input>, usize>,
    /// Registerされたモジュールの一覧
    /// 値はモジュールインデックス
    reg_modules: HashMap<String, usize>,
}

const MODULE: &str = "Module";

impl<'input> CsProj<'input> {
    pub fn new() -> Self {
        let dir = tempdir().unwrap();

        // .NETプロジェクトを作成
        Command::new("dotnet")
            .args(["new", "console"])
            .current_dir(dir.path())
            .status()
            .unwrap();

        Self {
            dir,
            modules: Vec::new(),
            id_to_module: HashMap::new(),
            reg_modules: HashMap::new(),
        }
    }

    pub fn add_module(&mut self, id: Option<Id<'input>>, data: &[u8]) {
        let index = self.modules.len();
        let class_name = format!("{MODULE}{index}");

        // クラス名.csのファイルに書き込み
        let cs_path = self.dir.path().join(format!("{class_name}.cs"));
        let mut cs_file = File::create(cs_path).unwrap();
        let import_map = |module: &_| format!("Module{}", self.reg_modules.get(module).unwrap());
        let imports = convert(data, class_name, None, true, &mut cs_file, &import_map).unwrap();

        // インポート宣言を変換
        let imports = imports
            .into_iter()
            .map(|import| CsImport {
                module: *self.reg_modules.get(import).unwrap(),
                name: convert_to_ident(import),
            })
            .collect();

        self.modules.push(CsModule { index, imports });

        if let Some(id) = id {
            assert!(self.id_to_module.insert(id, index).is_none());
        }
    }

    pub fn register(&mut self, name: String, module: &Option<Id<'_>>) {
        let module = match module {
            Some(x) => *self.id_to_module.get(x).unwrap(),
            None => self.modules.len() - 1,
        };

        self.reg_modules.insert(name, module);
    }

    pub fn run(&self, input: CsProjInput) -> String {
        self.create_main_cs();

        // .NETプロジェクトをビルド
        let status = Command::new("dotnet")
            .args(["build", "--no-restore", "--nologo", "-v", "q"])
            .current_dir(self.dir.path())
            .status()
            .unwrap();
        assert!(status.success());

        // 入力ファイルを作成
        let input_path = self.dir.path().join("input.txt");
        File::create(&input_path)
            .unwrap()
            .write_all(input.input.as_bytes())
            .unwrap();

        // .NETプロジェクトを実行
        let status = Command::new("dotnet")
            .args(["run", "--no-build"])
            .current_dir(self.dir.path())
            .status()
            .unwrap();
        assert!(status.success());

        // 出力ファイルを読み込み
        let mut output = String::new();
        File::open(self.dir.path().join("output.txt"))
            .unwrap()
            .read_to_string(&mut output)
            .unwrap();
        output
    }

    fn create_main_cs(&self) {
        // Program.csファイルに書き込み
        let cs_path = self.dir.path().join("Program.cs");
        let mut cs_file = File::create(cs_path).unwrap();

        writeln!(cs_file, "class Program {{").unwrap();
        writeln!(cs_file, "static void Main(string[] programArgs) {{").unwrap();
        writeln!(
            cs_file,
            "var instances = new object[{}];",
            self.modules.len()
        )
        .unwrap();

        for module in &self.modules {
            let i = module.index;
            writeln!(cs_file, "var instance{i} = new {MODULE}{i}();").unwrap();

            for CsImport {
                module: im_module,
                name,
            } in &module.imports
            {
                writeln!(cs_file, "instance{i}.{name} = instance{im_module};").unwrap();
            }

            writeln!(cs_file, "instances[{i}] = instance{i};").unwrap();
        }

        writeln!(cs_file, r#"
using var inputFile = new StreamReader("input.txt");
using var outputFile = new StreamWriter("output.txt");

string? line;
while ((line = inputFile.ReadLine()) != null) {{
    var args = line.Split(' ');

    string command = args[0];
    int iModule = int.Parse(args[1]);
    var t = instances[iModule].GetType();

    switch (command) {{
        case "invoke":
            string method = args[2];
            var mi = t.GetMethod(method) ?? throw new ArgumentException($"Method '{{method}}' is not found");

            object[]? parameters = null;
            if (args.Length > 3) {{
                parameters = new object[(args.Length - 3) / 2];
                for (int i = 0; i < parameters.Length; i++) {{
                    int iArgs = 3 + i * 2;
                    string ty = args[iArgs];
                    string val = args[iArgs + 1];
                    parameters[i] = ty switch {{
                        "i32" => (object)int.Parse(val),
                        "i64" => (object)long.Parse(val),
                        "f32" => (object)BitConverter.UInt32BitsToSingle(uint.Parse(val)),
                        "f64" => (object)BitConverter.UInt64BitsToDouble(ulong.Parse(val)),
                        _ => throw new ArgumentException($"Unsupported parameter type: '{{ty}}'"),
                    }};
                }}
            }}

            WriteResult(mi.Invoke(instances[iModule], parameters), outputFile);

            break;

        case "get":
            string name = args[2];
            var fi = t.GetField(name) ?? throw new ArgumentException($"Global variable '{{name}}' is not found");
            WriteResult(fi.GetValue(instances[iModule])!, outputFile);

            break;

        default:
            throw new ArgumentException($"Unsupported command: '{{command}}'");
    }}
}}"#,
        ).unwrap();

        writeln!(cs_file, "}}").unwrap();

        writeln!(cs_file, r#"
static void WriteResult(object? result, StreamWriter outputFile)
{{
    string resultStr = result switch
    {{
        int x => $"i32 {{x}}",
        long x => $"i64 {{x}}",
        float x => $"f32 {{BitConverter.SingleToUInt32Bits(x)}}",
        double x => $"f64 {{BitConverter.DoubleToUInt64Bits(x)}}",
        null => "",
        _ => throw new InvalidOperationException($"Unsupported result type: '{{result.GetType()}}'"),
    }};
    outputFile.WriteLine(resultStr);
}}"#).unwrap();

        writeln!(cs_file, "}}").unwrap();
    }
}

impl Default for CsProj<'_> {
    fn default() -> Self {
        Self::new()
    }
}

struct CsModule {
    index: usize,
    imports: Vec<CsImport>,
}

struct CsImport {
    module: usize,
    name: String,
}

pub struct CsProjInput {
    last_module: Option<usize>,
    input: String,
}

impl CsProjInput {
    pub fn new() -> Self {
        Self {
            last_module: None,
            input: String::new(),
        }
    }

    /// wastのdirectiveのイテレートでwat読み込みの際にこの関数を呼ぶことで、
    /// IDとモジュールの対応関係がCsProjと同じようになる
    pub fn next_module(&mut self, cs_proj: &CsProj<'_>, module: Option<Id<'_>>) {
        self.last_module = Some(match self.last_module {
            Some(x) => x + 1,
            None => 0,
        });

        self.invoke(cs_proj, module, INIT);
    }

    fn get_module<'input, 'a>(
        &self,
        cs_proj: &'a CsProj<'input>,
        module: Option<Id<'input>>,
    ) -> &'a CsModule {
        &cs_proj.modules[match module {
            Some(id) => *cs_proj.id_to_module.get(&id).unwrap(),
            None => self.last_module.unwrap(),
        }]
    }

    /// 指定したモジュールIDのインスタンスに対して関数を実行する
    ///
    /// この際に渡すデータはスペース区切りで、
    /// 1番目に "invoke" (この関数で追加される)、
    /// 2番目にモジュール番号 (この関数で追加される)、
    /// 3番目に関数名、
    /// 関数に渡す引数があれば、3番目以降に引数の型と10進整数でビット列を表現した引数の値を順番に渡す
    pub fn invoke<'input>(
        &mut self,
        cs_proj: &CsProj<'input>,
        module: Option<Id<'input>>,
        args: &str,
    ) {
        let module = self.get_module(cs_proj, module);

        let args = format!("invoke {} {args}\n", module.index);
        self.input += &args;
    }

    /// 指定したモジュールIDのインスタンスのグローバル変数を取得する
    ///
    /// この際に渡すデータはスペース区切りで、
    /// 1番目に "get" (この関数で追加される)、
    /// 2番目にモジュール番号 (この関数で追加される)、
    /// 3番目にグローバル変数名を順番に渡す
    pub fn get_global<'input>(
        &mut self,
        cs_proj: &CsProj<'input>,
        module: Option<Id<'input>>,
        global: &str,
    ) {
        let module = self.get_module(cs_proj, module);

        let args = format!("get {} {global}\n", module.index);
        self.input += &args
    }
}

impl Default for CsProjInput {
    fn default() -> Self {
        Self::new()
    }
}
