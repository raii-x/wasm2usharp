use std::{
    collections::{hash_map::Entry, HashMap},
    fs::File,
    io::{BufRead, BufReader, Write},
    process::{Child, Command, Stdio},
};

use tempfile::{tempdir, TempDir};
use wasm2usharp::converter::{Converter, INIT};
use wast::token::Id;

pub struct CsProj<'input> {
    dir: TempDir,
    /// このVecのインデックスがモジュールインデックスとなる
    modules: Vec<CsModule>,
    /// モジュールIDに対して、同じIDに設定されたモジュールのインデックスが
    /// wastでの定義順にVecに格納される
    id_to_indices: HashMap<CsId<'input>, Vec<usize>>,
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
            .args(["new", "console", "-o", dir.path().to_str().unwrap()])
            .status()
            .unwrap();

        Self {
            dir,
            modules: Vec::new(),
            id_to_indices: HashMap::new(),
            reg_modules: HashMap::new(),
        }
    }

    pub fn add_module(&mut self, id: CsId<'input>, data: &[u8]) {
        let index = self.modules.len();
        let class_name = format!("{MODULE}{index}");
        let mut conv = Converter::new(data, &class_name, true);

        // クラス名.csのファイルに書き込み
        let cs_path = self.dir.path().join(format!("{class_name}.cs"));
        let mut cs_file = File::create(cs_path).unwrap();
        let imports = conv.convert(&mut cs_file).unwrap();

        // インポート宣言を変換
        let imports = imports
            .into_iter()
            .filter_map(|import| {
                self.reg_modules.get(import.module).map(|module| CsImport {
                    module: *module,
                    name: import.name,
                    full_name: import.full_name,
                })
            })
            .collect();

        self.modules.push(CsModule { index, imports });

        let indices = match self.id_to_indices.entry(id) {
            Entry::Occupied(o) => o.into_mut(),
            Entry::Vacant(v) => v.insert(Vec::new()),
        };
        indices.push(index);
    }

    pub fn register(&mut self, name: String, module: &CsId<'_>) {
        self.reg_modules.insert(
            name,
            *self.id_to_indices.get(module).unwrap().last().unwrap(),
        );
    }

    pub fn run(&self) -> CsProjExec<'input, '_> {
        self.create_main_cs();

        // .NETプロジェクトをビルド
        let status = Command::new("dotnet")
            .args([
                "build",
                self.dir.path().to_str().unwrap(),
                "--no-restore",
                "--nologo",
                "-v",
                "q",
            ])
            .status()
            .unwrap();
        assert!(status.success());

        // .NETプロジェクトを実行
        let child = Command::new("dotnet")
            .args([
                "run",
                "--project",
                self.dir.path().to_str().unwrap(),
                "--no-build",
            ])
            .stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .spawn()
            .unwrap();

        CsProjExec::new(self, child)
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
                full_name,
            } in &module.imports
            {
                writeln!(
                    cs_file,
                    "instance{i}.{full_name} = instance{im_module}.{name};"
                )
                .unwrap();
            }

            writeln!(cs_file, "instances[{i}] = instance{i};").unwrap();
        }

        writeln!(cs_file, r#"
string? line;
while ((line = Console.ReadLine()) != null) {{
    var args = line.Split(' ');

    int iModule = int.Parse(args[0]);
    var t = instances[iModule].GetType();
    var mi = t.GetMethod(args[1]);
    if (mi == null) {{
        throw new ArgumentException($"Method '{{args[1]}}' is not found");
    }}

    object[]? parameters = null;
    if (args.Length > 2) {{
        parameters = new object[(args.Length - 2) / 2];
        for (int i = 0; i < parameters.Length; i++) {{
            int iArgs = 2 + i * 2;
            string ty = args[iArgs];
            string val = args[iArgs + 1];
            parameters[i] = ty switch {{
                "i32" => (object)uint.Parse(val),
                "i64" => (object)ulong.Parse(val),
                "f32" => (object)BitConverter.UInt32BitsToSingle(uint.Parse(val)),
                "f64" => (object)BitConverter.UInt64BitsToDouble(ulong.Parse(val)),
                _ => throw new ArgumentException($"Unsupported parameter type: '{{ty}}'"),
            }};
        }}
    }}

    var result = mi.Invoke(instances[iModule], parameters);
    string resultStr = result switch {{
        uint x => $"i32 {{x}}",
        ulong x => $"i64 {{x}}",
        float x => $"f32 {{BitConverter.SingleToUInt32Bits(x)}}",
        double x => $"f64 {{BitConverter.DoubleToUInt64Bits(x)}}",
        null => "",
        _ => throw new InvalidOperationException($"Unsupported result type: '{{result.GetType()}}'"),
    }};
    Console.WriteLine(resultStr);
}}"#,
        ).unwrap();

        writeln!(cs_file, "}}").unwrap();
        writeln!(cs_file, "}}").unwrap();
    }
}

impl<'a> Default for CsProj<'a> {
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
    full_name: String,
}

pub struct CsProjExec<'input, 'a> {
    cs_proj: &'a CsProj<'input>,
    child: Child,
    /// 現在のIDとモジュールの対応関係。
    /// wastのdirectiveのイテレート中に変更される。
    /// 格納されている値はid_to_indicesの値のVecのインデックス（モジュールインデックスではない）
    id_to_index: HashMap<CsId<'input>, usize>,
}

impl<'input, 'a> CsProjExec<'input, 'a> {
    fn new(cs_proj: &'a CsProj<'input>, child: Child) -> Self {
        Self {
            cs_proj,
            child,
            id_to_index: HashMap::new(),
        }
    }

    /// wastのdirectiveの2回目のイテレートでwat読み込みの際にこの関数を呼ぶことで、
    /// IDとモジュールの対応関係が1回目のイテレートの時と同じようになる
    pub fn next_module(&mut self, module: CsId<'input>) {
        self.id_to_index
            .entry(module)
            .and_modify(|x| *x += 1)
            .or_insert(0);

        self.invoke(module, INIT);
    }

    fn get_module(&self, module: CsId<'input>) -> &CsModule {
        let indices = self.cs_proj.id_to_indices.get(&module).unwrap();
        let indices_index = *self.id_to_index.get(&module).unwrap();
        let module_index = indices[indices_index];
        &self.cs_proj.modules[module_index]
    }

    /// 指定したモジュールIDのインスタンスに対して関数を実行する
    ///
    /// この際に渡すデータはスペース区切りで、
    /// 1番目にモジュール番号 (この関数で追加される)、
    /// 2番目に関数名、
    /// 関数に渡す引数があれば、3番目以降に引数の型と10進整数でビット列を表現した引数の値を順番に渡す
    pub fn invoke(&mut self, module: CsId<'input>, args: &str) -> String {
        let module = self.get_module(module);

        let args = format!("{} {args}\n", module.index);
        println!("{:?}", args);
        let stdin: &mut std::process::ChildStdin = self.child.stdin.as_mut().unwrap();
        stdin.write_all(args.as_bytes()).unwrap();

        let stdout = self.child.stdout.as_mut().unwrap();
        let mut stdout = BufReader::new(stdout);
        let mut line = String::new();
        stdout.read_line(&mut line).unwrap();

        line
    }
}

pub type CsId<'a> = Option<Id<'a>>;
