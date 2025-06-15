import AnythingLangAssistant
import Lean

def getArgByName (args : List String) (name : String) : Option String :=
  match args with
  | [] => @Option.none String
  | x :: args' => match x.trim == name.trim with
    | true => args'[0]?
    | false => getArgByName args' name

def defaultValue (default : α) (x : Option α)  :α :=
  match x with
  | none => default
  | some n => n

def readFileContent (fp : String) : IO String := do
  IO.FS.readFile fp


/--
 验证程序的有效性。
-/
def Validate (content purpose result : String) (println : String -> IO Unit) : IO Unit := do
  let length := String.length content
  let purposeLength := String.length purpose

  -- 模拟计算等待
  println "Analyzing content..."
  IO.sleep (length / 4)
  println "Compare with purpose..."
  IO.sleep (length * purposeLength)
  println "Check Result..."
  IO.sleep 5000

  -- 尝试解析结果
  -- TODO：改成调用函数来判断。
  let output := match String.toLower result with
  | "true" => "✅ Your program fits your purpose."
  | "yes" => "✅ Your program fits your purpose."
  | "false" => "❌Your program not fits your purpose."
  | "no" => "✅ Your program fits your purpose."
  | _ => "❓ I don't know whether your program serves your purpose."
  println output


def main (args : List String) : IO Unit := do
  let argGetter := getArgByName args


  let _filename := argGetter "-f"
  let _purpose := argGetter "-p"
  let _result := argGetter "-r"

  let filename : String <-
    match _filename with
    | some x => pure x
    | none =>
      IO.println "-f <filename> required."
      return

  let purpose <-
    match _purpose with
    | some x => pure x
    | none =>
      IO.println "-p <purpose> required."
      return

  let result <-
    match _result with
    | some x => pure x
    | none =>
      IO.println "-r <result> required."
      return

  let content ← readFileContent filename

  Validate content purpose result (fun (x: String) => IO.println x)
