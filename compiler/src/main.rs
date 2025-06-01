
use greenp_compiler::compile_file_to_executable;
// use greenp_compiler::GreenPCompilationError; // Supondo que este é o erro de lib.rs
use std::error::Error; // <<< ADICIONE ESTA LINHA
use std::fs;
use std::process::Command;
use std::env;

fn main() {
    let args: Vec<String> = env::args().collect();
    let (source_file_path, output_executable_name) = match args.as_slice() {
        [_, input] => (input.as_str(), if cfg!(windows) { "a.exe" } else { "a.out" }),
        [_, input, flag, output] if flag == "-o" => (input.as_str(), output.as_str()),
        _ => {
            eprintln!("Uso:");
            eprintln!("  greenp_compiler <arquivo_entrada.gp>");
            eprintln!("  greenp_compiler <arquivo_entrada.gp> -o <arquivo_saida>");
            std::process::exit(1);
        }
    };

    if !std::path::Path::new(source_file_path).exists() && source_file_path == "hello.gp" {
        // ... (código para criar hello.gp se não existir) ...
        let greenp_ts_like_example = r#"
function main(): void {
  const message: string = "Hello from GreenP (via lib.rs)!";
  printString(message);
  
  let x: number = 200;
  const y: number = 50;
  let result: number = x + y; // 250
  printNumber(result); 
  
  result = x - y; // 150
  printNumber(result);
}
"#;
        fs::write(source_file_path, greenp_ts_like_example)
            .expect(&format!("Unable to write example file {}", source_file_path));
        println!("Arquivo de exemplo '{}' criado.", source_file_path);
    }

    match compile_file_to_executable(source_file_path, output_executable_name, true) {
        Ok(()) => {
            // ... (código para executar o programa compilado) ...
            println!("\nCompilation successful! Running ./{}\n-------------------------", output_executable_name);
            match Command::new(format!("./{}", output_executable_name)).output() {
                Ok(run_output) => {
                    if !run_output.stdout.is_empty() {
                        println!("Saída do Programa:\n{}", String::from_utf8_lossy(&run_output.stdout));
                    }
                    if !run_output.stderr.is_empty() {
                        eprintln!("Erros do Programa:\n{}", String::from_utf8_lossy(&run_output.stderr));
                    }
                    if !run_output.status.success() {
                        eprintln!("Execução do programa falhou com status: {}", run_output.status);
                    }
                }
                Err(e) => eprintln!("Falha ao executar o programa compilado '{}': {}", output_executable_name, e),
            }
            println!("-------------------------");
        }
        Err(e) => {
            eprintln!("\nFalha na compilação:\n{}", e);
            let mut cause = e.source(); // Agora e.source() deve funcionar
            while let Some(source) = cause {
                eprintln!("  Causado por: {}", source);
                cause = source.source();
            }
            std::process::exit(1);
        }
    }
}