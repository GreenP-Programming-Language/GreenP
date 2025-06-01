// Em tests/integration_tests.rs ou similar
use greenp_compiler::compile_file_to_executable; // Importa a função principal da lib
use std::fs;
use std::process::Command;
use std::path::Path;

fn run_full_compilation_test(test_name: &str, source_code: &str, expected_stdout: &str) {
    let gp_file_name = format!("{}.gp", test_name);
    let exe_file_name = if cfg!(windows) { format!("{}.exe", test_name) } else { test_name.to_string() };

    fs::write(&gp_file_name, source_code)
        .unwrap_or_else(|e| panic!("Test [{}]: Failed to write .gp file: {}", test_name, e));

    let compilation_result = compile_file_to_executable(&gp_file_name, &exe_file_name, false);
    assert!(compilation_result.is_ok(), 
            "Test [{}]: Compilation failed: {:?}", 
            test_name, compilation_result.err());

    assert!(Path::new(&exe_file_name).exists(), 
            "Test [{}]: Executable was not created.", test_name);

    let output = Command::new(format!("./{}", exe_file_name)) // Adicionar ./ para caminho relativo
        .output()
        .unwrap_or_else(|e| panic!("Test [{}]: Failed to run compiled executable: {}", test_name, e));

    let stdout = String::from_utf8_lossy(&output.stdout);
    assert_eq!(stdout.trim().replace("\r\n", "\n"), expected_stdout.trim().replace("\r\n", "\n"),
               "Test [{}]: Stdout mismatch.", test_name);

    // Clean up
    fs::remove_file(gp_file_name).ok();
    fs::remove_file(exe_file_name).ok();
}

#[test]
fn test_integration_hello_string() {
    run_full_compilation_test(
        "hello_string",
        "function main(): void { printString(\"Hello GreenP!\"); }",
        "Hello GreenP!"
    );
}

#[test]
fn test_integration_print_numbers_and_vars() {
    run_full_compilation_test(
        "numbers_vars",
        r#"
        function main(): void {
            printNumber(123);
            const x: number = 7;
            let y: number = 3;
            printNumber(x + y); // 10
            printNumber(x - y); // 4
        }
        "#,
        "123\n10\n4" // printf "%d\n" adiciona newline
    );
}