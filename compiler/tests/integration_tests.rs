// tests/integration_tests.rs
use greenp::compile_file_to_executable; // Usar o nome da lib se for diferente do pacote
// use greenp::GreenPCompilationError;

use std::fs;
use std::process::Command;
use std::path::Path;

// Helper para compilar, executar e verificar a saída
fn run_integration_test(
    test_name: &str,
    source_code: &str,
    expected_stdout: &str,
    expect_compilation_success: bool,
) {
    let gp_file_name = format!("{}.gp", test_name);
    let exe_file_name_stem = test_name;
    let exe_file_name = if cfg!(windows) {
        format!("{}.exe", exe_file_name_stem)
    } else {
        exe_file_name_stem.to_string()
    };

    fs::write(&gp_file_name, source_code)
        .unwrap_or_else(|e| panic!("[{}] Failed to write .gp file: {}", test_name, e));

    let compilation_result = compile_file_to_executable(&gp_file_name, &exe_file_name, false);

    if expect_compilation_success {
        assert!(compilation_result.is_ok(),
                "[{}] Expected successful compilation, but got error: {:?}",
                test_name, compilation_result.err());

        assert!(Path::new(&exe_file_name).exists(),
                "[{}] Executable '{}' was not created.", test_name, exe_file_name);

        let output = Command::new(format!("./{}", exe_file_name))
            .output()
            .unwrap_or_else(|e| panic!("[{}] Failed to run compiled executable: {}", test_name, e));

        let stdout = String::from_utf8_lossy(&output.stdout);
        let stderr = String::from_utf8_lossy(&output.stderr);

        // Normaliza newlines para comparação
        let normalized_stdout = stdout.trim().replace("\r\n", "\n");
        let normalized_expected_stdout = expected_stdout.trim().replace("\r\n", "\n");

        assert_eq!(normalized_stdout, normalized_expected_stdout,
                   "[{}] Stdout mismatch.\nExpected:\n{}\nGot:\n{}\nStderr:\n{}",
                   test_name, expected_stdout, stdout, stderr);
        
        assert!(output.status.success(), "[{}] Program execution failed. Stderr: {}", test_name, stderr);

    } else {
        assert!(compilation_result.is_err(),
                "[{}] Expected compilation to fail, but it succeeded.", test_name);
        // Opcional: verificar o tipo específico de GreenPCompilationError
    }

    // Clean up
    fs::remove_file(gp_file_name).ok();
    fs::remove_file(exe_file_name).ok();
}

#[test]
fn integration_hello_world() {
    run_integration_test(
        "hello_world_integration",
        r#"
        function main(): void {
            printString("Hello from GreenP Integration Test!");
        }
        "#,
        "Hello from GreenP Integration Test!",
        true,
    );
}

#[test]
fn integration_print_numbers_and_variables() {
    run_integration_test(
        "numbers_vars_integration",
        r#"
        function main(): void {
            printNumber(1);
            const x: number = 10;
            let y: number = x + 5; // y = 15
            printNumber(y);      // Saída: 15
            y = y - x;           // y = 15 - 10 = 5
            printNumber(y);      // Saída: 5
        }
        "#,
        "1\n15\n5", // printf "%d\n" adiciona newlines
        true,
    );
}

#[test]
fn integration_empty_statements_parsed() {
    run_integration_test(
        "empty_statements_integration",
        r#"
        function main(): void {
            printString("Before");
            ;
            ; // Múltiplos statements vazios
            printString("After");
        }
        "#,
        "Before\nAfter",
        true,
    );
}

#[test]
fn integration_compilation_error_parse() {
    // Este teste espera que a compilação FALHE devido a um erro de parse
    run_integration_test(
        "parse_error_integration",
        r#"
        function main(): void {
            let x: number = 10 + ; // Erro de sintaxe
        }
        "#,
        "", // Não esperamos stdout porque não deve executar
        false, // Esperamos que a compilação falhe
    );
}

// Adicionar mais testes de integração cobrindo diferentes aspectos do MVP