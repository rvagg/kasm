//! Test coverage analyzer for WebAssembly runtime
//!
//! This tool analyzes which spec tests would be enabled by implementing
//! specific instructions, helping prioritize development efforts.

use base64::{engine::general_purpose, Engine as _};
use kasm::parser::{self, module::Module};
use serde::Deserialize;
use std::collections::{HashMap, HashSet};
use std::fs;
use std::path::Path;

#[derive(Deserialize)]
struct TestData {
    bin: HashMap<String, String>, // Base64 encoded binaries
    spec: TestSpec,
}

#[derive(Deserialize)]
struct TestSpec {
    commands: Vec<TestCommand>,
}

#[derive(Deserialize)]
#[serde(untagged)]
enum TestCommand {
    Module {
        r#type: String,
        filename: String,
        #[allow(dead_code)]
        line: i32,
    },
    AssertReturn {
        r#type: String,
        action: TestAction,
        #[allow(dead_code)]
        line: i32,
    },
    Other(#[allow(dead_code)] serde_json::Value),
}

#[derive(Deserialize)]
struct TestAction {
    field: String,
    #[serde(default)]
    #[allow(dead_code)]
    module: String,
}

/// Analyzes a single test file
fn analyze_test_file(
    path: &Path,
    implemented: &HashSet<String>,
) -> Result<TestFileAnalysis, Box<dyn std::error::Error>> {
    let content = fs::read_to_string(path)?;
    let test_data: TestData = serde_json::from_str(&content)?;

    let mut analysis = TestFileAnalysis {
        filename: path.file_name().unwrap().to_string_lossy().to_string(),
        total_tests: 0,
        runnable_tests: 0,
        blocked_tests: HashMap::new(),
        required_instructions: HashSet::new(),
    };

    // Parse all modules
    let mut parsed_modules = HashMap::new();
    for cmd in &test_data.spec.commands {
        match cmd {
            TestCommand::Module { r#type, filename, .. } if r#type == "module" => {
                if let Some(encoded) = test_data.bin.get(filename) {
                    if let Ok(wasm_data) = general_purpose::STANDARD.decode(encoded) {
                        let mut reader = parser::reader::Reader::new(wasm_data);
                        if let Ok(module) = parser::parse(&HashMap::new(), filename, &mut reader) {
                            parsed_modules.insert(filename.clone(), module);
                        }
                    }
                }
            }
            _ => {}
        }
    }

    // Analyze assert_return commands
    for cmd in &test_data.spec.commands {
        if let TestCommand::AssertReturn { r#type, action, .. } = cmd {
            if r#type != "assert_return" {
                continue;
            }
            analysis.total_tests += 1;

            // Find the module and function
            let module = parsed_modules
                .values()
                .find(|m| m.exports.exports.iter().any(|e| e.name == action.field));

            if let Some(module) = module {
                let instructions = get_function_instructions(module, &action.field);
                let unimplemented = find_unimplemented_instructions(&instructions, implemented);

                if unimplemented.is_empty() {
                    analysis.runnable_tests += 1;
                } else {
                    for inst in &unimplemented {
                        *analysis.blocked_tests.entry(inst.clone()).or_insert(0) += 1;
                        analysis.required_instructions.insert(inst.clone());
                    }
                }
            }
        }
    }

    Ok(analysis)
}

/// Gets all instructions used in a function
fn get_function_instructions(module: &Module, func_name: &str) -> Vec<String> {
    let mut instructions = Vec::new();

    // Find the function index
    for export in &module.exports.exports {
        if export.name == func_name {
            if let parser::module::ExportIndex::Function(idx) = export.index {
                if let Some(body) = module.code.code.get(idx as usize) {
                    for inst in &body.instructions {
                        instructions.push(inst.kind.mnemonic().to_string());
                    }
                }
            }
        }
    }

    instructions
}

/// Finds unimplemented instructions
fn find_unimplemented_instructions(instructions: &[String], implemented: &HashSet<String>) -> Vec<String> {
    instructions
        .iter()
        .filter(|inst| !implemented.contains(*inst))
        .cloned()
        .collect::<HashSet<_>>()
        .into_iter()
        .collect()
}

#[derive(Debug)]
struct TestFileAnalysis {
    filename: String,
    total_tests: usize,
    runnable_tests: usize,
    blocked_tests: HashMap<String, usize>, // instruction -> count of blocked tests
    required_instructions: HashSet<String>,
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    // Currently implemented instructions
    let mut implemented = HashSet::new();
    implemented.insert("i32.const".to_string());
    implemented.insert("i64.const".to_string());
    implemented.insert("f32.const".to_string());
    implemented.insert("f64.const".to_string());
    implemented.insert("nop".to_string());
    implemented.insert("drop".to_string());
    implemented.insert("end".to_string());
    implemented.insert("local.get".to_string());
    implemented.insert("local.set".to_string());
    implemented.insert("local.tee".to_string());
    implemented.insert("block".to_string());
    implemented.insert("loop".to_string());
    implemented.insert("br".to_string());
    implemented.insert("br_if".to_string());
    implemented.insert("if".to_string());
    implemented.insert("else".to_string());
    implemented.insert("return".to_string());

    let args: Vec<String> = std::env::args().collect();

    if args.len() > 1 && args[1] == "--add" {
        // Add new instruction to see impact
        if args.len() < 3 {
            eprintln!("Usage: {} --add <instruction>", args[0]);
            std::process::exit(1);
        }
        implemented.insert(args[2].clone());
        println!("=== Analysis WITH '{}' implemented ===\n", args[2]);
    } else {
        println!("=== Current Test Coverage Analysis ===\n");
        println!("Implemented instructions: {implemented:?}\n");
    }

    let test_dir = Path::new("tests/spec");
    let mut total_runnable = 0;
    let mut total_tests = 0;
    let mut instruction_impact = HashMap::new();

    // Analyze each test file
    let mut results = Vec::new();
    for entry in fs::read_dir(test_dir)? {
        let entry = entry?;
        let path = entry.path();
        if path.extension().and_then(|s| s.to_str()) == Some("json") {
            if let Ok(analysis) = analyze_test_file(&path, &implemented) {
                total_runnable += analysis.runnable_tests;
                total_tests += analysis.total_tests;

                // Track impact of each missing instruction
                for (inst, count) in &analysis.blocked_tests {
                    *instruction_impact.entry(inst.clone()).or_insert(0) += count;
                }

                results.push(analysis);
            }
        }
    }

    // Sort results by number of runnable tests
    results.sort_by(|a, b| b.runnable_tests.cmp(&a.runnable_tests));

    // Display results
    println!("Test Files with Runnable Tests:");
    println!("{:-<60}", "");
    for result in &results {
        if result.runnable_tests > 0 {
            println!(
                "{:<30} {:>10}/{:<10} ({:.1}%)",
                result.filename,
                result.runnable_tests,
                result.total_tests,
                (result.runnable_tests as f64 / result.total_tests as f64) * 100.0
            );
        }
    }

    println!("\n{:-<60}", "");
    println!(
        "TOTAL: {}/{} tests runnable ({:.1}%)\n",
        total_runnable,
        total_tests,
        (total_runnable as f64 / total_tests as f64) * 100.0
    );

    // Show instruction impact
    let mut impact_vec: Vec<_> = instruction_impact.into_iter().collect();
    impact_vec.sort_by(|a, b| b.1.cmp(&a.1));

    println!("Instructions Blocking Most Tests:");
    println!("{:-<60}", "");
    println!("{:<30} {:>20}", "Instruction", "Blocked Tests");
    println!("{:-<60}", "");
    for (inst, count) in impact_vec.iter().take(20) {
        println!("{inst:<30} {count:>20}");

        // Show which test files need this instruction
        let mut files_needing: Vec<_> = results
            .iter()
            .filter(|r| r.required_instructions.contains(inst))
            .map(|r| (r.filename.clone(), r.blocked_tests.get(inst).unwrap_or(&0)))
            .filter(|(_, count)| **count > 0)
            .collect();
        files_needing.sort_by(|a, b| b.1.cmp(a.1));

        for (file, count) in files_needing.iter().take(3) {
            println!("  - {file:<25} ({count} tests)");
        }
        if files_needing.len() > 3 {
            println!("  - ... and {} more files", files_needing.len() - 3);
        }
    }

    // If adding an instruction, show the delta
    if args.len() > 1 && args[1] == "--add" {
        println!("\n{:-<60}", "");
        println!("Impact of adding '{}':", args[2]);

        // Re-analyze without the new instruction
        let mut base_implemented = implemented.clone();
        base_implemented.remove(&args[2]);

        let mut new_runnable = 0;
        for entry in fs::read_dir(test_dir)? {
            let entry = entry?;
            let path = entry.path();
            if path.extension().and_then(|s| s.to_str()) == Some("json") {
                if let Ok(base_analysis) = analyze_test_file(&path, &base_implemented) {
                    if let Ok(new_analysis) = analyze_test_file(&path, &implemented) {
                        if new_analysis.runnable_tests > base_analysis.runnable_tests {
                            let delta = new_analysis.runnable_tests - base_analysis.runnable_tests;
                            new_runnable += delta;
                            println!("  {:<30} +{} tests", new_analysis.filename, delta);
                        }
                    }
                }
            }
        }
        println!("\nTotal new tests enabled: +{new_runnable}");
    }

    Ok(())
}

// Add these dependencies to Cargo.toml:
// base64 = "0.21"
