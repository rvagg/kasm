use clap::{Parser, Subcommand};
use kasm::parser;
use kasm::runtime::store::Store;
use kasm::runtime::wasi::{WasiContext, create_wasi_instance};
use std::collections::HashMap;
use std::fs;
use std::io::{stderr, stdin, stdout};
use std::process::ExitCode;
use std::sync::Arc;

#[derive(Parser)]
#[command(name = "kasm")]
#[command(about = "WebAssembly runtime and toolkit")]
struct Cli {
    #[command(subcommand)]
    command: Commands,
}

#[derive(Subcommand)]
enum Commands {
    /// Execute a WASI module
    Run {
        /// Path to the WebAssembly module
        file: String,

        /// Preopen a directory for filesystem access
        #[arg(long = "dir", value_name = "PATH")]
        dirs: Vec<String>,

        /// Arguments to pass to the module (after --)
        #[arg(last = true)]
        args: Vec<String>,
    },

    /// Dump module information (defaults to detailed view)
    Dump {
        /// Path to the WebAssembly module
        file: String,

        /// Show only module header (magic, version)
        #[arg(long)]
        header: bool,

        /// Show disassembled code
        #[arg(long, short = 'd')]
        disassemble: bool,
    },
}

fn main() -> ExitCode {
    let cli = Cli::parse();

    match cli.command {
        Commands::Run { file, dirs, args } => run_wasi_module(&file, dirs, args),
        Commands::Dump {
            file,
            header,
            disassemble,
        } => dump_module(&file, header, disassemble),
    }
}

fn dump_module(file: &str, header: bool, disassemble: bool) -> ExitCode {
    let bytes = match fs::read(file) {
        Ok(b) => b,
        Err(e) => {
            eprintln!("Error reading {}: {}", file, e);
            return ExitCode::FAILURE;
        }
    };

    let module_registry = HashMap::new();
    let module = match parser::parse(&module_registry, file, &mut parser::reader::Reader::new(bytes)) {
        Ok(m) => m,
        Err(e) => {
            eprintln!("Error parsing {}: {}", file, e);
            return ExitCode::FAILURE;
        }
    };

    let format = if header {
        parser::module::ParsedUnitFormat::Header
    } else if disassemble {
        parser::module::ParsedUnitFormat::Disassemble
    } else {
        parser::module::ParsedUnitFormat::Details
    };

    println!("{}", module.to_string(format));
    ExitCode::SUCCESS
}

fn run_wasi_module(file: &str, dirs: Vec<String>, module_args: Vec<String>) -> ExitCode {
    // Read the module file
    let bytes = match fs::read(file) {
        Ok(b) => b,
        Err(e) => {
            eprintln!("Error reading {}: {}", file, e);
            return ExitCode::FAILURE;
        }
    };

    // Parse the module
    let module_registry = HashMap::new();
    let module = match parser::parse(&module_registry, file, &mut parser::reader::Reader::new(bytes)) {
        Ok(m) => m,
        Err(e) => {
            eprintln!("Error parsing {}: {}", file, e);
            return ExitCode::FAILURE;
        }
    };

    // Build args array: program name + provided args
    let mut args = vec![file.to_string()];
    args.extend(module_args);

    // Create WASI context with real stdio and preopened directories
    // Note: WasiContext uses RefCell internally which isn't Sync, but we don't
    // share it across threads, so Arc is fine here.
    let mut builder = WasiContext::builder()
        .args(args)
        .stdin(Box::new(stdin()))
        .stdout(Box::new(stdout()))
        .stderr(Box::new(stderr()));

    for dir in &dirs {
        let path = std::path::Path::new(dir);
        if !path.is_dir() {
            eprintln!("Error: --dir path is not a directory: {}", dir);
            return ExitCode::FAILURE;
        }
        builder = builder.preopen_dir(dir, dir);
    }

    #[allow(clippy::arc_with_non_send_sync)]
    let ctx = Arc::new(builder.build());

    // Create store and WASI instance (with AssemblyScript support)
    let mut store = Store::new();
    let instance_id = match create_wasi_instance(&mut store, &module, ctx.clone(), true) {
        Ok(id) => id,
        Err(e) => {
            eprintln!("Error instantiating module: {}", e);
            return ExitCode::FAILURE;
        }
    };

    // WASI modules must export _start as the entry point
    let has_start = store
        .get_instance(instance_id)
        .map(|i| i.get_function_addr("_start").is_ok())
        .unwrap_or(false);

    if !has_start {
        eprintln!("Error: module does not export _start (not a WASI command module)");
        return ExitCode::FAILURE;
    }

    // Execute _start
    let result = store.invoke_export(instance_id, "_start", vec![], None);

    // Handle result
    match result {
        Ok(_) => {
            // Check if proc_exit was called
            if let Some(code) = ctx.exit_code() {
                ExitCode::from(code as u8)
            } else {
                ExitCode::SUCCESS
            }
        }
        Err(e) => {
            // Check if this was a proc_exit trap
            if let Some(code) = ctx.exit_code() {
                ExitCode::from(code as u8)
            } else {
                eprintln!("Error: {}", e);
                ExitCode::FAILURE
            }
        }
    }
}
