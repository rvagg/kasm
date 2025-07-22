fn main() {
    // Tell Cargo to rerun this build script if any test fixture changes
    println!("cargo:rerun-if-changed=tests/spec");
    
    // Also rerun if any JSON file in the spec directory changes
    let spec_dir = std::path::Path::new("tests/spec");
    if spec_dir.exists() {
        for entry in std::fs::read_dir(spec_dir).unwrap() {
            let entry = entry.unwrap();
            let path = entry.path();
            if path.extension().and_then(|s| s.to_str()) == Some("json") {
                println!("cargo:rerun-if-changed={}", path.display());
            }
        }
    }
}