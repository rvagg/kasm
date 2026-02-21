// Compile a .wast spec test to a JSON fixture containing binary modules
// and wasm-objdump reference output for dump comparison testing.
//
// Output format: { "bin": { "name.wasm": "<base64>", ... }, "dump": { ... } }
//
// Requires wabt tools (wast2json, wasm-objdump) on $PATH or at the paths below.
//
// Usage: node compile_test.mjs <input.wast> <output.json>
//        node compile_test.mjs --batch <dir-of-wast-files> <output-dir>

import { readFile, writeFile, mkdtemp, readdir, rm, stat, mkdir } from 'fs/promises'
import { execSync } from 'child_process'
import { join, basename } from 'node:path'
import { tmpdir } from 'node:os'

const wast2json = process.env.WAST2JSON || 'wast2json'
const wasmObjdump = process.env.WASM_OBJDUMP || 'wasm-objdump'

if (process.argv.length < 4) {
  console.error('usage: compile_test.mjs <input.wast> <output.json>')
  console.error('       compile_test.mjs --batch <wast-dir> <output-dir>')
  process.exit(1)
}

if (process.argv[2] === '--batch') {
  const wastDir = process.argv[3]
  const outDir = process.argv[4] || 'tests/spec'
  await mkdir(outDir, { recursive: true })
  const files = (await readdir(wastDir)).filter(f => f.endsWith('.wast')).sort()
  console.log(`Compiling ${files.length} .wast files from ${wastDir} to ${outDir}`)
  for (const file of files) {
    const input = join(wastDir, file)
    const output = join(outDir, file.replace('.wast', '.json'))
    try {
      const json = await compileWast(input)
      await writeFile(output, json)
      console.log(`  ${file} -> ${basename(output)}`)
    } catch (e) {
      console.error(`  FAILED: ${file}: ${e.message}`)
    }
  }
} else {
  const inputPath = process.argv[2]
  const outputPath = process.argv[3]
  if (await stat(inputPath).catch(() => null) === null) {
    console.error(`file not found: ${inputPath}`)
    process.exit(1)
  }
  const json = await compileWast(inputPath)
  await writeFile(outputPath, json)
  console.log(`${inputPath} -> ${outputPath}`)
}

async function compileWast (wastPath) {
  const dir = await mkdtemp(join(tmpdir(), 'wast-'))
  try {
    execSync(`${wast2json} ${wastPath}`, { cwd: dir, stdio: 'pipe' })

    let specJson = ''
    const bins = []
    for (const file of await readdir(dir)) {
      const content = await readFile(join(dir, file))
      if (file.endsWith('.json')) {
        specJson = content.toString('utf8')
      } else if (file.endsWith('.wasm')) {
        bins.push({ file, bin: content.toString('base64') })
      }
    }
    if (specJson === '') {
      throw new Error('wast2json did not produce a spec JSON file')
    }

    const specParsed = JSON.parse(specJson)

    // Sanity check: count binary modules vs spec commands
    const expectedBinaries = specParsed.commands.filter(cmd =>
      (cmd.type === 'module' ||
      cmd.type === 'assert_invalid' ||
      cmd.type === 'assert_malformed' ||
      cmd.type === 'assert_unlinkable' ||
      cmd.type === 'assert_uninstantiable') &&
      cmd.module_type !== 'text'
    ).length

    if (bins.length !== expectedBinaries) {
      console.warn(`  WARNING: binary count mismatch in ${wastPath}: found ${bins.length}, expected ${expectedBinaries}`)
    }

    // Build output: bin + dump (no spec or code sections)
    const output = { bin: {} }

    for (const { file, bin } of bins) {
      output.bin[file] = bin
    }

    // Add dump section for regular module commands (wasm-objdump output)
    const moduleCommands = specParsed.commands.filter(({ type }) => type === 'module')
    if (moduleCommands.length > 0) {
      output.dump = {}
      for (const cmd of moduleCommands) {
        const wasmPath = join(dir, cmd.filename)
        output.dump[cmd.filename] = {
          header: execSync(`${wasmObjdump} -h ${wasmPath}`).toString('utf8'),
          details: execSync(`${wasmObjdump} -x ${wasmPath}`).toString('utf8'),
          disassemble: execSync(`${wasmObjdump} -d ${wasmPath}`).toString('utf8')
        }
      }
    }

    return JSON.stringify(output)
  } finally {
    await rm(dir, { recursive: true })
  }
}
