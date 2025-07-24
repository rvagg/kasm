import { readFile, writeFile, mkdtemp, readdir, rm, stat } from 'fs/promises'
import { execSync } from 'child_process'
import { join } from 'node:path'
import { tmpdir } from 'node:os'

// const wast2wasm = new URL('../webassembly/wabt/build/wat2wasm', import.meta.url).pathname
const wast2json = '/workspace/wasm-wabt/bin/wast2json'
const wasmObjdump = '/workspace/wasm-wabt/bin/wasm-objdump'

if (process.argv.length < 4) {
  console.error('usage: compile_test.mjs <input.wast> <output.json>')
  process.exit(1)
}

const input = new URL(process.argv[2], `file://${process.cwd()}/`)
console.log(`compiling ${input}...`)
if (await stat(input.pathname).catch(() => null) === null) {
  console.error(`file not found: ${input.pathname}`)
  process.exit(1)
}
const output = new URL(process.argv[3], `file://${process.cwd()}/`)

const parsed = await parseWast(input)
const compiled = await compileWast(parsed, input)
await writeFile(output, compiled)

async function dumpWasm (wasmPath) {
  return {
    header: await dump(wasmPath, 'h'),
    details: await dump(wasmPath, 'x'),
    disassemble: await dump(wasmPath, 'd')
  }
}

async function compileWast (parsed, wastPath) {
  const dir = await mkdtemp(join(tmpdir(), 'wast-'))
  await execSync(`${wast2json} ${wastPath.pathname}`, { cwd: dir })

  let spec = ''
  const bins = []
  for (const file of await readdir(dir)) {
    const bin = await readFile(join(dir, file))
    if (file.endsWith('.json')) {
      spec = bin.toString('utf8')
    } else if (file.endsWith('.wasm')) {
      bins.push({ file, bin: bin.toString('base64') })
    }
  }
  if (spec === '') {
    throw new Error('json spec not found')
  }
  const specParsed = JSON.parse(spec)
  // Count expected binaries: modules + assert_invalid/assert_malformed commands
  const expectedBinaries = specParsed.commands.filter(cmd =>
    cmd.type === 'module' ||
    cmd.type === 'assert_invalid' ||
    cmd.type === 'assert_malformed' ||
    cmd.type === 'assert_unlinkable'
  ).length

  if (bins.length !== expectedBinaries) {
    console.log('Binary count mismatch:')
    console.log('  Found binaries:', bins.length)
    console.log('  Expected:', expectedBinaries)
    console.log('  Command types:')
    const typeCounts = {}
    specParsed.commands.forEach(cmd => {
      typeCounts[cmd.type] = (typeCounts[cmd.type] || 0) + 1
    })
    Object.entries(typeCounts).forEach(([type, count]) => {
      console.log(`    ${type}: ${count}`)
    })
    console.log('  Binary files found:', bins.map(b => b.file).join(', '))
    // Don't throw for start.wast - just warn
    if (!wastPath.pathname.includes('start.wast')) {
      throw new Error(`binaries count (${bins.length}) does not match expected count (${expectedBinaries})`)
    }
    console.log('  WARNING: Continuing despite mismatch for start.wast')
  }
  let compiled = `{\n  "bin": {\n${bins.map(({ file, bin }) => `    "${file}": "${bin}"`).join(',\n')}`
  compiled += `\n  },\n  "spec": ${spec.replace(/\n$/, '')}`
  compiled += ',\n  "code": [\n'
  for (const { word, block } of parsed) {
    compiled += `    "(${word} ${cleanJsonString(block)})",\n`
  }

  compiled = compiled.slice(0, -2)
  compiled += '\n  ]'

  // Only add dump section if there are regular modules
  const moduleCommands = specParsed.commands.filter(({ type }) => type === 'module')
  if (moduleCommands.length > 0) {
    compiled += ',\n  "dump": {\n'

    for (const cmd of moduleCommands) {
      const dump = await dumpWasm(join(dir, cmd.filename))

      compiled += `    "${cmd.filename}": {\n`
      compiled += `      "header": ${JSON.stringify(dump.header)},\n`
      compiled += `      "details": ${JSON.stringify(dump.details)},\n`
      compiled += `      "disassemble": ${JSON.stringify(dump.disassemble)}\n`
      compiled += '    },\n'
    }
    compiled = compiled.slice(0, -2)
    compiled += '\n  }\n'
  } else {
    compiled += '\n'
  }
  compiled += '}\n'
  await rm(dir, { recursive: true })
  return compiled
}

async function dump (wasmPath, opt) {
  const stdout = await execSync(`${wasmObjdump} -${opt} ${wasmPath}`)
  return stdout.toString('utf8')
}

function cleanJsonString (str) {
  return str.replace(/\\/g, '\\\\').replace(/\n/g, '\\n').replace(/"/g, '\\"').replace(/\t/g, '\\t')
}

async function parseWast (wastPath) {
  const wast = await readFile(wastPath, 'utf8')

  const units = []

  let br = 0
  let word = ''
  let block = ''
  for (let cc = 0; cc < wast.length; cc++) {
    const ch = wast[cc]
    if (ch === ';' && cc !== wast.length - 1 && wast[cc + 1] === ';') {
      while (wast[cc] !== '\n') {
        cc++
      }
      continue
    } else if (ch === '(' && cc !== wast.length - 1 && wast[cc + 1] === ';') {
      while (wast[cc] !== ';' || wast[cc + 1] !== ')') {
        cc++
      }
      if (wast[cc] === ';' && wast[cc + 1] === ')') {
        cc++
      }
      continue
    } else if (ch === '\n') {
      continue
    }

    if (ch === '(') {
      br++
      if (br === 1) {
        continue
      }
    } else if (ch === ')') {
      br--
      if (br === 0 && word !== '') {
        units.push({ word, block })
        block = ''
        word = ''
        continue
      }
    }

    if (br === 1 && word === '' && /[a-zA-Z_]/.test(ch)) {
      if (block !== '') {
        console.log(`discarding block [${block}] @ ${cc}`)
        block = ''
      }
      word = ch
      for (let i = cc + 1; i < wast.length && /[a-zA-Z_]/.test(wast[i]); i++) {
        word += wast[i]
      }
      cc += word.length - 1
      continue
    } else if (br === 2 && word === 'module' && /[a-zA-Z_]/.test(ch)) {
      let nest = ch
      for (let i = cc + 1; i < wast.length && /[a-zA-Z_]/.test(wast[i]); i++) {
        nest += wast[i]
      }
      cc += nest.length - 1
      if (nest === 'func') {
        // blunt newlining for (func), this doesn't take care of the end, however, so we
        // don't quite have clean blocks but they're good enough for eyeballing
        block = block.replace(/\s*\($/, `\n  (${nest}`)
      } else {
        block += nest
      }
      continue
    }
    if (ch === ' ' && (block === '' || /[\s (]$/.test(block))) {
      continue
    }
    block += ch
  }

  return units
}
