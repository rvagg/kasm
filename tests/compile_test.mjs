import { readFile, writeFile, mkdtemp, readdir, rm, stat } from 'fs/promises'
import { execSync } from 'child_process'
import { join, basename } from 'node:path';
import { tmpdir } from 'node:os';

// const wast2wasm = new URL('../webassembly/wabt/build/wat2wasm', import.meta.url).pathname
const wast2json = new URL('../webassembly/wabt/build/wast2json', `file://${process.cwd()}/`).pathname
const wasmObjdump = new URL('../webassembly/wabt/build/wasm-objdump', `file://${process.cwd()}/`).pathname

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

async function compileWast (parsed, wastPath) {
  const dir = await mkdtemp(join(tmpdir(), 'wast-'));
  await execSync(`${wast2json} ${wastPath.pathname}`, { cwd: dir })
  const dumpPath = join(dir, basename(wastPath.pathname).replace(/\.wast$/, '') + '.0.wasm')
  const headerDump = await dump(dumpPath, 'h')
  const detailsDump = await dump(dumpPath, 'x')
  const disassembleDump = await dump(dumpPath, 'd')

  let spec = ''
  const bins = []
  for (const file of await readdir(dir)) {
    const bin = await readFile(join(dir, file))
    if (file.endsWith('.json')) {
      spec = bin.toString('utf8')
    } else {
      bins.push({ file, bin: bin.toString('base64') })
    }
  }
  if (spec === '') {
    throw new Error('json spec not found')
  }
  const specParsed = JSON.parse(spec)
  if (bins.length === 0) {
    throw new Error('binaries not found')
  }
  if (specParsed.commands.length !== parsed.length) {
    throw new Error(`binaries count (${bins.length}) does not match parsed count (${parsed.length})`)
  }
  let compiled = `{\n  "bin": {\n${bins.map(({ file, bin }) => `    "${file}": "${bin}"`).join(',\n')}`
  compiled += `\n  },\n  "spec": ${spec.replace(/\n$/, '')}`
  compiled += ',\n  "code": [\n'
  for (const { word, block } of parsed) {
    compiled += `    "(${word} ${cleanJsonString(block)})",\n`
  }
  compiled = compiled.slice(0, -2)
  compiled += '\n  ],\n'
  compiled += '  "dump": {\n'
  compiled += `    "header": ${JSON.stringify(headerDump)},\n`
  compiled += `    "details": ${JSON.stringify(detailsDump)},\n`
  compiled += `    "disassemble": ${JSON.stringify(disassembleDump)}\n`
  compiled += '  }\n'
  compiled += '}\n'
  await rm(dir, { recursive: true })
  return compiled
}

async function dump (wasmPath, opt) {
  const stdout = await execSync(`${wasmObjdump} -${opt} ${wasmPath}`)
  return stdout.toString('utf8')
}

function cleanJsonString (str) {
  return str.replace(/\n/g, '\\n').replace(/"/g, '\\"')
}

async function parseWast (wastPath) {
  const wast = await readFile(wastPath, 'utf8')

  const units = []

  let br = 0
  let word = ''
  let block = ''
  for (let cc = 0; cc < wast.length; cc++) {
    const ch = wast[cc]
    if (ch === ';' && cc != wast.length - 1 && wast[cc + 1] === ';') {
      while (wast[cc] !== '\n') {
        cc++
      }
      continue
    } else if (ch == '\n') {
      continue
    }

    if (ch === '(') {
      br++
      if (br === 1) {
        continue
      }
    } else if (ch === ')') {
      br--
      if (br === 0 && word != '') {
        units.push({ word, block })
        block = ''
        word = ''
        continue
      }
    }

    if (br === 1 && word == '' && /[a-zA-Z_]/.test(ch)) {
      if (block !== '') {
        console.log(`discarding block [${block}]`)
        block = ''
      }
      word = ch
      for (let i = cc + 1; i < wast.length && /[a-zA-Z_]/.test(wast[i]); i++) {
        word += wast[i]
      }
      cc += word.length - 1
      continue
    } else if (br === 2 && word == 'module' && /[a-zA-Z_]/.test(ch)) {
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
    if (ch === ' ' && (block === '' || /[\s \(]$/.test(block))) {
      continue
    }
    block += ch
  }

  return units
}
