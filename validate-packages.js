#!/usr/bin/env node
/**
 * validate-packages.js - Validates package structure and integrity
 * 
 * Checks:
 * 1. All packages have a valid pkg.lsp
 * 2. pkg.lsp :NAME matches directory name
 * 3. All files in :FILES exist
 * 4. No encoding issues (UTF-8)
 * 5. Required dependencies exist
 * 6. Version format is valid
 */

const fs = require('fs');
const path = require('path');

const PACKAGES_DIR = __dirname;
let errors = 0;
let warnings = 0;

function log(type, msg) {
  if (type === 'error') { console.error(`  ❌ ${msg}`); errors++; }
  else if (type === 'warn') { console.warn(`  ⚠️  ${msg}`); warnings++; }
  else { console.log(`  ✅ ${msg}`); }
}

function checkFileEncoding(filePath) {
  try {
    const buf = fs.readFileSync(filePath);
    // Check for BOM
    if (buf[0] === 0xEF && buf[1] === 0xBB && buf[2] === 0xBF) {
      log('warn', `${filePath}: Has UTF-8 BOM, consider removing`);
    }
    // Check for null bytes (binary)
    for (let i = 0; i < buf.length; i++) {
      if (buf[i] === 0) {
        log('error', `${filePath}: Contains null bytes (binary content)`);
        return false;
      }
    }
    return true;
  } catch (e) {
    log('error', `${filePath}: Cannot read file`);
    return false;
  }
}

function parsePkgLsp(content) {
  const pkg = {};
  // Extract :NAME (case-insensitive for both key and format)
  const nameMatch = content.match(/\(:name\s*\.\s*"([^"]+)"\)/i);
  if (nameMatch) pkg.name = nameMatch[1];
  
  // Extract :VERSION
  const verMatch = content.match(/\(:version\s*\.\s*"([^"]+)"\)/i);
  if (verMatch) pkg.version = verMatch[1];
  
  // Extract :REQUIRED
  const reqMatch = content.match(/\(:required\s*\.\s*"([^"]+)"\)/i);
  if (reqMatch) pkg.required = reqMatch[1];
  
  // Extract :FILES - handle both (:FILES "a" "b") and (:FILES . ("a" "b"))
  // Also handle multi-line
  const filesMatch = content.match(/\(:files\s+(?:\.?\s*)?\(([\s\S]*?)\)\)/i)
    || content.match(/\(:files\s+([\s\S]*?)\)\)/i);
  if (filesMatch) {
    const filesStr = filesMatch[1];
    pkg.files = filesStr
      .replace(/\n/g, ' ')
      .match(/"([^"]+)"/g)
      ?.map(f => f.replace(/"/g, ''))
      || [];
  }
  
  // Extract :FULL-NAME
  const fullNameMatch = content.match(/\(:full-name\s*\.\s*"([^"]+)"\)/i);
  if (fullNameMatch) pkg.fullName = fullNameMatch[1];
  
  return pkg;
}

function validatePackage(dirName) {
  const pkgDir = path.join(PACKAGES_DIR, dirName);
  const pkgLspPath = path.join(pkgDir, 'pkg.lsp');
  
  console.log(`\n📦 ${dirName}`);
  
  // Check pkg.lsp exists
  if (!fs.existsSync(pkgLspPath)) {
    log('error', 'Missing pkg.lsp');
    return;
  }
  
  // Check encoding
  checkFileEncoding(pkgLspPath);
  
  // Parse pkg.lsp
  const content = fs.readFileSync(pkgLspPath, 'utf-8');
  const pkg = parsePkgLsp(content);
  
  // Check :NAME matches directory
  if (!pkg.name) {
    log('error', 'Missing :NAME in pkg.lsp');
  } else if (pkg.name !== dirName) {
    log('error', `:NAME "${pkg.name}" does not match directory "${dirName}"`);
  } else {
    log('ok', `:NAME matches directory`);
  }
  
  // Check version format
  if (pkg.version) {
    if (/^\d+\.\d+\.\d+$/.test(pkg.version)) {
      log('ok', `Version: ${pkg.version}`);
    } else {
      log('warn', `Version "${pkg.version}" doesn't follow semver format`);
    }
  }
  
  // Check dependency exists
  if (pkg.required) {
    const depDir = path.join(PACKAGES_DIR, pkg.required);
    if (fs.existsSync(depDir)) {
      log('ok', `Dependency "${pkg.required}" exists`);
    } else {
      log('error', `Dependency "${pkg.required}" not found`);
    }
  }
  
  // Check files exist
  // Binary/external resources that may not be in repo
  const BINARY_EXTS = ['.dll', '.fas', '.zelx', '.shx', '.zip', '.pc3', '.pmp', '.pc5', '.txt', '.libdoc'];
  
  if (pkg.files && pkg.files.length > 0) {
    let missingLsp = 0;
    let missingBinary = 0;
    for (const file of pkg.files) {
      // Files without extension = .lsp files
      const lspFile = file.includes('.') ? file : file + '.lsp';
      const filePath = path.join(pkgDir, lspFile);
      if (!fs.existsSync(filePath)) {
        const ext = path.extname(lspFile).toLowerCase();
        if (BINARY_EXTS.includes(ext)) {
          missingBinary++;
        } else {
          log('error', `File not found: ${lspFile}`);
          missingLsp++;
        }
      }
    }
    const lspCount = pkg.files.filter(f => {
      const ext = path.extname(f.includes('.') ? f : f + '.lsp').toLowerCase();
      return !BINARY_EXTS.includes(ext);
    }).length;
    if (missingLsp === 0) {
      log('ok', `All ${lspCount} source files found`);
    }
    if (missingBinary > 0) {
      log('warn', `${missingBinary} binary/external files not in repo (may be downloaded)`);
    }
  }
  
  // Check for non-ASCII in :FULL-NAME or :DESCRIPTION (garbled text detection)
  const fullNameMatch = content.match(/\(:FULL-NAME\s*\.\s*"([^"]+)"\)/);
  if (fullNameMatch) {
    const val = fullNameMatch[1];
    // Check for common garbled patterns
    const garbledPatterns = ['寤虹瓚', '鐢垫皵', '閫氱敤椤圭', '绠＄悊', '鐢垫'];
    for (const p of garbledPatterns) {
      if (val.includes(p)) {
        log('error', `Possible garbled text in :FULL-NAME: "${val}"`);
        break;
      }
    }
  }
}

// Main
console.log('🔍 @lisp Package Validator');
console.log('========================');

const dirs = fs.readdirSync(PACKAGES_DIR, { withFileTypes: true })
  .filter(d => d.isDirectory())
  .map(d => d.name);

const packageDirs = [];
for (const dir of dirs) {
  const pkgLsp = path.join(PACKAGES_DIR, dir, 'pkg.lsp');
  if (fs.existsSync(pkgLsp)) {
    packageDirs.push(dir);
  }
}

console.log(`Found ${packageDirs.length} packages\n`);

for (const dir of packageDirs) {
  validatePackage(dir);
}

console.log('\n========================');
console.log(`📊 Summary: ${errors} errors, ${warnings} warnings`);

if (errors > 0) {
  console.log('❌ VALIDATION FAILED');
  process.exit(1);
} else {
  console.log('✅ VALIDATION PASSED');
  process.exit(0);
}
