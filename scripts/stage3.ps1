#!/usr/bin/env pwsh
# Cross-platform Stage 3 fixed-point check for PantherCompilerKit.

$ErrorActionPreference = "Stop"
$repoRoot = (Resolve-Path (Join-Path $PSScriptRoot "..")).Path

Push-Location $repoRoot
try {
    Write-Host "Building Stage 2 and verifying the Stage 3 fixed point..." -ForegroundColor Green
    sbt "pnc/compile" "stage3/run"
    if ($LASTEXITCODE -ne 0) {
        exit $LASTEXITCODE
    }
}
finally {
    Pop-Location
}
