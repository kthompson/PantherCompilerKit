#!/usr/bin/env pwsh
# Cross-platform stage2 script for PantherCompilerKit
# Runs sbt pnc/compile on Windows, macOS, and Linux

Write-Host "Running sbt pnc/compile..." -ForegroundColor Green
sbt "pnc/compile"
