#!/usr/bin/env pwsh
# Cross-platform transpile script for PantherCompilerKit
# Runs sbt pncs/transpile on Windows, macOS, and Linux

Write-Host "Running sbt pncs/transpile..." -ForegroundColor Green
sbt "pncs/transpile"
