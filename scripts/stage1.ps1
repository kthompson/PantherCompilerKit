#!/usr/bin/env pwsh
# Cross-platform bootstrap script for PantherCompilerKit
# Runs sbt pncs/bootstrap on Windows, macOS, and Linux

Write-Host "Running sbt pncs/bootstrap..." -ForegroundColor Green
sbt "pncs/bootstrap"
