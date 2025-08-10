#!/usr/bin/env pwsh
# Script to run Tests.scala using sbt
# Usage: .\scripts\tests.ps1

Write-Host "Running PantherCompilerKit tests..." -ForegroundColor Green
sbt "test/runMain Tests"
