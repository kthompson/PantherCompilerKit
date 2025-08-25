#!/usr/bin/env pwsh

# Lint script - runs sbt scalafmt to format Scala code
Write-Host "Running sbt scalafmt..." -ForegroundColor Green

sbt scalafmt

if ($LASTEXITCODE -eq 0) {
    Write-Host "Code formatting completed successfully!" -ForegroundColor Green
} else {
    Write-Host "Code formatting failed with exit code $LASTEXITCODE" -ForegroundColor Red
    exit $LASTEXITCODE
}
