import panther._

case class CompilerSettings(
    kindRecoveryAttempts: int,
    diagnosticsToPrint: int,
    stackSize: int,
    heapSize: int,
    debug: bool,
    enableTracing: bool,
    printSymbols: bool,
    printBoundAssembly: bool,
    printLoweredAssembly: bool,
    transpile: bool
)

object CompilerSettingsFactory {
  // Default instance
  val default = CompilerSettings(
    kindRecoveryAttempts = 5,
    diagnosticsToPrint = 20,
    stackSize = 50,
    heapSize = 1024,
    debug = false,
    enableTracing = false,
    printSymbols = false,
    printBoundAssembly = false,
    printLoweredAssembly = false,
    transpile = false
  )
}
