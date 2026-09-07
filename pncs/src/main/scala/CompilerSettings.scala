import panther._

case class CompilerSettings(
    kindRecoveryAttempts: int,
    diagnosticsToPrint: int,
    /** The VM's value stack, in slots rather than frames. A call occupies its
      * arguments, its receiver if it has one, three slots of bookkeeping — the
      * return address and the caller's `localp` and `argsp` — and its locals,
      * with whatever the caller left on the stack still live underneath.
      *
      * A derived `Eq` over a recursive type is the most stack-hungry thing the
      * VM runs: measured at `20 + 16n` slots for a list of `n` elements, and a
      * plain recursive function at `15 + 12n`.
      */
    stackSize: int,
    /** The VM's object heap, in slots. `alloc` is a bump pointer and nothing is
      * ever reclaimed, so this is the whole run's allocation budget rather than
      * a live-set limit.
      */
    heapSize: int,
    debug: bool,
    enableTracing: bool,
    printSymbols: bool,
    printBoundAssembly: bool,
    printLoweredAssembly: bool,
    transpile: bool,
    /** Compile and execute in one step, instead of emitting. There is no
      * output file in this mode — every positional argument is a source.
      */
    run: bool
)

object CompilerSettingsFactory {
  // Default instance. Panther has no named arguments, so the order follows
  // the constructor above.
  val default = CompilerSettings(
    5, // kindRecoveryAttempts
    20, // diagnosticsToPrint
    // 8192 slots is roughly 680 plain frames, or a derived `Eq` over a
    // 500-element list. Was 50, which is two or three frames of anything and
    // one element of a derived `Eq` — small enough that recursion of any kind
    // failed, which went unnoticed while nothing recursive ran on the VM.
    8192, // stackSize
    65536, // heapSize
    false, // debug
    false, // enableTracing
    false, // printSymbols
    false, // printBoundAssembly
    false, // printLoweredAssembly
    false, // transpile
    false // run
  )
}
