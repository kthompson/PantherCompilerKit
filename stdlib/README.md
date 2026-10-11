# User standard library

Both `pncs` and the self-hosted `pnc` automatically compile the standard library
alongside user sources. Its names are available at the root: `Option`, `Result`,
`Either`, `List`, `Dictionary`, `Tuple2`, and their helper modules.

```panther
val answer = Option.Some(42)
println(answer.get())
```

The default bundle is embedded as Panther source text in each compiler. It does
not depend on the working directory or an installed source directory. Emitted
`.pnb` images contain the library code and need only the VM to execute.

`--no-stdlib` disables inclusion. Use it when compiling the compiler's own
sources, which already declare the runtime types, or when defining replacements
in an isolated program. Transpilation automatically omits the library.

`--stdlib <directory>` replaces the embedded bundle. The directory must contain
`Dictionary.pn`, `Either.pn`, `List.pn`, `Option.pn`, `Result.pn`, and `Tuple2.pn`.
All six files are required; their normal dependencies must be satisfied. Paths
are relative to the working directory unless absolute. The two flags are
mutually exclusive. A declaration that conflicts with a bundled type produces
a duplicate-definition diagnostic.

The canonical implementations live in `runtime/src/main/scala`. Run
`sbt pncs/transpile` after changing them: this updates both `pnc/src` and the
selected user library files in `stdlib/src`. Commit both generated sets.
`project/StdlibBundle.scala` generates the Scala compiler's embedded source
object from `stdlib/src` during compilation, and the transpile task refreshes
its Panther equivalent after generating the library.

Library inclusion belongs to `Stdlib.withSources`, used by the command-line
driver and doccheck. `MakeCompilation.create` remains a raw compilation API,
so parser/binder tests can define their own types without implicit library
inclusion. Call `Stdlib.withSources` first when compiling user programs through
that API.

Tests cover library execution, derived equality, replacement sources, missing
files, conflicting declarations, and standalone images. Stage 3 additionally
requires the self-hosted compiler to emit the same user image as `pncs`, execute
it, and support both the default and replacement library in `--run` mode.
