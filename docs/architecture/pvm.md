# Panther Virtual Machine (PVM) specification

This document specifies the `.pnb` image format and the virtual machine that
executes it, precisely enough to implement a second VM from it. It is Phase 1
of [removing the Scala dependency](scala-exit.md).

**Reference implementation:** [`VM.scala`](../../pncs/src/main/scala/VM.scala),
[`ManagedHeap.scala`](../../pncs/src/main/scala/ManagedHeap.scala), and the
metadata library under [`metadata/`](../../metadata/src/main/scala/). This spec
was written against `5bb9b97`. The edge cases in it were checked by running
probe programs through `pncs --run` on 2026-10-24.

## Conventions

- **MUST**, **MUST NOT**, **SHOULD**, and **MAY** are used as in RFC 2119.
- A *word* is a 32-bit two's-complement integer.
- *Trap* means a runtime error as defined in [§8](#8-traps). It ends the run.
- **Reference:** notes say what `VM.scala` does where that differs from the
  requirement, or where it relies on JVM behaviour. Each such note cites a
  defect `D<n>` from [§9](#9-defects-to-settle). A conforming VM follows the
  requirement, not the note.
- `[a, b) → [c]` describes a stack effect. The rightmost item is the top of the
  stack.

---

## 1. Values

Every stack slot, local, argument, static field, object field, and array
element holds one value of one of these five kinds:

| Kind | Payload | Produced by |
| --- | --- | --- |
| `Int` | one word | `ldc.i4`, arithmetic, `ldlen`, string indexing, builtins |
| `Bool` | true or false | `ldtrue`, `ldfalse`, comparisons, builtins |
| `String` | an immutable sequence of UTF-16 code units | `ldstr`, `add`, builtins |
| `Ref` | a type token and a heap address | `newobj`, `newarr`, builtins that return arrays |
| `Unit` | none | builtins and casts that return `unit`, fresh locals, cleared slots |

`Unit` is called `Value.Uninitialized` in the reference. It is both the `unit`
value and the contents of any slot that has not been written.

- **`char` has no kind of its own.** A `char` is an `Int` holding a UTF-16 code
  unit. The VM cannot tell `'a'` from `97`, so only the compiler's static type
  decides how one prints (see `CharToString`, [§7](#7-builtins)). As a
  consequence `println('a')` prints `97`.
- **`Int` arithmetic wraps** modulo 2³² ([§6.6](#66-arithmetic-and-bitwise)).
- **`String` values have value semantics.** They are immutable, compare by
  content, and are not heap-slot allocated (see [§5.4](#54-strings)).
- **A `Ref`'s type token** is the owning type definition for an object. For an
  array it is the **element type** token passed to `newarr` (or chosen by the
  builtin that allocated it). `Array[int]` and `Array[string]` therefore carry
  different tokens. A token alone does not say whether a `Ref` is an array or
  an object; the heap block kind does ([§5.2](#52-blocks)).
- **Addresses are not observable.** No instruction or builtin converts a `Ref`
  to an `Int` or a string that contains the address. A VM MAY therefore move
  objects, provided every `Ref` is updated and identity is preserved.

---

## 2. Image format

### 2.1 Encoding

An image is a sequence of words. Each word is written as four bytes, most
significant first (big-endian). There are no other primitive encodings: tables,
strings, and instructions are all words.

A reader MUST reject a file shorter than 12 bytes. Trailing bytes that do not
form a whole word, and words after the signature table, are ignored by the
reference reader. Writers MUST NOT produce either.

### 2.2 Layout

```text
offset  contents
0       magic      1347305984  (0x504E4200, the bytes "PNB\0")
1       version    1
2       entry      method token of the entry point, or -1 for none
3       chunk      §2.3
…       typeDefs   §2.4
…       fields     §2.5
…       methods    §2.6
…       params     §2.7
…       strings    §2.8
…       signatures §2.9
```

A reader MUST reject a wrong magic word or a version other than `1`. Any change
to the layout, a table's record shape, or the meaning of an opcode or builtin
id MUST increment the version (see
[scala-exit.md Phase 4](scala-exit.md#phase-4--seed-image-and-bootstrap)).

### 2.3 Chunk

```text
n                    instruction word count
content[0 .. n)      opcode and operand words
lines[0 .. n)        source line for each content word
```

`content` is the instruction stream. Opcodes and their operands are each one
word, and an operand directly follows its opcode. A method's code starts at its
`address` ([§2.6](#26-methods)). Methods are laid out back to back; there is no
separator.

`lines[i]` is the 1-based source line of `content[i]`, operands included. Lines
are not needed to execute but are what traces and trap reports use. Line
numbers identify a line within some source file; the image does not record
which file.

### 2.4 Type definitions

```text
count
count × { name: string token, namespace: string token, flags, fieldList, methodList }
```

- `name` and `namespace` are string tokens. The **qualified name** is
  `namespace + "." + name`, or just `name` when the namespace is empty.
- `fieldList` and `methodList` are indices into the field and method tables.
  Type `t` owns fields `[fieldList(t), fieldList(t+1))` and methods
  `[methodList(t), methodList(t+1))`. The last type owns everything up to the
  end of each table. Both lists are non-decreasing.
- The **owner** of field or method `i` is the last type `t` whose list start is
  `≤ i`. (Several empty types can share a start index; the last one wins.)
- The **instance size** of type `t` is the number of fields it owns,
  `fieldList(t+1) − fieldList(t)`. **Reference:** for the last type the
  reference computes one fewer (D7).

The compiler registers six primitive types first, with empty namespaces, so
they are tokens 0–5: `int`, `bool`, `string`, `char`, `unit`, `any`. A VM MUST
NOT rely on those token numbers. The reference locates primitives by
qualified name, and so does this spec (`newarr`, `isinst`, `cast`, and the
array-returning builtins).

The compiler emits one type per class, object, enum, enum case, and given.

### 2.5 Fields

```text
count
count × { name: string token, flags, index, signature }
```

- For an instance field (`flags = 0`), `index` is its slot offset within the
  object. An enum case repeats its parent enum's fields first, at the same
  indices, so the case's instance size includes them.
- For a static field (`flags = 1`), `index` is its slot in the static area
  ([§5.1](#51-layout)). Static indices are numbered from 0 across the whole
  image.
- The number of static slots is the count of fields with `flags = 1`.

### 2.6 Methods

```text
count
count × { name: string token, flags, signature, paramList, locals, address, hasThis }
```

- `paramList` is an index into the param table. A method's **parameter count**
  is `paramList(m+1) − paramList(m)`, or `params.count − paramList(m)` for the
  last method. It counts declared parameters (including evidence parameters)
  and **excludes the receiver**.
- `locals` is the number of local slots the frame reserves.
- `address` is the chunk offset of the first instruction, or `-1` for a method
  with no body (an `extern`). Calling a method whose address is `-1` MUST trap.
- `hasThis` is `1` for an instance method, which receives its receiver as
  argument 0. Any other value means false. Methods of an `object` are static
  (`hasThis = 0`).
- The qualified method name, used for traces and trap reports, is the owning
  type's qualified name, `"."`, and the method name. Constructors are named
  `.ctor`.

### 2.7 Parameters

```text
count
count × { name: string token, flags, signature }
```

The VM uses this table only through `paramList`, to count parameters.

### 2.8 Strings

```text
count
count × { length, length × code unit }
```

Each string is stored as its UTF-16 code units, one word each, in `0..65535`.
A string token is the string's index in this table. The table holds string
literals (`ldstr`) and every name the other tables refer to. Entries are not
deduplicated.

### 2.9 Signatures

```text
total word count
blob*                      each blob is { length, length × word }
```

A signature token is the word offset of a blob within this table. The VM does
not read signatures. The compiler currently writes an empty blob (`length = 0`)
for every field, method, and parameter. The element codes reserved for future
use are in [`SignatureBuilder.scala`](../../metadata/src/main/scala/SignatureBuilder.scala).

### 2.10 Flags

`flags` in every table is `0` (none) or `1` (static). A reader MUST reject any
other value. **Reference:** host panic while reading (D2).

---

## 3. Execution model

### 3.1 State

| Register | Meaning |
| --- | --- |
| `ip` | offset of the next word to read from `content` |
| `sp` | number of live stack slots; the top of the stack is `sp − 1` |
| `argsp` | stack index of the current frame's argument 0 |
| `localp` | stack index of the current frame's local 0 |

The stack is an array of `stackSize` value slots. Operands, arguments, frame
bookkeeping, and locals all share it.

### 3.2 Frames

A frame looks like this, from the bottom up:

```text
argsp  →  arg 0           the receiver, when hasThis
          arg 1 …         declared parameters
          return address  Int: offset to resume at, or -1 for the entry frame
          saved localp    Int
          saved argsp     Int
localp →  local 0 … local (locals − 1), initialised to Unit
          operand stack
```

A frame costs `hasThis + parameters + 3 + locals` slots plus its operands. The
frame layout itself is not observable. A VM MAY use a different layout, but
MUST support at least the call depth the reference supports for the same
`stackSize`.

**Call** (`call`, `calli`, `newobj`): the caller has pushed the receiver (if
`hasThis`) and then the arguments in declaration order. The VM pushes the three
bookkeeping words, sets `argsp` to the slot of the first argument, sets
`localp`, pushes `locals` `Unit` values, and jumps to the method's `address`.

**Return** (`ret`): pops the result value, clears the whole frame (arguments,
bookkeeping, locals, and any operands left above the locals), stores the result
where argument 0 was, and restores `argsp`, `localp`, and `ip`. The caller sees
the arguments replaced by exactly one value. Every method, including `unit`
ones, returns exactly one value.

If the restored `ip` is `-1`, the entry method has returned and the run ends
([§3.4](#34-termination)).

### 3.3 Starting a run

1. Allocate the static area ([§5.1](#51-layout)), every slot `Unit`.
2. Start the entry method in one of two modes:
   - **With arguments** (`runWithArgs`, used by `pnc` and any command-line
     program). The entry method MUST have exactly one parameter, or the VM
     traps. The VM allocates an array whose type token is the type named
     `string`, fills it with the command-line arguments as `String` values,
     pushes the `Ref`, and calls the entry method.
   - **Without arguments** (`run`, used by `pncs --run` and tests). The entry
     method is called with an empty stack. It SHOULD have no parameters.
3. The entry frame's return address is `-1`.

The VM runs no initialisers of its own. The compiler emits a `$runtimeInit`
method that builds enum singletons, evidence records, and object state, and
calls it from `main`.

A loader for a command-line VM MUST reject an image with no entry point.
**Reference:** `run` with no entry point executes from offset 0 (D12).

### 3.4 Termination

A run ends with one of these results:

| Result | Cause | Process exit code |
| --- | --- | --- |
| `Ok` / `OkValue(v)` | the entry method returned `v` | 0 |
| `Exit(n)` | the program called `exit(n)` | `n` (the OS keeps the low 8 bits on POSIX) |
| `RuntimeError` | a trap ([§8](#8-traps)) | 1 |

The value an entry method returns is never an exit code. A program whose last
expression is `2` exits 0. This mapping is `Program.execute` in the
reference, and a standalone `pvm` MUST use it.

---

## 4. Instruction encoding

Each instruction is an opcode word, optionally followed by one operand word.
The operand kinds are:

| Kind | Meaning |
| --- | --- |
| `i4` | an immediate word |
| `index` | an argument or local index |
| `target` | an absolute chunk offset |
| `string` | a string token |
| `field` | a field token |
| `method` | a method token |
| `type` | a type definition token |
| `builtin` | a builtin id ([§7](#7-builtins)) |

Executing a word that is not one of the 56 opcodes in [§6](#6-instructions)
MUST trap. **Reference:** host panic (D2).

Opcode numbers not listed are unassigned. 53–58 are reserved for the
compare-and-branch family (`beq`, `bne`, `bge`, `bgt`, `ble`, `blt`), which is
commented out in [`Opcode.scala`](../../metadata/src/main/scala/Opcode.scala).

---

## 5. Heap

### 5.1 Layout

The heap is an array of `heapSize` value slots. It contains:

- **The static area**, slots `[0, staticCount)`, where `staticCount` is the
  number of static fields. Static field `f` lives in slot `index(f)`.
- **Blocks** allocated by `newobj`, `newarr`, and builtins.

### 5.2 Blocks

| Kind | Size in slots | Contents |
| --- | --- | --- |
| Object | `max(1, instance size)` | field `i` at slot `i`, all initialised to `Unit` |
| Array | `length + 1` | slot 0 is `Int(length)`; element `i` at slot `i + 1` |

A zero-field object still occupies one slot, so two of them have distinct
identities. A VM MUST track each block's kind and size. `ldfld` and `stfld`
require an object block, `ldelem`, `stelem`, and `ldlen` require an array
block, and both check bounds against the block.

### 5.3 Collection

The reference uses a precise, non-moving, stop-the-world mark-and-sweep
collector ([ADR 0007](adr/0007-garbage-collection.md)). The design is not part
of this spec, but these properties are:

- **Roots** are the live stack slots `[0, sp)`, the static area, and any value
  a builtin is holding while it allocates.
- **Reachability is precise.** Every `Ref` in a reachable slot is followed. An
  array's length slot is not a reference.
- **Identity is preserved.** A `Ref` compares equal to the same object for its
  whole lifetime.
- **A collection may happen at any allocation**, and only at an allocation.
- **Capacity.** For the same `heapSize`, a VM MUST NOT fail an allocation the
  reference would satisfy. The reference fails only when, after a full
  collection, there is no free run of contiguous slots large enough. Running
  out of heap traps.

The reference collects only when the bump-pointer tail is exhausted and no free
block fits. When a slot is freed its contents are cleared to `Unit`. Neither is
observable.

### 5.4 Strings

Strings are not stored in heap slots and do not count against `heapSize`. The
reference keeps them as JVM strings and relies on the JVM collector. A native
VM MUST reclaim strings that are no longer reachable from the roots in
[§5.3](#53-collection) (by reference counting, a separate string heap, or
anything else). A long compiler run allocates millions of short-lived strings.

### 5.5 Capacities

The stack and heap sizes are host settings, not part of the image.

| Configuration | `stackSize` | `heapSize` |
| --- | ---: | ---: |
| Default (`CompilerSettingsFactory.default`) | 8,192 | 65,536 |
| Stage 3, the compiler compiling itself | 2,097,152 | 33,554,432 |
| Stage 3, a stdlib user program | 65,536 | 1,048,576 |

Exceeding `stackSize` MUST trap. **Reference:** host fault (D2).

---

## 6. Instructions

Each entry gives the opcode number, mnemonic, operand, stack effect, and
behaviour. Unless a row says otherwise, popping a value of the wrong kind
traps.

### 6.1 Arguments and locals

| # | Mnemonic | Operand | Stack | Behaviour |
| ---: | --- | --- | --- | --- |
| 0 | `nop` | | `[] → []` | Nothing. |
| 1–4 | `ldarg.0` … `ldarg.3` | | `[] → [v]` | Push argument 0–3. Argument 0 is the receiver when `hasThis`. |
| 5 | `ldargn` | `index` | `[] → [v]` | Push argument `index`. |
| 6–9 | `ldloc.0` … `ldloc.3` | | `[] → [v]` | Push local 0–3. |
| 10 | `ldlocn` | `index` | `[] → [v]` | Push local `index`. |
| 11–14 | `stloc.0` … `stloc.3` | | `[v] → []` | Pop into local 0–3. |
| 15 | `stlocn` | `index` | `[v] → []` | Pop into local `index`. |

The compiler uses the short forms for indices 0–3 and the `n` forms above
that. An index outside the frame is undefined in the reference; a VM SHOULD
trap.

### 6.2 Constants

| # | Mnemonic | Operand | Stack | Behaviour |
| ---: | --- | --- | --- | --- |
| 30 | `ldc.i4` | `i4` | `[] → [Int]` | Push the operand. |
| 31 | `ldstr` | `string` | `[] → [String]` | Push the string with that token. |
| 32 | `ldtrue` | | `[] → [Bool]` | Push true. |
| 33 | `ldfalse` | | `[] → [Bool]` | Push false. |

### 6.3 Control

| # | Mnemonic | Operand | Stack | Behaviour |
| ---: | --- | --- | --- | --- |
| 20 | `ret` | | `[v] → ` | Return `v` ([§3.2](#32-frames)). |
| 50 | `br` | `target` | `[] → []` | `ip = target`. |
| 51 | `brfalse` | `target` | `[Bool] → []` | Branch if false. |
| 52 | `brtrue` | `target` | `[Bool] → []` | Branch if true. |
| 60 | `call` | `method` | `[this?, a₁ … aₙ] → [r]` | Call the method ([§3.2](#32-frames)). |
| 61 | `calli` | | `[this?, a₁ … aₙ, Int t] → [r]` | Pop a method token `t`, then behave as `call t`. Used for calls through evidence records. |
| 62 | `callx` | `builtin` | builtin-specific | Run builtin `id` ([§7](#7-builtins)). No frame is built. |

`brfalse` and `brtrue` require a `Bool`; `Int` is not accepted. **Reference:**
any other kind is a host panic (D2).

The compiler emits `&&` and `||` as `and` and `or` on two `Bool`s
([§6.6](#66-arithmetic-and-bitwise)), so **both operands are always
evaluated**: `false && f()` calls `f()`. That is a compiler issue, not a VM
one. Short-circuiting would be lowered to branches and needs no new
instruction.

### 6.4 Objects and fields

| # | Mnemonic | Operand | Stack | Behaviour |
| ---: | --- | --- | --- | --- |
| 40 | `ldfld` | `field` | `[Ref o] → [v]` | Push slot `index(field)` of object `o`. |
| 42 | `stfld` | `field` | `[Ref o, v] → []` | Store `v` in slot `index(field)` of `o`. |
| 41 | `ldsfld` | `field` | `[] → [v]` | Push static slot `index(field)`. |
| 43 | `stsfld` | `field` | `[v] → []` | Store `v` in static slot `index(field)`. |
| 44 | `newobj` | `method` | `[a₁ … aₙ] → [r]` | See below. |

`ldfld` and `stfld` trap if `o` is not a `Ref` to an object block or the index
is outside the block. They do not check that the field belongs to `o`'s type.

`newobj ctor`:

1. Let `T` be the owner of `ctor` ([§2.4](#24-type-definitions)).
2. Allocate an object block of `max(1, instance size(T))` slots, all `Unit`.
   This may collect.
3. Insert `Ref(T, address)` beneath the constructor's `n` arguments, where `n`
   is its parameter count.
4. Call `ctor`, which has `hasThis = 1`.

The constructor's return value is the result. The compiler ends every
constructor with `ldarg.0; ret`, so the result is the new object. The new
`Ref` is on the stack from step 3, so it is a root if the constructor
allocates.

### 6.5 Arrays

| # | Mnemonic | Operand | Stack | Behaviour |
| ---: | --- | --- | --- | --- |
| 70 | `newarr` | `type` | `[Int n] → [Ref]` | Allocate an `n`-element array. Its `Ref` carries the operand as its type token. |
| 71 | `ldelem` | | `[Ref a, Int i] → [v]` / `[String s, Int i] → [Int]` | Push element `i`, or code unit `i` of a string. |
| 72 | `stelem` | | `[Ref a, Int i, v] → []` | Store `v` as element `i`. Strings are immutable. |
| 73 | `ldlen` | | `[Ref a] → [Int]` / `[String s] → [Int]` | Push the array length, or the string length in code units. |

- `newarr` initialises elements according to the element type's
  **unqualified** name: `int` and `char` give `Int(0)`, `bool` gives false,
  `string` gives `""`, and anything else gives `Unit`. A negative `n` traps.
  **Reference:** reports it as out of memory (D11).
- Indices outside `[0, length)` trap.

### 6.6 Arithmetic and bitwise

All of these take `Int` operands and produce an `Int`, except where noted.
Results wrap modulo 2³².

| # | Mnemonic | Stack | Behaviour |
| ---: | --- | --- | --- |
| 83 | `add` | `[a, b] → [a + b]` | `Int + Int`, or `String + String` (concatenation). Any other pair traps. |
| 84 | `sub` | `[a, b] → [a − b]` | |
| 85 | `mul` | `[a, b] → [a × b]` | |
| 86 | `div` | `[a, b] → [a ÷ b]` | Truncates toward zero. `MIN ÷ −1` is `MIN`. Division by zero traps. |
| 87 | `rem` | `[a, b] → [a rem b]` | The sign follows the dividend: `−7 % 2` is `−1`. `MIN rem −1` is 0. Zero divisor traps. |
| 88 | `neg` | `[a] → [−a]` | `−MIN` is `MIN`. |
| 89 | `and` | `[a, b] → [a & b]` | Bitwise on two `Int`s, or logical (not short-circuiting) on two `Bool`s. |
| 90 | `or` | `[a, b] → [a \| b]` | As `and`. |
| 91 | `xor` | `[a, b] → [a ^ b]` | `Int` only. |
| 92 | `not` | `[a] → [~a]` | Bitwise complement, `Int` only. Logical `!x` is compiled as `ldc.i4 0; ceq`, and `a != b`, `a <= b`, `a >= b` as `ceq`, `cgt`, `clt` followed by `ldfalse; ceq`. |
| 93 | `shl` | `[a, b] → [a << (b & 31)]` | The count is masked to 5 bits: `1 << 33` is 2. |
| 94 | `shr` | `[a, b] → [a >> (b & 31)]` | Arithmetic (sign-extending): `−8 >> 1` is `−4`. There is no logical shift. |

**Reference:** division or remainder by zero is a JVM `ArithmeticException`
(D2). A C implementation MUST handle `MIN ÷ −1`, `MIN rem −1`, overflow, and
shift counts explicitly, since each is undefined behaviour in C.

### 6.7 Comparison

| # | Mnemonic | Stack | Behaviour |
| ---: | --- | --- | --- |
| 100 | `ceq` | `[a, b] → [Bool]` | Equality. |
| 101 | `cgt` | `[a, b] → [Bool]` | `a > b`. |
| 102 | `clt` | `[a, b] → [Bool]` | `a < b`. |

The operand pair decides the meaning:

| `a`, `b` | `ceq` | `cgt` / `clt` |
| --- | --- | --- |
| `Ref`, `Ref` | same type token and same address | trap |
| `String`, `String` | same code units | lexicographic by code unit (Java `compareTo`) |
| `Int` or `Bool`, `Int` or `Bool` | compare as integers, with false = 0 and true = 1 | as integers |
| anything else (any `Unit`, or `Ref` or `String` against another kind) | trap | trap |

The `Int`/`Bool` mixing is load-bearing: `!x` compiles to `ldc.i4 0; ceq` on a
`Bool`. **Reference:** the trap cases are host panics (D2).

### 6.8 Stack

| # | Mnemonic | Stack | Behaviour |
| ---: | --- | --- | --- |
| 80 | `dup` | `[v] → [v, v]` | |
| 81 | `pop` | `[v] → []` | |
| 82 | `swap` | `[a, b] → [b, a]` | |

### 6.9 Types

| # | Mnemonic | Operand | Stack | Behaviour |
| ---: | --- | --- | --- | --- |
| 120 | `isinst` | `type` | `[v] → [Bool]` | Whether `v` is an instance of the type. |
| 121 | `cast` | `type` | `[v] → [v′]` | Convert `v` to the type, or trap. |

`isinst T`, where `name` is `T`'s qualified name:

| `v` | Result |
| --- | --- |
| `Int` | `name == "int"`. A `char` value answers `int`, not `char`. |
| `Bool` | `name == "bool"` |
| `String` | `name == "string"` |
| `Ref(t, _)` | `t == T`. For an array, `t` is the element type. |
| `Unit` | false |

`cast T`:

| `v` | Target `T` | Result |
| --- | --- | --- |
| any | `unit` | `Unit`. This is how a block discards a value. |
| any | `any` | `v` |
| `Int` | `int`, `char` | `v` |
| `Int` | `string` | its decimal form |
| `Int` | `bool` | `v ≠ 0` |
| `Bool` | `bool` | `v` |
| `Bool` | `int` | 0 or 1 |
| `Bool` | `string` | `"true"` or `"false"` |
| `String` | `string` | `v` |
| `Ref(t, _)` | the type `t` | `v` |
| anything else | | trap |

**Reference:** a failed cast prints the runtime-error line, pushes `Unit`, and
**keeps running** (D1).

---

## 7. Builtins

`callx id` runs a builtin. The receiver (for `string` and `int` members) and
then the arguments are on the stack, as for `call`. A builtin pops them and
pushes exactly one result; a `unit` builtin pushes `Unit`. `panic` and `exit`
push nothing and end the run.

Ids are stable and part of the image format. Id 0 means "not a builtin" and is
never emitted. The emitter picks the id from the callee's qualified name, except
for 19, which it picks when the argument of `string(…)` has static type `char`.

| Id | Name | Stack | Behaviour |
| ---: | --- | --- | --- |
| 1 | `println` | `[v] → [Unit]` | Write `text(v)` and a newline to stdout. |
| 2 | `print` | `[v] → [Unit]` | Write `text(v)` to stdout. |
| 3 | `panic` | `[v] → ` | Trap with message `text(v)`. |
| 4 | `exit` | `[Int n] → ` | End the run with `Exit(n)`. |
| 5 | `assert` | `[Bool c, v] → [Unit]` | If `c` is false, trap with message `text(v)`. |
| 6 | `mod` | `[Int a, Int b] → [Int]` | Same as `rem`. A zero divisor traps (the reference does trap here). |
| 7 | `string.apply` | `[v] → [String]` | `text(v)`. |
| 8 | `int.apply` | `[v] → [Int]` | See below. |
| 9 | `bool.apply` | `[v] → [Bool]` | `Int`: `v ≠ 0`. `Bool`: `v`. Anything else traps, **including `String`** (D5). |
| 10 | `char.apply` | `[v] → [Int]` | `Int`: `v`, unchanged and unchecked. `String` of length 1: its code unit. Anything else traps. |
| 11 | `string.substring` | `[String s, Int start, Int end] → [String]` | Code units `[start, end)`. Traps unless `0 ≤ start ≤ end ≤ length`. |
| 12 | `string.endsWith` | `[String s, String suffix] → [Bool]` | Code-unit suffix test. |
| 13 | `string.compareTo` | `[String a, String b] → [Int]` | −1, 0, or 1, lexicographic by code unit. |
| 14 | `int.compareTo` | `[Int a, Int b] → [Int]` | −1, 0, or 1. |
| 15 | `File.readAllText` | `[String path] → [String]` | Read a file as UTF-8. |
| 16 | `File.writeAllText` | `[String path, String text] → [Unit]` | Create or truncate the file and write `text` as UTF-8. |
| 17 | `Path.combine` | `[String a, String b] → [String]` | See below. |
| 18 | `Path.nameWithoutExtension` | `[String path] → [String]` | See below (D3). |
| 19 | `string.apply(char)` | `[Int c] → [String]` | The one-code-unit string for the low 16 bits of `c`. A `String` argument is returned as is. |
| 20 | `File.readAllBytes` | `[String path] → [Ref]` | A new `Array[int]` (type token: `int`) of the file's bytes (D6). |
| 21 | `File.writeAllBytes` | `[String path, Ref bytes] → [Unit]` | Create or truncate the file and write the low 8 bits of each element. |
| 22 | `File.writeAllInts` | `[String path, Ref values, Int count] → [Unit]` | Write the first `count` elements as big-endian words. Traps unless `0 ≤ count ≤ length`. |
| 23 | `File.writeAllIntPages` | `[String path, Ref pages, Int count] → [Unit]` | See below. |
| 24 | `File.exists` | `[String path] → [Bool]` | True if the path names a regular file. A directory answers false. |

Unknown ids MUST trap.

A builtin that takes a `String` or array argument traps on any other kind.
Array elements read by the file writers are read as integers (`Bool` counts as
0 or 1); any other element kind traps.

#### `text(v)`

`println`, `print`, `panic`, `assert`, and `string.apply` all render a value
the same way:

| `v` | Text |
| --- | --- |
| `Int` | decimal, with a leading `-` when negative |
| `Bool` | `true` or `false` |
| `String` | itself |
| `Unit` | `unit` |
| `Ref(t, _)` | the qualified name of type `t`. An `Array[int]` prints `int`. |

#### `int.apply`

| `v` | Result |
| --- | --- |
| `Int` | `v` |
| `Bool` | 0 or 1 |
| `String` | an optional leading `-`, then ASCII digits only. No `+`, no whitespace. Empty traps. Any other character traps. |
| anything else | trap |

**Reference:** the digit loop wraps on overflow (`int("2147483648")` is
`−2147483648`), and `int("-")` is 0 (D4).

#### `Path.combine(a, b)`

With `sep` the host path separator (`/` on POSIX, `\` on Windows):

1. If `a` is empty, return `b`.
2. If `b` is empty, return `a`.
3. If `a` ends with `sep`, return `a + b`.
4. Otherwise return `a + sep + b`.

#### `Path.nameWithoutExtension(path)`

Let `s` be the index of the last `sep` (−1 if none) and `d` the index of the
last `.` (−1 if none). The reference returns `path` unchanged if `d < s`, and
code units `[s + 1, d)` otherwise. That covers the two cases the compiler uses
(`dir/file.pn` gives `file`), but it has two accidental results (D3):

- No `.` and no `sep` (`abc`): `d = s = −1`, and the substring is invalid. The
  reference crashes.
- No `.` after the last `sep` (`dir/file`, `a.b/c`): the whole path comes back,
  directory included.

#### `File.writeAllIntPages(path, pages, count)`

`pages` is an array of `Array[int]` pages. Write, in order, the first `count`
elements of the concatenation of the pages, as big-endian words. Every page is
validated **before** the file is opened, so a bad page list never truncates
the destination. The call traps if `count < 0`, if any element of `pages` is
not an array, or if the pages together hold fewer than `count` elements. This
is what writes `.pnb` images, so it MUST NOT allocate heap slots in proportion
to `count`.

#### File I/O failures

Reading a missing or unreadable file, writing where the file cannot be
created, and malformed UTF-8 in `readAllText` MUST trap. **Reference:** each is
an uncaught JVM exception (D2).

---

## 8. Traps

A trap ends the run with `RuntimeError`, which is exit code 1. The VM writes
one line to **stdout**:

```text
Runtime error: <message>
```

For `panic` and `assert` the message is exactly the program's message. For
traps the VM detects, the message is informative and not part of conformance.
A VM SHOULD also report the qualified method name and source line of the
faulting instruction (from the chunk's line table) and the call stack, on
stderr so stdout stays comparable.

**Reference:** a host fault (D2) is an uncaught JVM exception. It prints a JVM
stack trace on stderr instead of the `Runtime error:` line, and the process
still exits 1. With `--trace` the reference turns every trap into a host panic.

---

## 9. Defects to settle

These are reference behaviours that look accidental. The exit criterion for
[Phase 1](scala-exit.md#phase-1--specify-the-pvm) is that each is either fixed in Scala, so both VMs agree, or
deliberately adopted into this spec. Until one or the other happens, the
differential tests in Phase 3 have to treat these cases as known
differences.

| # | Reference behaviour | Proposed resolution |
| --- | --- | --- |
| D1 | A failed `cast` prints `Runtime error:`, pushes `Unit`, and continues. `performCast` discards `runtimeError`'s result. | Trap. |
| D2 | These are host exceptions, not traps: stack overflow; `div`/`rem` by zero; an unknown opcode; `brfalse`/`brtrue` on a non-`Bool`; an `Int` instruction given another kind; `ceq` on `Unit` or mixed kinds; an invalid metadata flag; a file I/O failure. All of them still exit 1. | Turn each into a trap with a message. |
| D3 | `Path.nameWithoutExtension` crashes on a name with no `.` and no separator, and returns the whole path when the final component has no `.`. It recognises only the host separator. | Return the final component with its last extension removed. Also accept `/` on Windows. |
| D4 | `int(string)` wraps on overflow and parses `"-"` as 0. | Trap on both. |
| D5 | `bool(string)` traps in the VM, but the Scala shim used by `pncs`-compiled code returns `s.toLowerCase == "true"`. The two compilers disagree. | Make the shim trap too, or make the binder reject a `string` argument. The compiler does not call it. |
| D6 | `File.readAllBytes` returns **signed** bytes, −128 to 127 (`Byte.toInt`). The binder comment says "one byte per element", and `PnbFile.fromBytes` masks each element with `& 255` to cope. | Return 0–255. `PnbFile` keeps working either way. |
| D7 | `getTypeDefSize` gives the **last** type definition one slot fewer than the rule in [§2.4](#24-type-definitions) (`fields.size − 1`). It is harmless while the last type is never instantiated. That holds today but is not checked. | Use the rule. |
| D8 | A `char` is an `Int`, so `println(c)` prints its code and `isinst char` is never true. | Adopt. This is a language decision, not a VM fix. |
| D9 | An array's `Ref` carries its element type's token, so `string(array)` prints the element type, and `isinst` on an array tests the element type. | Adopt for now. Revisit if arrays get a type of their own. |
| D10 | `stdout` uses the JVM's line separator and default charset. On Windows that is CRLF and, on Java 17, the locale's code page. Unpaired surrogates encode as `?`. | LF and UTF-8 on every platform, with `?` for unpaired surrogates. |
| D11 | `newarr` with a negative size reports "Out of memory allocating array". | Trap with "negative array size". |
| D12 | `run` on an image with no entry point starts executing at offset 0. Tests may depend on this. | Keep it for in-process tests only. `pvm` rejects such images. |

---

## 10. Conformance

A VM conforms when, for every image and argument list, it produces the same
stdout bytes and the same exit code as the reference, outside the cases in §9
that are still open. The suites that exercise this, and that Phase 3's
differential runner replays through a second VM, are:

- `VmTests`, `CalliTests`, `BuiltinTests`, `GarbageCollectorTests`,
  `PnbTests`, `SampleTests`, and `StdlibTests`.
- Stage 3: the compiler image compiling `pnc/src` to a byte-identical image,
  plus the stdlib user-program checks.

New behaviour in the VM SHOULD land together with an update to this document,
a test in one of those suites, and, if it changes the image, a version bump
([§2.2](#22-layout)).
