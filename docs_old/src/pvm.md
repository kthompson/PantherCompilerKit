# Panther VM

The Panther Virtual Machine (PVM) is designed to execute bytecode instructions for the Panther programming language. This document provides an overview of the stack types, op codes, and other relevant components of the PVM.

## Stack Types

The PVM supports several stack types, which are used to manage different kinds of data during execution:

- `i1`
- `i4`
- `i8`
- `ref array`
- `ref string`
- `ref object`

## Op Codes

| Op Code   | Operand Types | Pops      | Pushes | Description                                       |
| --------- | ------------- | --------- | ------ | ------------------------------------------------- |
| `nop`     |               | 0         | 0      | No operation                                      |
| `ldarg.n` | n/a           | 0         | 1      | Loads a function argument onto the stack          |
| `ldloc.n` | n/a           | 0         | 1      | Loads a local variable onto the stack             |
| `stloc.n` | n/a           | 1         | 0      | Stores the top of the stack into a local variable |
| `ldc.i4`  | i4            | 0         | 1      | Loads a constant integer onto the stack           |
| `dup`     |               | 1         | 2      | Duplicates the top value on the stack             |
| `pop`     |               | 1         | 0      | Removes the top value from the stack              |
| `call`    |               | arg count | 0 or 1 | Calls a function                                  |
| `ret`     |               | 1         | 0      | Returns from a function                           |
| `br`      |               | 0         | 0      | Unconditional branch                              |
| `brfalse` |               | 1         | 0      | Branch if false                                   |
| `brtrue`  |               | 1         | 0      | Branch if true                                    |
| `beq`     |               | 2         | 0      | Branch if equal                                   |
| `bgt`     |               | 2         | 0      | Branch if greater than                            |
| `bge`     |               | 2         | 0      | Branch if greater than or equal                   |
| `blt`     |               | 2         | 0      | Branch if less than                               |
| `ble`     |               | 2         | 0      | Branch if less than or equal                      |
| `add`     | i4            | 2         | 1      | Adds two numbers                                  |
| `sub`     | i4            | 2         | 1      | Subtracts two numbers                             |
| `mul`     | i4            | 2         | 1      | Multiplies two numbers                            |
| `div`     | i4            | 2         | 1      | Divides two numbers                               |
| `rem`     | i4            | 2         | 1      | Computes the remainder of division                |
| `and`     | i4            | 2         | 1      | Bitwise AND of two numbers                        |
| `or`      | i4            | 2         | 1      | Bitwise OR of two numbers                         |
| `xor`     | i4            | 2         | 1      | Bitwise XOR of two numbers                        |
| `shl`     | i4            | 1         | 1      | Shifts a number left                              |
| `shr`     | i4            | 1         | 1      | Shifts a number right                             |
| `neg`     | i4            | 1         | 1      | Negates a value                                   |
| `not`     | i4            | 1         | 1      | Bitwise complement                                |
| `ldstr`   | n/a           | 0         | 1      | Loads a string onto the stack                     |
| `newobj`  |               | arg count | 1      | Creates a new object                              |
| `ldfld`   |               | 1         | 1      | Loads a field of an object                        |
| `ldflda`  |               | 1         | 1      | Loads the address of a field                      |
| `stfld`   |               | 2         | 0      | Stores a value in a field                         |
| `ldsfld`  |               | 0         | 1      | Loads a static field                              |
| `ldsflda` |               | 0         | 1      | Loads the address of a static field               |
| `stsfld`  |               | 1         | 0      | Stores a value in a static field                  |
| `newarr`  |               | 1         | 1      | Creates a new array                               |
| `ldlen`   |               | 1         | 1      | Loads the length of an array                      |
| `ldelem`  |               | 2         | 1      | Loads an element from an array                    |
| `stelem`  |               | 3         | 0      | Stores an element in an array                     |
| `box`     |               | 1         | 1      | Boxes a value type                                |
| `unbox`   |               | 1         | 1      | Unboxes a value type                              |

## PNB Files

PNB files are the compiled version of the given set of sources for running in the PVM (Panther Virtual Machine). These files contain the bytecode that the PVM executes. The compilation process translates the high-level instructions into a lower-level, platform-independent representation that the PVM can interpret and run efficiently.

### Constant Table

The constant table holds a list of all constants used within a PNB file. These constants can include strings, numbers, and other immutable values that are referenced throughout the bytecode.
