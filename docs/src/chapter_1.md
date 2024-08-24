# Chapter 1

## Parts of the Panther Compiler Kit

1. Panther to PVM compiler (in Panther)
   - `pnc`
2. Panther to PVM compiler (in PVM)
   - `pnc-bootstap`
3. PVM interpreter (in Panther)
   - `panther` command

## Bootstrapping

1. Panther to PVM compiler (in Scala)
   - using only a subset of scala that is also panther
   - `pncs`
   - panther -> pvm on Scala
2. Write the PVM interpreter in Scala
   - using only a subset of scala that is also panther
   - `panthers`
   - run pvm on scala
3. This lets us compile `pncs` to PVM to make `pnc-bootstrap`, and then run it on `panthers`
4. At this point we can build the PVM interpreter in Panther

# PVM

## stack types

i1
i4
i8
ref array
ref string
ref object

## op codes

| op code | operand types | pops      | pushes | description                   |
| ------- | ------------- | --------- | ------ | ----------------------------- |
| nop     |               | 0         | 0      |                               |
| ldarg.n | n/a           | 0         | 1      | loads a fn arg onto the stack |
| ldloc.n | n/a           | 0         | 1      | loads a local onto the stack  |
| stloc.n | n/a           | 0         | 1      | loads a local onto the stack  |
| ldc.i4  | i4            | 0         | 1      |                               |
| dup     |               |           |        |                               |
| pop     |               |           |        |                               |
| call    |               | arg count | 0 or 1 |                               |
| ret     |               |           |        |                               |
| br      |               | 0         | 0      |                               |
| brfalse |               | 1         | 0      |                               |
| brtrue  |               | 1         | 0      |                               |
| beq     |               |           |        |                               |
| bgt     |               |           |        |                               |
| bge     |               |           |        |                               |
| blt     |               |           |        |                               |
| ble     |               |           |        |                               |
| add     | i4            | 2         | 1      | add two numbers               |
| sub     | i4            | 2         | 1      | add two numbers               |
| mul     | i4            | 2         | 1      | add two numbers               |
| div     | i4            | 2         | 1      | add two numbers               |
| rem     | i4            | 2         | 1      | add two numbers               |
| and     | i4            | 2         | 1      | bitwise and two numbers       |
| or      | i4            | 2         | 1      | bitwise or two numbers        |
| xor     | i4            | 2         | 1      | add two numbers               |
| shl     | i4            | 1         | 1      |                               |
| shr     | i4            | 1         | 1      |                               |
| neg     | i4            | 1         | 1      | negate value                  |
| not     | i4            | 1         | 1      | bitwise complement            |
| ldstr   | n/a           | 0         | 1      | loads a string onto the stack |
| newobj  |               |           |        |                               |
| ldfld   |               |           |        |                               |
| ldflda  |               |           |        |                               |
| stfld   |               |           |        |                               |
| ldsfld  |               |           |        |                               |
| ldsflda |               |           |        |                               |
| stsfld  |               |           |        |                               |
| newarr  |               |           |        |                               |
| ldlen   |               |           |        |                               |
| ldelem  |               |           |        |                               |
| stelem  |               |           |        |                               |
| box     |               |           |        |                               |
| unbox   |               |           |        |                               |

## pnb files

### constant table
