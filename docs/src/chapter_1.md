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
   - panther -> pvm in Scala
   - also support transpiling `pncs` to panther (`pnc`)
2. Write the PVM interpreter in Scala
   - using only a subset of scala that is also panther
   - `panthers`
   - run pvm on scala
3. This lets us compile `pncs` to PVM to make `pnc-bootstrap`, and then run it on `panthers`
4. At this point we can build the PVM interpreter in Panther
