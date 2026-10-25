---
layout: home

hero:
  name: Panther
  text: A small, self-hosting language
  tagline: Panther compiles to bytecode for the Panther Virtual Machine, and its compiler is written in Panther.
  actions:
    - theme: brand
      text: Get Started
      link: /language/basics/getting-started
    - theme: alt
      text: PVM Specification
      link: /architecture/pvm
    - theme: alt
      text: GitHub
      link: https://github.com/kthompson/PantherCompilerKit

features:
  - title: Language guide
    details: Variables, functions, flow control, and data types. Every example is checked against the compiler in CI.
    link: /language/basics/
  - title: Panther Virtual Machine
    details: The .pnb image format and the instruction set, written precisely enough to build a second VM.
    link: /architecture/pvm
  - title: Design records
    details: The decisions behind generics, traits and given evidence, equality, and garbage collection.
    link: /architecture/adr/0001-generic-type-inference
---
