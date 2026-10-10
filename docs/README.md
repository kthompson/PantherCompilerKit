# Panther documentation

The language documentation site uses Astro and Starlight. Pages live under
`src/content/docs/`; the site configuration is in `astro.config.mjs`.
Architecture notes and ADRs live separately under `architecture/`.

Run these commands from the repository root:

```sh
pnpm --dir docs install
pnpm --dir docs dev
```

| Command | Purpose |
| --- | --- |
| `pnpm --dir docs dev` | Start the local documentation server |
| `pnpm --dir docs build` | Build the static site into `docs/dist/` |
| `pnpm --dir docs preview` | Preview the built site |
| `sbt "doccheck/run docs/src/content/docs"` | Check Panther examples with the compiler |

Every Panther fence is checked unless explicitly annotated. The latest check
reports 201 blocks: 199 pass, 0 fail, and 2 are skipped for unsupported
higher-order functions. The failure baseline is empty and enforced in CI.
See [doccheck](../tools/doccheck/README.md) for directives and limitations.

Keep language examples consistent across the topic pages and the existing
`guides/` pages until those duplicates are consolidated. `docs_old/` is an
archived mdbook tree; add new documentation here.

See [CONTRIBUTING.md](../CONTRIBUTING.md) for the development workflow and
[ROADMAP.md](../ROADMAP.md) for current compiler capabilities and open work.
