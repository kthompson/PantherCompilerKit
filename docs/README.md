# Panther documentation

The documentation site uses VitePress. Language pages live under `language/`,
and architecture notes and ADRs under `architecture/`; both are published. The
home page is `index.md` and the site configuration, including the sidebar, is
in `.vitepress/config.mts`. Add new pages to the sidebar there.

Run these commands from the repository root:

```sh
pnpm --dir docs install
pnpm --dir docs dev
```

| Command | Purpose |
| --- | --- |
| `pnpm --dir docs dev` | Start the local documentation server |
| `pnpm --dir docs build` | Build the static site into `docs/.vitepress/dist/`; fails on dead links |
| `pnpm --dir docs preview` | Preview the built site |
| `sbt "doccheck/run docs/language docs/architecture"` | Check Panther examples with the compiler |

Every Panther fence is checked unless explicitly annotated. The latest check
reports 197 blocks: 184 pass, 0 fail, 6 are skipped and 7 are ignored, because
the snippet elides its body or uses a feature that is not built yet. The
failure baseline is empty and enforced in CI.
See [doccheck](../tools/doccheck/README.md) for directives and limitations.

Links from architecture pages to source files or `ROADMAP.md` keep their
repository-relative paths, so they work on GitHub; the site rewrites any link
that is not a site page to the GitHub copy of the file.

See [CONTRIBUTING.md](../CONTRIBUTING.md) for the development workflow and
[ROADMAP.md](../ROADMAP.md) for current compiler capabilities and open work.
