#!/usr/bin/env node
// Validate that every code/docs path in docs/architecture/primitives.yaml
// resolves on disk. Run after editing the map (adding/removing/reshaping a
// primitive) — see SKILL.md's source-of-truth rules.
import { checkPrimitivesMapPaths, loadPrimitivesMap } from './primitives-map.mjs'

const map = loadPrimitivesMap()
const errors = checkPrimitivesMapPaths(map)

if (errors.length === 0) {
	console.log(
		`OK: ${map.primitives.length} primitive(s), ${map.invariants.length} invariant(s), all code/docs paths resolve.`,
	)
	process.exit(0)
}

console.error(`${errors.length} problem(s) in docs/architecture/primitives.yaml:`)
for (const error of errors) {
	console.error(`  [${error.kind} ${error.id}] ${error.path}: ${error.reason}`)
}
process.exit(1)
