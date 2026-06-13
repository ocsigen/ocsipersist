# How the Ocsipersist documentation is generated

The Ocsipersist documentation published at <https://ocsigen.org/ocsipersist/>
is built with **odoc** and themed with the Ocsigen site chrome by
[**wodoc**](https://github.com/ocsigen/wodoc) (an odoc driver). The same odoc
sources are also what ocaml.org renders.

## Sources

| What | Where | Format |
|---|---|---|
| API | the `.mli` of the `ocsipersist*` packages (frontend + lib + the pgsql/sqlite/dbm backends and their server configs) | odoc comments |
| Overview landing | [`doc/index.mld`](index.mld) | odoc page |
| Site configuration (nav, …) | [`doc/wodoc`](wodoc) | wodoc config (S-expression) |

Ocsipersist ships several packages (a common interface plus the pgsql/sqlite/dbm
backends), so the API is built with a plain `dune build @doc --profile release`
(no client/server split, no odoc-driver). The page theming and the left
navigation — which package goes in which section — are declared in
[`doc/wodoc`](wodoc) (`(packages …)` + `(nav …)`). See the
[wodoc README](https://github.com/ocsigen/wodoc) for the config syntax.

## Build

```
wodoc build --config doc/wodoc --label dev --out _doc-site/dev \
  --menu https://ocsigen.org/doc/menu.html
```

`wodoc build` runs `dune build @doc --profile release`, assembles every page into
the Ocsigen site (shared header/menu/drawer, the version `<select>`, the left
navigation from `doc/wodoc`). `--menu` is fetched from its single canonical copy
in `ocsigen.github.io`. Add `--local` to also fetch the shared `/css//img/`
assets and preview offline.

## Deployment (CI)

[`.github/workflows/doc.yml`](../.github/workflows/doc.yml) builds and publishes
to the project's **`gh-pages`** branch (served at `ocsigen.org/ocsipersist/`).
On **push to `master`** it rebuilds and deploys the **`dev`** docs only. Each run
replaces only the `dev/` directory; the other version directories already on
`gh-pages` are preserved.

## Releasing a stable version

The CI builds only `dev/`. To publish a stable version, trigger the
**Documentation** workflow's `release` job with the **version** input — either:

- **CLI** (from a clone of the repo): `gh workflow run doc.yml -f version=1.2.3`
- **GitHub UI**: repo → *Actions* → *Documentation* (left sidebar) → *Run workflow*
  (top-right) → set **version** (e.g. `1.2.3`) → *Run workflow*.

The `release` job freezes the current `dev/` docs as `/<version>/`, repoints the
`latest` symlink, writes the root redirect and refreshes `versions.json` — via
`wodoc release --from dev --version <version>`. No rebuild: the docs of a release
are exactly the `dev` docs at that point.

Equivalently, by hand on a `gh-pages` checkout:

```
wodoc release --site . --from dev --version <version>
git add -A && git commit -m "Release doc <version>" && git push
```
