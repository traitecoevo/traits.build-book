# traits.build-book — agent & contributor guide

`traits.build-book` is the **`traits.build` user manual** — a Quarto book covering the
traits.build data standard, R package, and workflow, with best-practice advice and tutorials.
The rendered manual lives at <https://traitecoevo.github.io/traits.build-book/>.

## Repo-local guidance

- **Type:** a [Quarto book](https://quarto.org/docs/books/) project, **not** an R package
  (the top-level `DESCRIPTION`/`NAMESPACE` exist only to declare the R packages the chapters
  need at render time).
- **Layout:** a single `_quarto.yml` at the top level defines the book (title, parts, chapter
  order, HTML format), and each chapter is one `.qmd` source file. `_metadata.yml` holds shared
  chapter options; `references.bib` is the bibliography; `figures/` and `data/` hold supporting
  assets; `_book/` and `_freeze/` are generated output/cache.
- **Chapters:** organised in `_quarto.yml` into parts — Introduction (`index.qmd`,
  `motivation.qmd`, `workflow.qmd`, ...), Data structure & standard, Creating with
  `traits.build`, a step-by-step Guide to adding data (`tutorial_dataset_1..7.qmd`), Using
  outputs, and Getting help; appendices `csv.qmd`, `yaml.qmd`.
- **Build / preview:** `quarto render` builds the book into `_book/`; `quarto preview` serves it
  with live reload. Rendering executes the `.qmd` code, so the R packages in `DESCRIPTION`
  (incl. `traits.build`, `austraits`, `APCalign`, tidyverse, `galah`, `sf`) must be installed.
- **Publishing:** a `render` GitHub Actions workflow builds the book (see the README badge); the
  rendered site is served from the `gh-pages` branch.
- **Default branch:** `master`.

> Heads-up: the PDF `format` in `_quarto.yml` is commented out — the book is HTML-only as
> configured. Don't assume `quarto render --to pdf` works without re-enabling that block.

---

## AusTraits family — cross-package context

`traits.build-book` is part of the **AusTraits family** (a subset of the
[`traitecoevo`](https://github.com/traitecoevo) org) — here, the traits.build user manual / book.
Family-wide concerns are documented centrally in
**[austraits-meta](https://github.com/traitecoevo/austraits-meta)** — don't restate them here, read
them there:

- **Start with [`AGENTS.md`](https://github.com/traitecoevo/austraits-meta/blob/main/AGENTS.md)** —
  pipeline order, who owns what, dependency direction, source-of-truth rules, cross-boundary
  artifacts, gotchas.
- **[`dependencies.yml`](https://github.com/traitecoevo/austraits-meta/blob/main/dependencies.yml)** —
  machine-readable package graph + cross-boundary artifacts.
- **[`governance/`](https://github.com/traitecoevo/austraits-meta/tree/main/governance)** —
  label taxonomy, board #9 conventions, release playbooks, triage.

**Filing issues:** the whole family is tracked on one board,
[AusTraits #9](https://github.com/orgs/traitecoevo/projects/9) (new issues auto-add to it). Follow
the [issue & labelling guide](https://github.com/traitecoevo/austraits-meta/blob/main/governance/issue-guide.md):
pick one work-type label (`bug` / `task` / `epic`); Status and Priority are set on the board, not as
labels.

> austraits-meta is hand-maintained prose — a map, not ground truth. Verify specifics against the
> actual repos.
