# traits.build-book — agent & contributor guide

`traits.build-book` is the **`traits.build` user manual** — a Quarto book covering the
traits.build data standard, R package, and workflow, with best-practice advice and tutorials.
The rendered manual lives at <https://traitecoevo.github.io/traits.build-book/>.

## Repo-local guidance

- **Type:** a [Quarto book](https://quarto.org/docs/books/) project, **not** an R package
  (the top-level `DESCRIPTION`/`NAMESPACE` exist only to declare the R packages the chapters
  need at render time).
- **Layout:** a single `_quarto.yml` at the top level defines the book (title, parts, chapter
  order, HTML format). Chapters live in **`content/`**, one `.qmd` each — except `index.qmd`,
  which must stay at the project root because Quarto hard-fails a book without a root home page
  (`ERROR: Book contents must include a home page`). `figures/` and `data/` hold supporting
  assets; `_book/` and `_freeze/` are generated output/cache. (`references.bib` exists but is
  unused — no chapter declares a bibliography or cites anything.)
- **Chapters:** organised in `_quarto.yml` into parts — Introduction (`index.qmd`,
  `content/motivation.qmd`, `content/workflow.qmd`, ...), Data structure & standard, Creating
  with `traits.build`, a step-by-step Guide to adding data (`content/tutorial_dataset_1..7.qmd`),
  Using outputs, and Getting help; appendices `content/csv.qmd`, `content/yaml.qmd`.
- **Two settings the `content/` layout depends on**, both in `_quarto.yml`:
  `project: execute-dir: project`, without which each chapter executes in its own directory and
  the ~34 project-root-relative paths (`data/…`, `config/…`) all break; and a project-level
  `execute: freeze: auto`. Freeze **must** be declared in `_quarto.yml`, not a root
  `_metadata.yml` — that file does not reach chapters in a subdirectory, so freeze silently stops
  caching and every render re-executes all 38 chapters.
- **Renaming or moving a chapter changes its published URL**, and only five chapters are
  protected against that. `AusTraits_tutorial`, `tutorial_compilation`, `tutorial_datasets`,
  `database_structure` and `help` each carry an `aliases:` entry pointing at their pre-`content/`
  path, which emits a redirect stub at the old URL. Those five are the ones linked from outside
  this repo — from `austraits.org`, and from the READMEs of `austraits`, `traits.build`,
  `traits.build-template`, `austraits.build` and `ausinvertraits.build`. The other 32 chapters
  have no alias by decision (#38): old links to them are allowed to die. So if you rename one of
  the five, carry its alias forward; and if a chapter starts being linked from another repo,
  either give it an alias or accept that its URL is not stable.
- **Build / preview:** `quarto render` builds the book into `_book/`; `quarto preview` serves it
  with live reload. Rendering executes the `.qmd` code, so the R packages in `DESCRIPTION`
  (incl. `traits.build`, `austraits`, `APCalign`, tidyverse, `galah`, `sf`) must be installed.
- **Publishing:** a `render` GitHub Actions workflow builds the book (see the README badge); the
  rendered site is served from the `gh-pages` branch.
- **Default branch:** `master`.

## Building the PDF

The book also builds as a single PDF (~260 pages), but the `pdf` format lives in the **`_quarto-pdf.yml` profile**, not in `_quarto.yml`: `quarto render --profile pdf --to pdf`.

Three invariants here, each of which has already broken once:

- **Never move `pdf` into `_quarto.yml`.** A plain `quarto render` builds *every* format listed there, so a LaTeX failure takes the gh-pages deploy down with it. That is exactly why the block was commented out in Dec 2023 (`7507982`, "comment out pdf to deploy website"). The website workflow must never see the pdf format.
- **Never pass `full_width = TRUE` to `kable_styling()` on the latex path.** kableExtra renders it as a `tabu` environment, and `tabu` is unmaintained and fails against current LaTeX with `Undefined control sequence \tabu@cleanup`. Use `latex_options = "scale_down"` instead. Chapters that style tables define a latex `my_kable_styling()` and swap in `util_kable_styling_html` under `knitr::is_html_output()` — keep that shape.
- **Write cross-chapter links as `other_chapter.qmd#anchor`, never `other_chapter.html` and never a bare `.qmd`.** Only an anchored `.qmd` link resolves to an internal PDF destination; both a `.html` link and a bare `other_chapter.qmd` become external URIs that are dead links in the PDF. Every chapter H1 therefore carries `{#sec-<filename>}` (e.g. `# File organisation {#sec-file_organisation}`) so there is always an anchor to point at, and links to a whole chapter use that id. `@sec-<filename>` also works but substitutes "Chapter N" for your link text.

Because freeze results are stored per format (`_freeze/<chapter>/execute-results/tex.json` vs `html.json`), the HTML and PDF builds cannot share cache entries — the two CI workflows key their caches separately. The PDF is built by `.github/workflows/pdf.yaml` on published releases and on `workflow_dispatch` only, never on push: a full render executes the R in all 38 chapters.

Since a single file merges every chapter's anchors into one namespace, duplicate heading ids that are harmless across separate HTML pages collide in the PDF. Watch for `Duplicate identifier` warnings in the render log.

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

**Commit messages:** every family repo squash-merges, so the **PR title and body become the permanent
commit message**. Keep the subject ≤50 characters as typed and the body ≤10 lines; put the working
detail — what you tried, benchmarks, test counts, rejected alternatives — in the **first PR comment**
instead. Full convention:
[`commit-messages.md`](https://github.com/traitecoevo/austraits-meta/blob/master/governance/commit-messages.md).

> austraits-meta is hand-maintained prose — a map, not ground truth. Verify specifics against the
> actual repos.
