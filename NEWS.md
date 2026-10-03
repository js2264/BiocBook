# BiocBook 1.11.3

## New features

- `from_bookdown()` migrates a `bookdown` book to a `BiocBook` in one call. It
  creates the book package from the template, as `init()` does, and converts
  the `bookdown` project into it: pages (with parts and appendices),
  cross-references, figure layout options and `msmbstyle` question/solution
  blocks, bibliographies, CSS and images, `DESCRIPTION` (title, authors,
  dependencies), and the setup of the single R session `bookdown` renders
  every chapter in. `MIGRATION.md` lists how often each rule was applied and
  what still needs to be done by hand.
- The template's `biocbook` GitHub workflow can be run manually on any branch:
  it then builds the book against Bioconductor devel without pushing its
  `Docker` image or deploying it, so that a branch can be checked before it
  is merged. Deployments also index the `llms.txt` of every deployed version
  of a book in `docs/llms.txt`.

## Bug fixes

- `chapters()` drops header attributes (e.g. `{#sec-intro}`) from chapter
  titles.
- The template's question and answer callouts are styled again: `quarto`
  (>= 1.3) drops extra classes from callouts, so they are now wrapped in a
  `.callout-question` or `.callout-answer` div.
- The template's `Dockerfile` retries the `quarto` download on transient
  errors (e.g. HTTP 503 from GitHub), which failed a book's build before
  anything was installed.

## Documentation

- `?BiocBook-python` and the template's `requirements.yml` explain why
  `matplotlib` >= 3.11 cannot be imported on the Bioconductor images, and how
  to pin it.

# BiocBook 1.11.2

## New features

- New books are created from template 1.1.0:
    - `python` chunks run through `reticulate` (`python.reticulate: true`), in
      the `conda` environment declared in `inst/requirements.yml`, which now
      uses the `conda-forge` and `bioconda` channels only and pins `python`;
    - the book's `Docker` image provisions that environment when a page runs
      `python`, and receives `GITHUB_PAT` as a BuildKit secret, so that
      GitHub-only dependencies no longer hit the API rate limit (#6);
    - the `biocbook` GitHub workflow uses the current major version of every
      action (`actions/upload-artifact@v3`, which GitHub has rejected since
      January 2025, made every new book's workflow fail), and can pin the
      `quarto` version a book is built with;
    - books serve an `llms.txt` file for AI assistants (see below);
    - PDF output is no longer configured.
- `enrich_llms_txt()`: books built with `quarto` >= 1.11 serve `/llms.txt`
  and a Markdown copy of every page, for AI assistants. Template 1.1.0
  switches this on (through an `llms` `quarto` profile, only when the
  installed `quarto` supports it), and runs `enrich_llms_txt()` after every
  render to add the book package, its `Docker` image, its `python`
  environment and its other versions to `llms.txt`. With an older `quarto`,
  `enrich_llms_txt()` writes a minimal `llms.txt` listing the chapters.
- `python_envs()` lists the `conda` environments `setup_python()` has cached
  on a machine, and `python_envs(remove = ...)` deletes them.
- `add_python_chapter()` declares `BiocBook` and `reticulate` in the book's
  `Suggests`, and pins the new page to the `knitr` engine.

## Bug fixes

- `setup_python()` keys each `conda` environment on the contents of
  `requirements.yml` (`envs/<name>-<hash>`). Editing the file now gives a new
  environment, rather than silently re-using the existing one on machines
  that already had it, and two books only share an environment when they
  declare exactly the same thing.

# BiocBook 1.11.1

## New features

- `BiocBook`s can execute `python` code: `add_python_chapter()` adds a page
  whose `python` chunks run through `reticulate`, in a `conda` environment
  that `setup_python()` creates from `inst/requirements.yml` while the book
  builds, including on the Bioconductor builders.
- `micromamba()` finds a `micromamba` binary, or downloads a pinned and
  checksummed one, so that books can build their `python` environment on
  machines without `conda`.

## Bug fixes

- `add_chapter()` and `add_preamble()` insert new pages under the
  `chapters:` entry of `_book.yml` wherever it is, rather than assuming it
  sits on the third line.
