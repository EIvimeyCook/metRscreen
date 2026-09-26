<p align="center">
  <img src="https://github.com/EIvimeyCook/metRscreen/blob/master/inst/metRscreen/www/logo/metRscreen.png" width = "200"/>
</p>

<div align="center">
 <h1>metRscreen</h1>
</div>

<!-- badges: start -->
[![License: MIT](https://img.shields.io/badge/license-MIT-green)](LICENSE.md)
<!-- badges: end -->

metRscreen is an R package providing a Shiny app for **title and abstract
screening in meta-analyses and systematic reviews**. It presents each reference
one at a time, highlights your keywords in multiple colours so relevant terms
jump out of the abstract, and records an include/reject decision — with a
reason — for every paper.

Screening is the slowest and most error-prone stage of a systematic review.
Hundreds or thousands of abstracts have to be judged consistently against the
same criteria, often across several sessions and sometimes several people.
metRscreen keeps that process in one place: decisions are saved as you go,
rejection reasons are chosen from a list you define rather than typed freehand,
and the whole session state is restored when you reopen the app.

It works best with references exported as a `.csv` from Zotero, but should work
with most references given in `.csv` format — be sure that your data matches the
arguments in the app. **It now also accepts the `.RIS` format from Zotero.**

```r
Title of MS        = "Title"
Authors of MS      = "Author"
Year of MS         = "Publication.Year"
Publishing Journal = "Publication.Title"
Abstract of MS     = "Abstract"
Keywords of MS     = "Manual.Tags"
```

## Features

- **One paper at a time.** Title, authors, year, journal, abstract, and keywords,
  with components you can show or hide to suit your screening style.
- **Multi-colour keyword highlighting.** Pre-load terms per colour so inclusion
  and exclusion signals are visually distinct at a glance.
- **Custom rejection reasons.** Define your own list and select several at once,
  so exclusions are auditable rather than a single undifferentiated "no".
- **Resumable sessions.** An `.rds` file records your decisions and is reloaded
  automatically next time, returning the app to exactly the state you left it in.
- **Collaborative screening.** Name your screeners and each gets their own
  decision files, so two or more people can screen the same references
  independently, switch between screeners in the app, optionally reveal each
  other's decisions, and get a combined file that flags conflicts.
- **Keyboard shortcuts.** `y` = accept, `m` = no decision, `n` = reject.
- **Zotero `.csv` and `.RIS` support.** Export straight from your reference
  manager.
- **Wildcards in keywords.** `parent*` matches parent, parental, parenting.
- **Runs locally.** Your reference library never leaves your machine.

## Installation

metRscreen is not on CRAN, but you can install the development version from
GitHub using the devtools package:

```r
install.packages("devtools")
devtools::install_github("EIvimeyCook/metRscreen")
library(metRscreen)
```

## Usage

The only function is `metRscreen()`. The most important argument is
`screen.file`. This can be used in conjunction with the `here::here()` package to
enable relative pathing.

```r
library(metRscreen)
metRscreen(screen.file = "~/Desktop/Example.csv")
```

## Arguments

| Argument | Purpose |
| :------- | :------ |
| `screen.file` | Path to your `.csv` or `.RIS` of references |
| `reject.list` | Character vector of your own rejection reasons |
| `collab.names` | Character vector of screener names; switches on [collaborative screening](#collaborative-screening) |
| `collab.split` | How papers are shared out in collaborative mode: `"all"` (default, everyone screens every paper) or `2` ([double screening](#splitting-papers-between-screeners-double-screening)) |
| `keywords` | Named list of keyword vectors, one element per highlight colour |

You can give specific rejection reasons as a vector with `reject.list = c()`, and
select multiple rejection reasons together. You can also specify preloaded
keywords using the `keywords` argument — note that to specify multiple keywords,
include a `","` in addition to the former keyword in the sequence (e.g. `"old,"`,
`"young"`).

```r
metRscreen(
  screen.file = "~/Desktop/Examples.csv",
  reject.list = c("no control", "wrong study system"),
  keywords    = list(green = c("old,", "young"), red = c("parent*"))
)
```

## Resuming a screening session

The `.rds` file that's produced allows for reloading of previous screening
decisions. This will be automatically reloaded the next instance metRscreen is
run and will return screening to the same state — the same reject list, hidden or
showing paper components, and any previous screening decisions.

## Collaborative screening

Systematic reviews usually need at least two people to screen every reference
independently, with disagreements resolved afterwards. Collaborative mode
supports this: give metRscreen the names of your screeners and each person's
decisions are kept in their own files.

```r
metRscreen(
  screen.file  = "~/Desktop/refs.csv",
  reject.list  = c("no control", "wrong study system"),
  collab.names = c("Ed", "Joel")
)
```

### How it works

1. **Choose who is screening.** Pick your name under **Who is screening?**.
   Decisions (buttons or keyboard shortcuts) are blocked until a screener is
   chosen, so nothing is recorded against the wrong person.
2. **Screen independently.** Each screener has their own decisions, comments,
   keyword searches and place in the list. Their name is recorded in the
   `Screen.Name` column of their file.
3. **Switch screener at any time.** Selecting a different name saves the
   current screener and loads the other one, returning to the paper they were
   last on. A half-typed comment or ticked reject reason is cleared so it can't
   be recorded against the next person.
4. **Show or hide other screeners' decisions.** These are **hidden by
   default**, so screening stays blind. Turn on **Show other screeners'
   decisions** to see, for the current paper, each other screener's decision,
   reject reason and comment, and whether you agree (`Agree`, `Conflict` or
   `Incomplete`). While switched on it refreshes every few seconds, so it picks
   up collaborators screening at the same time from a shared or synced folder
   (e.g. Dropbox, OneDrive).
5. **Resolve conflicts.** Every decision updates a combined summary file (see
   below) that flags where screeners disagree. It is also rebuilt whenever a
   screener is chosen, so it picks up decisions synced or pulled from others.

### Splitting papers between screeners (double screening)

By default every screener screens every paper. With a larger team that is often
more work than needed, so `collab.split = 2` shares the papers out so that
**each paper is screened by exactly two people**:

```r
metRscreen(
  screen.file  = "~/Desktop/refs.csv",
  collab.names = c("Ed", "Joel", "Shinichi", "Malgorzata"),
  collab.split = 2
)
```

- **Even and random.** Papers are assigned at random, and every screener gets
  the same number of papers (give or take one). Every pair of screeners also
  shares a similar number of papers, so no two people only ever check each
  other. With 100 papers and four screeners, each person screens 50.
- **Made once, then fixed.** The split is saved as
  `refs.csv_collab_assignment.csv` (paper, title and its two screeners) the
  first time and reused every session after, so it never reshuffles part-way
  through. You don't need to pass `collab.split` again. Your own random-number
  seed is not affected.
- **You only see your own papers.** After choosing your name, **Next**,
  **Previous** and each decision move through your papers only, the progress
  line shows e.g. `3 of your 50 papers`, and decisions on a paper not assigned
  to you are blocked. **Show other screeners' decisions** shows the one other
  person assigned to that paper, and **Assigned to:** shows the paper's two
  screeners.
- **Browse all papers (view only).** Turn this on to read every paper, e.g. to
  look over the whole set or talk through a conflict. Next and Previous then go
  through all papers, but the decision buttons and keyboard shortcuts are
  switched off, even for your own papers, so nothing can be recorded while
  browsing. Turn it off to go back to the paper you were on. Other screeners'
  decisions stay hidden unless **Show other screeners' decisions** is also on.
- **Summary file.** An `Assigned.To` column lists each paper's two screeners,
  papers not assigned to someone show `Not assigned` in their `.Screen`
  column, and `Agreement` compares only the two assigned screeners.
- **With exactly two screeners** `collab.split = 2` is the same as `"all"`:
  both screen every paper.
- **To go back to everyone screening everything**, pass `collab.split = "all"`
  (the saved split is kept but not used). To make a new split, e.g. after adding
  a screener, delete `refs.csv_collab_assignment.csv` before any screening has
  started. A screener added after the split has been made has no papers until
  the split is remade.

### Files produced

For `screen.file = "refs.csv"` and `collab.names = c("Ed", "Joel Pick")`, these
are written next to `refs.csv`:

| File | Contents |
| :--- | :------- |
| `refs.csv_Ed_Screened.csv` | Ed's decisions, one row per reference |
| `refs.csv_Ed_history.rds` | Ed's resumable session |
| `refs.csv_Joel-Pick_Screened.csv` | Joel Pick's decisions (spaces and punctuation in names become `-`) |
| `refs.csv_Joel-Pick_history.rds` | Joel Pick's resumable session |
| `refs.csv_Collab_Summary.csv` | Everyone's decisions side by side plus an `Agreement` column |
| `refs.csv_collaborators.rds` | The screeners for this project |
| `refs.csv_collab_assignment.csv` | Only with `collab.split = 2`: the two screeners for each paper |

The summary has `Title`, `Author`, `Publication.Year` and `Publication.Title`,
(and `Assigned.To` with `collab.split = 2`), then `<name>.Screen`,
`<name>.Reason` and `<name>.Comment` for each screener, and finally `Agreement`:

- `Agree`: every screener of the paper has screened it and made the same decision
- `Conflict`: every screener of the paper has screened it but the decisions differ
- `Incomplete`: at least one screener of the paper hasn't screened it yet

("Every screener of the paper" means everyone, or the two assigned screeners
with `collab.split = 2`.)

To pull out the conflicts to discuss:

```r
# tidyverse
library(readr)
library(dplyr)

summary_dat <- read_csv("~/Desktop/refs.csv_Collab_Summary.csv")
conflicts <- summary_dat |>
  filter(Agreement == "Conflict")
count(summary_dat, Agreement)

# base R
summary_dat <- read.csv("~/Desktop/refs.csv_Collab_Summary.csv")
conflicts <- summary_dat[summary_dat$Agreement == "Conflict", ]
table(summary_dat$Agreement)
```

### Working as a team

Every screener only ever writes their own two files
(`refs.csv_<name>_Screened.csv` and `refs.csv_<name>_history.rds`), so the
project can be shared in whichever way suits your team. Whichever you choose,
everyone should use the same version of metRscreen.

**On one computer.** Run `metRscreen()` once and switch between screeners
under **Who is screening?**.

**In a shared or synced folder** (Dropbox, OneDrive, SharePoint, Google Drive
for desktop, or a network drive):

1. Put `refs.csv` in the shared folder. One person runs `metRscreen()` once
   with `collab.names` (and `collab.split = 2` if you're splitting the papers)
   to create the project files. Let them finish syncing before anyone else
   starts.
2. Everyone else runs `metRscreen()` on the same file from their own computer,
   with no other arguments. The screeners and the split are read from the
   shared files. The path will differ on each computer, e.g.
   `metRscreen("~/Dropbox/Review/refs.csv")`.
3. People can screen at the same time. With **Show other screeners'
   decisions** on, you see others' decisions for your current paper within a
   few seconds of them syncing.

Keep to one app window per screener, and don't edit `refs.csv` once screening
has started (metRscreen refuses screening files that no longer match it). If
two people save at the same moment, the sync service may create a "conflicted
copy" of `refs.csv_Collab_Summary.csv`. It's safe to delete, because the
summary is rebuilt from everyone's own files.

**With GitHub** (e.g. using GitHub Desktop). Use a private repository if your
references or decisions shouldn't be public yet.

1. **One person sets up the project.** Add `refs.csv` to the repository, run
   `metRscreen()` once with `collab.names` (and `collab.split = 2` if
   splitting) and close the app. Add a `.gitignore` file containing
   `*_Collab_Summary.csv`, then commit and push `refs.csv`, `.gitignore`,
   `refs.csv_collaborators.rds` and, if splitting,
   `refs.csv_collab_assignment.csv`. Set this up **before** anyone starts, so
   everyone uses the same split.
2. **Each screener clones the repository** and runs `metRscreen()` on their
   copy of `refs.csv` with no other arguments.
3. **Pull before you start, and commit and push your own two files when you
   stop.** Because nobody else edits your files, pulls and merges don't
   conflict.
4. **To see everyone's decisions,** pull, open metRscreen and choose your
   name. The summary is rebuilt from all the screeners' files you have. The
   summary is ignored by git because it changes whenever anyone screens, and
   would otherwise cause merge conflicts.

With GitHub you only see other people's decisions once they have pushed and
you have pulled. Only the person who set up the project should add screeners,
and they should commit the updated `refs.csv_collaborators.rds` straight away.

### Good to know

- **Screener names are remembered.** In later sessions you can leave
  `collab.names` out, or pass only new names to add screeners.
- **Names must be distinct once spaces and punctuation are removed.** For
  example, `"Joel Pick"` and `"Joel-Pick"` would share files, so `metRscreen()`
  stops with an error rather than mixing their decisions.
- **Reject reasons are shared by the whole project.** Keyword searches and
  shown/hidden fields are saved per screener.
- **Upgrading from a shared session.** If you previously screened with
  `collab.names` in a single shared file, each person's earlier decisions (and
  your keywords and reject reasons) are copied into their own file the first
  time they are chosen. The old files are left untouched.
- **Each screener should work in one app window at a time.** Several people can
  screen at once from a shared folder, but the same screener shouldn't have two
  sessions open, or the last one to save wins.
- **Without `collab.names` nothing changes.** Single-screener projects still
  produce `refs.csv_Screened.csv` and `refs.csv_history.rds`.

## Bug reports and contributions

Please file issues and feature requests at
<https://github.com/EIvimeyCook/metRscreen/issues>. Pull requests are welcome.

## Related tools

- [**shinyDigitise**](https://github.com/EIvimeyCook/shinyDigitise) — extract data
  from published figures once screening is done
- [**DCQC**](https://github.com/EIvimeyCook/DCQC) — data and code quality control
  checklist for editors and reviewers
- [**READMEBuilder**](https://github.com/EIvimeyCook/READMEBuilder) — document the
  resulting project for archiving

## Citation

If metRscreen helps with your work, please cite it:

> Ivimey-Cook, E. R. (2026). *metRscreen: Screening for meta-analysis and
> systematic reviews in R.* R package.
> <https://github.com/EIvimeyCook/metRscreen>

A machine-readable [`CITATION.cff`](CITATION.cff) is included, so GitHub's
"Cite this repository" button gives formatted APA and BibTeX.

## License

Released under the [MIT License](LICENSE.md).

## AI Declaration

Claude Sonnet 4.6 was used in the latter stages of development. Claude
(Anthropic) was used to develop and test collaborative mode.
