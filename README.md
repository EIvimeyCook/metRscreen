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

## Coming soon

Adding a new argument which allows for non-simultaneous collaborative screening
(`collab.names = c()`).

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

If metRscreen helps with your work, please cite it (adjust the year as needed):

> Ivimey-Cook, E. R. (2026). *metRscreen: Screening for meta-analysis and
> systematic reviews in R.* R package.
> <https://github.com/EIvimeyCook/metRscreen>

## License

Released under the [MIT License](LICENSE.md).

## AI Declaration

Claude Sonnet 4.6 was used in the latter stages of development.
