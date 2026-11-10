# metRscreen 0.1.1 (development version)

Version 0.1.1, on the `development` branch. The stable version on `master` is
0.0.1.0000. Check which one you have with `packageVersion("metRscreen")`.

## New features

* **Collaborative screening.** `collab.names` now switches on a full
  collaborative mode (on `master` it only recorded a screener's name against each
  decision in one shared file):
  * each screener gets their own files (`<screen.file>_<name>_Screened.csv` and
    `<screen.file>_<name>_history.rds`), and you switch screener in the app;
  * other screeners' decisions are hidden by default, so screening stays
    independent, and can be shown with a switch;
  * a combined `<screen.file>_Collab_Summary.csv` flags agreements and
    conflicts;
  * screener names and reject reasons are remembered for the project, so later
    sessions can leave `collab.names` empty or add new screeners;
  * decisions from an earlier single-screener session can be claimed by a
    screener.
* **Double screening** with the new `collab.split = 2` argument: every paper is
  screened by exactly two screeners, spread evenly and at random across the team.
  The split is saved and reused, so it never reshuffles part-way through. Each
  screener sees only their own papers, and can browse all papers in a view-only
  mode.
* The left and right arrow keys move between papers.
* A shortcuts bar above the abstract lists the keyboard shortcuts.
* New `?` wildcard in keywords, matching exactly one letter or number (`wom?n` matches
  woman and women). It replaces the accidental regex meaning `?` had before,
  where `colou?r` meant an optional "u".
* Hovering over a keyword box shows how to separate words and use the `*` and
  `?` wildcards.

## Bug fixes

* A relative path for `screen.file` (e.g. `"data/refs.csv"`) did not work,
  because the app runs with its own folder as the working directory. Relative
  paths, `~` and `here::here()` paths now all work.
* Calling `metRscreen()` without `screen.file` printed an error but carried on.
  It now stops.
* Screening history was loaded from *any* `.rds` file in the same folder, so a
  different project's history could be picked up. Only this file's own
  `_history.rds` is now used.
* The app failed to start on R versions before 4.4.0, because it used `%||%`,
  which base R only has from 4.4.0.
* `.ris` import:
  * abstracts wrapped over several lines kept only their first line;
  * the `A1` (author) and `N2` (abstract) tags were ignored;
  * empty tags such as `N1  -` were not recognised in some exports;
  * dates such as `2020///` or `2020/05/01` are now reduced to the year;
  * a `.ris` that had already been converted is no longer converted again over
    the existing `.csv`, so papers can't change under a session that has started
    (delete the `.csv` to import the `.ris` again).
* Keyboard shortcuts (`y`, `n`, `m`) fired with Ctrl/Cmd held down, clashing
  with browser and system shortcuts; did not work with Caps Lock on; and stopped
  working after ticking a reject reason. They now also respect greyed-out
  buttons.
* In the list of screened papers, a reference button could jump to the wrong
  paper whenever papers earlier in the file had not been screened yet.
* The reference buttons were re-wired after every decision, so one click could
  run several times as a session went on.
* If the reference file no longer matched a saved single-screener session (e.g.
  a paper had been added), the app warned you to revert but then overwrote
  `_Screened.csv` and `_history.rds` with nothing. These files are now left
  untouched.
* Keyword highlighting:
  * `*` now matches zero or more letters or numbers, so `parent*` also highlights
    "parent";
  * a word straight after a highlighted word was skipped (e.g. only the first
    and third of "parents parents parents");
  * keywords such as "span", "style" or "the" could match inside the HTML added
    for other keywords and break the display;
  * keywords containing characters such as `+`, `(` or `.` caused an error or
    matched the wrong thing; they are now matched literally;
  * terms separated by a comma without a space were not split, although the
    help said to use no space; spaces are now optional;
  * accented letters (e.g. café) did not match wildcards.
* Holding down `y`, `n` or `m` repeated the key, recording the same decision on
  each paper that came up. One press is now one decision.

## Other changes

* Documentation for `screen.file`, `collab.names` and the new `collab.split`
  argument expanded, and the README now covers collaborative screening, the
  files it produces and working as a team.
