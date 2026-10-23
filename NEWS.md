# metRscreen (development version)

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

## Other changes

* Documentation for `screen.file`, `collab.names` and the new `collab.split`
  argument expanded, and the README now covers collaborative screening, the
  files it produces and working as a team.
