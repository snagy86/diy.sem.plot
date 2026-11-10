# diy.sem.plot 1.1.0

## New features

* New `template()` prints ready-to-run starter code for a fitted model: a
  `node_list`, `path_list`, `title_list`, `note_list` and the `diyPaths()` call.
* New `panel_note()` adds a note beneath a panel as a caption. `diyPaths()`
  gains `panel_notes` and `note_text_size` to support it.
* New `title_text_size` argument in `diyPaths()` controls panel title size.

## Changes

* Fixed a bug where `nudge_text_x` and `nudge_text_y` moved the variance arc rather than just the estimate text. 
* Fixed a bug where `side_from` and `side_to` in `path()` were applied to the
  wrong nodes when a covariance/correlation path was written in the reverse
  order to the `lavaan` syntax. The direction of
  `cov_curve` is unaffected by the order `from` and `to` are given in.

## Documentation

* Documentation and examples now point to `template()` as the recommended
  first step.

# diy.sem.plot 1.0.0

* Initial CRAN submission.
