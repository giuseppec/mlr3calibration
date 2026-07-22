# mlr3calibration 0.1.0.9000

* This release redesigns the package around native `mlr3` dictionaries and extends calibration from binary learners to binary and multiclass learners and graphs.
* Calibration diagnostics are now first-class `mlr3` measures, adding adaptive, classwise, confidence, top-label, Cox, Hosmer-Lemeshow, and Spiegelhalter measures; the standalone `ece()` and `ici()` functions have been retired.
* `calibration_plot()` replaces `calibrationplot()` and provides a consistent interface for plotting learners or existing predictions.
* `Calibrator`, `clb()`, and the calibrator registry provide a common extension API, built-in Platt, beta, and isotonic calibration, one-vs-rest multiclass support, and calibrator selection during tuning.
* `PipeOpCalibrate` (`po("calibrate")`) replaces the four previous calibration PipeOps with unified out-of-fold and per-fold strategies, support for learners and graphs, optional reuse of resample results, and compatibility with outer tuning.
* `tune_then_calibrate()` provides a convenience workflow for sequentially tuning a learner and calibrating the selected configuration.
