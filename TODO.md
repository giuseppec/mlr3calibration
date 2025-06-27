# 1. Calibrator should allow to generate own calibration methods

<<interface>>
Calibrator
+ new(calibration_data, task)
+ predict(calibration_data, task)

calibration data: data.table with columns "truth", "response"

is implemented by

- CalibratorPlatt
- CalibratorIsotonic
- CalibratorBeta

what is still to do

- create a dictionary?
- refactor the code using these calibrators

----------------
New ideas
----------------

- Calibrators should inherit from learner (example s. https://github.com/mlr-org/mlr3learners/blob/main/R/LearnerClassifLogReg.R)
- Calibrator lives in a Calibrator dict, similar to here (https://github.com/mlr-org/mlr3data/blob/main/R/zzz.R)

- open questions:
  - how does autotuner help in OOF, PerFold?? - not used yet
  - what do we do with rr object (usual learner does only take resampling --> rename resampling)

# 2. Unifiy per fold, OOF, TuneFirst approaches

<< interface>>
CalibrationStrategy
+ new()
+ train()
+ predict()

is implemented by

- CalibrationStrategyOOF
- CalibrationStrategyperFold
- CalibrationStrategyTuneFirstOOF
- CalibrationStrategyTuneFirstPerFold




# 3. Put everything together in one PipeOp

gets as arguments

- string for Calibrator
- CalibrationStrategy (also as a string?)

