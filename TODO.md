the repo: https://github.com/giuseppec/mlr3calibration/tree/perfold_oof_tunefirst


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

what is still to do --> done (04.07.2025)

- create a dictionary?
- refactor the code using these calibrators

----------------
New ideas
----------------

- Calibrators should inherit from learner (example s. https://github.com/mlr-org/mlr3learners/blob/main/R/LearnerClassifLogReg.R)
- Calibrator lives in a Calibrator dict, similar to here (https://github.com/mlr-org/mlr3data/blob/main/R/zzz.R)

# 2. Unifiy per fold, OOF, TuneFirst approaches

- open questions:
  - DONE how does autotuner help in OOF, PerFold -- we are getting an optimal (tuned first) learner via the autotuner and then predict on this
  - DONE what do we do with rr object (usual learner does only take resampling --> rename resampling)

<< interface>>
CalibrationStrategy

params: learner, rr, method, resampling parameters, param_vals
BUT: OOF does not have param rr --> DONE

+ new()
+ train()
+ predict()

is implemented by

- CalibrationStrategyOOF
- CalibrationStrategyperFold
- CalibrationStrategyTuneFirstOOF
- CalibrationStrategyTuneFirstPerFold

idea: glue them together in one interface class, make them accessible via a dictionary as well, have one dictinory
should they inherit from learner as well? Or from pipeline? --> They already do this

--> We would only need a dictionary with all of them in it




# 3. Put everything together in one PipeOp

gets as arguments

- string for Calibrator
- CalibrationStrategy (also as a string?)



# issues to discuss:

## I am getting this warning in BeatCalibration PerFold:

lm.fit: fitted probabilities numerically 0 or 1 occurred
...is not necessarily "bad", but it's something to be aware of.

📌 What It Means
This warning comes from the logistic regression model (glm with family = binomial) used internally by your calibration step (likely PipeOpCalibration). It means that during fitting, some predicted probabilities are very close to 0 or 1, which can cause numerical instability in the estimation process (e.g., due to log-likelihood values going to ±infinity).

In this case, the warning happened during:

PipeOp classif.rpart's $train()
This likely means:

Your base learner (classif.rpart, i.e., decision tree) gave hard 0 or 1 probabilities.
Then, PipeOpCalibration tried to calibrate these using a logistic regression model.
But logistic regression struggles when the inputs are already extreme (close to 0/1), because there's no useful gradient to learn from.

## autotuner in TuneFirst approach

for tune first, we need to pass an auto tuner, simple learner is not supported

- in OOF we enforce this
- in inFold, we don't, that is why I removed at$learner

--> How do we want to handle it?

## Furhter features

- possibility for own calibration metrics --> does not really need new implementation, maybe rather a dictionary that assigns names
- Cox calibration slope and intercept
- HL test --> done

how metrics work

