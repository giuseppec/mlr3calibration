# 1. Calibrator should allow to generate own calibration methods

<<interface>>
Calibrator
+ new(calibration_data)
+ predict(calibration_data)

calibration data: data.table with columns "truth", "response"

is implemented by

- CalibratorPlatt
- CalibratorIsotonic
- CalibratorBeta

what is still to do

- create a dictionary?
- refactor the code using these calibrators

# 2. A
