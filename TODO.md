# 1. Calibrator should allow to generato own calibration methods

<<interface>>
Calibrator
+ new(calibration_data)
+ predict(calibration_data)

calibration data: data.table with columns "truth", "response"

is implemented by

- CalibratorPlatt
- CalibratorIsotonic
- CalibratorBeta
