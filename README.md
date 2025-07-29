# MLclock
Machine learning scripts for estimation of sampling time from transcriptome datasets

PyCharm: Python 3.7.5 with following packages, Scikit-learn (0.21.3), LGBM (2.2.3), Keras (2.3.1), and TensorFlow (1.14.0).

Models for Random Forest (reg.MLclockRFmHT) and LightGBM (reg.MLclockGMmHT) was uploaded as ZIP.

### MLclockRFmHT.py
Parameter search for Random Forest

### MLclockLGBmHT.py
Parameter search for LightGBM

### score96.py
Leave-one-out cross-validation for Random Forest and LightGBM

### scoreNN.py
Leave-one-out cross-validation for NNdis and NNang

### score_importance.py
Importance estimation for NN models

## IntAmpR
R-scripts for estimating integrated amplitude from single-time-point transcriptome

## RNAseqAnalysis
R-scripts for RNA-seq analysis for DRG (diel rhythmic gene) detection
