# Variable Names, Type, and Range


| Variable Name | Type | Range | Description | Acronym
| --- | --- | --- | --- | --- |
| 'subject_id' | Integer | 0-303 | ID of the subject | 'ID' |
| 'age'| Integer  | 29-77  |  Age of the Subject | 'AGE' |
|  'sex' | Categorical  | [0,1]  |  Sex of the Subject | 'SEX' |
| 'cp' | Categorical  | [1,2,3,4]  | Chest Pain Type  | 'CP' |
| 'trestbps'  |  Integer |  94-200 |  resting blood pressure | 'BPr' |
|  'chol' | Integer  | 126-564  | serum cholestoral  | 'Chol' |
|  'fbs' | Categorical  | [0,1]  |  fasting blood sugar > 120mg/dl| 'FBS' |
|  'restecg' | Categorical  | [0,1, 2]  | Categorical Outcome of resting ECG  | 'ECGr' |
| 'thalach'  | Integer  |  71-202 |  maximum heart rate achieved during Thalach test|'HRmax'  |
| 'exang'  | Categorical  | [0,1]  |  exercise induced angina | 'ANGe'|
| 'oldpeak'  | Float  |  0.0-6.2 |  ST depression induced by exercise relative to rest | 'STd' |
| 'slope' | Categorical | [1,2,3] | Slope of the ST curve | 'STs' |
| 'ca' | Integer | [0,1,2,3] | Number of Blood Vessels counted | 'CA' |
| 'thal' | Categorical  | [3,6,7]  | Categorical Outcome of Thalach Test | 'Thal' |
| 'num'  | Categorical  | [0,1,2,3]  | Diagnosis of Heart Disease |  'HD' |

## Description

* Age, BPr, Chol, HRmax, and STd are continuous
* FBS, ANGe, SEX are binary
* ECGr, CA, and HD are ordered categorical
* CP, STs, and Thal are unordered categorical
