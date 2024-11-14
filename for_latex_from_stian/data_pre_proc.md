The data required some pre-processing before we could perform adequate analysis on it. Of the 14 variables, in the original dataset, seven were numeric, and 7 categorical. Of the categorical variables, SEX, FBS, and exercise-induced angina (ANe),  were already binary, and were therefore left untouched. The final variables, CP, resting ECG, STs, CA and Thal, needed to be dealt with. We decided to binarize CP and STs, and turn into ordered resting ECG, CA, and Thal. 

CP originally had four categories; typical pain, atypical pain, non-anginal pain, and asymptomatic. We coded typical pain as 0, and the rest as 1. STs originally had three categories; Up, down, and flat. After some reading, we coded Up as 0 and Down & Flat as 1. 

Resting ECG was turned into a factor with three levels. CA was reported as "number of vessels", and therefore allowed to be turned directly into an ordered factor with 4 levels. Tha was turned into a factor with three levels. The original data was coded as; 3 - no defect, 7 - reversible defect, and 6 - fixed defect. After some reading we decided to code this as 0 - no defect, 1 - reversible defect, and 2 - fixed defect, as it seemed that "fixed defect" was actually more akin to "chronic" or "non-reversible" defect, and that there seemed to be a sliding scale between the two; that patients could be anywhere on the gradient between no defect, reversible defect, and fixed defect.

All numeric data was scaled to have mean 0 and variance 1.

This resulted in the final data you see below:
$$
\begin{tabular}{lrrrrrrrrr} 
\hline name & idx & nobs & type & exo & user & mean & var & nlev & lnam \\
\hline AGE & 1 & 297 & numeric & 0 & 0 & 0.000 & 1.000 & 0 & \\ 
SEX & 2 & 297 & binary & 0 & 0 & 0.677 & 0.219 & 0 & \\ 
CP & 3 & 297 & binary & 0 & 0 & 0.923 & 0.072 & 0 & \\
BPr & 4 & 297 & numeric & 0 & 0 & 0.000 & 1.000 & 0 & \\
Cho & 5 & 297 & numeric & 0 & 0 & 0.000 & 1.000 & 0 & \\
FBS & 6 & 297 & binary & 0 & 0 & 0.145 & 0.124 & 0 & \\
ECr & 7 & 297 & ordered & 0 & 0 & NA & NA & 3 & 0|1|2 \\
HRm & 8 & 297 & numeric & 0 & 0 & 0.000 & 1.000 & 0 & \\
ANe & 9 & 297 & binary & 0 & 0 & 0.327 & 0.221 & 0 & \\
STd & 10 & 297 & numeric & 0 & 0 & 0.000 & 1.000 & 0 & \\
STs & 11 & 297 & binary & 0 & 0 & 0.532 & 0.250 & 0 & \\
CA & 12 & 297 & ordered & 0 & 0 & NA & NA & 4 & 0|1|2|3 \\
Tha & 13 & 297 & ordered & 0 & 0 & NA & NA & 3 & 0|1|2 \\
HD & 14 & 297 & binary & 0 & 0 & 0.946 & 1.524 & 0 & \\
\hline \end{tabular}
$$
