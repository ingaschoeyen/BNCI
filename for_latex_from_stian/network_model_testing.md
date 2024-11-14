We tested our network structure in two phases. First, we computed a polychoric correlation matrix $M$ for our dataset using the `lavCor` functionality of `lavaan`. Using this matrix, we next used the `dagitty` `localTests` function to test the measured covariances against the implied conditional independencies of our network model.  

$$\begin{tabular}{lrrrr}
& estimate & p-value & 2.5 $\%$ & 97.5 $\%$ \\
\hline
AGE \ci CA $\mid$ FBS, HD & 0.4009385 & 3.916471e-13 & 0.3006682 & 0.4947189 \\
SEX \ci Tha $\mid$ CA, CP, ECr, HD & 0.4193977 & 2.709924e-14 & 0.3204311 & 0.5120506 \\
SEX \ci Tha $\mid$ FBS, HD     & 0.4217148 & 1.520743e-14 & 0.3233197 & 0.5138878 \\
AGE \ci CA $\mid$ BPr, HD      & 0.4237024 & 1.099998e-14 & 0.3254938 & 0.5157212 \\
SEX \ci Tha $\mid$ CA, CP, HD, STd & 0.4277333 & 7.001323e-15 & 0.3295535 & 0.5197366 \\
SEX \ci Tha $\mid$ BPr, CP, ECr, HD & 0.4372212 & 1.425789e-15 & 0.3399646 & 0.5284846 \\
SEX \ci Tha $\mid$ BPr, CP, HD, STd & 0.4416928 & 6.606333e-16 & 0.3448817 & 0.5326075 \\
SEX \ci Tha $\mid$ BPr, CP, HD, HRm & 0.4497803 & 1.591165e-16 & 0.3537920 & 0.5400649 
\hline \end{tabular}$$

In figure, we show the most egregious of these violations. As can be seen, most involved either a conditional independence assumption between SEX and Tha, or AGE and CA. Therefore, we fixed these one by one, confirming that after adding an edge between SEX and Tha, that the violations involving AGE and CA still held. 

After making these changes, we fit the new model. Then, given the new fit, we examined the coefficients of the fit, and for any of the coefficients that had an absolute value smaller than $0.01$, we removed that edge from the graph. This resulted in only one edge being removed, namely the edge from FBS to HD, which is interesting in its own right.

After this, we ended up with our final network model [include final network model here, which is the one from my_pruned_dag.txt]