### Working with Geometric Statistics

Geometric means, Standard deviations, 95% Confidence intervals, and SEMs are defined based on the log-transformed data.
If  we have a set of measurements $X_1, X_2, \dots X_n$, 

The geometric mean is $\exp{(\mu_l)}$ where 

$$\mu_l = \frac{1}{n}\sum_i^n  \ln(X_i)$$

The geometric standard deviation (GSD) is $\exp{(\sigma_l)}$ where

$$\sigma_l = \sqrt{\frac{\sum_i^n \left ( \ln(X_i) - \ln(\mu_l) \right )^2}{n-1} }$$

The geometric standard error of the mean (GSEM) is $\exp{(sem_l)}$ where 

$$sem_l = \frac{\sigma_l}{\sqrt{n}}$$

For a confidence level of 95%, for a number of datapoints $df$, the confidence interval is 
given by multiplying the $sem_l$ by the 97.5% quantile of a $t$-distribution with $df= k-1$, 
then exponentiating to $\exp{\text{(lower, upper)}}$, where (lower, upper) are :

$$\text{(lower, upper)} = \mu_l \pm t(k-1, 0.975)\cdot sem_l$$
   
This means that the ends of the confidence intervals are
   
$$\exp(\mu_l - t(k-1, 0.975)\cdot sem_l), ~ \exp(\mu_l + t(k-1, 0.975)\cdot sem_l)$$
   
Dividing the upper limit by the mean gives
   
$$\frac{\exp(\mu_l + t(k-1, 0.975)\cdot sem_l)}{\exp(\mu_l )} = \exp(t(k-1, 0.975)\cdot sem_l)$$
   
Calling that result $R$, we will be able to get an estimate of the $sem_l$:

$$sem_l = \frac{\ln(R)} {t(k-1, 0.975)}$$
   
Including the definition for $sem_l$ lets us solve for $\sigma_l$:
 
$$\sigma_l = \frac{\sqrt{n}\cdot \ln(R)} {t(k-1, 0.975)}$$
  
And since the geometric coefficient of variance (GCV) is $\text{GCV} = \sqrt{\exp{(\sigma_l^2)} ~– 1}$
We are able to solve for GCV, and Convert between the different statistics.

### Functions Defined:

`GMEAN` : Calculates Geometric Mean of a set of data points.

`GSD` : Calculates Geometric Mean of a set of data points.

`GSEM` : Calculates Geometric Mean of a set of data points.

`GCV2GCI` : Converts Geometric Coefficient of Variation (expressed as a percentage if `percent = TRUE`) to Confidence interval.

`kGCV2GSD` : Converts Geometric Coefficient of Variation (expressed as a percentage if `percent = TRUE`) to Confidence interval using the definition of Kirkwood (1982).

`GCV2GSD` : Converts Geometric Coefficient of Variation (expressed as a percentage if `percent = TRUE`) to Geometric Standard Deviation.

`GCI2GCV` : Converts Geometric Confidence Interval to Geometric Coefficient of Variation (expressed as a percentage if `percent = TRUE`).

`GCV2GSEM` : Converts Geometric Coefficient of Variation (expressed as a percentage if `percent = TRUE`) to Geometric Standard Error of the Mean

`GSD2GSEM` : Converts Geometric Standard Deviation to Geometric Standard Error of the Mean


