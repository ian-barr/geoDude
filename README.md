### Working with Geometric Statistics

Geometric means, Standard deviations, 95% Confidence intervals, and SEMs are defined based on the log-transformed data.
If  we have a set of measurements $X_1, X_2, \dots X_n$, 

The geometric mean is $\exp{(\mu_g)}$ where 

$$\mu_g = \frac{1}{n}\sum_i^n  \ln(X_i)$$

The geometric standard deviation (GSD) is $\exp{(\sigma_g)}$ where

$$\sigma_g = \sqrt{\frac{\sum_i \left ( \ln(X_i) - \ln(\mu_g) \right )^2}{n-1} }$$

The geometric standard error of the mean (GSEM) is $\exp{(SEM_g)}$ where 

$$SEM_g = \frac{\sigma_g}{\sqrt{n}}$$

For a confidence level of 95%, for a number of datapoints $df$, the confidence interval is given by multiplying the $SEM_g$ by the 97.5% quantile of a $t$-distribution with $df= k-1$, then exponentiating to $\exp{\text{(lower, upper)}}$, where (lower, upper) are :

$$\text{(lower, upper)} = \mu_g \pm t(k-1, 0.975)\cdot SEM_g$$
   
This means that the ends of the confidence intervals are
   
$$\exp(\mu_g - t(k-1, 0.975)\cdot SEM_g), ~ \exp(\mu_g + t(k-1, 0.975)\cdot SEM_g)$$
   
Dividing the upper limit by the mean gives
   
$$\frac{\exp(\mu_g + t(k-1, 0.975)\cdot SEM_g)}{\exp(\mu_g )} = \exp(t(k-1, 0.975)\cdot SEM_g)$$
   
So, if we divide the upper interval limit by the Geometric Mean, calling that result $R$, we will be able to get an estimate of the $SEM_g$:

$$SEM_g = \frac{\ln(R)} {t(k-1, 0.975)}$$
   
Including the definition for $SEM_g$ lets us solve for $\sigma_g$:
 
$$\sigma_g = \frac{\sqrt{n}\cdot \ln(R)} {t(k-1, 0.975)}$$
  
- And since the geometric coefficient of variance (GCV) is $\text{GCV} = \sqrt{\exp{(\sigma_g^2)} ~– 1}$
We are able to solve for GCV.
