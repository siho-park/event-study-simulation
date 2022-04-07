# event-study-simulation

This is a simple Monte Carlo simulation to show how to do event study analysis when there is anticipation. 

## Data structure

There are in total 20 states with 1 treated state and 19 untreated states. There are 12 periods from -6 to 5. Our main shock happens at period 0, but there will also be an anticipatory shock at period -1.

## Shock

Outcome variable is a sum of following terms.

- state specific shock: drawn from unif([50, 100])
- date specific shock: drawn from unif([-5, 5])
- tax shock: Positive shock of +5 in period -1 and negative shock of -5 in period 0
- IID shock: drawn from standard normal distribution

## Event study model

4 types of model will be estimated.

1. OLS event study model
2. Callaway-Sant'Anna event study with first treatment date = 0
3. Callaway-Sant'Anna event study with first treatment date = -1
4. Callaway-Sant'Anna event study with first treatment date = -2

## Results

The folder figures contain simulation results. Colored line shows the average of 1,000 coefficients for each period.

- OLS produces unbiased estimates. There is only one treated unit and OLS does not suffer from heterogeneous treatment effects, differential treatment timing, etc.

- CS estimator produces biased estimates when first treatment date = 0. 

This happens from period 0 and persists in the following periods. This shows that adjusting first treatment date is important when there is potential anticipatory effect.

- CS estimator produces unbiased estimates when first treatment date = -1. Adjusting the first treatment date to be the date when anticipation starts to kick in can remedy the problem.

- CS estimator still produces unbiased estimates when first treatment date = -2. 

This shows when we do not have exact knowledge of when the anticipation can start to affect our outcome, it is better to be conservative and choose earlier date as first treatment date. It will still generate unbiased estimates. Another way to look at it is as a robustness check. When we use one period earlier first treatment date, the event study figure has to be robust. If there is a large change in event study plots with this adjustment, this means there is an anticipation effect that we did not expect. In such situation, CS estimator produces biased estimates and we need to check underneath what is really going on.

