# Power_analysis_simulations_for_RCT
This provides power analysis simulations for an RCT with one and with two treatment arms

## Folders
Project is organized into three folders:
1. code scripts for simulations. Contains simulations for treatments with one and two arms 
2. data generated from the simulations 
3. plots of power x sample sizes for each type of RCT

## How it works?

For the treatment with a single treatment arm, we follow the steps:

1. Define the sample sizes and the number of simulations to run for each treatment effect and each sample size 
2. Define the hypothetical treatment effects (in our case, these will be the minimum detectable effects - MDEs) 
3. Construct our simulated sample: 
3.1. Potential outcomes for not receiving the treatment (Y0) are taken from a normal distribution with mean zero and standard deviation one (this makes sense for this experiment, given that outcomes are standardized this way) 
3.2. Potential outcomes for receiving the treatment (Y1) are Y0 + treatment effect
3.3. Generate a random vector of treatment assignments $T$, used to constructed the observed outcomes for each observation in the sample ($$Y = T*Y1 + (1 - T)*Y0$$) 
4. Run OLS regression of $$Y$$ on $$T$$ and save the coefficients and p-value
5. Repeat steps 3 to 4 for each treatment effect, each sample size and each simulation 
6. Compute the number of times we rejected the null hypothesis for each treatment effect and each sample size - this will be our power for that effect and sample size

The difference in the treatment with two treatment arms is that we will

2. Define a pair of hypothetical MDEs 
3. Construct a sample divided into three groups (Control, Treatment 1, Treatment 2) 
4. Run 3 OLS regressions: Treatment 1 x Control, Treatment 2 x Control, Treatment 1 x Treatment 2
