## Cross-Over Analysis

![Cross-Over Analysis](/cross-over-analysis.png)  
*Figure 1: Cross-over analysis: Sequence Effect and Treatment Effect*

Both t-tests on sequence and notation return no significant differences in means (mean_duration).  
Welch Two Sample t-test  
data: mean_duration by sequence  
p-value = 0.9047  

Welch Two Sample t-test  
data: mean_duration by notation.r  
p-value = 0.4056  

Null Hypothesis: there is a significant difference in means between NL-KV / KV-NL sequences --> this means there is a carry over effect (use only period 1)  
Alternative Hypothesis: there is no significant difference in means between NL-KV / KV-NL sequences --> this means there is NO carry over effect (use both periods)  
p-value > 0.05 shows that there is no significant difference in means (mean_duration) between NL-KV and KV-NL sequence

Interpretation from plot and t-tests would be that there seems to be a period effect, no carry-over or sequence effect as treatment duration differences are not significantly different for both periods.
