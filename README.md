# GENIE3-on-DREAM4

## Introduction
GENIE3 (Huynh-Thu et al.) uses random forest variable importance to recover gene regulatory networks, and was the best performer in the DREAM4 *In Silico Multifactorial* challenge. This project does the following:

**(1)** Implements the GENIE3 gene regulatory network algorithm using base R functions

**(2)** Analyses data from the DREAM4 In Silico Size 100 Multifactorial subchallenge

**(3)** Compares performance to that of the original GENIE3 algorithm which was the best performer in this subchallenge

**GENIE3 Publication link:** http://journals.plos.org/plosone/article?id=10.1371/journal.pone.0012776

**DREAM4 Data link:** http://dreamchallenges.org/project/dream4-in-silico-network-challenge/

*In Progress* **(4)** Improving algorithm by accounting for indirect associations that may lead to spurious detections

## Running the Algorithm

R scripts are provided for both the original and new implementation of GENIE3. Just make sure that you change the "directory" object to match the directory where the data files are located. After that the script should run and save results to the same directory.

**genie3_original.R** Original implementation, directly from the creator

**genie3_spurgeon.R** New implementation

## Performance Assessment

I compare the original and Spurgeon implementations of GENIE3 using precision vs recall (PR) curves. PR curves and the area underneath curve (AUC) are calculated for all 5 datasets - these results are plotted and saved in png files with names beginning with "Performance_PrecisionRecall_DREAM4_Dataset". The Spurgeon implementation has greater AUC in 4/5 datasets.

## Discussion

Although these implementations are based on the same basic algorithm and use mostly the same random forest parameters (notably number of candidate variables per split, number of trees in forest, & node split criterion), minor differences remain. At present the only difference I can identify is that the randomForest function from which the original GENIE3 algorithm grows a random forest imposes a minimum node size of 5 samples. The Spurgeon implementation allows for no fewer than 10% of the total sample size to be in a single node, which means that trees will on average be smaller except at small sample sizes. This nodesize stopping criterion is one characteristic of random forest that prevents overfitting - therefore, the Spurgeon implementation will be less prone to overfitting than the default original GENIE3 implementation. However, this does not make it globally superior since growing large trees can have benefits in certain systems.

Another factor to consider is the stochastic nature of random forest. Due to the different architectures used to grow the random forest, it would be very difficult to assure that the same candidate variables were chosen at every node in both implementations. Since 5 datasets is relatively few in the realm of performance benchmarking, current information does not allow us to say how much of the difference in performance is due to stochasticity and how much is due to minor differences between the two GENIE3 algorithms.
