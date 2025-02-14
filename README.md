# StatFeatEngi_Select_Clustering_Health_Severity
Repository with data and code of the paper "Statistical Feature Engineering and Selection for Clustering-Based Decision Rules in Health Severity Models". The paper is given at this url ...

## **Abstract**

Accurate classification of continuous health measurements into discrete diagnostic categories poses major challenges across clinical domains. We propose a statistical framework that integrates multiple tests, robust feature engineering, and unsupervised machine learning to address these issues. While motivated by hearing-loss assessment—where pure tone thresholds and speech-recognition scores help identify distinct impairment levels—our methodology generalizes to other contexts with complex measurement structures. Systematic feature extraction from raw audiological tests (pure tone thresholds at various frequencies, speech-in-quiet, and speech-in-noise measures) produces a higher-dimensional space. We then apply k-means and hierarchical clustering to these engineered features, revealing clinically relevant subgroups within hearing-loss categories. Silhouette scores confirm that combining multiple measures significantly improves discrimination compared to relying on pure tone averages alone. By reconciling continuous biological variation with the need for discrete labels, our approach yields heightened interpretability and accommodates data imbalance. Even moderate datasets can produce clinically informative clusters when guided by robust statistical feature engineering. Overall, this work provides a blueprint for integrating diverse clinical tests into a cohesive analytical framework, offering broader relevance to any medical setting where complex measurements must be translated into actionable diagnostic insight.


## Contributions of the paper
The paper has multiple contributions, at both methodological and applied level:
1. An automated computational methodology is implemented to classify hearing loss categories using multiple audiological measurements. The method proposed is unsupervised. Unlike traditional approaches that rely primarily on pure tone averages, this method integrates multiple audiological tests, making it more comprehensive and clinically informative. The approach is robust to small sample sizes and data imbalances, providing direct interpretation of feature selection, compatibility with supervised and unsupervised methods, and ease of replication on similar datasets. The results are interpretable, offering insights into potential markers for different types and degrees of hearing loss.
2. Efficient performances of the clustering solution proposed in differentiating between hearing loss categories have been achieved. These rely on clinical attributes being both pure tone thresholds and speech recognition measurements. Results improved with simulated bigger sample sizes hence confirming that the proposed clinical attributes have high power in solving the hearing loss classification task.
3. The most effective audiological measurements for clustering hearing loss categories are identified through our framework. Pure tone thresholds at specific frequencies and speech recognition scores in different conditions play crucial roles in hearing assessment. Pure tone thresholds, particularly at speech frequencies (500-4000 Hz), are significant for basic hearing sensitivity assessment. On the other hand, speech recognition thresholds, especially in noise, provide vital information about functional hearing ability. The framework reveals which combinations of these measurements are most discriminative for different degrees of hearing loss.    

## Motivations For this Study

This study introduces a statistical tool for improved discriminatory testing, facilitating the differentiation of hearing loss categories based on multiple audiological tests. By employing specialised statistical machine learning feature extraction and inference methods, we demonstrate the ability to identify discriminative information crucial for classifying different degrees of hearing loss, enabling audiologists to improve the accuracy of diagnoses. This work addresses a notable gap, as previous hearing loss assessments, based solely on pure tone averages, may not fully capture the complexity of auditory function. 

## Organization of the Repository
The repository is organized in the following folders:

```diff
+ 1) Rfiles
```
This folder contains the R files derived during the analysis. One can find the generated features with parametric and non-parametric distribution with seval sample sizes for both the screened and the unscreened features and the results from the statistical tests.

```diff
+ 2) code 
```

1.  **clustering.R**. This file containes the functions and code implemented to perform the K-Means and the HWM in the main body of the paper.
2. **data_analysis.R**. This file containes the initial data analysis and wrangling generating the tsne results and the violin plots.
3.  **feature_engineering.R**. This file performs the statistical tests and the feature engineering procedure described in the main body of the paper.


```diff
+ 3) figs 
```
All the figure implemented through R are provided in this folder. The code used to generate them is in the code folder. 

## Cite

If you use this code in your project, please cite:

@article{..,
  title={..},
  author={..},
  journal={..},
  year={..}
}

