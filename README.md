# Multivariate Screening and Feature Engineering for Statistical Decision Making in Disease Severity Diagnostics
Repository with data and code of the paper "Multivariate Screening and Feature Engineering for Statistical Decision Making in Disease Severity Diagnostics". The preprint of this paper is given at this url https://papers.ssrn.com/sol3/papers.cfm?abstract_id=5264112

## **Abstract**

Mapping multivariate continuous health measurements to discrete diagnostic or disease severity categories presents a persistent challenge in clinical practice, particularly with the increasing automation of diagnostics and the push toward standardized, evidence-based decision-making. We propose a three-stage statistical framework to address this challenge in a principled and generalizable manner. The methodology requires access to patient samples that include multivariate diagnostic test results alongside expert-assessed disease severity classifications, including the possibility of no disease. In the first stage, diagnostic tests are selected based on clinical focus, yielding a multivariate profile of physiological responses in the relevant diagnostic space. In the second stage, we transform these test responses into an abstract feature space through: (1) feature construction, (2) statistical evaluation and ranking of each feature's discriminatory power using hypothesis testing, and (3) bootstrapping to address class imbalance across disease severity categories and improve generalization. In the third stage, we learn a partitioning function from the feature space to discrete diagnostic or severity categories. This mapping can then be applied to new patients to support systematic, replicable disease classification. We illustrate this framework using audiological diagnostics, where hearing-loss severity categorization relies on combining pure-tone audiogram thresholds with speech-recognition scores. This domain, characterized by complex interdependencies between measurements, offers a compelling real-world test case for the framework's accuracy and utility. Overall, this approach offers a standardized, data-driven solution for translating multivariate diagnostic information into clinically actionable categories.


## Contributions of the paper
The paper has multiple contributions, at both methodological and applied level:
1. An unsupervised computational method is developed to classify hearing loss categories based on multiple audiological measures. Unlike conventional approaches, it integrates diverse tests, remains robust to small and imbalanced samples, enables interpretable feature selection, and can be adapted to both supervised and unsupervised tasks. The method provides clinically relevant insights into markers of hearing loss.
2. Strong clustering performance is demonstrated, using pure tone thresholds and speech recognition scores. Simulations with larger sample sizes confirm the high discriminatory power of the selected features.
3. The most effective audiological predictors are identified, with pure tone thresholds (500–4000 Hz) and speech recognition in noise emerging as key contributors to differentiating hearing loss severity.

## Motivations For this Study

This study introduces a statistical tool for improved discriminatory testing, facilitating the differentiation of hearing loss categories based on multiple audiological tests. By employing specialised statistical machine learning feature extraction and inference methods, we demonstrate the ability to identify discriminative information crucial for classifying different degrees of hearing loss, enabling audiologists to improve the accuracy of diagnoses. This work addresses a notable gap, as previous hearing loss assessments, based solely on pure tone averages, may not fully capture the complexity of auditory function. 

## Organization of the Repository
The repository is organized in the following folders:

```diff
+ 1) Rfiles
```
This folder contains the R files derived during the analysis. Note that the bigger files for bootstrap with 500 or 1000 are too big - hence these are available under request to the corresponding author of the paper.

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

@article{campi2025multivariate,
  title={Multivariate Screening and Feature Engineering for Statistical Decision Making in Disease Severity Diagnostics},
  author={Campi, Marta and Peters, Gareth and Morvan, Perrine and Buhl, Mareike and Thai-Van, Hung},
  journal={Available at SSRN 5264112},
  year={2025}
}

