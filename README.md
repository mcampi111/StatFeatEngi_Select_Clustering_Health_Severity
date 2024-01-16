# NeuroDeficits-ML
Repository with data and code of the paper "Eye Movement Features for Clustering of Neurodevelopmental deficits with Machine Learning Approaches"

## **Abstract**

This study investigates vestibulo-ocular reflex (VOR) abnormalities in children with neurodevelopmental disorders (NDDs) using the functional Head Impulse Test (fHIT). With a focus on autism spectrum disorder (ASD), dyslexia (DYS), and attention-deficit/hyperactivity disorder (ADHD), the research addresses challenges in discriminating these disorders through VOR performances. Applied to 77 children (20 controls, 20 ADHD, 18 Dyslexic, 19 ASD), our statistical methodology overcomes discrimination issues by establishing significant sample descriptive statistics and employing machine learning techniques for clustering NDD deficits. The study introduces a cost-effective and widely applicable alternative for discriminatory testing, leveraging clinical measurements through fHIT data. Robust to small sample sizes and imbalances, the method provides a direct interpretation of feature selection and compatibility with supervised and unsupervised methods. Clustering results efficiently differentiate between ASD, ADHD, and DYS, emphasizing the high power of fHIT responses in solving NDD clustering tasks. Additionally, the study identifies the lateral (horizontal) and anterior planes as effective vestibular planes for clustering NDDs, offering insights into potential biomarkers for dyslexia, ADHD, and ASD. This research advances diagnostic methods for NDDs, providing a comprehensive approach to leveraging VOR abnormalities as discriminative features in clinical settings.


## Contributions of the paper
The paper has multiple contributions, at both methodological and applied level:
1. An automated computational methodology is implemented to classify NDDs (ASD, ADHD, dyslexia) and controls using VOR data. Unlike traditional modalities like FMRI or EEG, this method relies solely on clinical measurements, making it cost-effective and widely applicable. The approach is robust to small sample sizes and data imbalances, providing direct interpretation of feature selection, compatibility with supervised and unsupervised methods, and ease of replication on similar datasets. The results are interpretable, offering insights into potential biomarkers for neurodevelopmental disorders.

2. Efficient performances of the clustering solution proposed in differentiating between ASD, ADHD and DYS have been achieved. These rely on clinical attributes being fHIT responses assessing the VORs. Results improved with simulated bigger sample sizes hence confirming that the proposed clinical attributes have high power in solving the NDDs clustering task.
    
3. The most effective vestibular planes for clustering neurodevelopmental disorders (NDDs) are the lateral (horizontal) and anterior planes. These planes play crucial roles in spatial awareness, balance, sensory processing, emotional regulation, attention, and focus—key aspects relevant to ADHD, ASD, and dyslexia. The lateral plane, responsible for perceiving horizontal head movements, is particularly significant for tasks like reading and language acquisition, making it a distinctive feature in dyslexia. On the other hand, the anterior plane, associated with the anterior semicircular canal, plays a vital role in maintaining equilibrium and balance. Issues with this canal may impact stability and posture, indirectly affecting physical activities and learning, especially in the early stages of motor and postural control development. Consequently, the lateral plane is indicative of dyslexia, while the anterior plane is linked to ASD and ADHD. 


## Motivations For this Study

This study introduces a statistical tool for improved discriminatory testing, facilitating the differentiation of children with ASD, ADHD, and dyslexia (DYS) based on fHIT data. By employing specialised statistical machine learning feature extraction and inference methods, we demonstrate the ability to identify discriminative information crucial for classifying different NDDs, enabling medical experts to improve the accuracy of diagnoses. This work addresses a notable gap, as previous fHIT results assessments, based solely on direct output, lacked the capacity for effective NDD differentiation. The motivation for this study stands behind the idea of utilising clinical attributes that could possibly represent a potentional biomarker for NDDs. We select VOR attributes given the complex relationship between the vestibulo and the cerebellum.


## Organization of the Repository
The repository is organized in the following folders:

```diff
+ 1) Rfiles
```
This folder contains the R files derived during the analysis. One can find the generated features with parametric and non-parametric distribution with seval sample sizes for both the screened and the unscreened features and the results from the statistical tests. The original data are available under request.

```diff
+ 2) code 
```

1.  **clustering.R**. This file containes...
2. **data_analysis.R**. This file containes...
3.  **feature_engineering.R**. This file containes...
4.  **functions_param_bootstrap.R**. This file containes...


```diff
+ 3) figs 
```


## Cite

If you use this code in your project, please cite:

@article{..,
  title={..},
  author={..},
  journal={..},
  year={..}
}

