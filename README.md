# How is personality pathology related to negative perceptions of others? (SRP 2021)
A repository for my research poster (**"How is personality pathology related to negative perceptions of others?"**) presented at the 2021 Society for Research in Psychopathology (SRP) conference. This repo contains substantive information regarding the content of the research project and files related to its statistical analyses (i.e., datasets, R syntax, R Markdown, R output).

# Table of Contents
* [Description and Methods](https://github.com/michaelcarnovale/SRP-2021-Poster#description-and-methods)
* [Files/Usage](https://github.com/michaelcarnovale/SRP-2021-Poster#files-in-this-repousage)
* [Poster and Summary of Results](https://github.com/michaelcarnovale/SRP-2021-Poster#full-summary-of-posterresults)

# Description and Methods
This project was concerned with two research questions:
1. Which personality pathology/personality disorder traits are associated with judging other people negatively?
2. Can these trait ratings from both the self and an informant help predict these negative perceptions?

This project also involved analyzing data using the following methodologies:
* Integrating responses from three different forms of data
  * Main participants (targets) provided self-reported ratings of personality pathology/personality disorder traits
  * Targets provided round-robin ratings of normal-range personality traits (i.e., targets in groups of 4-5 people they are unfamiliar with and subsequently rating each other after group games)
  * Informant-reported ratings of targets' personality pathology/personality disorder traits
* Various statistical models
  * Principal component analysis
  * Latent variable modeling/Structural equation modeling using the Structural-After-Measurement approach (see [here](https://osf.io/xme9g/) and [here](https://users.ugent.be/~yrosseel/lavaan/slides/rosseel_eam2021.pdf) for more info)
  * Regression/General linear modeling with robust standard errors and multiple testing corrections
  * Ordinal hypothesis tests (Jonckheere's Trend Test)

# Files in this Repo/Usage
* [Data folder](Data/)
  * .csv files of [baseline data](Data/Baseline_Data_Carnovale.csv) with self-reported personality pathology ratings, [informant data](Data/Informant_Data_Carnovale.csv) with informant-reported personality pathology ratings, and [round robin data](Data/RoundRobin_Data_Carnovale.csv) containing the round robin personality ratings
* [R folder](R/)
  * [Rendered R Markdown syntax and output viewable on GitHub](R/Rmd_Carnovale.md)
  * [The raw .R syntax file](R/R_Syntax_Carnovale.R)
  * [The raw .rmd R Markdown file](R/R_md_toc_SRP_2021_Carnovale.Rmd)
* A [.pdf](https://github.com/michaelcarnovale/SRP-2021-Poster/blob/main/Carnovale%2C%20Goghari%2C%20%26%20Carlson%2C%20SRP%202021.pdf) and a [.jpg](https://github.com/michaelcarnovale/SRP-2021-Poster/blob/main/Poster_Carnovale_Image.jpg) of the poster itself

If you would like to download the entire repo to your computer, one can either use the following command line (assuming Git is installed)
```
git clone https://github.com/michaelcarnovale/SRP-2021-Poster.git
``` 
or click the green 'Code' button above and download the repo as a zip file.
# Poster and Summary of Results
---
![Poster](https://github.com/michaelcarnovale/SRP-2021-Poster/blob/main/Poster_Carnovale_Image.jpg)

---

### Summary  
**Background:** *A perceiver effect is defined as a trait-like general tendency to perceive or judge others’ personality in particular ways – for example, generally judging others’ as rude. Latent variable modeling has also suggested that the variance in perceiver effects can be explained by a general positivity-negativity bipolar factor. Several studies have shown that various psychopathology constructs are associated with certain perceiver effects. For example, narcissism is related to perceiving others as less likeable. To date, however, no studies have examined the relations between perceiver effects and more contemporary models of personality pathology - specifically, within the Alternative Model of Personality Disorders for the DSM-5, the Hierarchical Taxonomy of Psychopathology, and as assessed by the Personality Inventory for DSM-5 (PID-5). These are the domain constructs of negative affect, detachment, antagonism, disinhibition, and psychoticism. Studying their relations with perceiver effects would specifically help further validate and clarify the constructs and their associated theories, as some of them are defined by interpersonal dysfunction and schemas - such as Antagonism most notably. We were interested in two questions: first, what domains of personality pathology are related to positive/negative perceptions of others; and second, who can predict these perceptions - the self and/or an informant?*

**Method:** *Our sample consisted of unacquainted undergraduate students who first completed self-reports of the PID-5-BF (brief form), were divided into groups of at least four to complete group-based games, and lastly, made first impression personality ratings of each other using the Ten Item Personality Inventory. Knowledgeable informants subsequently rated these same students on the PID-5-BF. Generally, the statistical analyses involved regressing perceiver effects on individual personality pathology domains and total scores in a series of univariate models. These analyses were done using ordinary regression and also structural equation modeling (SEM) using the Structural-After-Measurement approach which is more suitable for estimating SEMs in smaller sample sizes. Perceiver effect scores were estimated in two steps: first, the Social Relations Model was used to estimate perceiver effect scores for each TIPI item. Then, for the ordinary regression analyses, a single principal component was estimated on these scores thought to represent positivity-negativity, whereas for SEM, these scores loaded on a single latent variable.*

**Results:** *First, results corroborated that those who thought of themselves as having more antagonism, disinhibition, and general personality pathology tended to see others as generally more negative. The other self-reported domains and informant scores did not significantly predict perceiver effects. Further exploratory analyses suggested that, using Jonckheere’s trend test, the ordering of the Likert scale for 2/5 self-reported Antagonism items and 4/5 Disinhibition items had a significant increasing monotonic relation with negative perceptions.*

**Conclusions:** *In conclusion, the results support the theoretical negative interpersonal schemas in the constructs of Antagonism and Disinhibition - as assessed by the PID-5-BF. Results are inconclusive with respect to informant reports - however, this may be due to low statistical power. There are many future directions that research projects can take – for example, studies may want to look at whether these negative perceptions result in actual observable negative interpersonal behaviours.*



 <!-- badges: start -->
  [![Launch Rstudio Binder](http://mybinder.org/badge_logo.svg)](https://mybinder.org/v2/gh/michaelcarnovale/SRP-2021-Poster/main?urlpath=rstudio)
  <!-- badges: end -->
