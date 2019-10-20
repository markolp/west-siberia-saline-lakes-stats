## Multivariate statistical analysis for understanding of chemical evolution of saline lakes of western Siberia

Major ion and trace elements chemistry of fifty-eight samples of southern Siberia (Russia) were analysed with a suit of statistical techniques, in an effort to explain the key processes affecting geochemical evolution of saline lakes. The database included data about groundwater, cell water, river water and lakes water. 

  According to performed cluster analysis of chemical data, all lakes in the area can be divided in to 5 main groups and their chemical composition very similar to rivers and deep groundwater. Shallow groundwaters are more enriched in Ca2+ and Mg2+ content. 
  
  Statistical analysis was conducted using R version 3.5.1 with addition of several packages. 
  
  The dataset used in this analysis constitutes a data frame of 58 observations by 25 chemical parameters: pH, Eh, HCO3, CO3, SO4, Cl, Ca, Mg, Na, Li, B, Al, Si, P, V, Cr, Mn, Fe, Cu, As, Br, Sr, Rb, Th, U. Data normality was evaluated using Kolmogorov–Smirnov (K–S) test before the PCA analysis, and non-normally distributed data were logarithmically transformed. The standardization was also applied to ensure that each variable has equal weight in the statistical analyses.
  
  Principal component analysis (PCA) identified four main principal components, with the first principal component (PC1) accounting for 44.3% and representing the process of salinization, and PC2 and PC3 (17.9 and 8.6% of total variance) controlled by water-rock interaction process of carbonates/sulphates precipitation and aluminosilicates formation.
 
<img width="1085" alt="image" src="https://user-images.githubusercontent.com/40539651/67161164-1c5c6680-f358-11e9-9dd2-494856c9e7df.png">
 
Principal component analysis: variable loadings for (a) PC1 (Dim.1) and PC2 (Dim.2) principal components and (b) PC1 and PC3 (Dim.3) with varimax rotation. The arrows are colored by the level of their contribution to the principal components. 
  
Full article can be found [here](https://github.com/markolp/west-siberia-saline-lakes-stats/blob/master/e3sconf_wri-162018_07012.pdf).
