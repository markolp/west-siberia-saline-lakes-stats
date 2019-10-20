setwd("~/R")

library(readxl)
library(ggplot2)
library(gridExtra)
library(ggpmisc)
library(corrgram)
library(dplyr)
library(pcaMethods)
library(hydrogeo)

# read xls file
database <- read_excel("WRI/databaseall.xls", sheet = "databaseall")
View(database)

# filter data for Lake Waters (LW)
LWdata <- database[database$Type=="LW", ]
# filter data for Russia
LWdata <- LWdata[LWdata$Country=="RS", ]
# filter data for only comlete cases of major chemical elements
LWdata <- LWdata[complete.cases(LWdata[ ,c('Ca', 'Mg', 'Na', 'K', 'Carb_Alk', 'Cl', 'CO3', 'SO4')]), ]
write.table(LWdata, "~/LWdata.txt", sep="\t")

# create summary table
LWdataSum <- LWdata %>%
  group_by(Type) %>% 
  summarise_all( funs(mean(., na.rm = TRUE), min(., na.rm = TRUE), max(., na.rm = TRUE)))
LWdataSum <- t(LWdataSum)
write.table(LWdataSum, "~/LWdataSum.txt", sep="\t")

# select the data containing only variables we need for calculations
LWnum <- select(LWdata, pH, 
                Carb_Alk, SO4, Cl, Ca, Mg, Na, Li, 
                B, Al, Si, P, V, Cr, Mn, Fe, Cu, As, 
                Br, Sr, Rb, Th, U, Zn, Ni, Cd, Co, Sb, Sn, Bi, Pb)
write.table(LWnum, "~/LWnum.txt", sep="\t")

# create function to count the logarifm of the values
LogIfNotNA <- function(value) {
  if (is.numeric(value)) {
    return(log10(value))
  }
  return(value)
}
LWlog <- as.data.frame(apply(LWnum, c(1, 2), LogIfNotNA))

# deleting rows with all NA data
LWdata <- subset(LWdata, (!is.na(LWdata$SampleID)))

# creating the table with summary: mean, median, 25th and 75th quartiles, min, max
LWsum <- lapply(
  select(LWdata, pH:U), 
  summary
  )
LWsum <- as.data.frame(t(sapply(
  LWsum, `[`, c("Min.","1st Qu.", "Median", "Mean", "3rd Qu.", "Max.", "NA's")
  )))
write.csv(LWsum, file = "LWsum.csv")

# normality test for our data shows that only pH data are normaly destributed
LWsharpo <- lapply(LWnum,
                   shapiro.test
)
t(sapply(LWsharpo, `[`, c("statistic",
                          "p.value")
))
# check if log data are normaly destibuted
LWsharpo <- lapply(LWlog,
                   shapiro.test
                   )
t(sapply(LWsharpo, `[`, c("statistic",
                          "p.value")
         ))

# draw correlation matrix for 7 major components
LWlog_pairs <- LWlog[ ,1:7]
pairs(LWlog_pairs, upper.panel=NULL)

# check the correlation between Salinity-HCO3 and ph-HCO3 on data with complete cases 
cor(LWdata$Sal, LWdata$HCO3, use="complete.obs")
cor(LWdata$pH, LWdata$HCO3, use="complete.obs")

# transform data to percentage form
LWdataPerc <- toPercent(LWdata)
LWdataPerc<- data.frame(lapply(LWdataPerc, as.numeric, check.names=F))
LWdataPerc[is.na(LWdataPerc)] <- 0
LWdataPerc <- mutate(LWdataPerc, HCO3 = HCO3 + CO3)

# create Piper diagram
piperLW <- piper(LWdataPerc)
piperLW@pt.col= LWdata$Type
plot(piperLW, cex=1.8)

# create chemical type of the sample based on its concentration in format Anion1-Anion2 Cation1-Cation2 in decrease order
ToChemType <- function(x) {
  
  cations <- x[c('Ca', 'Mg', 'Na')]
  SubCat <- cations[cations >= 25]
  
  anions <- x[c('HCO3', 'SO4', 'Cl')]
  SubAn <- anions[anions >= 25]

  return(paste(
    paste(names(SubAn[order(SubAn, decreasing=TRUE)]), collapse = '-'),
    paste(names(SubCat[order(SubCat, decreasing=TRUE)]), collapse = '-'), collapse = ' '))
  }

# creation of chemical types and assignment to the colours
ChemTypes <- as.data.frame(apply(LWdataPerc, 1, ToChemType))
colnames(ChemTypes) <- "Type"
GroupTypes <- function(x) {
  x[2] <- 0
  x[3] <- 0
  if (x[1] == 'Cl Mg-Na' | x[1] == 'Cl Na-Mg' | x[1] == 'Cl Na') {
    x[2] <- 'Cl Na'
    x[3] <- 'red'
  }
  if (x[1] == 'Cl-HCO3-SO4 Na' | x[1] == 'Cl-HCO3 Na') {
    x[2] <- 'Cl Na' #'Cl-HCO3 Na' 
    x[3] <- 'orangered'
  }
  if (x[1] == 'Cl-SO4 Na-Mg' | x[1] == 'Cl-SO4-HCO3 Na' | x[1] == 'Cl-SO4 Mg-Na' | x[1] =='Cl-SO4 Na') {
    x[2] <- 'Cl Na' #'Cl-SO4 Na'
    x[3] <- 'orange'
  }
  if (x[1] == 'HCO3 Ca-Na' | x[1] == 'HCO3 Ca') {
    x[2] <- 'HCO3 Ca'
    x[3] <- 'deepskyblue'
  }
  if (x[1] == 'HCO3 Na-Ca' | x[1] =='HCO3 Na-Ca-Mg' | x[1] =='HCO3 Na-Mg'| x[1] == 'HCO3 Na') {
    x[2] <- 'HCO3 Na'
    x[3] <- 'navy'
  }
  if (x[1] == 'HCO3-Cl-SO4 Na' | x[1] == 'HCO3-Cl Na') {
    x[2] <- 'HCO3 Na' # 'HCO3-Cl Na'
    x[3] <- 'dodgerblue2'
  }
  if (x[1] == 'HCO3-SO4 Ca-Na' | x[1] =='HCO3-SO4 Na-Mg' | x[1] =='HCO3-SO4-Cl Na' | x[1] =='HCO3-SO4 Na') {
    x[2] <- 'HCO3 Na' #'HCO3-SO4 Na'
    x[3] <- 'cornflowerblue'
  }
  if (x[1] == 'SO4-Cl Na-Mg' | x[1] =='SO4-Cl Na') {
    x[2] <- 'SO4 Na' #'SO4-Cl Na'
    x[3] <- 'chartreuse3'
  }
  if (x[1] == 'SO4-HCO3 Na-Mg' | x[1] =='SO4-HCO3-Cl Na' | x[1] == 'SO4-HCO3 Na') {
    x[2] <- 'SO4 Na' #'SO4-HCO3 Na'
    x[3] <- 'forestgreen'
  }
  if (x[1] == 'SO4 Na') {
    x[2] <- 'SO4 Na'
    x[3] <- 'darkgreen'
  }
  return(x)
}
# generate new columns for chemical types
GroupedChemTypes <- as.data.frame(t(apply(ChemTypes, 1, GroupTypes))) 
colnames(GroupedChemTypes) <- c("Type", "ChemicalType", "ColorType")

# library for PCA
library(ade4)
library(factoextra)
library(ggbiplot)

# tests if a numeric value is NaN
is.nan.data.frame <- function(x)
  do.call(cbind, lapply(x, is.nan))
LWlog[is.nan(LWlog)] <- NA

# calculate mean values
calculateMean <- function(concentration) {
  meanConc <- mean(concentration, na.rm = TRUE)
  concentration[is.na(concentration)] <- meanConc
  return(concentration)
}

# apply PCA to the LWlog database
PCAforLWlog = apply(LWlog,2,calculateMean)
pca <- princomp(PCAforLWlog, cor=TRUE, scores=TRUE)
pca$n.obs
pca$center
pca$loadings

# draw a scree plot that displays how much variation each principal component captures from the data
screeplot(pca, type="lines", col=3)
fviz_eig(pca)

# Draw PCA loadings ploat and score plots
fviz_pca_var(pca, 
             axes = c(2, 3),
             col.var  = "contrib", # Color by the quality of representation
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)

# Draw biplot of PCA
g <- ggbiplot(pca, choices = 1:2, 
              obs.scale = 1, 
              var.scale = 0,
              var.axes = F,
              group = ChemType$Type, 
              ellipse = TRUE
              )
g <- g + xlim(-8, 8) + ylim(-6, 8)
g <- g + theme(legend.direction = 'vertical', 
               legend.position = 'right')
print(g)

# hierarhial clusters analysis
clusters <- hclust(dist(LWlog), method = "ward.D"
                   )
plot(clusters, labels=LWdata$Name
     )

rect.hclust(clusters, k = 4, border = 2:5)
clusterCut <- cutree(clusters, 4)

# visualize clustering results
LWlog2 <- data.frame(t(na.omit(t(LWlog))))
pca.Sample.2 <- prcomp(LWlog2, retx=TRUE)
fviz_cluster(list(data = LWlog2, cluster = clusterCut))

# see how many samples in each type
table(clusterCut, LWdata$Type)

# cluster dendrogram for columns
clusters_col <- hclust(dist(t(LWlog)), method = "ward.D")
plot(clusters_col)

# two dendrogams together for both cols and rows
clustersm <- as.matrix(LWlog)
heatmap(clustersm, Colv=F, scale='none')

# ploting 2 point diagrams on one figure
salinityAndConcHC03 <- 
  ggplot(data=LWdata, aes(log(Sal), log(HCO3))) +
  geom_point(mapping = aes(col=Country)) +
  geom_smooth(method = lm, color = "black", formula = y ~ x) +
  labs(x = "log(Sal), g/L", y = "log(Conc.), g/L", title = "Sal-HCO3")
plot(salinityAndConcHC03)

pHAndConcHC03 <- 
  ggplot(data=LWdata, aes(pH, log(HCO3))) +
  geom_point(mapping = aes(col=Country)) +
  geom_smooth(method = lm, color = "black") +
  labs( x = "pH", y = "log(Conc.), g/L", title = "pH-HCO3")
plot(pHAndConcHC03)

grid.arrange(salinityAndConcHC03, pHAndConcHC03)
