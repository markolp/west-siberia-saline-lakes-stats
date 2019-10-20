setwd("~/R")
library(readxl)
library(ggplot2)
library(gridExtra)
library(ggpmisc)
library(corrgram)
library(dplyr)
library(pcaMethods)
library(hydrogeo)
# read xls file---------------------------------
database <- read_excel("WRI/databaseall.xls", sheet = "databaseall")
#View(database)

# create new dataset with Lake Waters LW in the Type column----------------------
LWdata <- database[database$Type=="LW", 
  #| database$Type=="RW" | database$Type=="CW" | database$Type=="GW",
  ]
LWdata <- LWdata[LWdata$Country=="RS",
                 ]
#LWdata
LWdata <- LWdata[complete.cases(LWdata[ ,c('Ca', 'Mg', 'Na', 'K', 'Carb_Alk', 'Cl', 'CO3', 'SO4')]), ]
write.table(LWdata, "~/LWdata.txt", sep="\t")

# ????Calculating average for the same objects
# LWdata <- as.data.frame(lapply(split(!is.na(LWdata), LWdata$Name), mean))
# LWdata %>% 
#   group_by(Name) %>%
#   summarise(mean)

# Creat summary table---------------------------------------------------
LWdataSum <- LWdata %>%
  group_by(Type) %>% 
  summarise_all( funs(mean(., na.rm = TRUE), min(., na.rm = TRUE), max(., na.rm = TRUE)))
LWdataSum <- t(LWdataSum)
write.table(LWdataSum, "~/LWdataSum.txt", sep="\t")

#Selecting the data containing only variables we need for calculations----------------------
#LWdata <- LWdata[-c(117:119, 126, 144, 173:175), ]
# Pb, Cu, Zn, Ni, Cd, Co, Sb, Sn, Bi, Hg 
LWnum <- select(LWdata, pH, #Eh, 
                Carb_Alk, SO4, Cl, Ca, Mg, Na, Li, 
                B, Al, Si, P, V, Cr, Mn, Fe, Cu, As, 
                Br, Sr, Rb, Th, U, Zn, Ni, Cd, Co, Sb, Sn, Bi, Pb)
#LWnum
#cor(na.omit(LWnum))
#sum(is.na(LWnum))
write.table(LWnum, "~/LWnum.txt", sep="\t")
LogIfNotNA <- function(value) {
  # if (value == 0) {
  #  return(value)
  # }
  if (is.numeric(value)) {
    return(log10(value))
  }
  return(value)
}
log.LW <- as.data.frame(apply(LWnum, c(1, 2), LogIfNotNA)) # TODO: rename log.LW into LWlog?
#View(log.LW)
# LWdata <- subset(LWdata, (!is.na(LWdata$SampleID))) # deleting rows with all NA data
#LWdata <- tbl_df(LWdata)

# Creating the table with summary: mean,median,25th and 75th quartiles,min,max---------------
LWsum <- lapply(
  select(LWdata, pH:U), 
  summary
  )
LWsum <- as.data.frame(t(sapply(
  LWsum, `[`, c("Min.","1st Qu.", "Median", "Mean", "3rd Qu.", "Max.", "NA's")
  )))
write.csv(LWsum, file = "LWsum.csv")
# Density plot???? Don't adapted----------------------
# par(mfrow=c(3, 3))
# colnames <- dimnames(crime.new)[[2]]
# for (i in 2:8) {
#   d <- density(crime.new[,i])
#   plot(d, type="n", main=colnames[i])
#   polygon(d, col="red", border="gray")
# }

#normality test for our data shows that only pH data are normaly destributed---------------------------------------
LWsharpo <- lapply(LWnum,
                   shapiro.test
)
t(sapply(LWsharpo, `[`, c("statistic",
                          "p.value")
))
# we check if log data are normaly destibuted (TRUE for pH, )
LWsharpo <- lapply(log.LW,
                   shapiro.test
                   )
t(sapply(LWsharpo, `[`, c("statistic",
                          "p.value")
         ))
log.LW_pairs <- log.LW[ ,1:7]
pairs(log.LW_pairs, upper.panel=NULL)
# histogramms-------------------------------------
hist(LWdata)

# correlation------------------------------------
cor(LWdata$Sal, LWdata$HCO3, use="complete.obs")
cor(LWdata$pH, LWdata$HCO3, use="complete.obs")

# chemical elements to percents
LWdataPerc <- toPercent(LWdata)
LWdataPerc[is.na(LWdataPerc)] <- 0 # FIXME: do we need this?
LWdataPerc<- data.frame(lapply(LWdataPerc, as.numeric, check.names=F))
LWdataPerc[is.na(LWdataPerc)] <- 0
LWdataPerc <- mutate(LWdataPerc, HCO3 = HCO3 + CO3)

piperLW <- piper(LWdataPerc)
piperLW@pt.col= LWdata$Type
plot(piperLW, cex=1.8)

# intoEqL <- function(y) {
#   Eq.Ca <- y['Ca'] / 20.04
#   Eq.Mg = y['Mg']
#   Eq.K = y['K']
#   Eq.Na = y['Na']
#   Eq.HCO3 = y['HCO3']
#   Eq.CO3 = y['CO3']
#   Eq.Cl = y['Cl']
#   Eq.SO4 = y['SO4']
#   
#   return(y['Ca'])}

ToChemType <- function(x) {
  
  cations <- x[c('Ca', 'Mg', 'Na')]
  SubCat <- cations[cations >= 25]
  
  anions <- x[c('HCO3', 'SO4', 'Cl')]
  SubAn <- anions[anions >= 25]

  return(paste(
    paste(names(SubAn[order(SubAn, decreasing=TRUE)]), collapse = '-'),
    paste(names(SubCat[order(SubCat, decreasing=TRUE)]), collapse = '-'), collapse = ' '))
#  return(SubCat[order(SubCat, decreasing=TRUE)])
  }
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
# ChemType$WaterType <- 0
GroupedChemTypes <- as.data.frame(t(apply(ChemTypes, 1, GroupTypes))) 
colnames(GroupedChemTypes) <- c("Type", "ChemicalType", "ColorType")

# PCA--------------------------------------
# apply PCA - scale. = TRUE is highly 
# advisable, but default is FALSE. 
# log.LW.pca <- prcomp(log.LW,
#                  center = TRUE,
#                  scale. = TRUE)

library(ade4) # TODO: lift up?
is.nan.data.frame <- function(x) # TODO: What is it?
  do.call(cbind, lapply(x, is.nan))
log.LW[is.nan(log.LW)] <- NA

f1 <- function(vec) { # TODO: rename to calculateMean?
  m <- mean(vec, na.rm = TRUE)
  vec[is.na(vec)] <- m
  return(vec)
}

Y = apply(log.LW,2,f1) # TODO: what is Y?

#PCA 
pca1 <- princomp(Y, cor=TRUE, scores=TRUE)
# pca1 <- prcomp(Y, scale. = TRUE)
pca1$n.obs
pca1$center
PoV <- pca1$sdev^2/sum(pca1$sdev^2) # TODO: what is PoV?
PoV
pca1$loadings
head(pca1$loadings)
screeplot(pca1, type="lines", col=3)
Loadings <- as.data.frame(pca1$loadings[,1:4])
scores <- as.data.frame(pca1$scores[,1:4])
library(factoextra) # TODO: lift up?
fviz_eig(pca1)
fviz_pca_var(pca1, 
             axes = c(2, 3),
             col.var  = "contrib", # Color by the quality of representation
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)
# Draw ...
library(ggbiplot) # TODO: lift up?
log.LW.Sal <- log.LW[, 11]
pairs(pca1$x)
g <- ggbiplot(pca1, choices = 1:2, 
              obs.scale = 1, 
              var.scale = 0,
              var.axes = F,
              group = ChemType$Type, 
              ellipse = TRUE
              #circle = TRUE
              )
g <- g + xlim(-8, 8) + ylim(-6, 8)
g <- g + theme(legend.direction = 'vertical', 
               legend.position = 'right')
print(g)

# hierarhial clusters analysis-------------------------
clusters <- hclust(dist(log.LW), method = "ward.D"
                   )
plot(clusters, labels=LWdata$Name
     )
rect.hclust(clusters, k = 4, border = 2:5)
clusterCut <- cutree(clusters, 4)
log.LW.2 <- data.frame(t(na.omit(t(log.LW))))
pca.Sample.2 <- prcomp(log.LW.2, retx=TRUE)
fviz_cluster(list(data = log.LW.2, cluster = clusterCut))
table(clusterCut, LWdata$Type)
#hierarhial for columns-------------------------------
clusters <- hclust(dist(t(log.LW)), method = "ward.D"
)
plot(clusters
)
#heatmap or two dendrogams-----------------------------
clustersm <- as.matrix(log.LW)
heatmap(clustersm, Colv=F, scale='none')
#plot 3D diagramm--------------------------------------
# library(rgl) #package doesn't load
# plot3d(pca1$scores[ ,1:3], col=ChemType$ColorType, xlim = c(-5, 5), zlim = c(-5, 5))
# # text3d(pca1$scores[ ,1:3], texts=rownames(log.LW))
# text3d(pca1$loadings[ ,1:3], texts=rownames(pca1$loadings), col="red")
# coords <- NULL
# for (i in 1:nrow(pca1$loadings)) {
#   coords <- rbind(coords, rbind(c(0,0,0), pca1$loadings[i, 1:3]))
# }
# lines3d(coords, col="red", lwd = 4)

# pcaY = dudi.pca(Y,center=TRUE,scale=FALSE,nf=5,scannf=FALSE) # haven't used

# s.label(pcaY$li) 
# head(pcaY$li)
# scores = as.data.frame(pca1$x)
# ggplot(data = scores, aes(x = PC1, y = PC2, label = rownames(scores))) +
#   geom_hline(yintercept = 0, colour = "gray65") +
#   geom_vline(xintercept = 0, colour = "gray65") +
#   geom_text(colour = "tomato", alpha = 0.8, size = 4) +
#   ggtitle("PCA plot of USA States - Crime Rates")

# ploting point diagrams-----------------------------------------
salinityAndConcHC03 <- ggplot(data=LWdata, 
             aes(log(Sal), 
                 log(HCO3))) +
  geom_point(
    mapping = aes(col=Country)) +
#  geom_point(data=LWdata, aes(Sal/1000, Ca/1000, col=Country), shape=17) +
  geom_smooth(method = lm, color = "black", formula = y ~ x) +
#  scale_x_log10() +
#  scale_y_log10() +
  labs( x = "log(Sal), g/L", 
        y = "log(Conc.), g/L",
        title = "Sal-HCO3")

plot(salinityAndConcHC03)

pHAndConcHC03 <- ggplot(data=LWdata, 
             aes(pH, log(HCO3)))+
 geom_point(mapping = aes(col=Country))+
 geom_smooth(method = lm, color = "black") +
#  geom_point(data=LWdata, aes(pH, Ca/1000), color = "blue")+
#  scale_y_log10() +
  labs( x = "pH", 
        y = "log(Conc.), g/L", 
        title = "pH-HCO3")
plot(pHAndConcHC03)

grid.arrange(salinityAndConcHC03, pHAndConcHC03)
