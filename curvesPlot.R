if(!require(gplots)) install.packages("gplots")
library(gplots)
library (ggplot2)
library(scales)
if(!require(randomcoloR)) install.packages("randomcoloR")
library(randomcoloR)
library(statmod)

dataDir <- "/Users/anacabanilles/Desktop/curves/data/"
fileName <- "ana_datos_od_continua_2.xlsx"
plotDir <- "/Users/anacabanilles/Desktop/curves/plots/"

if(!dir.exists(plotDir)){
  dir.create(plotDir)
}

dataRaw <- as.data.frame(readxl::read_xlsx(paste0(dataDir, fileName)))
control_medium <- dataRaw$`0`[dataRaw$conc == "c-"]



data <- as.data.frame(readxl::read_xlsx(paste0(dataDir, fileName)))



data

data <- data[!data$conc %in% c("h2o", "c-"), ]

data$conc <- as.numeric(data$conc)

plotDF <- data.frame(matrix(nrow = (ncol(data) - 2) * length(data$`Hora[s]`),
                            ncol = 4,
                            dimnames = list(NULL,
                                            c("sample", "conc", "time",  "od"))))



plotDF$time <- as.numeric(rep(colnames(data)[3:ncol(data)], length(data$`Hora[s]`)))
plotDF$sample <- rep(data$`Hora[s]`, each = (ncol(data) - 2))



data
for(sample in data$`Hora[s]`){
    od_row <- as.numeric(data[data$`Hora[s]` == sample, 3:ncol(data)])
    sampConc <- data$conc[data$`Hora[s]` == sample]
    plotDF$od[plotDF$sample == sample] <- od_row
    plotDF$conc[plotDF$sample == sample] <- sampConc
}


plotDF$od
plotDF


plotDF$conc <- factor(plotDF$conc, levels = sort(unique(plotDF$conc)))
as.numeric(data[data$`Hora[s]` == 'B2', 3:ncol(data)])

length(unlist(plotDF$od))

#plotDF$


samples <- unique(plotDF$sample)

allPlots <- list()
for(sample in samples){
  plot <- ggplot(data = plotDF[plotDF$sample == sample, ], mapping = aes(x = time, y = od, color = conc)) + 
    geom_point()
  allPlots[[sample]] <- plot
}
allPlots$B2
allPlots$C2
allPlots$D2 #
allPlots$E2
allPlots$G2

allPlots$B3
allPlots$C3
allPlots$D3
allPlots$E3
allPlots$G3

allPlots$B4
allPlots$C4
allPlots$D4
allPlots$E4
allPlots$G4

allPlots$B5
allPlots$C5
allPlots$D5
allPlots$E5
allPlots$G5

allPlots$B6
allPlots$C6
allPlots$D6
allPlots$E6
allPlots$G6

allPlots$B7
allPlots$C7
allPlots$D7
allPlots$E7
allPlots$G7

allPlots$B8
allPlots$C8
allPlots$D8
allPlots$E8
allPlots$G8

allPlots$B9
allPlots$C9
allPlots$D9
allPlots$E9
allPlots$G9

allPlots$B10
allPlots$C10
allPlots$D10
allPlots$E10
allPlots$G10

plotDF$od[plotDF$sample == "C10" & plotDF$time == 142680] <- NA


allPlots$B11
allPlots$C11
allPlots$D11
allPlots$E11
allPlots$G11

plotDF$od[plotDF$sample == "E11" & plotDF$od == 2] <- NA

concs <- levels(plotDF$conc)
plotsConcs <- list()
for(conc in concs){
  plot <- ggplot(data = plotDF[plotDF$conc == conc, ], mapping = aes(x = time, y = od, color = conc)) + 
    geom_point()
  plotsConcs[[conc]] <- plot
}

plotsConcs$`0` # G11 rara
plotsConcs$`2`
plotsConcs$`3`
plotsConcs$`4`
plotsConcs$`5`
plotsConcs$`6` # C7 rara
plotsConcs$`7` 
plotsConcs$`8`
plotsConcs$`9` # C4 rara
plotsConcs$`10`
plotsConcs$`11` # C2 rara

plotDF[plotDF$conc == "6", ]
plotDF[plotDF$conc == "9", ]
plotDF[plotDF$conc == "11", ]

# Eliminar repliques rares

plotDF <- plotDF[!plotDF$sample %in% c("G11", 
                                       "C7", 
                                       "C4",
                                       "C2"), ]


plotDF_odNorm <- data.frame(matrix(nrow = 0,
                                   ncol = ncol(plotDF),
                                   dimnames = list(NULL,
                                                   colnames(plotDF))))
for(s in unique(plotDF$sample)){
  sampDF <- plotDF[plotDF$sample == s, ]
  sampODT0 <- sampDF$od[sampDF$time == 0]
  sampDF$od <-  sampDF$od - sampODT0
  plotDF_odNorm <- rbind.data.frame(plotDF_odNorm, sampDF)
}


concColDF <- data.frame(conc = as.character(sort(unique(plotDF$conc))))
set.seed(555)
concColDF$col <- distinctColorPalette(nrow(concColDF))

concColDF$col[concColDF$conc == "0"] <- "#000000"
concColDF$col[concColDF$conc == "2"] <- "#03B900"
concColDF$col[concColDF$conc == "4"] <- "#00FFFF"
concColDF$col[concColDF$conc == "6"] <- "#FFB200"
concColDF$col[concColDF$conc == "7"] <- "#FF0000"
concColDF$col[concColDF$conc == "9"] <- "#FFFB00"
concColDF$col[concColDF$conc == "10"] <- "#0008FF"

#save(concColDF, file = "concColDF.RData")
#load("concColDF.RData")

plotAll <- ggplot(data = plotDF, mapping = aes(x = time, y = od, color = conc)) + 
  geom_point() +
  scale_discrete_manual(aesthetics = "colour",
                        values = concColDF$col) + 
  theme(axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        panel.background = element_blank(),
        panel.grid.major = element_line(color = "grey"),
        #panel.grid.minor = element_line(color = "black"),
        panel.border = element_rect(colour = "black",
                                    fill = NA,
                                    size = 1))

plotAll
ggsave(paste0(plotDir, "allCurves.pdf"), height = 20, width = 20)

plotAll_odNorm <- ggplot(data = plotDF_odNorm, mapping = aes(x = time, y = od, color = conc)) + 
  geom_point() +
  geom_smooth(se =F) +
  scale_discrete_manual(aesthetics = "colour",
                        values = concColDF$col) + 
  theme(axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        panel.background = element_blank(),
        panel.grid.major = element_line(color = "grey"),
        #panel.grid.minor = element_line(color = "black"),
        panel.border = element_rect(colour = "black",
                                    fill = NA,
                                    size = 1))

plotAll_odNorm
ggsave(paste0(plotDir, "allCurves_odNorm.pdf"), height = 20, width = 20)


getMedDF <- function(DF){
  medDF <- data.frame(matrix(nrow = 0, 
                             ncol = ncol(DF) -1,
                             dimnames = list(NULL,
                                             colnames(DF)[2:ncol(DF)])))
  concs <- levels(DF$conc)
  for(conc in concs){
    concDF <- DF[DF$conc == conc, ]
    for(t in unique(concDF$time)){
      meanOD <- mean(concDF$od[concDF$time == t])
      subConcDF <- data.frame(matrix(nrow = 1, 
                                     ncol = 3, 
                                     dimnames = list(NULL,
                                                     colnames(medDF))))
      subConcDF$conc <- conc
      subConcDF$time <- t
      subConcDF$od <- meanOD
      
      medDF <- rbind.data.frame(medDF, subConcDF)
    }
  }
  medDF$conc <- factor(medDF$conc,
                       levels = as.character(sort(unique(as.numeric(medDF$conc)))))
  return(medDF)
}

concMedDF <- getMedDF(plotDF)


plotMedAll <- ggplot(data = concMedDF, mapping = aes(x = time, y = od, color = conc)) + 
  geom_smooth(se = F) +
  scale_discrete_manual(aesthetics = "colour",
                        values = concColDF$col) + 
  theme(axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        panel.background = element_blank(),
        panel.grid.major = element_line(color = "grey"),
        #panel.grid.minor = element_line(color = "black"),
        panel.border = element_rect(colour = "black",
                                    fill = NA,
                                    size = 1))

plotMedAll

ggsave(paste0(plotDir, "medCurves.pdf"))

concMedDF_odNorm <- getMedDF(plotDF_odNorm)


plotMedAll_odNorm <- ggplot(data = concMedDF_odNorm, mapping = aes(x = time, y = od, color = conc)) + 
  geom_point() +
  scale_discrete_manual(aesthetics = "colour",
                        values = concColDF$col) + 
  theme(axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        panel.background = element_blank(),
        panel.grid.major = element_line(color = "grey"),
        #panel.grid.minor = element_line(color = "black"),
        panel.border = element_rect(colour = "black",
                                    fill = NA,
                                    size = 1))

plotMedAll_odNorm
ggsave(paste0(plotDir, "medCurves_points_odNorm.pdf"))

plotAll_odNorm <- ggplot(data = plotDF_odNorm, mapping = aes(x = time, y = od, color = conc)) + 
  geom_smooth(se = F, method = "loess") +
  scale_discrete_manual(aesthetics = "colour",
                        values = concColDF$col) + 
  theme(axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        panel.background = element_blank(),
        panel.grid.major = element_line(color = "grey"),
        #panel.grid.minor = element_line(color = "black"),
        panel.border = element_rect(colour = "black",
                                    fill = NA,
                                    size = 1))

plotAll_odNorm


plotGrowthCurves(concMedDF$conc, as.matrix(concMedDF$od))

compareRes <- compareGrowthCurves(plotDF$conc, as.matrix(plotDF$od), nsim = 1000)


compareRes


tstDF <- plotDF[plotDF$conc %in% c("0", "11"), ]
y <- as.matrix(tstDF$od)
group <- tstDF$conc
nsim <- 100

group <- as.vector(group)
g <- unique(group)
if (length(g) != 2) 
  stop("Must be exactly 2 groups")
stat.obs <- fun(y[group == g[1], , drop = FALSE], y[group == 
                                                      g[2], , drop = FALSE])
asbig <- 0
for (i in 1:nsim) {
  pgroup <- sample(group)
  stat <- fun(y[pgroup == g[1], , drop = FALSE], y[pgroup == 
                                                     g[2], , drop = FALSE])
  if (abs(stat) >= abs(stat.obs)) 
    asbig <- asbig + 1
}
list(stat = stat.obs, p.value = asbig/nsim)


transf2TimeInCols <- function(DF){
  outDF <- data.frame(matrix(nrow = 0,
                             ncol = length(unique(DF$time)) + 1,
                             dimnames = list(NULL,
                                             c(as.character(unique(DF$time)), "conc"))),
                      check.names = F)
  for(s in unique(DF$sample)){
    sampDF <- DF[DF$sample == s, ]
    concentration <- unique(sampDF$conc)
    toAppend <- data.frame(matrix(sampDF$od, nrow = 1,
                                  ncol = length(unique(DF$time)),
                                  dimnames = list(s, unique(DF$time))),
                           check.names = F)
    toAppend$conc <- concentration
    outDF <- rbind.data.frame(outDF, toAppend)
  }
  return(outDF)
}


transf2TimeInCols(plotDF)

nuDF <- transf2TimeInCols(plotDF)

nuDF_odNorm <- transf2TimeInCols(plotDF_odNorm)

compareRes_byTime <- compareGrowthCurves(as.factor(nuDF$conc), 
                                         as.matrix(nuDF[, colnames(nuDF) != "conc"]), 
                                         nsim = 1000, 
                                         adjust = "BH")

compareRes_byTime_odNorm <- compareGrowthCurves(as.factor(nuDF_odNorm$conc), 
                                                as.matrix(nuDF_odNorm[, colnames(nuDF_odNorm) != "conc"]), 
                                                nsim = 1000, 
                                                adjust = "BH")


data4Clust <- nuDF[, colnames(nuDF) != "conc"]

curveClust <- hclust(dist(data4Clust), method = "ward.D")

?heatmap.2


nuDF$conc
colConcs <- hue_pal()(length(unique(nuDF$conc)))
names(colConcs) <- levels(nuDF$conc)

concColors <- concColDF$col[match(nuDF$conc, concColDF$conc)]

pdf(paste0(plotDir, "curveHM.pdf"))
heatmap.2(as.matrix(data4Clust),
          Colv = F,
          distfun = function(x) dist(x, method = "euclidean"),
          hclust = function(x) hclust(x, method = "ward.D"),
          trace = "none",
          dendrogram = "row",
          scale = "none",
          col = greenred(75),
          RowSideColors = concColors)
dev.off()

heatmap.2(as.matrix(data4Clust),
          Colv = F,
          distfun = function(x) dist(x, method = "euclidean"),
          hclust = function(x) hclust(x, method = "ward.D"),
          trace = "none",
          dendrogram = "row",
          scale = "none",
          col = greenred(75),
          RowSideColors = concColors)

legend()


time0 <- nuDF$`0`
names(time0) <- nuDF$conc
sort(time0)
