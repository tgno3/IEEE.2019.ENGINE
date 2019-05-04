#########################################################################################################################
### Project  : Technology assessment of Engines: TSC vs NA
### Script   : Engine on GIT.R
### Contents : IEEE Access paper
#########################################################################################################################

#########################################################################################################################
### Setting up environment
#########################################################################################################################

# Load library
pkgs <- c("DJL")
sapply(pkgs, require, character.only = T)

# Load data
df <- dataset.engine.2015

# Parameter
id.x <- c(4)
id.y <- c(6, 7, 5)
id.t <- c(2)
fy   <- 2015
rts  <- "crs"
dv   <- cbind(rep(0, nrow(df)), df[, id.y])
wd   <- matrix(c(0, 0, 1), 1)
sg   <- "min"


#########################################################################################################################
### Analysis
#########################################################################################################################

## Table 1. Descriptive statistics
descs <- function(x){
  res <- c(Min  = min(x),  Med = median(x), Mean = mean(x), Max = max(x), Std = sd(x))
  return(res)
}
table.1 <- cbind(sapply(df[df$Cylinder == 4, c(id.x, id.y)], descs),
                 sapply(df[df$Cylinder == 6, c(id.x, id.y)], descs),
                 sapply(df[df$Cylinder == 8, c(id.x, id.y)], descs))
cbind(Cylinders  = rep(c(4, 6, 8), each = 4), round(t(table.1), 1))


## Table 2. SOA maps for 4
n.cyl   <- 4
res.m.4 <- map.soa.sf(df[df$Cylinder == n.cyl, id.x], df[df$Cylinder == n.cyl, id.y], 
                      df[df$Cylinder == n.cyl, id.t], rts, dv[df$Cylinder == n.cyl,], wd, mk = "eff")
id.soa  <- as.numeric(rownames(df[df$Cylinder == n.cyl, ])[as.numeric(rownames(res.m.4))])
data.frame(Model = as.character(df$Name[id.soa]), 
           MY    = df$MY[id.soa],
           Type  = as.character(df$Type[id.soa]), 
           round(res.m.4, 4))


## Table 3. SOA maps for 6
n.cyl   <- 6
res.m.6 <- map.soa.sf(df[df$Cylinder == n.cyl, id.x], df[df$Cylinder == n.cyl, id.y], 
                      df[df$Cylinder == n.cyl, id.t], rts, dv[df$Cylinder == n.cyl,], wd, mk = "eff")
id.soa  <- as.numeric(rownames(df[df$Cylinder == n.cyl, ])[as.numeric(rownames(res.m.6))])
data.frame(Model = as.character(df$Name[id.soa]), 
           MY    = df$MY[id.soa],
           Type  = as.character(df$Type[id.soa]), 
           round(res.m.6, 4))


## Table 4. SOA maps for 8
n.cyl   <- 8
res.m.8 <- map.soa.sf(df[df$Cylinder == n.cyl, id.x], df[df$Cylinder == n.cyl, id.y], 
                      df[df$Cylinder == n.cyl, id.t], rts, dv[df$Cylinder == n.cyl,], wd, mk = "eff")
id.soa  <- as.numeric(rownames(df[df$Cylinder == n.cyl, ])[as.numeric(rownames(res.m.8))])
data.frame(Model = as.character(df$Name[id.soa]), 
           MY    = df$MY[id.soa],
           Type  = as.character(df$Type[id.soa]), 
           round(res.m.8, 4))


## Table 5. Top 5 engines for each segment
res.top5 <- data.frame()
for(n.cyl in unique(df$Cylinder)){
  p        <- 1
  id.first <- c()
  df.ex    <- df[df$Cylinder == n.cyl,]  
  res.t    <- matrix(NA, nrow(df.ex), 3, dimnames = list(1:nrow(df.ex), c("T", "DMU", "Local RoC")))
  for(i in unique(df.ex$MY)[-1]){
    res.roc <- roc.sf(df.ex[, id.x], df.ex[, id.y], df.ex[, id.t], 
                      i, rts, dv[df$Cylinder == n.cyl,], wd, sg)  
    if(any(res.roc$roc_local > 1, na.rm = T)){
      id.lroc             <- which(res.roc$roc_local > 1)  
      j                   <- length(id.lroc) - 1
      res.t[p:(p + j), 1] <- i
      res.t[p:(p + j), 2] <- as.numeric(rownames(df.ex)[id.lroc])
      res.t[p:(p + j), 3] <- res.roc$roc_local[id.lroc]
      p                   <- p + j + 1
    }
  }
  res.t.eff <- res.t[1:(p - 1),]
  for(j in unique(res.t.eff[, 2])){id.first <- c(id.first, min(which(res.t[, 2] == j)))}  
  res.top5 <- rbind(res.top5, 
                    data.frame(n.cyl = n.cyl, res.t.eff[id.first,][order(-res.t.eff[id.first, 3])[1:5], ]))
}
table.5 <- data.frame(Cylinder = rep(c(4, 6, 8), each = 5),
                      Rank     = rep(1:5, 3),
                      Model    = df$Name[res.top5$DMU],
                      MY       = df$MY  [res.top5$DMU],
                      Type     = df$Type[res.top5$DMU],
                      df[res.top5$DMU, c(id.x, id.y)],
                      L.RoC    = paste0(round((res.top5$Local.RoC - 1) * 100, 2), "%"))
print(table.5)
