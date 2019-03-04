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
n.cyl  <- 4
res.ec <- map.soa.sf(df[df$Cylinder == n.cyl, id.x], df[df$Cylinder == n.cyl, id.y], 
                     df[df$Cylinder == n.cyl, id.t], rts, dv[df$Cylinder == n.cyl,], wd, mk = "eff")
id.soa <- as.numeric(rownames(df[df$Cylinder == n.cyl, ])[as.numeric(rownames(res.ec))])
data.frame(Model = as.character(df$Name[id.soa]), 
           MY    = df$MY[id.soa],
           Type  = as.character(df$Type[id.soa]), 
           round(res.ec, 4))


## Table 3. SOA maps for 6
n.cyl  <- 6
res.ec <- map.soa.sf(df[df$Cylinder == n.cyl, id.x], df[df$Cylinder == n.cyl, id.y], 
                     df[df$Cylinder == n.cyl, id.t], rts, dv[df$Cylinder == n.cyl,], wd, mk = "eff")
id.soa <- as.numeric(rownames(df[df$Cylinder == n.cyl, ])[as.numeric(rownames(res.ec))])
data.frame(Model = as.character(df$Name[id.soa]), 
           MY    = df$MY[id.soa],
           Type  = as.character(df$Type[id.soa]), 
           round(res.ec, 4))


## Table 4. SOA maps for 8
n.cyl  <- 8
res.ec <- map.soa.sf(df[df$Cylinder == n.cyl, id.x], df[df$Cylinder == n.cyl, id.y], 
                     df[df$Cylinder == n.cyl, id.t], rts, dv[df$Cylinder == n.cyl,], wd, mk = "eff")
id.soa <- as.numeric(rownames(df[df$Cylinder == n.cyl, ])[as.numeric(rownames(res.ec))])
data.frame(Model = as.character(df$Name[id.soa]), 
           MY    = df$MY[id.soa],
           Type  = as.character(df$Type[id.soa]), 
           round(res.ec, 4))







