#########################################################################################################################
### Project  : Technology assessment of Engines: TSC vs NA
### Script   : Engine.R
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


c <- matrix(as.numeric(dataset.engine.2015[,3]), ncol = 1)
n <- matrix(c(dataset.engine.2015[,1]),ncol=1)
p <- matrix(c(dataset.engine.2015[,8]),ncol=1)
x <- matrix(as.numeric(dataset.engine.2015[,4]),ncol=1)
y <- matrix(as.numeric(dataset.engine.2015[,5:7]),ncol=3)
d <- matrix(as.numeric(dataset.engine.2015[,2]),ncol=1)
g <- matrix(c(rep(0,nrow(x)),y),nrow=nrow(x),ncol=4)


## subset
x_4 <- subset(x, c==4);y_4<-subset(y,c==4);d_4<-subset(d,c==4);g_4<-subset(g,c==4);n_4<-subset(n,c==4)
x_6 <- subset(x, c==6);y_6<-subset(y,c==6);d_6<-subset(d,c==6);g_6<-subset(g,c==6);n_6<-subset(n,c==6)
x_8 <- subset(x, c==8);y_8<-subset(y,c==8);d_8<-subset(d,c==8);g_8<-subset(g,c==8);n_8<-subset(n,c==8)


#########################################################################################################################
### Analysis
#########################################################################################################################

## Table 1. Descriptive statistics
table.1 <- cbind(sapply(df[df$Cylinder == 4, c(id.x, id.y)], function(x) c(Min  = min(x),  Med = median(x), 
                                                                           Mean = mean(x), Max = max(x), Std = sd(x))),
                 sapply(df[df$Cylinder == 6, c(id.x, id.y)], function(x) c(Min  = min(x),  Med = median(x), 
                                                                           Mean = mean(x), Max = max(x), Std = sd(x))),
                 sapply(df[df$Cylinder == 8, c(id.x, id.y)], function(x) c(Min  = min(x),  Med = median(x), 
                                                                           Mean = mean(x), Max = max(x), Std = sd(x))))
data.frame(Cylinders  = rep(c(4, 6, 8), each = 4), round(t(table.1), 1))


## Table 2. SOA maps
table.2 <- map.soa.sf(df[df$Cylinder == 4, id.x], df[df$Cylinder == 4, id.y], df[df$Cylinder == 4, id.t], 
                      rts, dv[df$Cylinder == 4,], wd, mk = "eff")
round(table.2, 4)

# (work in progress)