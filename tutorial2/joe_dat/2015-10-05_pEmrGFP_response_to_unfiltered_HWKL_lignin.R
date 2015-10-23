HWKL <-read.csv("C:/Users/Ho/Desktop/2015_10_05_HWKL_normalized_fluorescence.csv")

# Dividing FU/OD to get normalized fluorescence

HWKL$normalized  <- HWKL$FU/HWKL$OD

# Group all the IDs together

library(reshape)
HWKL_norm <- HWKL[,c(1,4,5)]

HWKL_norm_or <- HWKL_norm[order(HWKL_norm$ID),]
A <- melt(HWKL_norm_or, id=c("ID","Treatment"),) # Puts all data in single column using the individual IDs.
A_or <- A[order(A$ID),]

library(plyr)

# Averaging by IDs

data <- ddply(A_or,.(ID,Treatment), summarize,mean_NFU=mean(value),sd_NFU=sd(value))


## Subsetting by IDs
```{r warning=FALSE, message=FALSE}
a <- subset(data, data$ID == c("empty_pEmr_GFP_0U_laccase")) 
b <- subset(data, data$ID == c("empty_pEmr_GFP_0.1U_laccase")) 
c <- subset(data, data$ID == c("empty_pEmr_GFP_1U_laccase")) 
d <- subset(data, data$ID == c("empty_pEmr_GFP_10U_laccase")) 
e <- subset(data, data$ID == c("empty_pEmr_GFP_100U_laccase"))

empty_pEmr_GFP <- rbind(a, b, c, d, e)

a <- subset(data, data$ID == c("J23106_pEmr_GFP_0U_laccase")) 
b <- subset(data, data$ID == c("J23106_pEmr_GFP_0.1U_laccase")) 
c <- subset(data, data$ID == c("J23106_pEmr_GFP_1U_laccase")) 
d <- subset(data, data$ID == c("J23106_pEmr_GFP_10U_laccase")) 
e <- subset(data, data$ID == c("J23106_pEmr_GFP_100U_laccase")) 

J23106_pEmr_GFP <- rbind(a, b, c, d, e)

a <- subset(data, data$ID == c("J23113_pEmr_GFP_0U_laccase")) 
b <- subset(data, data$ID == c("J23113_pEmr_GFP_0.1U_laccase")) 
c <- subset(data, data$ID == c("J23113_pEmr_GFP_1U_laccase")) 
d <- subset(data, data$ID == c("J23113_pEmr_GFP_10U_laccase")) 
e <- subset(data, data$ID == c("J23113_pEmr_GFP_100U_laccase")) 

J23113_pEmr_GFP <- rbind(a, b, c, d, e)

a <- subset(data, data$ID == c("J23114_pEmr_GFP_0U_laccase")) 
b <- subset(data, data$ID == c("J23114_pEmr_GFP_0.1U_laccase")) 
c <- subset(data, data$ID == c("J23114_pEmr_GFP_1U_laccase")) 
d <- subset(data, data$ID == c("J23114_pEmr_GFP_10U_laccase")) 
e <- subset(data, data$ID == c("J23114_pEmr_GFP_100U_laccase")) 

J23114_pEmr_GFP <- rbind(a, b, c, d, e)

empty_pEmr_GFP <- empty_pEmr_GFP[order(empty_pEmr_GFP$ID),]
J23106_pEmr_GFP <- J23106_pEmr_GFP[order(J23106_pEmr_GFP$ID),]
J23113_pEmr_GFP <- J23113_pEmr_GFP[order(J23113_pEmr_GFP$ID),]
J23114_pEmr_GFP <- J23114_pEmr_GFP[order(J23114_pEmr_GFP$ID),]
empty_pEmr_GFP$Treat_ID <- as.factor(empty_pEmr_GFP$Treatment)
J23106_pEmr_GFP$Treat_ID <- as.factor(J23106_pEmr_GFP$Treatment)
J23113_pEmr_GFP$Treat_ID <- as.factor(J23113_pEmr_GFP$Treatment)
J23114_pEmr_GFP$Treat_ID <- as.factor(J23114_pEmr_GFP$Treatment)

```{r warning=FALSE, message=FALSE, fig.height=6, fig.width=11}
library(ggplot2) # loads the ggplot library

ggplot(data=empty_pEmr_GFP, aes(x=Treat_ID, y=mean_NFU)) +
  geom_bar(colour="black", fill="#DD8888", width=.8, stat="identity") + 
  geom_errorbar(aes(ymin=mean_NFU-sd_NFU, ymax=mean_NFU+sd_NFU),
                size=.3,    # Thinner lines
                width=.2,
                position=position_dodge(.9)) + 
  xlab("Laccase Treatment (units)") +
  ylab("Normalized Fluorescence (481/520)") +
  ggtitle("pEmrGFP_empty response to Fungal laccase-treated HWKL lignin ") +
  theme_bw()

ggplot(data=J23106_pEmr_GFP, aes(x=Treat_ID, y=mean_NFU)) +
  geom_bar(colour="black", fill="#DD8888", width=.8, stat="identity") + 
  geom_errorbar(aes(ymin=mean_NFU-sd_NFU, ymax=mean_NFU+sd_NFU),
                size=.3,    # Thinner lines
                width=.2,
                position=position_dodge(.9)) + 
  xlab("Laccase Treatment (units)") +
  ylab("Normalized Fluorescence (481/520)") +
  ggtitle("pEmrGFP_J23106 response to Fungal laccase-treated HWKL lignin ") +
  theme_bw()

ggplot(data=J23113_pEmr_GFP, aes(x=Treat_ID, y=mean_NFU)) +
  geom_bar(colour="black", fill="#DD8888", width=.8, stat="identity") + 
  geom_errorbar(aes(ymin=mean_NFU-sd_NFU, ymax=mean_NFU+sd_NFU),
                size=.3,    # Thinner lines
                width=.2,
                position=position_dodge(.9)) + 
  xlab("Laccase Treatment (units)") +
  ylab("Normalized Fluorescence (481/520)") +
  ggtitle("pEmrGFP_J23113 response to Fungal laccase-treated HWKL lignin ") +
  theme_bw()

ggplot(data=J23114_pEmr_GFP, aes(x=Treat_ID, y=mean_NFU)) +
  geom_bar(colour="black", fill="#DD8888", width=.8, stat="identity") + 
  geom_errorbar(aes(ymin=mean_NFU-sd_NFU, ymax=mean_NFU+sd_NFU),
                size=.3,    # Thinner lines
                width=.2,
                position=position_dodge(.9)) + 
  xlab("Laccase Treatment (units)") +
  ylab("Normalized Fluorescence (481/520)") +
  ggtitle("pEmrGFP_J23114 response to Fungal laccase-treated HWKL lignin ") +
  theme_bw()
