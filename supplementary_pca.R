### PCA
library(ggfortify)
library(cluster)
library(grid)
library(gridExtra)

spec.bin <- procspec(allspec.orn.avg, opt = c("bin", "center"))
colnames(spec.bin) <- allcategory 

spec.bin <- t(spec.bin) 
spec.bin <- spec.bin[-1, ]
pca1 <- prcomp(spec.bin, scale. = TRUE)

summary(pca1) #PC1 explains 65% of the variation in spectral shape and describes the ratio of short to long wavelengths reflected

ao <- as.character(cate)
sb <- cbind(spec.bin, ao)

autoplot(pca1, data=sb, colour="ao")

autoplot(pca1, data=sb, colour="ao", frame=TRUE, frame.type = "norm")

####
## Remove for better visualization
### change names for subsetting
soa <- allspec.orn.avg
colnames(soa) <- allcategory

## Within Baltimore, historic vs modern
baltmodhist <- subset(soa, c("Balt_mod", "Balt_hist")) # n = 40 but names changed? 

spec.bin.bamh <- procspec(baltmodhist, opt = c("bin", "center"))

spec.bin.bamh <- t(spec.bin.bamh) 
spec.bin.bamh <- spec.bin.bamh[-1, ]
pca.bamh <- prcomp(spec.bin.bamh, scale. = TRUE)

summary(pca.bamh) # 71.7% of variation

bamh <- as.character(colnames(baltmodhist))
bamh <- gsub("\\..*", "", bamh) # remove everything after "."
bamh <- bamh[-1]
sb.bamh <- cbind(spec.bin.bamh, bamh) 

autoplot(pca.bamh, data=sb.bamh, colour = "bamh", frame=TRUE, frame.type = "norm", legendlabs = c("Baltimore historic", "Baltimore modern")) + 
  theme_classic() +
  mytheme+
  labs(fill="Specimen type", color="Specimen type")
ggsave("pca_baltimore.tiff", plot = ggplot2::last_plot(), width=200, height=150, units="mm", dpi=300)

# Use ggplot
df.bamh <- as.data.frame(pca.bamh$x)
df.bamh$group <- bamh

ggplot(df.bamh, aes(x=PC1, y=PC2, color=group))+
  geom_point()+
  theme_classic() +
  mytheme


#######
# Within Bullock's, historic vs modern
bullmodhist <- subset(soa, c("Bull_mod", "Bull_hist"))

spec.bin.bumh <- procspec(bullmodhist, opt = c("bin", "center"))

spec.bin.bumh <- t(spec.bin.bumh) 
spec.bin.bumh <- spec.bin.bumh[-1, ]
pca.bumh <- prcomp(spec.bin.bumh, scale. = TRUE)

summary(pca.bumh) # 55.5% of variation

bumh <- as.character(colnames(bullmodhist))
bumh <- gsub("\\..*", "", bumh) # remove everything after "."
bumh <- bumh[-1]
sb.bumh <- cbind(spec.bin.bumh, bumh) 

autoplot(pca.bumh, data=sb.bumh, colour = "bumh", frame=TRUE, frame.type = "norm") + 
  theme_classic() +
  mytheme +
  labs(fill="Specimen type", color="Specimen type")

ggsave("pca_bullocks.tiff", plot = ggplot2::last_plot(), width=200, height=150, units="mm", dpi=300)

# Use ggplot
df.bumh <- as.data.frame(pca.bumh$x)
df.bumh$group <- bumh

ggplot(df.bumh, aes(x=PC1, y=PC2, color=group))+
  geom_point()+
  theme_classic() +
  mytheme

################
## Within Baltimore, all 3
baltmodhistref <- subset(soa, c("Balt_mod", "Balt_hist", "Balt_ref")) # n = 40 but names changed? 

spec.bin.bamhr <- procspec(baltmodhistref, opt = c("bin", "center"))

spec.bin.bamhr <- t(spec.bin.bamhr) 
spec.bin.bamhr <- spec.bin.bamhr[-1, ]
pca.bamhr <- prcomp(spec.bin.bamhr, scale. = TRUE)

bamhr <- as.character(colnames(baltmodhistref))
bamhr <- gsub("\\..*", "", bamhr) # remove everything after "."
bamhr <- bamhr[-1]
sb.bamhr <- cbind(spec.bin.bamhr, bamhr) 

autoplot(pca.bamhr, data=sb.bamhr, colour = "bamhr", frame=TRUE, frame.type = "norm", legendlabs = c("Baltimore historic", "Baltimore modern")) + 
  theme_classic() +
  mytheme+
  labs(fill="Specimen type", color="Specimen type")
ggsave("pca_baltimore_all.tiff", plot = ggplot2::last_plot(), width=200, height=150, units="mm", dpi=300)


#######
# Within Bullock's, historic vs modern
bullmodhistref <- subset(soa, c("Bull_mod", "Bull_hist", "Bull_ref")) # n = 40 but names changed? 

spec.bin.bumhr <- procspec(bullmodhistref, opt = c("bin", "center"))

spec.bin.bumhr <- t(spec.bin.bumhr) 
spec.bin.bumhr <- spec.bin.bumhr[-1, ]
pca.bumhr <- prcomp(spec.bin.bumhr, scale. = TRUE)

bumhr <- as.character(colnames(bullmodhistref))
bumhr <- gsub("\\..*", "", bumhr) # remove everything after "."
bumhr <- bumhr[-1]
sb.bumhr <- cbind(spec.bin.bumhr, bumhr) 

autoplot(pca.bumhr, data=sb.bumhr, colour = "bumhr", frame=TRUE, frame.type = "norm") + 
  theme_classic() +
  mytheme +
  labs(fill="Specimen type", color="Specimen type")

ggsave("pca_bullocks_all.tiff", plot = ggplot2::last_plot(), width=200, height=150, units="mm", dpi=300)


#######
# contrast between species
## Historic
intercolorhist <- subset(soa, c("Bull_hist", "Balt_hist")) # n = 40 but names changed? 

spec.bin.ich <- procspec(intercolorhist, opt = c("bin", "center"))

spec.bin.ich <- t(spec.bin.ich) 
spec.bin.ich <- spec.bin.ich[-1, ]
pca.ich <- prcomp(spec.bin.ich, scale. = TRUE)

ich <- as.character(colnames(intercolorhist))
ich <- gsub("\\..*", "", ich) # remove everything after "."
ich <- ich[-1]
sb.ich <- cbind(spec.bin.ich, ich) 

autoplot(pca.ich, data=sb.ich, colour = "ich", frame=TRUE, frame.type = "norm") + 
  theme_classic() +
  mytheme +
  labs(fill="Specimen type", color="Specimen type")

ggsave("pca_inter_historic.tiff", plot = ggplot2::last_plot(), width=200, height=150, units="mm", dpi=300)

## modern
intercolormod <- subset(soa, c("Bull_mod", "Balt_mod")) # n = 40 but names changed? 

spec.bin.icm <- procspec(intercolormod, opt = c("bin", "center"))

spec.bin.icm <- t(spec.bin.icm) 
spec.bin.icm <- spec.bin.icm[-1, ]
pca.icm <- prcomp(spec.bin.icm, scale. = TRUE)

icm <- as.character(colnames(intercolormod))
icm <- gsub("\\..*", "", icm) # remove everything after "."
icm <- ich[-1]
sb.icm <- cbind(spec.bin.icm, icm) 

autoplot(pca.icm, data=sb.icm, colour = "icm", frame=TRUE, frame.type = "norm") + 
  theme_classic() +
  mytheme +
  labs(fill="Specimen type", color="Specimen type")

ggsave("pca_inter_modern.tiff", plot = ggplot2::last_plot(), width=200, height=150, units="mm", dpi=300)
```
