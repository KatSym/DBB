library(tidyverse)
library(vegan)

# load data
cop_counts = read.csv("Copepod_Diversity.csv", header = T, stringsAsFactors = T) %>% 
  select(-Volume) %>% 
  mutate(Treatment = gsub("[[:digit:]]", "", Mesocosm),
         Treatment = factor(Treatment, levels = c("C", "A", "P", "B"))) %>% 
  filter(Treatment != "B")

param = cop_counts %>% select(Day, Mesocosm, Treatment)

spe = cop_counts %>% 
  select(-Day, -Mesocosm, -Treatment) %>% 
  mutate_all(log1p) 

nmds = metaMDS(spe, "bray", try =999) %>% beepr::beep(1)

ordiplot(nmds, type = "n", main = paste( "NMDS - Stress =",round(nmds$stress, 3)))
#         ylim= c(-0.1,0.1), xlim= c(-0.2,0.2))
orditorp(nmds,display="sites", cex = 1.1)
ord = ordiellipse(nmds, param$Treatment, display = "sites",
                  kind = "se", conf = 0.95, label = T)

# to make the graph
NMDS = data.frame(NMDS1 = nmds$points[,1], 
                  NMDS2 = nmds$points[,2],
                  Treatment = param$Treatment,
                  Day = param$Day)

veganCovEllipse <- function(cov, center = c(0, 0), scale = 1, npoints = 100) 
{
  theta <- (0:npoints) * 2 * pi/npoints
  Circle <- cbind(cos(theta), sin(theta))
  t(center + scale * t(Circle %*% chol(cov)))
}

df_ell <- data.frame()
for (g in levels(NMDS$Treatment)) {
  df_ell <- rbind(df_ell, cbind(as.data.frame(with(NMDS[NMDS$Treatment == g,],
                                                   veganCovEllipse(cov.wt(cbind(NMDS1,NMDS2),wt=rep(1/length(NMDS1),length(NMDS1)))$cov,
                                                                   center=c(mean(NMDS1),mean(NMDS2)))))
                                ,Treatment=g))
}


(nmds.plot = 
    ggplot(data = NMDS, aes(NMDS1, NMDS2)) + 
    geom_point(aes(shape = Treatment, colour = Treatment), size = 2) +
    geom_text(aes(label = Day), hjust = -0.8, vjust = 0.5, size = 3) +
#    scale_color_manual(values = set2)+
#    scale_shape_manual(values = c(19, 15, 17)) +
#    geom_path(data = df_ell, aes(x = NMDS1, y = NMDS2, linetype = Treatment, colour = Treatment), size = 0.5) +
    #  annotate("text", label = "Stress = 0.071", 
    #           x = 0.03, y = 0.019, 
    #           size = 5, color = "grey50", fontface = "italic") +
    theme_bw() +
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          axis.title.y = element_text(size = 9, colour = "black"), 
          axis.title.x = element_text(size = 9, colour = "black"),
          axis.text = element_text(size = 8),
          #       legend.position = "none",
          legend.key = element_blank(),
          legend.box.background = element_blank(),
          legend.background = element_blank(),
          legend.title = element_blank(),
          legend.justification =  c(1,0),
          legend.position = c(1,0.02),
          legend.key.size = unit(1.5, "mm"),
          aspect.ratio = 1))
