# Created a new file with only id, ratio and group(treatment)
  # Named "data1_30_march.xlsx", save as "data1_30_march.csv"


#Packages may be used#####
library(RColorBrewer) # Colourful
library(ggplot2) # Draw a plot
library(ggsignif) # Could be signed the significant on the plot
##Set work diection and attach the data####
setwd("D:/OnkPat/DNA_fiber_assay")
setwd("D:/OnkPat/DNA_fiber_assay/20190415/")
setwd("/media/mingzhi/Seagate Backup Plus Drive/OnkPat/DNA_fiber_assay/20190411")
# Set the work direction, aka where the file is.
d <- read.csv("data1_30_march.csv", header = TRUE, sep = ",", dec = ".")
d <- read.csv("190327DNAfiber.csv", header = TRUE, sep = ",", dec = ".")
d <- read.csv("20190415data3.csv", header = TRUE, sep = ",", dec = ".")
# Create a variable called "d", is the data.frame from the data
attach(d)
# attach the various in data
  # so "id" is the number of fibers, "ratio" is the ratio of "green/red", 
  # "group" is different treatment.

##Plot the boxplot in Basic####
levels(group)
# Check the order of groups (will show later on x axis)
group_in_order <- factor(d$group, levels = c("UNT","0.2mM HU", "1uM NVP", "NVP + HU", 
                                    "100ng/ml IGF1","IGF1 + HU", "IGF1 + NVP", 
                                    "IGF1 + NVP + HU"))
# If the order of groups is not same like we want,
  # Run code above


library(RColorBrewer)
color_in_plot <- brewer.pal(n = length(levels(group_in_order)), name = "Set1")

windows() 
# open a new windows for the figure

levelProp <- summary(group_in_order)/length(group_in_order)

boxplot(ratio~group_in_order, main = "DNA fiber assay (HeLa)",
        xlab = "Treatment", ylab = "Ratio (Green/Red)", ylim = c(0,2),
        col = color_in_plot) 
# plot a boxplot of ratio by group


boxplot(length~group_in_order, main = "DNA fiber assay (HeLa)",
        xlab = "Treatment", ylab = "length", ylim = range(length),
        col = color_in_plot) 
# plot a boxplot of length by group



points(group,ratio, pch = 21,col="grey50" )  
# points new dots in the figure to show the every single ratio

##t.test and/or ANOVA####

t.test( ratio[group == "IGF1 + HU"],ratio[group == "0.2mM HU"], alternative = "l")$p.value
t.test(ratio[group == "UNT"], ratio[group == "1uM NVP"])$p.value
t.test(length[group == "UNT"], length[group == "100ng/ml IGF1"])
t.test(length[group == "IGF1 + NVP"], length[group == "IGF1 + NVP + HU"])
t.test(ratio[group == "NVP + HU"], ratio[group == "NVP + HU B"])$p.value
# Use t.test to calculate the ratio p value between group 0.2nM HU and IGF1 + HU
summary(aov(ratio ~ group))

##Outout the plot####
# Output as png file
png(filename = "Figure_of_DNA_fiber_assay.png", units = "in", 
    width = 15, height = 15, res = 300)
boxplot(ratio~group_in_order, main = "DNA fiber assay (HeLa)",
        xlab = "Treatment", ylab = "Ratio (Green/Red)", ylim = c(0,2),
        col = color_in_plot,width = levelProp, cex.main = 2.5, 
        cex.lab = 1.5,outline=FALSE) 
points(group,ratio, pch = 20,col="grey50" )  
dev.off()





##Violin plots####
# Violin Plots
  #library the ggplot2 package
install.packages("ggplot2")
library(ggplot2)

##Trying#####
violin_plot <- ggplot(d, aes(x = group_in_order, y = ratio, color = group_in_order)) + 
  geom_violin()
violin_plot

#trim the tails or not
windows()
ggplot(d,aes(x = group_in_order, y = ratio)) + 
  geom_violin(trim = FALSE)

## show up the specific group(s)
  #violin_plot + scale_x_discrete(limits=c("", ""))

# add mean points
violin_plot + stat_summary(fun.y = mean, geom = "point", shape = 23, size = 2)
# add median points
violin_plot + stat_summary(fun.y = median, geom = "point", size = 2, color = "red")
# add the boxplot inner the violin plot
violin_plot + geom_boxplot(width = 0.1)
# add the mean and standard deviation
data_summary <- function(x){
  m <- mean(x)
  ymin <- m - sd(x)
  ymax <- m + sd(x)
  return(c(y = m, ymin = ymin, ymax = ymax))
}
violin_plot + stat_summary(fun.data = data_summary)
  # OR
windows()
violin_plot + stat_summary(fun.data = mean_sdl,mult=1,
                           geom = "crossbar", width = 0.2)
  ## not clearly at all

# With dots
  ## jitter = 0.2 means the degree of jitter in x direction
violin_plot + geom_jitter(shape=16, position=position_jitter(0.2))
windows()
violin_plot + scale_color_brewer(palette = "Dark2")

vp <- ggplot(d, aes(x = group_in_order, y = ratio, fill = group_in_order)) + 
  scale_fill_brewer(palette = "Dark2") + # could be palette or scale_fill-grey() or fill_manual
  geom_violin(trim = TRUE) + # trim the tail or not, default is TRUE
  geom_boxplot(width = 0.1, fill = "white") + # add a boxplot in violin plot, otherwise could add dots in violinplot
  #OR
  geom_jitter(shape = 16, position = position_jitter(0.1))
  labs(title = " Plot of ratio by treatment", x = " Treatment", y = " Ratio") + # main, xlab and ylab
  theme(legend.position = "bottomright")

ggplot(d, aes(x = group_in_order, y = ratio, fill = group_in_order)) + 
  scale_fill_brewer(palette = "Dark2") + # could be palette or scale_fill-grey() or fill_manual
  geom_violin(trim = FALSE) + # trim the tail or not, default is TRUE
  geom_boxplot(width = 0.1, fill = "white") + # add a boxplot in violin plot, otherwise could add dots in violinplot
  #OR
  ## geom_jitter(shape = 16, position = position_jitter(0.2))
  labs(title = "Plot of ratio by treatment", 
       x = "Treatment", 
       y = "Ratio") + # main, xlab and ylab
  theme(plot.title = element_text(hjust = 0.5, size =35, face = "bold"),
        axis.title.x = element_text(hjust = 0.5, size = 22),
        axis.title.y = element_text(hjust = 0.5, size = 22),
        legend.position = "bottomright") + 
  geom_signif(comparisons = list(c("0.2mM HU","IGF1 + HU")),
              map_signif_level = TRUE)


##Formal finish plotting#####

png(filename = "4_VP_RATIO(1).png", units = "in", 
    width = 12, height = 10, res = 300)

ggplot(d, aes(x = group_in_order, y = ratio, fill = group_in_order)) + 
  scale_fill_brewer(palette = "Dark2") + # could be palette or scale_fill-grey() or fill_manual
  geom_violin(trim = TRUE) + # trim the tail or not, default is TRUE
  geom_boxplot(width = 0.1, fill = "white") + # add a boxplot in violin plot, otherwise could add dots in violinplot
  #OR
  ## geom_jitter(shape = 16, position = position_jitter(0.2))
  labs(title = "Plot of ratio by treatment (R-)", 
       x = "Treatment", 
       y = "Ratio", 
       caption = "P value = 0.074 of t-test between HU and IGF1 + HU",
       fill = "Treatment") + # main, xlab, ylab and title of legend
  theme(plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),
        axis.title.x = element_text(hjust = 0.5, size = 15),
        axis.title.y = element_text(hjust = 0.5, size = 15),
        axis.text = element_text(size = 10),
        legend.text = element_text(size = 10),
        legend.position = "right", 
        legend.margin = margin(0.2, 0.2, 0.2, 0.2, "cm"),
        legend.box.margin = margin(0.8,0.8,0.8,0.8,"cm"))  + 
  geom_signif(comparisons = list(c("0.2mM HU","IGF1 + HU")),
              map_signif_level = TRUE, test = "t.test",
              y_position = 3.25, tip_length = 0.01, vjust = 0.2,
              annotations = "NS.") + # Significant between HU and IGF1+HU
  geom_signif(comparisons = list(c("UNT","0.2mM HU"),c("1uM NVP","NVP + HU"),
                                 c("100ng/ml IGF1","IGF1 + HU"), c("IGF1 + NVP", "IGF1 + NVP + HU")),
              map_signif_level = TRUE, test = "t.test",
              y_position = 3, tip_length = 0.01, vjust = 0.2) + 
  geom_signif(comparisons = list(c("IGF1 + HU", "IGF1 + NVP + HU")),
              map_signif_level = TRUE, test = "t.test",
              y_position = 3.15, tip_length = 0.01, vjust = 0.2)# Significant between every two groups
  
dev.off()



##Formal finish length plotting#####

png(filename = "2_BP_LENGTH.png", units = "in", 
    width = 10, height = 10, res = 300)

ggplot(d, aes(x = group_in_order, y = length, fill = group_in_order)) + 
  scale_fill_brewer(palette = "Dark2") + # could be palette or scale_fill-grey() or fill_manual
  geom_boxplot() + # add a boxplot in violin plot, otherwise could add dots in violinplot
  #OR
  ## geom_jitter(shape = 16, position = position_jitter(0.2))
  labs(title = "Plot of length by treatment", 
       x = "Treatment", 
       y = "Length (um)") + # main, xlab and ylab
  theme(plot.title = element_text(hjust = 0.5, size =25, face = "bold"),
        axis.title.x = element_text(hjust = 0.5, size = 20),
        axis.title.y = element_text(hjust = 0.5, size = 20),
        legend.position = "right")  #+ 
  geom_signif(comparisons = list(c("UNT","100ng/ml IGF1")),
              map_signif_level = TRUE) 
dev.off()


