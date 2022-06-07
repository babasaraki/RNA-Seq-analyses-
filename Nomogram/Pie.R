library(tidyverse)

# Customise the pie chart
blank_theme <- theme_minimal()+
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(), axis.text.x=element_blank(),
    panel.border = element_blank(), panel.grid=element_blank(),
    axis.ticks = element_blank(), plot.title=element_text(size=14, face="bold")
  )

df <- data.frame(
  Students = c("Improved", "Deteriorated", "No effects"),
  value = c(30.3, 20.1, 49.6)
)

df <- df %>%
  mutate(
    pos = cumsum(value) - value / 2 ,
    Students = fct_reorder(Students, pos, .desc = TRUE)
  )

pie + scale_fill_brewer(palette="Blues")+
  theme_minimal()

# default plot
ggplot(df, aes(x="", y = value, fill = Students)) +
  geom_col(width = 1, position = "stack") +
  coord_polar("y", start = 0) +
  ggtitle("Perception of Academic Performance Affected by Smartphone") +
  scale_fill_manual(values=c ("#999999","#800080", "#56B4E9", "#800000")) +
  blank_theme +
  theme(axis.text.x=element_blank()) +
  geom_text(aes(y = pos, label = value),
            size=5)


# Lollipo plot

library(ggplot2)

#Data preparation
df <- data.frame(
  Applications = c("WhatsApp", "WeChat", "FaceBook", "Twitter", "Instagram", 
             "SnapChat", "Telegram", "Other"),
  Value = c(72, 0.8, 7.5, 4.2, 13.7, 0.4, 0.8, 0.5)
)
head(df)


ggplot(df, aes(x=Applications, y=Value) ) +
  geom_segment( aes(x=Applications ,xend=Applications, y=0, yend=Value), color="Grey") +
  ggtitle("Percentage distribution of the most frequently used application among students") +
  geom_point(size=3, color="#69b3a2") +
  coord_flip() +
  theme_classic()+
  theme(
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_blank(),
    legend.position="none"
  ) +
  xlab("")


df <- data.frame(faculty=c("FMHS", "FVM", "FHE", "FMLC", "FEM", "FE"),
                 len=c(4.2, 10, 29.5))
head(df)


df2 <- data.frame(supp=rep(c("VC", "OJ"), each=3),
                  dose=rep(c("D0.5", "D1", "D2"),2),
                  len=c(6.8, 15, 33, 4.2, 10, 29.5))
head(df2)

library(ggplot2)
# Basic barplot
p<-ggplot(data=df, aes(x=dose, y=len)) +
  geom_bar(stat="identity")
p

# Stacked barplot with multiple groups
ggplot(data=df2, aes(x=dose, y=len, fill=supp)) +
  geom_bar(stat="identity")
# Use position=position_dodge()
ggplot(data=df2, aes(x=dose, y=len, fill=supp)) +
  geom_bar(stat="identity", position=position_dodge())
