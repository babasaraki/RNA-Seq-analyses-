df <- data.frame(
  Nationalities = c("Burundi", "Cameroon", "India", "Mexico",
              "N. Mariana Islands", "Nigeria", "Sudan",
              "Sweden", "Switzerland", "Tanzania", "United Kingdom", "Zimbabwe"),
  value = c(1, 1, 2, 1, 1, 640, 1, 1, 1, 1, 4, 1)
)
head(df)

library(ggplot2)

# Barplot
bp<- ggplot(df, aes(x="", y=value, fill=Nationalities))+
  geom_bar(width = 1, stat = "identity") + ggtitle("Nationalities of 655 NBGN members")
bp

#Create a pie chart
pie <- bp + coord_polar("y", start=0)
pie

# Use custom color palettes
pie + scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9"))

# use brewer color palettes
pie + scale_fill_brewer(palette="Dark2")

pie + scale_fill_brewer(palette="Blues")+
  theme_minimal()

# Use grey scale
pie + scale_fill_grey() + theme_minimal()

#Create a blank theme
blank_theme <- theme_minimal()+
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.border = element_blank(),
    panel.grid=element_blank(),
    axis.ticks = element_blank(),
    plot.title=element_text(size=14, face="bold")
  )

# Apply blank theme
library(scales)
pie + theme_classic()+ blank_theme +
  theme(axis.text.x=element_blank())+
  geom_text(aes(y = value/12 + c(0, cumsum(value)[-length(value)]), 
                label = value), size=5)

# Use brewer palette
pie + scale_fill_brewer("Country") + blank_theme +
  theme(axis.text.x=element_blank())+
  geom_text(aes(y = value/8 + c(0, cumsum(value)[-length(value)]), 
                label = value), size=5)
