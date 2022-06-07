#Basic PieChart with %
install.packages("ggplot2")
library(ggplot2)

#Data preparation
df <- data.frame(
  Gender = c("Male", "Female"),
  value = c(50, 26)
)
head(df)

df <- df %>%
  arrange(desc(Gender)) %>%
  mutate(prop = round(value*100/sum(value), 1),
         lab.ypos = cumsum(prop) - 0.5*prop)
head(df, 4)

ggplot(df, aes(x = "", y = value, fill = Presentations)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  ggtitle("Country distribution of 195 online registered participants") +
  geom_text(aes(label = value), color = "white")+
  coord_polar("y", start = 0)+
  ggpubr::fill_palette("Set3")+
  theme_void()
................................................................................
#Basic PieChart without %
................................................................................
df <- data.frame(
  Country = c("Nigeria", "United Kingdom"),
  value = c(38, 2)
)
head(df)

# default plot
p <- ggplot(df, aes(x="", y = value, fill=Country)) + 
  geom_bar(width = 1, stat = "identity") +coord_polar("y", start=0) +
  ggtitle("Country distribution of 40 pre-conference workshop participants")
p


# Use custom fill color palettes
p + scale_fill_manual(values=c("#999999","#800080"))

# Customise the pie chart
blank_theme <- theme_minimal()+ theme(
  axis.title.x = element_blank(),
  axis.title.y = element_blank(), axis.text.x=element_blank(),
  panel.border = element_blank(), panel.grid=element_blank(),
  axis.ticks = element_blank(), plot.title=element_text(size=14, face="bold") )

# Apply blank theme
require(scales)
p + scale_fill_brewer("Country") + blank_theme +
  geom_text(aes(y = value/2 + c(0, cumsum(value)[-length(value)]), 
                label = value), size=5)

pie + scale_fill_brewer("Career") + blank_theme +
  theme(axis.text.x=element_blank())+
  geom_text(aes(y = value/7 + c(0, cumsum(value)[-length(value)]), 
                label = value), size=5)
