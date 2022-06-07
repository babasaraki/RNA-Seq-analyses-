library(ggplot2)
library(dplyr)
#Data preparation
df <- data.frame(
  Gender = c("Male", "Female"),
  value = c(28, 12)
)
head(df)


df <- df %>%
  arrange(desc(Gender)) %>%
  mutate(prop = round(value*100/sum(value), 1),
         lab.ypos = cumsum(prop) - 0.5*prop)
head(df, 2)

ggplot(df, aes(x = "", y = value, fill = Gender)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  ggtitle("Pre-conference workshop by gender") +
  geom_text(aes(y = value, label = value), color = "white")+
  coord_polar("y", start = 0)+
  ggpubr::fill_palette("Pastel1")+
  theme_void()


p + scale_fill_manual(values=c("#999999","#800080","#800000", "#E69F00", "#008080", "#008000", "#56B4E9"))
