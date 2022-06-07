library(ggplot2)

#Data preparation
df <- data.frame(
  Career = c("Assistant Professor", "MSc Student", "Others", "PhD Student",
             "Postdoctoral Researcher", "Professor", "Undergraduate Student"),
  Value = c(25, 191, 100, 206, 58, 14, 61)
)
head(df)


ggplot(df, aes(x=Career, y=Value) ) +
  geom_segment( aes(x=Career ,xend=Career, y=0, yend=Value), color="Grey") +
  ggtitle("Career stage distribution of 655 NBGN members") +
  geom_point(size=3, color="#69b3a2") +
  coord_flip() +
  theme_classic()+
  theme(
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_blank(),
    legend.position="none"
  ) +
  xlab("")
