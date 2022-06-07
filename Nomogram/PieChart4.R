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
  Country = c("Ethopia", "Mali", "Morroco", "Nigeria",
             "South Africa", "Sudan", "United Kingdom", "United States"),
  value = c(1, 1, 1, 179, 4, 1, 6, 2)
)
head(df)

df <- df %>%
  mutate(
    pos = cumsum(value) - value /8 ,
    Country = fct_reorder(Country, pos, .desc = TRUE)
  )

# default plot
ggplot(df, aes(x="", y = value, fill = Country)) +
  geom_col(width = 1, position = "stack") +
  coord_polar("y", start = 0) +
  ggtitle("Country distribution of 195 online registered participants") +
  scale_fill_manual(values=c ("#999999","#800080", "#56B4E9", "#800000","#5F9EA0", "#B0C4DE", "#8B4513", "#999999")) +
  blank_theme +
  theme(axis.text.x=element_blank()) +
  geom_text(aes(y = pos, label = value),
            size=5)

