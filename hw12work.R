library(ggplot2)
library(ggthemes)
library(patchwork)
library(TeachingDemos)

d <- read.csv("hw11Files/cleaned/allSotaliaFF.csv")

#qplots
qplot(x=d$mean,
      y=d$sd,
      geom=c("smooth","point"),
      method="lm")

qplot(x=d$behavior,
      y=d$sd,
      geom="boxplot",
      fill=I("lightsteelblue"))

qplot(x=d$group,
      y=d$sd,
      geom="boxplot",
      fill=I("lightsteelblue"))

#Themes
p1 <- ggplot(data=d,
             mapping=aes(x=start,y=end))+ geom_point() +geom_smooth()
print(p1)

p1 + theme_linedraw()
p1 + theme_base()
p1 + theme_bw()

p2 <- ggplot(data=d,mapping=aes(x=behavior, fill=group)) + geom_bar()
print(p2)

#playing with shape of scatterplots
p3 <- ggplot(data=d,
             mapping=aes(x=start,y=end, color=behavior))+
  geom_point()+
  labs(title="Start vs. End Frequency of whistles",
       x="Start Frequency",
       y="End Frequency") +
  xlim(5000,40000) +
  ylim(5000,40000)
print(p3)

p4 <- ggplot(data=d,
             mapping=(aes(x=start,
                          y=end,
                          color=sd))) +
  geom_point(size=5) +
  xlim(5000,40000) +
  ylim(5000,40000)
print(p4)

p5 <- ggplot(data=d,
             mapping=(aes(x=start,
                          y=end,
                          color=sd,
                          shape=behavior))) +
  geom_point(size=3) +
  xlim(5000,40000) +
  ylim(5000,40000)
print(p5)

# Faceting
p6 <- ggplot(data=d,
             mapping=(aes(x=mean,y=sd))) +
  geom_point()

p7 <- p6 + facet_grid(.~behavior)

p6 + facet_grid(group~behavior)

p6 + facet_wrap(~group + behavior, scales="free")

#box plot
p8.1 <- ggplot(d,aes(x=behavior,y=sd, fill=behavior)) + 
  geom_boxplot(outlier.shape=NA) + 
  geom_point(size=2,shape=21, position=position_jitter(width=0.1, height=0.7))
print(p8.1)

p8.2 <- ggplot(d,aes(x=behavior,y=sd, fill=behavior)) + 
  geom_boxplot(outlier.shape=NA)
print(p8.2)

p9 <- ggplot(d, aes(x=mean, y=sd, color=behavior, fill=behavior)) + geom_point() + geom_smooth()
print(p9)

#combining plots
(p7) / (p9 | p8.2) +
  plot_annotation(tag_levels = 'A')
