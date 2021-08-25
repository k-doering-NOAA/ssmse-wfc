# Natural mortality patterns
library(ggplot2)
library(nmfspalette)

M_colors <- nmfs_palette("oceans")(3)
M_vals <- data.frame(Year = rep(101:150, times = 3),
           pattern = rep(c("none", "low", "high"), times = c(50, 50, 50)), 
           M = c(rep(0.2, length.out = 50), 
                 rep(c(0.2,0.2,0.2, 0.2, 0.3), times = 10), 
                 rep(c(0.2,0.2,0.2, 0.2, 0.4), times = 10)
                 ))

ggplot(M_vals, aes(x = Year, y = M)) +
  geom_line(aes(color = pattern, linetype = pattern))+
  #geom_point(aes(color = pattern)) +
  scale_color_nmfs(palette = "regional web")+
  theme_classic()
ggsave("figures/M.png", width = 6, height = 4, units = "in")

# plot selectivity -----
sel_vals <- read.csv("sel_example.csv")
colnames(sel_vals) <- c("row", "Selectivity")
sel_vals$Year <- 101:150

ggplot(sel_vals, aes(x = Year, y = Selectivity))+
  geom_hline(yintercept = 5.27531) + # the selectivity val devs are around
  geom_line(color = nmfs_palette()(1))+
  geom_point(color = nmfs_palette()(1))+
  scale_color_nmfs()+
  theme_classic()
ggsave("figures/selectivity.png", width = 6, height = 4, units = "in")

# plot recdevs ----
recdevs <- read.csv("recdevs.csv")
ggplot(recdevs, aes(x = Year, y = Recdevs))+
  geom_hline(yintercept = 0) +
  geom_line(color = nmfs_palette("waves")(1))+
  geom_point(color = nmfs_palette("waves")(1))+
  scale_color_nmfs()+
  theme_classic()
ggsave("figures/recdevs.png", width = 6, height = 4, units = "in")



