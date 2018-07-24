library(ggplot2)
library(readxl)
library(plyr)
library(dplyr)
library(ggrepel)
library(extrafont)
library(lattice)
library(grid)
library(gridExtra)

# Data collection and subsetting ----------------------------------------------------------------

setwd("C:/Users/Corey/Documents/the-history-of-baseball")
batting <- read.csv("batting.csv")
batting_sub <- subset(batting, ab >= 150)
pitching <- read.csv("pitching.csv")
pitching_sub <- subset(pitching, ipouts >= 300)
attend <- read.csv("home_game.csv")
player <- read.csv("player.csv")
fielding <- read.csv("fielding.csv")
dp <- subset(fielding, pos == c("SS", "2B"))
dp <- subset(dp, g >= 10)

# Notable players -------------------------------------------------------------------------------

ted <- batting[29792,]
horns <- batting[20451,]
bonds <- batting[80751,]
ruth <- batting[18518,]
hen <- batting[59615,]
cob <- batting[15351,]
kee <- pitching[248,]
kou <- pitching[16960,]
ker <- pitching[41478,]
fel <- pitching[12389,]
sal <- pitching[44000,]
you <- pitching[2625,]
arr <- pitching[43356,]
ny1920 <- attend[830,]
ny2014 <- attend[2932,]
mus <- player[12163,]
har<- player[7034,]
jet <- fielding[149933,]
riz <- fielding[42859,]

# Function to adjust sig figs on legends -------------------------------------------------------  

fmt_dcimals <- function(decimals=0){
  function(x) format(x,nsmall = decimals,scientific = FALSE)
}

# Batting average -------------------------------------------------------------------------

plot_BA <- ggplot() +
  geom_point(data = batting_sub, aes(x = year, y = (h / ab)), alpha = 0.03, size = 1) +
  geom_smooth(size = 2, data = batting_sub, aes(x = year, y = (h / ab)), color = "dodgerblue") +
  scale_x_continuous(limits = c(1860,2020), breaks = seq(1860,2020,by=40)) +
  geom_label_repel(xlim = 1960, ylim = 0.415, data = ted, aes(x = year, y = (h / ab), label = "T. Williams")) +
  geom_point(data = ted, aes(x = year, y = (h / ab)), alpha = 1, size = 2, color = "dodgerblue") +
  geom_label_repel(xlim = 1860, ylim = 0.420, data = horns, aes(x = year, y = (h / ab), label = "R. Hornsby")) +
  geom_point(data = horns, aes(x = year, y = (h / ab)), alpha = 1, size = 2, color = "dodgerblue") +
  annotate(geom = "text", label = "Batting Average", x = 1950, y = 0.12, color = "black", size = 8) +
  scale_y_continuous(labels = fmt_dcimals(2)) +
  theme(
    axis.title = element_blank(),
    axis.text = element_text(size=14)
  )

# Home runs / at bat --------------------------------------------------------------------------

plot_hr <- ggplot() +
  geom_point(data = batting_sub, aes(x = year, y = (hr / ab)), alpha = 0.03, size = 1) +
  geom_smooth(size = 2, data = batting_sub, aes(x = year, y = (hr / ab)), color = "dodgerblue") +
  scale_x_continuous(limits = c(1860,2020), breaks = seq(1860,2020,by=40)) +
  geom_label_repel(xlim = 1960, ylim = 0.125, data = bonds, aes(x = year, y = (hr / ab), label = "B. Bonds")) +
  geom_point(data = bonds, aes(x = year, y = (hr / ab)), alpha = 1, size = 2, color = "dodgerblue") +
  geom_label_repel(xlim = 1860, ylim = 0.1, data = ruth, aes(x = year, y = (hr / ab), label = "B. Ruth")) +
  geom_point(data = ruth, aes(x = year, y = (hr / ab)), alpha = 1, size = 2, color = "dodgerblue") +
  annotate(geom = "text", label = "HR / AB", x = 1900, y = 0.14, color = "black", size = 8) +
  scale_y_continuous(labels = fmt_dcimals(2)) +
  theme(
    axis.title = element_blank(),
    axis.text = element_text(size=14)
  )

# Stolen bases per game -----------------------------------------------------------------------------

plot_sbg <- ggplot() +
  geom_point(data = batting_sub, aes(x = year, y = (sb / g)), alpha = 0.03, size = 1) +
  geom_smooth(size = 2, data = batting_sub, aes(x = year, y = (sb / g)), color = "dodgerblue") +
  scale_x_continuous(limits = c(1860,2020), breaks = seq(1860,2020,by=40)) +
  geom_label_repel(xlim = 1960, ylim = 1, data = hen, aes(x = year, y = (sb / g), label = "R. Henderson")) +
  geom_point(data = hen, aes(x = year, y = (sb / g)), alpha = 1, size = 2, color = "dodgerblue") +
  geom_label_repel(xlim = 1910, ylim = 0.7, data = cob, aes(x = year, y = (sb / g), label = "T. Cobb")) +
  geom_point(data = cob, aes(x = year, y = (sb / g)), alpha = 1, size = 2, color = "dodgerblue") +
  annotate(geom = "text", label = "SB / G", x = 1890, y = 1.01, color = "black", size = 8) +
  scale_y_continuous(labels = fmt_dcimals(2)) +
  theme(
    axis.title = element_blank(),
    axis.text = element_text(size=14)
  )

# Earned run average ------------------------------------------------------------------------------------

plot_era <- ggplot() +
  geom_point(data = pitching_sub, aes(x = year, y = (er / (ipouts/27))), alpha = 0.03, size = 1) +
  geom_smooth(size = 2, data = pitching_sub, aes(x = year, y = (er / (ipouts/27))), color="indianred1") +
  scale_x_continuous(limits = c(1860,2020), breaks = seq(1860,2020,by=40)) +
  geom_label_repel(xlim = 1875, ylim = 1.3, data = kee, aes(x = year, y = (er / (ipouts/27)), label = "T. Keefe")) +
  geom_point(data = kee, aes(x = year, y = (er / (ipouts/27))), alpha = 1, size = 2, color = "indianred1") +
  geom_label_repel(xlim = 1920, ylim = 0.8, data = kou, aes(x = year, y = (er / (ipouts/27)), label = "S. Koufax")) +
  geom_point(data = kou, aes(x = year, y = (er / (ipouts/27))), alpha = 1, size = 2, color = "indianred1") +
  geom_label_repel(xlim = 1970, ylim = 0.75, data = ker, aes(x = year, y = (er / (ipouts/27)), label = "C. Kershaw")) +
  geom_point(data = ker, aes(x = year, y = (er / (ipouts/27))), alpha = 1, size = 2, color = "indianred1") +
  annotate(geom = "text", label = "ERA", x = 1885, y = 7.6, color = "black", size = 8) +
  scale_y_continuous(labels = fmt_dcimals(2)) +
  theme(
    axis.title = element_blank(),
    axis.text = element_text(size=14)
  )

# Completed games per game started -----------------------------------------------------------------------

plot_cg <- ggplot() +
  geom_point(data = pitching_sub, aes(x = year, y = (cg / gs)), alpha = 0.03, size = 1) +
  geom_smooth(size = 2, data = pitching_sub, aes(x = year, y = (cg / gs)), color="indianred1") +
  scale_x_continuous(limits = c(1860,2020), breaks = seq(1860,2020,by=40)) +
  geom_label_repel(xlim = 1860, ylim = 0.375, data = you, aes(x = year, y = (cg / gs)), label = "C. Young") +
  geom_point(data = you, aes(x = year, y = (cg / gs)), alpha = 1, size = 2, color = "indianred1") +
  geom_label_repel(xlim = 1980, ylim = 0.5, data = arr, aes(x = year, y = (cg / gs)), label = "J Arrieta") +
  geom_point(data = arr, aes(x = year, y = (cg / gs)), alpha = 1, size = 2, color = "indianred1") +
  annotate(geom = "text", label = "CG / GS", x = 1980, y = 0.9, color = "black", size = 8) +
  scale_y_continuous(labels = fmt_dcimals(2)) +
  theme(
    axis.title = element_blank(),
    axis.text = element_text(size=14)
  )

# Strikeouts per nine innings pitched (27 outs pitched) -----------------------------------------------------

plot_sog <- ggplot() +
  geom_point(data = pitching_sub, aes(x = year, y = (so / (ipouts/27))), alpha = 0.03, size = 1) +
  geom_smooth(size = 2, data = pitching_sub, aes(x = year, y = (so / (ipouts/27))), color="indianred1") +
  scale_x_continuous(limits = c(1860,2020), breaks = seq(1860,2020,by=40)) +
  geom_label_repel(xlim = 1875, ylim = 8, data = fel, aes(x = year, y = (so / (ipouts/27)), label = "B. Feller")) +
  geom_point(data = fel, aes(x = year, y = (so / (ipouts/27))), alpha = 1, size = 2, color = "indianred1") +
  geom_label_repel(xlim = 1960, ylim = 12.5, data = sal, aes(x = year, y = (so / (ipouts/27)), label = "C. Sale")) +
  geom_point(data = sal, aes(x = year, y = (so / (ipouts/27))), alpha = 1, size = 2, color = "indianred1") +
  annotate(geom = "text", label = "SO / 9 IP", x = 1905, y = 12, color = "black", size = 8) +
  scale_y_continuous(labels = fmt_dcimals(1)) +
  theme(
    axis.title = element_blank(),
    axis.text = element_text(size=14)
  )

# Annual attendance ----------------------------------------------------------------------------------------

plot_att <- ggplot() +
  geom_point(data = attend, aes(x = year, y = (attendance / 10^6)), alpha = 0.03, size = 1) +
  geom_smooth(size = 2, data = attend, aes(x = year, y = (attendance / 10^6)), color="seagreen3") +
  scale_x_continuous(limits = c(1860,2020), breaks = seq(1860,2020,by=40)) +
  geom_label_repel(xlim = 1900, ylim = 2.5, data = ny1920, aes(x = year, y = (attendance / 10^6)), label = "NY Yankees") +
  geom_point(data = ny1920, aes(x = year, y = (attendance / 10^6)), alpha = 1, size = 2, color = "seagreen3") +
  geom_label_repel(xlim = 1900, ylim = 2.5, data = ny2014, aes(x = year, y = (attendance / 10^6), label = "NY Yankees")) +
  geom_point(data = ny2014, aes(x = year, y = (attendance / 10^6)), alpha = 1, size = 2, color = "seagreen3") +
  annotate(geom = "text", label = "Annual Attendance", x = 1940, y = 4.1, color = "black", size = 8) +
  annotate(geom = "text", label = "(Millions)", x = 1880, y = 3.7, color = "black", size = 4) +
  scale_y_continuous(labels = fmt_dcimals(2)) +
  theme(
    axis.title = element_blank(),
    axis.text = element_text(size=14)
  )

# Player weight ----------------------------------------------------------------------------------------

plot_wgt <- ggplot() +
  geom_point(data = player, aes(x = (birth_year + 22), y = weight), alpha = 0.03, size = 1) +
  geom_smooth(size = 2, data = player, aes(x = birth_year + 20, y = weight), color="seagreen3") +
  scale_x_continuous(limits = c(1860,2020), breaks = seq(1860,2020,by=40)) +
  geom_label_repel(xlim = 1880, ylim = 250, data = mus, aes(x = birth_year + 20, y = weight), label = "S. Musial") +
  geom_point(data = mus, aes(x = birth_year + 20, y = weight), alpha = 1, size = 2, color = "seagreen3") +
  geom_label_repel(xlim = 1940, ylim = 275, data = har, aes(x = birth_year + 20, y = weight, label = "B. Harper")) +
  geom_point(data = har, aes(x = birth_year + 20, y = weight), alpha = 1, size = 2, color = "seagreen3") +
  annotate(geom = "text", label = "Player Weight", x = 1960, y = 125, color = "black", size = 8) +
  annotate(geom = "text", label = "(Lbs)", x = 1915, y = 110, color = "black", size = 4) +
  scale_y_continuous(limits = c(100,300), labels = fmt_dcimals(0)) +
  theme(
    axis.title = element_blank(),
    axis.text = element_text(size=14)
  )

# Double plays per game by middle infielders ----------------------------------------------------------------

plot_dp <- ggplot() +
  geom_point(data = dp, aes(x = year, y = (dp / g)), alpha = 0.03, size = 1) +
  geom_smooth(size = 2, data = dp, aes(x = year, y = (dp / g)), color="seagreen3") +
  scale_x_continuous(limits = c(1860,2020), breaks = seq(1860,2020,by=40)) +
  geom_label_repel(xlim = 1860, ylim = 0.8, data = riz, aes(x = year, y = (dp / g)), label = "P. Rizzuto") +
  geom_point(data = riz, aes(x = year, y = (dp / g)), alpha = 1, size = 2, color = "seagreen3") +
  geom_label_repel(xlim = 1980, ylim = 1.25, data = jet, aes(x = year, y = (dp / g)), label = "D. Jeter") +
  geom_point(data = jet, aes(x = year, y = (dp / g)), alpha = 1, size = 2, color = "seagreen3") +
  annotate(geom = "text", label = "DP / G", x = 1890, y = 1.43, color = "black", size = 8) +
  annotate(geom = "text", label = "(SS + 2B)", x = 1883, y = 1.28, color = "black", size = 4) +
  annotate(geom = "text", label = "Corey Meyer", x = 2000, y = 0, color = "black", size = 3) +
  scale_y_continuous(labels = fmt_dcimals(2)) +
  theme(
    axis.title = element_blank(),
    axis.text = element_text(size=14)
  )

# Collate plots together and save png --------------------------------------------------------------------------------

combo <- arrangeGrob(plot_BA, plot_hr, plot_sbg, plot_era, plot_sog, plot_cg, plot_att, plot_wgt, plot_dp, ncol = 3, nrow = 3, 
                     top = textGrob("The History of Baseball", vjust = 0.5, hjust = 0.61, gp = gpar(fontsize=34, fontface = "bold", cex = 1.5)),
                     left = textGrob("Miscellaneous             Pitching                   Offense", hjust = 0.53, vjust = 0.5, gp=gpar(fontsize=28, fontface = "bold"), rot = 90)) 
ggsave(file="combo.png", combo, dpi = 1000, width = 11, height = 11)

dev.off()