library(tidyverse)
library(lubridate)
library(cowplot)

#read in capture data
mol <- readxl::read_xlsx("./data/Molmol_database_20220713.xlsx", sheet = "READY")

ages <- mol %>% filter(transp5 != "n") %>% 
  filter(sex == "f") %>% 
  group_by(transp5) %>% 
  summarize(firstCap = min(date),
            lastCap = max(date),
            age = as.integer(lastCap - firstCap),
            age.yr = age/364,
            ncaps = n())

ages %>% filter(ncaps > 1) %>% 
  summarize(median.days = median(age, na.rm = T),
            min.days = min(age, na.rm = T),
            max.days = max(age, na.rm = T))

# Yann: Lifespan of adult females from marking time ranged from five to 1709 days (4.7 years) with a median of 280 days (0.8 years).

#histogram assuming that bats are at least 1 year old at first capture
ages %>% filter(ncaps > 1) %>% 
  ggplot()+
  geom_histogram(aes(x = age / 364))+
  theme_bw()+
  geom_vline(xintercept = 321/364)+ #median of max time since first capture
  annotate("text", x = 1, y = 45, label = "median of time since first capture\nfor a bat's last observation (321 days)", hjust = 0)+
  geom_curve(x = 2, y = 40,
             xend = 321/364, yend = 30,
             color = 1,
             arrow = arrow(),
             curvature = -0.6, angle = 100)+
  labs(x = "years since first capture", 
       y = "count", 
       title = "Female M. molossus\ntime between first and last capture (2008-2015)")
ggsave(file = "./output/molossusRecapAgeF.png")

length(which(ages$age.yr > 2))
length(which(ages$age.yr > 3))
length(which(ages$age.yr > 4))
length(which(ages$age.yr > 1))
length(which(ages$ncaps > 1))

#What are we starting with?
mol %>% filter(year(date) >= 2021) %>% 
  summarize(total = n())
  
  ggplot()+
  geom_histogram(aes(x = age / 364))+
  theme_bw()

#Age of each bat per year
firstcaps <- mol %>% filter(transp5 != "n") %>% 
  group_by(transp5) %>% 
  summarize(firstCap = min(date))

mol <- mol %>% left_join(firstcaps)
mol <- mol %>% mutate(age.day = date(date) - date(firstCap),
                      age.yr = as.numeric(age.day / 364))

#There are some negative ages. Who are they to correct the dates? Just ignore them. This is fixed, but the problem was that they are listed as both f & m. 
tmp <- mol %>% filter(transp5 %in% c("30695", "87CE1", "FB49A")) %>% 
  group_by(transp5) %>% 
  summarize(firstCap = min(date))

mol$age.floor.yr <- floor(mol$age.yr)

agetable <- mol %>% group_by(year(date), age.floor.yr) %>%
  summarize(count = n()) %>% 
  mutate(freq = count / sum(count))

agetable<- agetable %>% replace_na(list(age.floor.yr = "no recap"))

ageOldLabs <- c("0", "1", "2", "3", "4", "6", "no recap")
ageNewLabs <- c("0-1", "1-2", "2-3", "3-4", "4-5", "6-7", "no recap")
mycols <- c(RColorBrewer::brewer.pal(9, "PuBuGn")[3:8], "grey44")


full <- agetable %>% 
  ggplot()+
  geom_bar(aes(fill = age.floor.yr, y = freq, x = `year(date)`), 
           position="dodge", stat="identity")+
  scale_fill_manual(values = mycols,
                    breaks = c("0", "1", "2", "3", "4", "6", "no recap"),
                    labels = c("0-1", "1-2", "2-3", "3-4", "4-5", "6-7", "not\nrecaptured"), 
                    name = "Age Category\n(years)")+
  labs(x ="year", 
       y = "frequency of total annual captures",
       title = "Annual age distributions")+
  theme_bw()

zoomed <- agetable %>% 
  ggplot()+
  geom_bar(aes(fill = age.floor.yr, y = freq, x = `year(date)`), 
           position="dodge", stat="identity")+
  scale_fill_manual(values = mycols,
                    breaks = c("0", "1", "2", "3", "4", "6", "no recap"),
                    labels = c("0-1", "1-2", "2-3", "3-4", "4-5", "6-7", "not\nrecaptured"), 
                    name = "Age Category\n(years)")+
  labs(x ="year", 
       y = "frequency of total annual captures",
       title = "Annual age distributions -- Zoomed in")+
  coord_cartesian(ylim=c(0, 0.20))+
  theme_bw()   

pdf(file = "./output/AnnualAgeDistribution_success.pdf", width = 16, height = 8)
plot_grid(full, zoomed)
dev.off()

mol %>% group_by(year(date)) %>% 
  summarize(nInds = length(unique(transp5, na.rm = TRUE)),
            nBatsCapt = n()) %>% 
  ggplot()+
  geom_point(aes(x = `year(date)`, 
              y = nInds, 
              size = nBatsCapt))+
    labs(x = "year", y = "number of unique bats captured")+
  theme_bw()
