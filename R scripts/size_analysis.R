library(tidyverse)
library(here)
library(ggsci); library(cowplot)

Student <- c("/Users/joy/Box/Joy_worm_images/Elliot")
files <- list.files(Student, full.names = T)[1:72]

## we can use a package called purrr to do this
worms <- purrr::map_dfr(files, ~readr::read_csv(.x)) %>%
  dplyr::mutate(Student = "Elliot")

# take a look at "worms" -- this should be a dataframe containing all your measurements

elliot <- worms %>%
  dplyr::mutate(row_num = row_number()) %>%
  dplyr::select(row_num, Label, Length, Student) %>%
  dplyr::group_by(Animal = rep(row_number(), length.out = n(), each = 2)) %>%
  dplyr::mutate_at(vars(row_num), ~dplyr::case_when(Length < 60 ~ "Width",
                                      Length >= 60 ~ "Length")) %>%
  tidyr::pivot_wider(names_from = row_num, values_from = Length) %>%
  tidyr::separate(Label, into=c("Plate", "Experiment", "Hour", "Magnification", "Well"), sep="[:punct:]") %>%
  dplyr::mutate(Hour = stringr::str_extract(Hour, pattern = "[:digit:]{2}")) %>%
  #tidyr::separate(Well, into=c("Row","Column"), sep=c("(?<=[A-Za-z])(?=[0-9])")) %>%
  dplyr::mutate(Radius = Width/2,
                Volume = pi*Radius^2*Length,
                Area = 2*pi*Radius*Length + 2*pi*Radius) %>%
  dplyr::mutate(Length = 3.2937*Length,
                Width = 3.2937*Width,
                Radius = 3.2937*Radius,
                Volume = 3.2937*Volume,
                Area = 3.2937*Area)
  

check <- elliot %>%
  dplyr::group_by(Plate, Hour) %>%
  dplyr::count()


all <- bind_rows(hannah, jordan, iris, justine, elliot, izzy)


justine %>%
  dplyr::mutate(hour = as.numeric(Hour)) %>%
  ggplot2::ggplot() + #geom_boxplot(aes(x = hour, y = Width, group = Hour), outlier.size = 0.1, alpha = 0.6, width = 0.6) +
  geom_jitter(aes(x = Hour, y = Volume), size = 0.6, alpha = 0.4, width = 0.2) + theme_cowplot() +
  #theme_cowplot(font_size = 20, rel_small = 14/22) + scale_x_continuous(breaks = seq(0,70,10)) + 
  labs(x = "Time (Hours)", y = "Animal Volume") + guides(color = F) #+ facet_wrap(~Student) + panel_border()


plot_grid(l, w, a, v, ncol = 2)
plot_grid(l, w, ncol = 2, nrow = 1, scale = 1)

sum <- all %>%
  dplyr::group_by(Hour, Student, Row, Column) %>%
  dplyr::summarize(mean.Length = mean(Length),
                   mean.Width = mean(Width),
                   mean.Area = mean(Area),
                   mean.Volume = mean(Volume), .groups = "drop")


wids <- sum %>%
  ggplot2::ggplot() + 
  geom_boxplot(aes(x = as.numeric(Hour), y = mean.Width, group = Hour), outlier.size = 0.2, alpha = 0.4) +
  geom_jitter(aes(x = as.numeric(Hour), y = mean.Width, color = Student), size = 0.2, width = 0.2, alpha = 0.5) +
  theme_cowplot() + labs(x = "Time (Hours)", y = "Mean Animal Width") +
  scale_color_nejm() + scale_x_continuous(breaks = seq(0,75,5)) + guides(color = F)

plot_grid(len, wid, are, vol, ncol = 2)
plot_grid(lens, wids, ares, vols, ncol = 2)


## Length vs Width
all %>%
  ggplot() +
  aes(x = Length, y = Width, color = Student) +
  geom_jitter(size = 0.6, alpha = 0.6) + 
  theme_cowplot(20) + scale_color_nejm() + 
  facet_wrap(~Student) + panel_border()


clustr <- students %>%
  dplyr::filter(Hour < 50) %>%
  dplyr::mutate(rep = Experiment) %>%
  dplyr::group_by(rep) %>% 
  tidyr::nest()

step1 <- function(data) {
  X <- data #%>%
  #dplyr::filter(hour==x,replicate==y)
  m <- X %>%
    dplyr::select(Length, Width) %>%
    Mclust(., G = 4)
  medians <- X %>%
    bind_cols(.,as_tibble(m$classification)) %>% 
    dplyr::group_by(value) %>%
    dplyr::summarize(cluster = median(Length), .groups = "drop") %>%
    dplyr::arrange(dplyr::desc(cluster)) %>%
    dplyr::pull()
  raw <- X %>%
    dplyr::bind_cols(.,as_tibble(m$classification)) %>% 
    dplyr::group_by(value) %>%
    dplyr::mutate(num = n()) %>%
    dplyr::mutate(median = median(Length), cluster_median = median) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(dplyr::desc(median)) %>%
    dplyr::mutate_at(vars(cluster_median),
                     ~dplyr::case_when(. == medians[1] ~ 1,
                                       . == medians[2] ~ 2,
                                       . == medians[3] ~ 3,
                                       . == medians[4] ~ 4,
                                       TRUE ~ 5)) %>%
    dplyr::select(-value)
  
  return(raw)
}

all_cluster <- purrr::map_dfr(clustr$data, ~step1(.x))


all_cluster %>%
  tidyr::separate(Well, into=c("Row","Column"), sep=c("(?<=[A-Za-z])(?=[0-9])")) %>%
  dplyr::mutate(hour = as.numeric(Hour)) %>%
  dplyr::mutate(cluster = factor(cluster_median, labels = c("L4","L3","L2","L1"))) %>%
  ggplot() +
  aes(x = hour, y = Length, color = cluster) +
  geom_jitter(size = 0.6, alpha = 0.6) + 
  theme_cowplot(20) + scale_color_nejm() + 
  facet_wrap(~Row) + panel_border() +
  scale_x_continuous(breaks = seq(0,50,5)) +
  labs(x = "Hour") +
  geom_vline(xintercept=c(mins), color="red", size = 0.3) +
  geom_vline(xintercept=c(maxs), color="gray", size = 0.3)

t <- all_cluster %>%
  dplyr::mutate(cluster = factor(cluster_median, labels = c("L4","L3","L2","L1"))) %>%
  dplyr::group_by(Hour, Student, cluster) %>%
  tidyr::nest() %>%
  select(-data) %>%
  tidyr::pivot_wider(names_from = Student, values_from = cluster) 
  
  
  
  
  
  
  

