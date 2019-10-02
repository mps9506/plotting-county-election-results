## load dplyr
library(dplyr)
library(ggplot2)
library(sf)
library(urbnmapr)
library(cowplot)

theme_map <- function(...) {
  theme_minimal() +
    theme(
      text = element_text(family = "Arial Narrow", color = "#22211d"),
      axis.line = element_blank(),
      axis.ticks = element_blank(),
      # panel.grid.minor = element_line(color = "#ebebe5", size = 0.2),
      panel.grid.major = element_line(color = "#ebebe5", size = 0.2),
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = "#f5f5f2", color = NA),
      panel.background = element_rect(fill = "#f5f5f2", color = NA),
      legend.background = element_rect(fill = "#f5f5f2", color = NA),
      panel.border = element_blank(),
      ...
    )
}

## read data from github
d <- readr::read_csv("https://github.com/mkearney/presidential_election_county_results_2016/blob/master/data/pres.elect16.results.2018.csv?raw=true")

## filter to trump and clinton, calculate relative vote shares, and generate
## one estimate (trump's relative vote share) per county
d %>%
  filter(cand %in% c("Donald Trump", "Hillary Clinton"), !is.na(county)) %>%
  group_by(st, county) %>%
  summarise(trump = pct[cand == "Donald Trump"] / sum(pct),
    pct = pct[cand == "Donald Trump"],
    fips = unique(fips),
    total_votes = unique(total_votes)) ->
  d

## fips code for AK, AL, AR, AZ, CA, CO, CT  need to be preceded by a zero
d %>%
  mutate(fips =
           case_when(
             st %in% c("AK", "AL", "AR", "AZ", "CA", "CO", "CT") ~ paste0("0",fips),
             !st %in% c("AK", "AL", "AR", "AZ", "CA", "CO", "CT") ~ fips)) -> d

## get simple features object of counties with FIPS codes
nms <- urbnmapr::get_urbn_map(map = "counties", sf = TRUE)

## Shannon County South Dakota (FIPS46113) changed to
## Oglala Lakota County, SD (FIPS46102) effective May 2015
## Vote share for Trump according to Mkearny is 0.66
## according to my calc it is 0.088: https://github.com/john-guerra/US_Elections_Results/blob/master/US%20presidential%20election%20results%20by%20county.csv

## Add Oglala Lakota County manually
d %>% bind_rows(tibble(st = "SD",
                   county = "Oglala Lakota County",
                   trump = 0.088,
                   pct = 0.083,
                   fips = "46102",
                   total_votes = 2905)) -> d


## join trump [relative] vote share estimate to county fips data
nms %>%
  left_join(d %>%
              select(fips, trump, total_votes),
            by = c("county_fips" = "fips")) -> nms

## scale votes to create an alpha scale
nms %>%
  mutate(alpha = round(log(total_votes)/max(log(total_votes), na.rm = TRUE), 4)) -> nms

## remove AK and HI from nms, since the source data/example didn't include it
nms %>%
  filter(state_abbv != "AK",
         state_abbv != "HI")  -> nms

## make a bivariate type legend
bivariate_scale <- tibble(trump = seq(0, 100, by = 25)/100,
                          alpha = seq(0, 100, by = 25)/100) %>%
  tidyr::expand(trump, alpha)

legend <- ggplot(bivariate_scale) +
  geom_tile(aes(x = trump,
                y = alpha,
                fill = trump,
                alpha = alpha),
            color = "white",
            size = 0.1) +
  scale_fill_gradient(name = "", limits = c(0, 1), low = "#2222dd", high = "#dd2222") +
  scale_alpha_continuous(name = "", range = c(.1, .9)) +
  scale_x_continuous(breaks = c(0, .5, 1)) +
  scale_y_continuous(breaks = c(0, .5, 1)) +
  coord_fixed() +
  labs(x = "Trump's Vote Share ➞",
       y = "Number of Voters ➞") +
  theme_map(legend.position = "none",
            axis.title = element_text(size = 8)) +
  theme(plot.background = element_rect(fill = "transparent"))




map <- ggplot(nms) +
  geom_sf(aes(fill = trump, alpha = alpha),
          color = "white",
          size = 0.1) +
  scale_fill_gradient(name = "", limits = c(0, 1), low = "#2222dd", high = "#dd2222") +
  scale_alpha_continuous(name = "", range = c(.1, .9)) +
  theme_map(legend.position = "none",
            axis.text = element_blank())


full_map <- ggdraw() +
  draw_plot(map, 0, 0, 1, 1) +
  draw_plot(legend, 0.1, 0.05, 0.2, 0.2)

ggsave(here::here("img/tidy-county-map-gradients.png"),
       plot = full_map,
       dpi = 300,
       width = 9,
       height = 4.8,
       units = "in")
