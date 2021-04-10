#' ---
#' title: "Class 6: Exercise 2"
#' author: "Michał Sałach"
#' date: "April 10, 2021"
#' output: 
#'  prettydoc::html_pretty:
#'    theme: cayman
#' params:
#'  season:
#'    label: "Season"
#'    value: 1
#'    input: select
#'    min: 1
#'    max: 8
#'    step: 1
#'    sep: ""
#' ---

#+ r setup, include = FALSE
library(tidyverse)
library(ggtext)
library(kableExtra)
knitr::opts_chunk$set(echo = FALSE)
data <- paste0('Data/season_', params$season, '.RData') 
load(data)

# format titles properly
season_data[, 'title'] <- substr(season_data[, 'title'], 2, nchar(season_data[, 'title'])-1)

# format dates
season_data[, 'premiere_date'] <- sub("\\s\\(.*", "", season_data[, 'premiere_date'])

#' # Game of Thrones - Season `r params$season` summary in numbers
#' 
#' <div align="justify">
#' ### __(*Warning:* spoilers ahead)__
#' 
#' ***
#' 
#' ### Overview
#' 
#' (From the [Wikipedia](https://en.wikipedia.org/wiki/Game_of_Thrones#Premise)) 
#' Game of Thrones is an American fantasy drama television series created by David Benioff and 
#' D. B. Weiss for HBO. It is an adaptation of A Song of Ice and Fire, a series of fantasy novels 
#' by George R. R. Martin, the first of which is A Game of Thrones.
#'
#' Set on the fictional continents of Westeros and Essos, Game of Thrones has a large ensemble 
#' cast and follows several story arcs throughout the course of the show. A major arc concerns 
#' the Iron Throne of the Seven Kingdoms of Westeros through a web of political conflicts among 
#' the noble families either vying to claim the throne or fighting for independence from it. 
#' Another focuses on the last descendant of the realm's deposed ruling dynasty, who has been 
#' exiled to Essos and is plotting a return to the throne. A third story arc follows the Night's 
#' Watch, a military order defending the realm against threats from the North.
#' 
#' ***
#' 
#' ### Season `r params$season` summary
#' 
#' Season `r params$season` of Game of Thrones consisted of `r nrow(season_data)` episodes that 
#' aired between `r season_data$premiere_date[1]` and `r tail(season_data$premiere_date, 1)`, on 
#' HBO. The show gathered an average of `r round(mean(season_data$viewers), 2)` million first-day TV viewers
#' in the US, with the smallest number of `r min(season_data$viewers)` M viewers for 
#' *`r season_data[season_data$viewers==min(season_data$viewers), 'title']`* and the largest number 
#' of `r max(season_data$viewers)` M viewers for 
#' *`r season_data[season_data$viewers==max(season_data$viewers), 'title']`*.
#' The first episode in the season, *`r season_data$title[1]`*, gathered 
#' `r season_data[1, 'viewers']` M audience, and the last one, *`r tail(season_data$title, 1)`*
#' - `r tail(season_data$viewers, 1)` M.
#' 
#' The most popular episode of the season was 
#' *`r season_data[season_data$viewers==max(season_data$viewers), 'title']`*, in which:
#' 
#' > `r season_data[season_data$viewers==max(season_data$viewers), 'description']`
#'  
#' ***
#' 
#' You can see how the viewership of the episodes changed in below.

#+ r viewers-plot, fig.align = 'center'

season_data %>% 
  ggplot(aes(x = as.numeric(no_season), y = viewers)) +
  geom_line(color = 'darkred', size = 1.2) +
  scale_x_continuous(breaks = 1:nrow(season_data), labels = 1:nrow(season_data)) +
  labs(title = '', x = 'Episode number', 
       y = '1<sup>st</sup> day TV viewers in the US (million)') +
  theme_minimal() + 
  theme(axis.title.y = element_markdown()) 

#' ***
#' 
#' Finally, the episodes with the above-average viewership were:

#+ r viewers-table

season_data %>% 
  filter(viewers > mean(viewers)) %>% 
  select(c(no_season, title, directed_by, viewers)) %>% 
  rename(`No. in season` = no_season,
         Title = title,
         `Directed by` = directed_by,
         `No. of viewers (M)` = viewers) %>% 
  kbl(row.names = FALSE) %>% 
  kable_styling(c('striped', 'hover'), full_width = FALSE)

#' <br></div>

# For producing reports for all seasons:
# NOTE: do not save this file (Exercise_2_MS.R) with the section underneth uncommented, in order 
# to prevent the rendering function from iterating 'infinitely'.
# for(i in 1:8){
#   rmarkdown::render('april_05_09/Exercise_2_MS.R',
#                     output_file = paste0('Exercise_2_MS.R_season_', i, '.html'), # Note: output file must not be given april_05_09/ directory in order to be placed there 
#                     params = list(season = i))
# }
