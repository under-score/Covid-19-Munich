ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}
packages <-c("ggplot2","openxlsx","ggrepel","dplyr","tidyverse","lubridate")
ipak(packages)

# contact database
fn <- c("/Users/wjst/Documents/Daten/Paper/CoronaBayern/Muenchen/plot.xlsx")
events <- 
  read.xlsx(fn, startRow = 1, colNames = TRUE, rowNames = FALSE, detectDates = TRUE, skipEmptyRows = TRUE, skipEmptyCols = TRUE, rows = NULL, cols = NULL, check.names = FALSE) %>%
  mutate( dt = yday(dt) ) %>%
  arrange( dt,id,id2 ) %>%
  group_by( dt ) %>%
  mutate( nr = (row_number()) /6 )

# stacked contact database
ids <- events[,c("id","dt","nr","event")]
cnd <- !is.na(events$id2)
ids[dim(ids)[1]+1:sum(cnd),] <- events[cnd, c("id2","dt","nr","event")] 
ids <- ids %>%
  group_by( id ) %>%
  mutate( mindt=min(dt), maxdt=max(dt) )

events %>% 
  ggplot() + 
  # patient
  geom_segment( data=ids %>% filter(row_number()==1), aes(x = id, y = mindt, xend = id, yend = maxdt), size=3, colour="grey") +
  # contact
  geom_segment( aes(y = dt+nr, x = id, yend = dt+nr, xend = id2), colour="black" ) +
  geom_point( data=ids %>% filter(event!="D"), aes(y=dt+nr, x=id), size=2, colour="black") +
  # diagnosis
  geom_point( data=. %>% filter(event=="D"), aes(y=dt, x=id, group=id ), shape=21, fill="red", size=3, colour="red") +
  # plot
  geom_label_repel( data=ids %>% filter(row_number()==1 & !id %in% c(5,6,9,11)), aes(y=maxdt, x=id, group=id, label=paste0("Pat",id) ), nudge_y=-.8, nudge_x =.001, segment.color = NA, force=10) +
  geom_label_repel( data=ids %>% filter(row_number()==1 & id %in% c(5,6,9,11)), aes(y=maxdt, x=id, group=id, label=paste0("Pat",id) ), fill="cyan", nudge_y=-.8, nudge_x =.001, segment.color = NA) +
  scale_y_reverse ( limits=c(35, 19), breaks=seq(35,19,-1), labels=format( as.Date( c(34:18), format = "%j", origin="1.1.20"), "%a %e.%m." ), name="") +
  scale_x_continuous( position = "top", name="" ) +
  theme_minimal()

