# Dados de popularidade
library(ggplot2)
library(plyr)
library(dplyr)
library(readxl)
library(lubridate)
library(SciencesPo)


pop <- read_excel(file.choose())

data %>% unite(col, ..., sep = " ", remove = TRUE)

unite_DF <- separate_DF %>% unite(Quarter, Time_Interval, Interval_ID, sep = ".")

data %>% spread(key, value, fill = NA, convert = FALSE)

data %>% filter(...)
expenditures %>% select(-X1980:-X2006)
expenditures %>% select(-starts_with("X"))
sub.exp <- expenditures %>% select(Division, State, X2007:X2011)

sub.exp %>% summarise(Min = min(X2011, na.rm=TRUE),
                      Median = median(X2011, na.rm=TRUE),
                      Mean = mean(X2011, na.rm=TRUE),
                      Var = var(X2011, na.rm=TRUE),
                      SD = sd(X2011, na.rm=TRUE),
                      Max = max(X2011, na.rm=TRUE),
                      N = n())


# Making time variable and store it as quarter
# Filter the one poll before Collor asseme office.
getQuarterData <- function(x){
x %>% mutate(date = paste0(year,month, end, sep="-")) %>%
        mutate(quarter = quarter(ymd(date), with_year = TRUE)) %>%
filter(!(leader=="Fernando Collor" & year==1990 & month=="Mar")) %>%
   group_by(quarter) %>%
  summarise(n= n(),
            Approving = round( mean(Approving, na.rm=TRUE), 1),
            Disapproving = round( mean(Disapproving, na.rm=TRUE), 1),
            Regular = round(mean( Regular, na.rm=TRUE),1),
            DK = round(mean( DK, na.rm=TRUE), 1),
            party = party,
            leader = leader) -> dat
  return(dat)
}

pop_qt = getQuarterData(pop)

myPalette <- c("")

# area plot

area <- ggplot(pop_qt, aes(x = quarter))
area + geom_area(aes(colour = Approving, fill= Approving), position = 'stack')

gplot <- ggplot(data = pop_qt, aes(x = quarter)) +
        geom_point(aes( y = Approving), color="blue") +
         geom_point(aes(y = Disapproving), color="red") +
  geom_smooth(aes(y=Approving), color="blue") +
  geom_smooth(data = pop, aes(x = year, y = Disapproving), color="red") +

geom_ribbon(aes(ymin=Approving-2, ymax=Approving+2), color="blue") +

geom_rect(data = pop_qt,
                       aes(xmin = quarter,  # and aesthetics...
                           xmax = quarter + 2,
                           fill = party),
                       ymin = -2, ymax = 2,
                       alpha = 9/10)

gplot <- gplot + geom_jitter(size = 1/2)  # The "foreground" geom
gplot <- gplot + ylim(-1, 1)
gplot <- gplot + scale_x_continuous(expand = c(0, 0))
gplot <- gplot + scale_fill_manual(values = myPalette(9)[c(8, 5, 2)])
print(gplot)

# sort Approving ratings:
pop_qt %>% arrange(Approving)
pop_qt %>% arrange(desc(Approving))

pop_qt %>% arrange(Disapproving)
pop_qt %>% arrange(desc(Disapproving))

pop_qt %>% arrange(Regular)
pop_qt %>% arrange(desc(Regular))




gplot <- ggplot() + theme_minimal() +
  xlab("Years in Office") +
  ylab("Approval Rating (quarterly average)") +
  geom_rect( mapping=aes(ymin = -Inf, ymax = Inf,
            xmin = 1987, xmax = 1990.2),
            fill = 'orange', alpha =.3) +
  annotate("text", label="Sarney \n(PMDB) >", x=1988, y=85, size=3, fontface="bold.italic") +
  geom_rect( mapping=aes(ymin = -Inf, ymax = Inf,
                         xmin = 1990.2, xmax = 1993),
             fill = 'green', alpha =.3) +
  annotate("text", label="Collor \n(PRN) >", x=1991, y=85, size=3, fontface="bold.italic") +
  geom_rect( mapping=aes(ymin = -Inf, ymax = Inf,
            xmin = 1993, xmax = 1995),
            fill = 'orange', alpha =.3) +
  annotate("text", label="Itamar \n(PMDB) >", x=1993.4, y=85, size=3, fontface="bold.italic") +
  geom_rect( mapping=aes(ymin = -Inf, ymax = Inf,
                         xmin = 1995, xmax = 1999),
             fill = 'blue', alpha =.3) +
  annotate("text", label="FHC \n(PSDB) >", x=1996, y=85, size=3, fontface="bold.italic") +
  geom_rect( mapping=aes(ymin = -Inf, ymax = Inf,
                         xmin = 1999, xmax = 2003),
             fill = 'blue', alpha =.3) +
  geom_rect(mapping=aes(ymin = -Inf, ymax = Inf,
                        xmin = 2003, xmax = 2007),
            fill = 'red', alpha =.3) +
  annotate("text", label="Lula \n(PT) >", x=2003.5, y=85, size=3, fontface="bold.italic") +
  geom_rect(mapping=aes(ymin = -Inf, ymax = Inf,
                        xmin = 2007, xmax = 2011),
            fill = 'red', alpha =.3) +
  geom_rect(mapping=aes(ymin = -Inf, ymax = Inf,
                               xmin = 2011, xmax = 2015),
            fill = 'red', alpha =.3) +
  annotate("text", label="Dilma \n(PT) >", x=2011.2, y=85, size=3, fontface="bold.italic") +
  geom_rect( mapping=aes(ymin = -Inf, ymax = Inf,
                               xmin = 2015, xmax = 2016),
            fill = 'red', alpha =.3) +
 # geom_point(data = pop_qt, aes(x = quarter, y = Disapproving), color="red") +
 geom_line(data = pop_qt, aes(x = quarter, y = Approving), color="lightblue",size=4) +
  geom_point(data = pop_qt, aes(x = quarter, y = Approving), color="blue", size=2) +
  annotate("text", label="Lowest: \nSarney (5%)", x=1993, y=4, size=3, fontface="bold.italic") +
  annotate("segment", x=1992, y=4, xend=1990, yend=5, size=0.5,
           arrow=arrow(length=unit(.2, "cm")))+
  annotate("text", label="Highest: \nLula II (84%)", x=2006, y=80, size=3, fontface="bold.italic") +
  annotate("segment", x=2007, y=80, xend=2010, yend=82, size=0.5,
            arrow=arrow(length=unit(.2, "cm")))
print(gplot)

ggsave("approvalRatings.png", width = 12, height = 6 )



pop %>% group_by(party) %>%
  summarise(approval=mean(Approving),
            disapproval=mean(Disapproving)) -> approval_by_party

bar.plot(approval_by_party, xvar ="party", yvar="approval")


pop %>% group_by(leader) %>%
  summarise(approval=mean(Approving),
            disapproval=mean(Disapproving)) -> approval_by_leader

ggplot(approval_by_leader)+
  geom_boxplot(aes(y="approval"))

bar.plot(approval_by_leader, xvar ="leader", yvar="approval")

sjp.frq(approval_by_leader$approval,
        sort.frq = "desc",       # sort frequencies
        showCI = TRUE,           # show confidence intervals
        type = "dots",           # dot plot
        coord.flip = TRUE,       # flip coordinates
        showCountValues = FALSE, # no counts, only percentages
        geom.size = 2.5)         # adjust geom size

sjp.frq(approval_by_leader$approval,
        showValueLabels = FALSE,
        type = "lines")

yankee_1998_career %>%
  select(-alpha1, -beta1, -eb_estimate) %>%
  knitr::kable()


bar.plot(pop, xvar ="leader", yvar="Approving")

hist(pop$Approving)

todygraph <-cbind(pop_qt$Approving,
                  pop_qt$Disapproving,
                  pop_qt$Regular)
rownames(todygraph) <- pop_qt$quarter
todygraph <- data.frame(todygraph)
colnames(todygraph) <- c("Approving","Disapproving","Regular")
# Problems with as.POSIXlt.character
dygraph(pop_qt) %>%
  dySeries("Approving", label = "Positive") %>%
  dySeries("Disapproving", label = "Negative") %>%
  dySeries("Regular", label = "Regular") %>%
  dyOptions(stackedGraph = TRUE) %>%
  dyRangeSelector(rating = 50)


# Average of the first year in office:
# Bar graphs for each president



