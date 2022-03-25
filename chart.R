library(epiR)

# load population sizes and number of deaths (tables 1 and 3: https://www.ons.gov.uk/peoplepopulationandcommunity/healthandsocialcare/causesofdeath/adhocs/006925deathsbyunderlyingcausedeprivationdecileareas5yearagegroupsandsexenglandandwales1981to2015populationsbydeprivationdecileareas5yearagegroupsandsexenglandandwales2001to2015)

deaths <- read.csv('https://raw.githubusercontent.com/danlewer/slopes_and_cliffs/main/deaths_by_imd.csv', header = T, stringsAsFactors = F)
pops <- read.csv('https://raw.githubusercontent.com/danlewer/slopes_and_cliffs/main/pops_by_imd.csv', header = T, stringsAsFactors = F)
deaths$year <- as.numeric(deaths$year)
pops$year <- as.numeric(pops$year)

# smr function

smr <- function(min.year = 2012, sex = 'Male', cause = 'all', deaths, pops) {
  d <- deaths[deaths$year >= min.year & deaths$sex == sex & ifelse(cause == 'all', T, deaths$cause == cause),]
  p <- pops[pops$year >= min.year & pops$sex == sex,]
  imd <- LETTERS[1:10]
  obs <- matrix(aggregate(d$All.Ages, by = list(d$imd), FUN = sum)[,2], dimnames = list(imd, ""))
  pop <- as.matrix(aggregate(p[,grepl('X', names(p))], by = list(p$imd), FUN = sum))
  pop <- pop[,2:ncol(pop)]
  row.names(pop) <- imd
  std <- colSums(d[,grepl('X', names(d))]) / colSums(pop)
  return(epi.indirectadj(obs, pop, std, units = 100))
}

# calculate general population SMRs and append results of systematic review

Ma <- smr(min.year = 2015, sex = 'Male', deaths = deaths, pops = pops)$smr.strata[,3:5]
Ma <- rbind(c(7.9, 7.0, 8.7), Ma)
Ma <- Ma[nrow(Ma):1,]
Fe <- smr(min.year = 2015, sex = 'Female', deaths = deaths, pops = pops)$smr.strata[,3:5]
Fe <- rbind(c(11.9, 10.4, 13.13), Fe)
Fe <- Fe[nrow(Fe):1,]

# CHARTS
cols <- c('#009ACD', '#90EE90')

# log chart: same plot

lc <- rbind(Ma, 0, Fe)

ax <- c(0.5, 1:15)
ax2 <- c(0.5, 1:5, 10, 15)

png('slopes_cliffs_excluded.png', height = 8, width = 12, units = 'in', res = 300)

par(cex = 1.7, xpd = F)
y <- barplot(log(lc[,1]), ylim = log(c(ax[1], tail(ax, 1))), axes = F, col = rep(c('#009ACD', '#90EE90'), 2, each = 12), ylab = 'SMR')
axis(2, at = log(ax), labels = F, las = 2, tck = -0.01)
axis(2, at = log(ax2), labels = ax2, las = 2)
arrows(y, log(lc[,2]), y, log(lc[,3]), code = 3, angle = 90, length = 0.05)
abline(h = 0)

par(cex = 1.2, xpd = T)
text(c(2, 16), log(0.6), 'Least deprived\nneighbourhoods')
text(c(10, 24), c(log(2), log(1.7)), 'Most deprived\nneighbourhoods')
text(c(12, 26), c(log(10.5), log(16)), 'Excluded\ngroups')

par(cex = 1.7)
text(6, log(20), 'Males')
text(20, log(20), 'Females')

dev.off()
