library(data.table)
library(RColorBrewer)

# load population sizes and number of deaths (tables 1 and 3: https://www.ons.gov.uk/peoplepopulationandcommunity/healthandsocialcare/causesofdeath/adhocs/006925deathsbyunderlyingcausedeprivationdecileareas5yearagegroupsandsexenglandandwales1981to2015populationsbydeprivationdecileareas5yearagegroupsandsexenglandandwales2001to2015)

deaths <- read.csv('https://raw.githubusercontent.com/danlewer/slopes_and_cliffs/main/deaths_by_imd.csv', header = T, stringsAsFactors = F)
pops <- read.csv('https://raw.githubusercontent.com/danlewer/slopes_and_cliffs/main/pops_by_imd.csv', header = T, stringsAsFactors = F)
deaths$year <- as.numeric(deaths$year)
pops$year <- as.numeric(pops$year)
setDT(deaths); setDT(pops)

deaths <- melt(deaths, id.vars = c('year', 'imd', 'sex', 'cause'), variable.name = 'age', value.name = 'deaths')
pops <- melt(pops, id.vars = c('year', 'imd', 'sex'), variable.name = 'age', value.name = 'pop')
deaths <- deaths[year >= 2001]
d <- pops[deaths, on = c('year', 'imd', 'sex', 'age')]

# all-cause SMR for all-age and young adults

ages <- c('X.1', 'X1.4', 'X5.9', 'X10.14', 'X15.19', 'X20.24', 'X25.29', 'X30.34', 'X35.39', 'X40.44', 'X45.49', 'X50.54', 'X55.59', 'X60.64', 'X65.69', 'X70.74', 'X75.79', 'X80.84', 'X85.89', 'X90.')

s1 <- deaths[age != 'All.Ages' & year == 2015, .(deaths = sum(deaths)), c('imd', 'age')][
  pops[age != 'All.Ages' & year == 2015, .(pop = sum(pop)), c('imd', 'age')], on = c('age', 'imd')]

smr <- function(include.ages = ages) {
  ref <- s1[age %in% include.ages, .(deaths = sum(deaths), pop = sum(pop)), age]
  ref$refrate <- ref$deaths / ref$pop
  ref <- ref[, c('age', 'refrate')]
  smr <- s1[age %in% include.ages]
  smr <- ref[smr, on = 'age']
  smr$e <- smr$pop * smr$refrate
  smr <- smr[, .(o = sum(deaths), e = sum(e)), imd]
  smr$s <- smr$o / smr$e
  return (smr)
}

smr_all_age <- smr()
smr_15_24 <- smr(ages[5:6])
smr_25_49 <- smr(ages[7:11])
smr_50_69 <- smr(ages[12:15])
smr_70_79 <- smr(ages[16:17])
smr_80 <- smr(ages[18:20])

xx <- c(0.5, 0.75, 1, 1.5, 2)
ys <- seq(-0.3, 0.4, length.out = 6)
cols <- brewer.pal(6, 'Set2')

png('age_stratified_SMR.png', height = 8, width = 10, units = 'in', res = 300)

par(mar = c(5, 5, 2, 12), xpd = F)
plot(1, type = 'n', xlim = c(0, 10), ylim = c(-0.7, 0.7), ylab = 'SMR', xlab = 'IMD', axes = F)

box()
abline(h = 0)
axis(1)
axis(2, log(xx), xx, las = 2)

with(smr_all_age, lines(1:10, log(s), type = 'b', col = cols[1], pch = 1))
with(smr_15_24, lines(1:10, log(s), type = 'b', col = cols[2], pch = 2))
with(smr_25_49, lines(1:10, log(s), type = 'b', col = cols[3], pch = 3))
with(smr_50_69, lines(1:10, log(s), type = 'b', col = cols[4], pch = 4))
with(smr_70_79, lines(1:10, log(s), type = 'b', col = cols[5], pch = 5))
with(smr_80, lines(1:10, log(s), type = 'b', col = cols[6], pch = 6))

par(xpd = NA)
for(i in 1:6) {
  lines(c(11, 11.75), c(ys[i], ys[i]), col = cols[i], type = 'b', pch = i)
}
text(12, ys, c('All age', '15-24', '25-49', '50-69', '70-79', '80+'), adj = 0)

dev.off()
