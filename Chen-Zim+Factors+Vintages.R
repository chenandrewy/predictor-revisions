# 2022 05: 

# compare main scatter using no factors, ff3 factors, old ff3 factors
# compare new and old 

# SETUP ====

rm(list = ls())

## Globals ====
library(tidyverse)
library(data.table)
library(googledrive)
library(gridExtra)

dir.create('../data/')


# use this for original papers
SUBDIR = 'Full Sets OP'; FILENAME = 'PredictorPortsFull.csv'

## Download ====

if (!file.exists('../data/new-PredictorPortsFull.csv')){

  # March 2022 cz data
  url <- "https://drive.google.com/drive/folders/1O18scg9iBTiBaDiQFhoGxdn4FdsbMqGo" 

  url %>% drive_ls() %>%
    filter(name == "Portfolios") %>% drive_ls() %>% 
    filter(name == SUBDIR) %>% drive_ls() %>% 
    filter(name == FILENAME) %>% 
    drive_download(path = paste0("../data/new-",FILENAME), overwrite = TRUE)
  
  # signal doc 
  url %>% drive_ls() %>% 
    filter(name == "SignalDoc.csv") %>% 
    drive_download(path = "../data/SignalDoc.csv", overwrite = TRUE)
  
  
  # download FF (new)
  url = 'http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/F-F_Research_Data_Factors_CSV.zip'
  download.file(url,'../data/deleteme.zip')
  unzip('../data/deleteme.zip', exdir = '../data')
  
  # dl from web.archive.org like this doesn't work
  # temp_url = 'https://web.archive.org/web/20011218003540/http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/F-F_Benchmark_Factors_Monthly.zip'
  # download.file(temp_url, '../data/deleteme.zip')
  # unzip('../data/deleteme.zip', exdir = '../data')


} # if exists

## Import ====

# cz
doc =  fread("../data/SignalDoc.csv") %>% 
  rename(signalname = Acronym) %>% 
  select(-c(`Detailed Definition`, `Notes`))


cz_all = fread(paste0("../data/new-",FILENAME)) %>% 
  mutate(vint = 2022) %>% 
  rbind(
    fread(paste0("../data/new-",FILENAME)) %>% 
      mutate(vint = 2021)
  ) %>% 
  select(vint, signalname, port, date, ret) %>% 
  left_join(
    doc %>% select(signalname, SampleStartYear, SampleEndYear)
    , by = 'signalname'
  ) %>% 
  mutate(
    insamp = (year(date) >= SampleStartYear) &  (year(date) <= SampleEndYear)
  ) 


  
## ff
ff_all = fread('../data/F-F_Research_Data_Factors.CSV')  %>% 
  mutate(vint = 2022) %>% 
  rbind(
    fread('FF-Vintage/F-F_Research_Data_Factors_2005.txt') %>% mutate(vint = 2005)
  ) %>% 
  rbind(
    fread('FF-Vintage/F-F_Research_Data_Factors_2012.txt') %>% mutate(vint = 2012)    
  )  
  
colnames(ff_all) = c('yearm', 'mktrf', 'smb', 'hml', 'rf', 'vint')

## Performance Measures ====

target_data = cz_all %>% 
  filter(vint == 2022 & insamp & port == 'LS') %>% 
  mutate(yearm = year(date)*100 + month(date)) %>% 
  select(signalname, yearm, ret)

# no adjustment
fit_raW = target_data[
  , list(
      alpha = summary(lm(ret~1))$coefficients['(Intercept)' , 'Estimate']
      , tstat = summary(lm(ret~1))$coefficients['(Intercept)' , 't value']
  )
  , by=signalname
] %>% 
  mutate(
    model = 'raw'
  )
 
## ff3 model 2022
fitme = target_data %>% 
  left_join(
    ff_all %>% filter(vint == 2022), by = 'yearm'
  )

fit_ff_2022 = fitme[
  , list(
    alpha = summary(lm(ret ~ mktrf+smb+hml))$coefficients['(Intercept)' , 'Estimate']
    , tstat = summary(lm(ret ~ mktrf+smb+hml))$coefficients['(Intercept)' , 't value']
  )
  , by=signalname
] %>% 
  mutate(
    model = 'ff3_2022'
  )

## ff3 model 2012
fitme = target_data %>% 
  left_join(
    ff_all %>% filter(vint == 2012), by = 'yearm'
  )
fit_ff_2012 = fitme[
  , list(
    alpha = summary(lm(ret ~ mktrf+smb+hml))$coefficients['(Intercept)' , 'Estimate']
    , tstat = summary(lm(ret ~ mktrf+smb+hml))$coefficients['(Intercept)' , 't value']
  )
  , by=signalname
] %>% 
  mutate(
    model = 'ff3_2012'
  )


## ff3 model 2005
fitme = target_data %>% 
  left_join(
    ff_all %>% filter(vint == 2005), by = 'yearm'
  )
fit_ff_2005 = fitme[
  , list(
    alpha = summary(lm(ret ~ mktrf+smb+hml))$coefficients['(Intercept)' , 'Estimate']
    , tstat = summary(lm(ret ~ mktrf+smb+hml))$coefficients['(Intercept)' , 't value']
  )
  , by=signalname
] %>% 
  mutate(
    model = 'ff3_2005'
  )

fit_all = fit_raW %>% rbind(fit_ff_2012) %>% rbind(fit_ff_2022) %>% rbind(fit_ff_2005)

# HML revisions rep ====

ff_all %>% 
  group_by(vint) %>% 
  summarize_at(.vars =vars(mktrf,smb,hml), sd)

temp = ff_all %>% 
  select(yearm,vint,hml) %>% 
  pivot_wider(
    names_from = vint, values_from = hml, names_prefix = 'vint'
  ) %>% 
  mutate(
    rev = vint2022- vint2012, time = floor(yearm/100) + (yearm/100 - floor(yearm/100))/0.12
  ) %>% 
  filter(
    !is.na(rev)
  )


ggplot(temp) +
  geom_line(aes(x=time,y=rev)) +
  theme_minimal(
    base_size = 20
  ) +
  ylab('HML revision (ppt)') +
  annotate(
    geom = 'text', x = 1980, y = 4.2
    , label = paste0(
      'SD(revision) = ', round(sd(temp$rev),3), '\n'
      , 'SD(HML 2022) = ', round(sd(temp$vint2022), 3)
    )
    , size = 6
  ) +
  xlab(NULL)



ggsave('../results/hml-rev.png', width = 8, height = 5)

# Name Scatter ====


# select comparable t-stats
fit_OP = doc %>% 
  mutate(
    tstat_OP = abs(as.numeric(`T-Stat`))
  ) %>% 
  select(
    signalname, tstat_OP, `Predictability in OP`, `Signal Rep Quality`, `Test in OP`
  ) %>% 
  filter(
    `Signal Rep Quality` %in% c('1_good','2_fair')
    , grepl('port sort', `Test in OP`)
    , `Predictability in OP` != 'indirect'
  ) 

# merge 
fitcomp = fit_all %>% 
  filter(model == 'raw') %>% 
  rename(tstat_CZ = tstat) %>% 
  inner_join(
    fit_OP
    , by = 'signalname'
  ) %>% 
  filter(!is.na(tstat_OP)) %>%  # some port sorts have only point estimates 
  filter(tstat_CZ>0)  # for activism you can get a negative ff alpha


# regression
reg = lm(tstat_CZ ~ tstat_OP, data = fitcomp) %>% summary()
regstr  = paste0(
  '[Chen-Zim] = ', round(reg$coefficients[1], 2)
  , ' + ', format(round(reg$coefficients[2], 2), nsmall = 2)
  , ' [Original], R-sq = ', round(100*reg$r.squared, 0), '%'
)

ablines = tibble(slope = c(1, round(reg$coefficients[2], 2)), 
                 intercept = c(0, round(reg$coefficients[1], 2)),
                 group = factor(x = c('45 degree line', 'OLS fit'),
                                levels = c('OLS fit', '45 degree line')))

fitcomp %>% 
  ggplot(aes(y=tstat_CZ, x = tstat_OP)) +
  geom_point(size=4) +
  coord_trans(x='log10', y='log10', xlim = c(1.5, 17), ylim = c(1.0, 15)) +
  ggrepel::geom_text_repel(aes(label=signalname), max.overlaps = 50, box.padding = 0.5) +
  scale_x_continuous(breaks=c(2, 5, 10, 15)) +
  scale_y_continuous(breaks=c(2, 5, 10, 15)) +
  theme_minimal(
    base_size = 20
  ) +
  theme(
    legend.position = c(.9, .25), legend.title = element_blank()
  ) +
  # geom_abline(
  #   data = ablines, aes(slope = slope, intercept = intercept, linetype = group)
  # ) +    
  annotate('text',x=3.3, y=15, label = regstr, size = 7) + 
  labs(y = 't-stat Chen-Zimmermann', 
       x = 't-stat Original Paper')  


ggsave('../results/rep_vs_op_raw.png', width = 10, height = 6, scale = 1.1)


# Name Scatter cleaner ====

ablines = tibble(slope = 1, 
                 intercept = 0,
                 group = factor(x = c('45 degree line'),
                                levels = c('45 degree line')))


# select comparable t-stats
fit_OP = doc %>% 
  mutate(
    tstat_OP = abs(as.numeric(`T-Stat`))
  ) %>% 
  select(
    signalname, tstat_OP, `Predictability in OP`, `Signal Rep Quality`, `Test in OP`
  ) %>% 
  filter(
    `Signal Rep Quality` %in% c('1_good','2_fair')
    , grepl('port', `Test in OP`)
    , `Predictability in OP` != 'indirect'
  ) 

# merge 
fitcomp = fit_all %>% 
  filter(model == 'raw') %>% 
  rename(tstat_CZ = tstat) %>% 
  inner_join(
    fit_OP
    , by = 'signalname'
  ) %>% 
  filter(!is.na(tstat_OP)) %>%  # some port sorts have only point estimates 
  filter(tstat_CZ>0)  # for activism you can get a negative ff alpha


fitcomp %>% 
  ggplot(aes(y=tstat_CZ, x = tstat_OP)) +
  geom_point(size=4) +
  coord_trans(x='log10', y='log10', xlim = c(1.5, 17), ylim = c(1.0, 15)) +
  ggrepel::geom_text_repel(aes(label=signalname), max.overlaps = Inf, box.padding = 0.5) +
  scale_x_continuous(breaks=c(2, 5, 10, 15)) +
  scale_y_continuous(breaks=c(2, 5, 10, 15)) +
  theme_minimal(
    base_size = 20
  ) +
  theme(
    legend.position = c(.9, .25), legend.title = element_blank()
  ) +
  geom_abline(
    data = ablines, aes(slope = slope, intercept = intercept, linetype = group) 
  ) +
  labs(y = 't-stat Replicated', 
       x = 't-stat Original Paper')  


ggsave('../results/rep_vs_op_cleaner.png', width = 10, height = 8, scale = 1.1)
ggsave('../results/rep_vs_op_cleaner_wide.png', width = 10, height = 6, scale = 1.1)


# Simple Scatterplots ====

## Prepare signal doc ====

catname = c('raw','factor or char adjusted','nonstandard lag')

# select comparable t-stats
fit_OP = doc %>% 
  mutate(
    tstat_OP = abs(as.numeric(`T-Stat`))
  ) %>% 
  select(
    signalname, tstat_OP, `Predictability in OP`
    , `Signal Rep Quality`, `Test in OP`, `Evidence Summary`
    , SampleEndYear
  ) %>% 
  filter(
    `Signal Rep Quality` %in% c('1_good','2_fair')
    , grepl('port', `Test in OP`)
    , `Predictability in OP` != 'indirect'
  ) %>% 
  filter(!is.na(tstat_OP)) %>%   # some port sorts have only point estimates 
  mutate(
    adjusted = if_else(
      `Test in OP` %in% c('port sort', 'LS port') # these are raw long-shorts
      , catname[1]
      , catname[2]
    ) 
    , adjusted = if_else(
      grepl('nonstandard', `Evidence Summary`)
      , catname[3], adjusted
    )
    , adjusted = factor(
      adjusted
      , levels = catname
    )
  )



## Raw vs OP plot ====


# merge 
fitcomp = fit_all %>% 
  filter(model == 'raw') %>% 
  rename(tstat_CZ = tstat) %>% 
  inner_join(
    fit_OP
    , by = 'signalname'
  ) 

# plot
tempname = 'Original Method'
fitcomp %>% 
  ggplot(aes(x=tstat_OP, y = tstat_CZ)) +
  geom_point(size=4, aes(shape =adjusted, fill = adjusted)) +
  theme_minimal(
    base_size = 15
  ) +
  theme(
    legend.position = c(.8, .25)
  ) +
  geom_abline(
    aes(slope = 1, intercept = 0)
  ) +
  scale_shape_manual(
    values = c(21, 22, 23), name = tempname
  ) +
  scale_fill_manual(
    values = c('blue', 'white', 'gray'), name = tempname
  ) +
  labs(x = 't-stat Original Paper (see legend)'
       , y = 't-stat Replicated (raw)')  +
  coord_trans(x='log10', y='log10', xlim = c(1.5, 17), ylim = c(1.0, 15)) +
  scale_x_continuous(breaks=c(2, 5, 10, 15)) +
  scale_y_continuous(breaks=c(2, 5, 10, 15)) +
  


ggsave('../results/raw_op.png', width = 10, height = 6, scale = 0.7)

## FF 22 vs OP ====

# merge 
fitcomp = fit_all %>% 
  filter(model == 'ff3_2022') %>% 
  rename(tstat_CZ = tstat) %>% 
  inner_join(
    fit_OP
    , by = 'signalname'
  ) 

# plot
tempname = 'Original Method'
fitcomp %>% 
  ggplot(aes(x=tstat_OP, y = tstat_CZ)) +
  geom_point(size=4, aes(shape =adjusted, fill = adjusted)) +
  theme_minimal(
    base_size = 15
  ) +
  theme(
    legend.position = c(.8, .25)
  ) +
  geom_abline(
    aes(slope = 1, intercept = 0)
  ) +
  scale_shape_manual(
    values = c(21, 22, 23), name = tempname
  ) +
  scale_fill_manual(
    values = c('blue', 'white', 'gray'), name = tempname
  ) +
  labs(x = 't-stat Original Paper (see legend)'
       , y = 't-stat Rep (FF3-alpha, 2022 vintage)')  +
  coord_trans(x='log10', y='log10', xlim = c(1.5, 17), ylim = c(1.0, 15)) +
  scale_x_continuous(breaks=c(2, 5, 10, 15)) +
  scale_y_continuous(breaks=c(2, 5, 10, 15)) +


ggsave('../results/ff22_op.png', width = 10, height = 6, scale = 0.7)

# check
fitcomp %>% 
  select(signalname, tstat_CZ, tstat_OP) %>% 
  mutate(delta = tstat_CZ - tstat_OP) %>% 
  arrange(-delta)

## FF 12 vs OP ====

# merge 
fitcomp = fit_all %>% 
  filter(model == 'ff3_2012') %>% 
  rename(tstat_CZ = tstat) %>% 
  inner_join(
    fit_OP, by = 'signalname'
  ) 

# plot
tempname = 'Original Method'
fitcomp %>% 
  ggplot(aes(x=tstat_OP, y = tstat_CZ)) +
  geom_point(size=4, aes(shape =adjusted, fill = adjusted)) +
  theme_minimal(
    base_size = 15
  ) +
  theme(
    legend.position = c(.8, .25)
  ) +
  geom_abline(
    aes(slope = 1, intercept = 0)
  ) +
  scale_shape_manual(
    values = c(21, 22, 23), name = tempname
  ) +
  scale_fill_manual(
    values = c('blue', 'white', 'gray'), name = tempname
  ) +
  labs(x = 't-stat Original Paper (see legend)'
       , y = 't-stat Rep (FF3-alpha, 2012 vintage)')  +
  coord_trans(x='log10', y='log10', xlim = c(1.5, 17), ylim = c(1.0, 15))  +
  scale_x_continuous(breaks=c(2, 5, 10, 15)) +
  scale_y_continuous(breaks=c(2, 5, 10, 15)) +

ggsave('../results/ff12_op.png', width = 10, height = 6, scale = 0.7)





## FF 05 vs OP ====

# merge 
fitcomp = fit_all %>% 
  filter(model == 'ff3_2005') %>% 
  rename(tstat_CZ = tstat) %>% 
  inner_join(
    fit_OP, by = 'signalname'
  ) %>% 
  filter(
    SampleEndYear <= 2005
  )

# plot
tempname = 'Original Method'
fitcomp %>% 
  ggplot(aes(x=tstat_OP, y = tstat_CZ)) +
  geom_point(size=4, aes(shape =adjusted, fill = adjusted)) +
  theme_minimal(
    base_size = 15
  ) +
  theme(
    legend.position = c(.8, .25)
  ) +
  geom_abline(
    aes(slope = 1, intercept = 0)
  ) +
  scale_shape_manual(
    values = c(21, 22, 23), name = tempname
  ) +
  scale_fill_manual(
    values = c('blue', 'white', 'gray'), name = tempname
  ) +
  labs(x = 't-stat Original Paper (see legend)'
       , y = 't-stat Rep (FF3-alpha, 2012 vintage)')  +
  coord_trans(x='log10', y='log10', xlim = c(1.5, 17), ylim = c(1.0, 15))  +
  scale_x_continuous(breaks=c(2, 5, 10, 15)) +
  scale_y_continuous(breaks=c(2, 5, 10, 15)) +


ggsave('../results/ff05_op.png', width = 10, height = 6, scale = 0.7)


## FF All vs OP ====

# merge 
fitcomp = fit_all %>% 
  rename(tstat_CZ = tstat) %>% 
  inner_join(
    fit_OP, by = 'signalname'
  ) %>% 
  filter(
    !(model == 'ff3_2005' & SampleEndYear > 2005)
    , model != 'raw'
  ) %>%  mutate(
    model = factor(model, levels = c('ff3_2022','ff3_2012','ff3_2005'))
  )

# plot
tempname = 'Vintage'
fitcomp %>% 
  ggplot(aes(x=tstat_OP, y = tstat_CZ)) +
  geom_point(size=4
       , aes(shape =model, color = model)) +
  theme_minimal(
    base_size = 15
  ) +
  theme(
    legend.position = c(.9, .25)
  ) +
  geom_abline(
    aes(slope = 1, intercept = 0)
  ) +
  scale_shape_manual(
    values = c(4, 1, 2), name = tempname
  ) +
  scale_color_manual(
    values = c('blue', 'green', 'red'), name = tempname
  ) +
  labs(x = 't-stat Original Paper (mostly raw)'
       , y = 't-stat Rep (FF3-alpha)')  +
  coord_trans(x='log10', y='log10', xlim = c(1.5, 17), ylim = c(1.0, 15)) 


ggsave('../results/ffall_op.png', width = 10, height = 6, scale = 0.7)

# Summary Stuff ====


## FF adjusted vs raw ====
plotme = fit_all %>% 
  filter(model == 'raw') %>% 
  transmute(signalname, tstat_cz = tstat) %>% 
  left_join(
    fit_all %>% filter(model != 'raw')
    , by = 'signalname'
  )

ggplot(plotme, aes(x=tstat_cz, y=tstat)) +
  geom_point(
    aes(color = model, shape = model), size = 2
  ) +
  theme_minimal() + 
  theme(
    legend.position = c(80,20)/100
  ) +
  geom_abline(aes(slope = 1, intercept = 0)) +
  geom_hline(yintercept = 2) +
  geom_vline(xintercept = 2) +
  labs(y = 't-stat, FF3 adjusted alpha', 
       x = 't-stat, raw return (CZ Fig 3)')  +
  scale_color_manual(
    name = 'Vintage'
    , values = c('ff3_2005' = 'red', 'ff3_2012' = 'green', 'ff3_2022' = 'blue')
    , labels = c('2005', '2012', '2022')
  ) +
  scale_shape_manual(
    name = 'Vintage'
    , values = c('ff3_2005' = 4, 'ff3_2012' = 16, 'ff3_2022' = 2)
    , labels = c('2005', '2012', '2022')
  )  +
  coord_cartesian(xlim = c(-5,15), ylim = c(-5,15))

ggsave('../results/ff3_vs_raw.pdf', width = 8, height = 8, scale = 0.6)





## FF 2022 vs FF 2012 ====

# compare directly ff vintages
plotme = fit_all %>% 
  filter(model != 'raw') %>% 
  select(signalname, tstat, model) %>% 
  pivot_wider(
      names_from = 'model', values_from = 'tstat'
  ) %>% 
  mutate(
    new_m_old = - ff3_2022 + ff3_2005, pct_chg = new_m_old/ff3_2022*100
  ) %>% 
  as.data.frame()
  

ggplot(plotme, aes(x = new_m_old)) +
  geom_histogram() +
  theme_minimal() +
  xlab('difference in t-stat: 2005 vs 2022 factor vintages')

ggsave('../results/hist_diff.pdf', width = 8, height = 5, scale = 0.6)
  

# Sanity Check ====

# find alphas and betas
target_data = cz_all %>% 
  filter(vint == 2022 & insamp & port == 'LS') %>% 
  mutate(yearm = year(date)*100 + month(date)) %>% 
  select(signalname, yearm, ret)

fitme = target_data %>% 
  left_join(
    ff_all %>% filter(vint == 2005), by = 'yearm'
  )

# reference
fitsum = summary(lm(ret ~ mktrf+smb+hml, data = fitme))
fitsum$coefficients


fitx = fitme[
  , list(
    alpha = summary(lm(ret ~ mktrf+smb+hml))$coefficients['(Intercept)' , 'Estimate']
    , tstat = summary(lm(ret ~ mktrf+smb+hml))$coefficients['(Intercept)' , 't value']
    , bmkt = summary(lm(ret ~ mktrf+smb+hml))$coefficients['mktrf' , 'Estimate']
    , bsmb = summary(lm(ret ~ mktrf+smb+hml))$coefficients['smb' , 'Estimate']
    , bhml = summary(lm(ret ~ mktrf+smb+hml))$coefficients['hml' , 'Estimate']    
  )
  , by=signalname
] %>% 
  mutate(
    model = 'ff3_2005'
  )

# find premiums
prems = fitme %>% group_by(signalname) %>% 
  summarize(
    rbar = mean(ret), emkt = mean(mktrf), esmb = mean(smb), ehml = mean(hml)
  )


# output check to console
fitx %>% 
  left_join(prems) %>% 
  mutate(
    rbarcheck = alpha + bmkt*emkt+bsmb*esmb+bhml*ehml
    , err = rbarcheck - rbar
  )


hist(fitx$bhml)
hist(fitx$bmkt)
hist(fitx$bsmb)

# Detail check ====


# list bad obs with evidence
plotme %>% 
  filter(model == 'ff3_2022') %>% 
  mutate(delta = tstat - tstat_cz) %>% 
  arrange(-delta) %>% 
  left_join(doc %>% select(signalname, `Evidence Summary`)) %>% 
  select(signalname, tstat_cz, tstat, `Evidence Summary`) %>% 
  as.data.frame() %>% 
  head(20)



# list bad obs with evidence
plotme %>% 
  filter(model == 'ff3_2022') %>% 
  mutate(delta = tstat - tstat_cz) %>% 
  arrange(delta) %>% 
  left_join(doc %>% select(signalname, `Evidence Summary`)) %>% 
  select(signalname, tstat_cz, tstat, `Evidence Summary`) %>% 
  as.data.frame() %>% 
  head(50)

fit_all %>% filter(signalname == 'hire')

fit_all %>% filter(grepl('InvG', signalname))

fit_all %>% filter(grepl('Brand', signalname))

fit_all %>% filter(grepl('CBOper', signalname))


# Decomp of revision ====



targetdat = cz_all %>% 
  filter(vint == 2022, insamp, port == 'LS') %>% 
  mutate(yearm = year(date)*100 + month(date)) %>% 
  select(signalname, yearm, ret)


revdat = targetdat %>% 
  left_join(
    ff_all %>% filter(vint == 2022) %>% transmute(yearm, hml22 = hml)
  ) %>% 
  left_join(
    ff_all %>% filter(vint == 2012) %>% transmute(yearm, hml12 = hml)
  )  %>% 
  mutate(
    rev_hml = hml22-hml12
  )

cordat  = revdat %>% 
  group_by(signalname) %>% 
  summarize(
    cor_rev = cor(ret, rev_hml)
    , acor = abs(cor_rev)
  )

ggplot(cordat, aes(x=acor)) +
  geom_histogram(bins = 10) +
  theme_minimal(
    base_size = 20
  ) +
  xlab('|cor(ret,rev)|')



ggsave('../results/cor_ret_rev.png')