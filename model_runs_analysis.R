library(ggplot2)
library(tidyverse)


naming_map <- data.frame(
  'scenario' = c("baseline_run", "baseline_migration2x_run", "infill_run", "infill_migration2x_run", 
                 "infill_noflood_run", "infill_noflood_migration2x_run", "noflood_run", "noflood_migration2x_run", 
                 "infill_nofire_run", "infill_nofire_migration2x_run", "nofire_run", 
                 "nofire_migration2x_run"),
  'Scenario' = factor(c('Baseline', 'Baseline with migration', 'Infill', 'Infill with migration', 'Infill with zeroed flood risk', 'Infill with migration and zeroed flood risk', 
                 'Baseline with zeroed flood risk', 'Baseline with migration and zeroed flood risk', 'Infill with zeroed fire risk', 'Infill with migration and zereoed fire risk', 
                 'Baseline with zeroed fire risk', 'Baseline with migration and zeroed fire risk'))
)

vars_map <- data.frame(
  'variable' = c('ag_loss',  "forest_loss", "risk_dev", "newurb", "risk_dev_new"),
  'Variable' = c('Decrease in agricultural land', 'Decrease in forest cover', 'Total area of development on risk-prone land', 'Area of new development', 'Risk-prone area developed since 2016')
)


csdf <- read.csv('D:\\GLISA-WWA\\Paper_runs\\Colorado_springs\\csdf_migration2.csv') %>%
  select(!X) %>%
  rename(scenario = run, run = scenario) %>%
  mutate(risk_dev_new = risk_dev - 83776)
grdf <- read.csv('D:\\GLISA-WWA\\Paper_runs\\Grand_rapids\\grdf_migration2.csv') %>%
  select(!X) %>%
  rename(scenario = run, run = scenario) %>%
  mutate(risk_dev_new = risk_dev - 88213)

# Mutate to pull out migration, migration2x and migration4x from **_run, and to grab scenario type 

summary_1_cs <- csdf %>%
  pivot_longer(
    cols = c(ag_loss, forest_loss, risk_dev, newurb, risk_dev_new),
    names_to = 'variable',
    values_to = 'value'
  ) %>%
  group_by(scenario, variable) %>%
  summarise(mean = mean(value), std = sd(value)) %>%
  mutate(
    mean_sqkm = mean/1111.11111111,
    std_sqkm = std/1111.11111111) 

summary_1_gr <- grdf %>%
  pivot_longer(
    cols = c(ag_loss, forest_loss, risk_dev, newurb, risk_dev_new),
    names_to = 'variable',
    values_to = 'value'
  ) %>%
  group_by(scenario, variable) %>%
  summarise(mean = mean(value), std = sd(value)) %>%
  mutate(
    mean_sqkm = mean/1111.11111111,
    std_sqkm = std/1111.11111111) 


cols = rainbow(26, s=.6, v=.9)[sample(1:26,26)]

# pcs <- ggplot(summary_1_cs, aes(x = variable, y = mean, fill = scenario)) +
#   geom_bar(stat = 'identity', color = 'black', position = position_dodge(.9)) +
#   scale_fill_manual(values = cols) +
#   geom_errorbar(aes(ymin=mean-std, ymax =mean+std), width = .2, position = position_dodge(.9))
# 
# pcs
# 
# pgr <- ggplot(summary_1_gr, aes(x = variable, y = mean, fill = scenario)) +
#   geom_bar(stat = 'identity', color = 'black', position = position_dodge(.9)) +
#   scale_fill_manual(values = cols) +
#   geom_errorbar(aes(ymin=mean-std, ymax =mean+std), width = .2, position = position_dodge(.9))
# pgr 

###############################
# Significance relative to baseline:

# GR

for (model_var in c("ag_loss", "forest_loss", "risk_dev", "newurb", "risk_dev_new")){
  for (scenar in c("baseline_migration2x_run", "infill_run", "infill_migration2x_run", 
                   "infill_noflood_run", "infill_noflood_migration2x_run", "noflood_run", 
                   "noflood_migration2x_run")) {
    pval <- t.test(grdf[grdf$scenario == 'baseline_run', model_var], grdf[grdf$scenario == scenar, model_var], alternative = 't')$p.value
    print(paste(model_var,
                scenar,
                pval))
    summary_1_gr[summary_1_gr$scenario == scenar & summary_1_gr$variable == model_var, 'p_val'] <- pval
  }
}

summary_1_gr$Significance <- ifelse(summary_1_gr$p_val < 0.05, 'Significantly different from baseline scenario', 'Not significantly different from baseline scenario')

# CS

for (model_var in c("ag_loss", "forest_loss", "risk_dev", "newurb", "risk_dev_new")){
  for (scenar in c("baseline_migration2x_run", "infill_run", "infill_migration2x_run", 
                   "infill_nofire_run", "infill_nofire_migration2x_run", "nofire_run", 
                   "nofire_migration2x_run")) {
    pval <- t.test(csdf[csdf$scenario == 'baseline_run', model_var], csdf[csdf$scenario == scenar, model_var], alternative = 't')$p.value
    print(paste(model_var,
                scenar,
                pval))
    summary_1_cs[summary_1_cs$scenario == scenar & summary_1_cs$variable == model_var, 'p_val'] <- pval
  }
}

summary_1_cs$Significance <- ifelse(summary_1_cs$p_val < 0.05, 'Significantly different from baseline scenario', 'Not significantly different from baseline scenario')

###############################
## Original estimates
# summary_1x_gr <- summary_1_gr %>%
#   filter(scenario %in% c("baseline_run", "baseline_migration_run", "infill_run", "infill_migration_run", 
#                          "infill_noflood_run", "infill_noflood_migration_run", "noflood_run", "noflood_migration_run"))
# 
# pgr1x <- ggplot(summary_1x_gr, aes(x = variable, y = mean, fill = scenario)) +
#   geom_bar(stat = 'identity', color = 'black', position = position_dodge(.9)) +
#   scale_fill_brewer(palette = 'Set3') +
#   geom_errorbar(aes(ymin=mean-std, ymax =mean+std), width = .2, position = position_dodge(.9))
# pgr1x
# 
# 
# ###############################
# ## 4x
# summary_4x_gr <- summary_1_gr %>%
#   filter(scenario %in% c("baseline_run", "baseline_migration4x_run", "infill_run", "infill_migration4x_run", 
#                          "infill_noflood_run", "infill_noflood_migration4x_run", "noflood_run", "noflood_migration4x_run"))
# 
# pgr4x <- ggplot(summary_4x_gr, aes(x = variable, y = mean, fill = scenario)) +
#   geom_bar(stat = 'identity', color = 'black', position = position_dodge(.9)) +
#   scale_fill_brewer(palette = 'Set3') +
#   geom_errorbar(aes(ymin=mean-std, ymax =mean+std), width = .2, position = position_dodge(.9))
# pgr4x


###############################
## 2x
summary_2x_gr <- summary_1_gr %>%
  filter(scenario %in% c("baseline_run", "baseline_migration2x_run", "infill_run", "infill_migration2x_run", 
                         "infill_noflood_run", "infill_noflood_migration2x_run", "noflood_run", "noflood_migration2x_run")) %>%
  mutate(scenario = as.factor(scenario))

summary_2x_gr$scenario <- ordered(summary_2x_gr$scenario, levels = 
                                    c("baseline_run", "baseline_migration2x_run", "infill_run", "infill_migration2x_run", 
                                      "infill_noflood_run", "infill_noflood_migration2x_run", "noflood_run", "noflood_migration2x_run"))

# pgr2x <- ggplot(summary_2x_gr, aes(x = variable, y = mean, fill = scenario)) +
#   geom_bar(stat = 'identity', color = 'black', position = position_dodge(.9)) +
#   scale_fill_brewer(palette = 'Set3') +
#   geom_errorbar(aes(ymin=mean-std, ymax =mean+std), width = .2, position = position_dodge(.9)) +
#   ggtitle('Pixel Allotments: Grand Rapids')
# pgr2x

pgr2x2 <- ggplot(summary_2x_gr, aes(x = scenario, y = mean, fill = Significance)) +
  geom_bar(stat = 'identity', color = 'black', position = position_dodge(.9)) +
  coord_flip() + 
  scale_fill_brewer(palette = 'Set3') +
  geom_errorbar(aes(ymin=mean-std, ymax =mean+std), width = .2, position = position_dodge(.9)) +
  facet_wrap(vars(variable), scales = 'free_x') +
  ggtitle('Pixel Allotments: Grand Rapids')
pgr2x2

summary_2x_cs <- summary_1_cs %>%
  filter(scenario %in% c("baseline_run", "baseline_migration2x_run", "infill_run", "infill_migration2x_run", 
                         "infill_nofire_run", "infill_nofire_migration2x_run", "nofire_run", "nofire_migration2x_run"))%>%
  mutate(scenario = as.factor(scenario))

summary_2x_cs$scenario <- ordered(summary_2x_cs$scenario, levels = 
                                    c("baseline_run", "baseline_migration2x_run", "infill_run", "infill_migration2x_run", 
                                      "infill_nofire_run", "infill_nofire_migration2x_run", "nofire_run", "nofire_migration2x_run"))


# pcs2x <- ggplot(summary_2x_cs, aes(x = variable, y = mean, fill = scenario)) +
#   geom_bar(stat = 'identity', color = 'black', position = position_dodge(.9)) +
#   scale_fill_brewer(palette = 'Set3') +
#   geom_errorbar(aes(ymin=mean-std, ymax =mean+std), width = .2, position = position_dodge(.9))+
#   ggtitle('Pixel Allotments: Colorado Springs')
# pcs2x

# pcs2x <- ggplot(summary_2x_cs, aes(x = scenario, y = mean, fill = Significance)) +
#   geom_bar(stat = 'identity', color = 'black', position = position_dodge(.9)) +
#   coord_flip() + 
#   scale_fill_brewer(palette = 'Set3') +
#   geom_errorbar(aes(ymin=mean-std, ymax =mean+std), width = .2, position = position_dodge(.9)) +
#   facet_wrap(vars(variable), scales = 'free_x') +
#   ggtitle('Pixel Allotments: Colorado Springs')
# pcs2x


###############################
# newurb_compare_gr <- summary_1_gr %>%
#   filter(scenario %in% c("baseline_run", "baseline_migration_run", "baseline_migration2x_run", "baseline_migration4x_run"))
# 
# pgr_comp <- ggplot(newurb_compare_gr, aes(x = variable, y = mean, fill = scenario)) +
#   geom_bar(stat = 'identity', color = 'black', position = position_dodge(.9)) +
#   scale_fill_brewer(palette = 'Set3') +
#   geom_errorbar(aes(ymin=mean-std, ymax =mean+std), width = .2, position = position_dodge(.9))
# pgr_comp
# 
# newurb_compare_cs <- summary_1_cs %>%
#   filter(scenario %in% c("baseline_run", "baseline_migration_run", "baseline_migration2x_run", "baseline_migration4x_run"))
# 
# pcs_comp <- ggplot(newurb_compare_cs, aes(x = variable, y = mean, fill = scenario)) +
#   geom_bar(stat = 'identity', color = 'black', position = position_dodge(.9)) +
#   scale_fill_brewer(palette = 'Set3') +
#   geom_errorbar(aes(ymin=mean-std, ymax =mean+std), width = .2, position = position_dodge(.9))
# pcs_comp



# summary_1_gr$p_val <- NA
# 
# for (rownum in seq(1:nrow(summary_1_gr))) {
#   summary_1_gr[rownum, 'p_val'] <- t.test(grdf[grdf$scenario == 'baseline_run', as.character(summary_1_gr[rownum, 'variable'])], 
#                                                grdf[grdf$scenario == summary_1_gr[rownum, 'scenario'], as.character(summary_1_gr[rownum, 'variable'])])$p.value
# }
# 
# summary_1_gr_test <- summary_1_gr %>%
#   rename(scenar = scenario, model_var = variable) %>%
#   mutate(p_val = t.test(grdf[grdf$scenario == 'baseline_run', model_var], grdf[grdf$scenario == scenar, model_var])$p.value)
#########################################

gr_sig <- summary_2x_gr %>%
  select(scenario, variable, Significance)
  
vdf_gr <- grdf %>% pivot_longer(cols = c(ag_loss, forest_loss, risk_dev, newurb, risk_dev_new), names_to = 'variable', values_to = 'value') %>%
  filter(scenario %in% c("baseline_run", "baseline_migration2x_run", "infill_run", "infill_migration2x_run", 
                         "infill_noflood_run", "infill_noflood_migration2x_run", "noflood_run", "noflood_migration2x_run")) %>%
  left_join(gr_sig) %>%
  left_join(naming_map) %>%
  left_join(vars_map) %>%
  mutate(sqkm = value/1111.11111111)

### Violin plots
vgr <- ggplot(vdf_gr, aes(x = Scenario, y = sqkm, fill = Significance, color = Significance)) + 
  geom_violin() +
  geom_boxplot(width=0.2, color = 'black', fill = NA) +
  coord_flip() +
  ylab('Sq. km') +
  xlab('')+
  facet_wrap(vars(Variable), scales = 'free_x') +
  ggtitle('Area Allotments: Grand Rapids') + theme(
    legend.position = c(0.85, 0.25), # c(0,0) bottom left, c(1,1) top-right.
    legend.background = element_rect(fill = "white", colour = NA)
  )
  
vgr

png(filename = file.path('../Paper_runs', 'Figs', 'Grand_rapids_violin.png'), width=1000, height=400)
plot(vgr)
dev.off()

########
cs_sig <- summary_2x_cs %>%
  select(scenario, variable, Significance)

vdf_cs <- csdf %>% pivot_longer(cols = c(ag_loss, forest_loss, risk_dev, newurb, risk_dev_new), names_to = 'variable', values_to = 'value') %>%
  filter(scenario %in% c("baseline_run", "baseline_migration2x_run", "infill_run", "infill_migration2x_run", 
                         "infill_nofire_run", "infill_nofire_migration2x_run", "nofire_run", "nofire_migration2x_run")) %>%
  left_join(cs_sig) %>%
  left_join(naming_map) %>%
  left_join(vars_map) %>%
  mutate(sqkm = value/1111.11111111) 

### Violin plots
vcs <- ggplot(vdf_cs, aes(x = Scenario, y = sqkm, fill = Significance, color = Significance)) + 
  geom_violin() +
  geom_boxplot(width=0.2, color = 'black', fill = NA) +
  coord_flip() +
  ylab('Sq. km') +
  xlab('')+
  facet_wrap(vars(Variable), scales = 'free_x') +
  ggtitle('Area Allotments: Colorado Springs') + theme(
    legend.position = c(0.85, 0.25), # c(0,0) bottom left, c(1,1) top-right.
    legend.background = element_rect(fill = "white", colour = NA)
  )

vcs

png(filename = file.path('../Paper_runs', 'Figs', 'Colorado_springs_violin.png'), width=1000, height=400)
plot(vcs)
dev.off()

t.test(csdf[csdf$scenario == 'baseline_run', 'newurb'], csdf[csdf$scenario == 'baseline_migration2x_run', 'newurb'])
plot(density(csdf[csdf$scenario == 'baseline_run', 'newurb']))
lines(density(csdf[csdf$scenario == 'baseline_migration2x_run', 'newurb']), add = T, col = 'red')

###
# Between migration / not migration pairings?
t.test(csdf[csdf$scenario == 'baseline_run', 'risk_dev'], csdf[csdf$scenario == 'baseline_migration2x_run', 'risk_dev'], alternative = 't')
t.test(csdf[csdf$scenario == 'baseline_run', 'newurb'], csdf[csdf$scenario == 'baseline_migration2x_run', 'newurb'], alternative = 't')
t.test(csdf[csdf$scenario == 'baseline_run', 'ag_loss'], csdf[csdf$scenario == 'baseline_migration2x_run', 'ag_loss'])
t.test(csdf[csdf$scenario == 'baseline_run', 'forest_loss'], csdf[csdf$scenario == 'baseline_migration2x_run', 'forest_loss'])

t.test(csdf[csdf$scenario == 'infill_run', 'risk_dev'], csdf[csdf$scenario == 'infill_migration2x_run', 'risk_dev'])
t.test(csdf[csdf$scenario == 'infill_run', 'newurb'], csdf[csdf$scenario == 'infill_migration2x_run', 'newurb'])
t.test(csdf[csdf$scenario == 'infill_run', 'ag_loss'], csdf[csdf$scenario == 'infill_migration2x_run', 'ag_loss'])
t.test(csdf[csdf$scenario == 'infill_run', 'forest_loss'], csdf[csdf$scenario == 'infill_migration2x_run', 'forest_loss'])

t.test(csdf[csdf$scenario == 'nofire_run', 'risk_dev'], csdf[csdf$scenario == 'nofire_migration2x_run', 'risk_dev'])
t.test(csdf[csdf$scenario == 'nofire_run', 'newurb'], csdf[csdf$scenario == 'nofire_migration2x_run', 'newurb'])
t.test(csdf[csdf$scenario == 'nofire_run', 'ag_loss'], csdf[csdf$scenario == 'nofire_migration2x_run', 'ag_loss'])
t.test(csdf[csdf$scenario == 'nofire_run', 'forest_loss'], csdf[csdf$scenario == 'nofire_migration2x_run', 'forest_loss'])

t.test(csdf[csdf$scenario == 'infill_nofire_run', 'risk_dev'], csdf[csdf$scenario == 'infill_nofire_migration2x_run', 'risk_dev'])
t.test(csdf[csdf$scenario == 'infill_nofire_run', 'newurb'], csdf[csdf$scenario == 'infill_nofire_migration2x_run', 'newurb'])
t.test(csdf[csdf$scenario == 'infill_nofire_run', 'ag_loss'], csdf[csdf$scenario == 'infill_nofire_migration2x_run', 'ag_loss'])
t.test(csdf[csdf$scenario == 'infill_nofire_run', 'forest_loss'], csdf[csdf$scenario == 'infill_nofire_migration2x_run', 'forest_loss'])

### GR
t.test(grdf[grdf$scenario == 'baseline_run', 'risk_dev'], grdf[grdf$scenario == 'baseline_migration2x_run', 'risk_dev'])
t.test(grdf[grdf$scenario == 'baseline_run', 'newurb'], grdf[grdf$scenario == 'baseline_migration2x_run', 'newurb'])
t.test(grdf[grdf$scenario == 'baseline_run', 'ag_loss'], grdf[grdf$scenario == 'baseline_migration2x_run', 'ag_loss'])
t.test(grdf[grdf$scenario == 'baseline_run', 'forest_loss'], grdf[grdf$scenario == 'baseline_migration2x_run', 'forest_loss'])

t.test(grdf[grdf$scenario == 'infill_run', 'risk_dev'], grdf[grdf$scenario == 'infill_migration2x_run', 'risk_dev'])
t.test(grdf[grdf$scenario == 'infill_run', 'newurb'], grdf[grdf$scenario == 'infill_migration2x_run', 'newurb'])
t.test(grdf[grdf$scenario == 'infill_run', 'ag_loss'], grdf[grdf$scenario == 'infill_migration2x_run', 'ag_loss'])
t.test(grdf[grdf$scenario == 'infill_run', 'forest_loss'], grdf[grdf$scenario == 'infill_migration2x_run', 'forest_loss'])

t.test(grdf[grdf$scenario == 'noflood_run', 'risk_dev'], grdf[grdf$scenario == 'noflood_migration2x_run', 'risk_dev'])
t.test(grdf[grdf$scenario == 'noflood_run', 'newurb'], grdf[grdf$scenario == 'noflood_migration2x_run', 'newurb'])
t.test(grdf[grdf$scenario == 'noflood_run', 'ag_loss'], grdf[grdf$scenario == 'noflood_migration2x_run', 'ag_loss'])
t.test(grdf[grdf$scenario == 'noflood_run', 'forest_loss'], grdf[grdf$scenario == 'noflood_migration2x_run', 'forest_loss'])

t.test(grdf[grdf$scenario == 'infill_noflood_run', 'risk_dev'], grdf[grdf$scenario == 'infill_noflood_migration2x_run', 'risk_dev'])
t.test(grdf[grdf$scenario == 'infill_noflood_run', 'newurb'], grdf[grdf$scenario == 'infill_noflood_migration2x_run', 'newurb'])
t.test(grdf[grdf$scenario == 'infill_noflood_run', 'ag_loss'], grdf[grdf$scenario == 'infill_noflood_migration2x_run', 'ag_loss'])
t.test(grdf[grdf$scenario == 'infill_noflood_run', 'forest_loss'], grdf[grdf$scenario == 'infill_noflood_migration2x_run', 'forest_loss'])


# Between scenarios?

# for (model1_var in c("ag_loss", "forest_loss", "risk_dev", "newurb", "risk_dev_new")){
#   for (scenar in c("baseline_migration2x_run", "infill_run", "infill_migration2x_run", 
#                    "infill_noflood_run", "infill_noflood_migration2x_run", "noflood_run", 
#                    "noflood_migration2x_run")) {
#     pval <- t.test(grdf[grdf$scenario == 'baseline_run', model_var], grdf[grdf$scenario == scenar, model_var])$p.value
# 
#   }
# }


sigmat_gr <- function(varb){
  var_mat <- matrix(0, ncol = 8, nrow = 8)
  rownames(var_mat) <- c("baseline_run", "baseline_migration2x_run", "infill_run", "infill_migration2x_run", 
                      "infill_noflood_run", "infill_noflood_migration2x_run", "noflood_run", 
                      "noflood_migration2x_run")
  colnames(var_mat) <- c("baseline_run", "baseline_migration2x_run", "infill_run", "infill_migration2x_run", 
                      "infill_noflood_run", "infill_noflood_migration2x_run", "noflood_run", 
                      "noflood_migration2x_run")

  for (scenar1 in c("baseline_run", "baseline_migration2x_run", "infill_run", "infill_migration2x_run", 
                 "infill_noflood_run", "infill_noflood_migration2x_run", "noflood_run", 
                 "noflood_migration2x_run")) {
    for (scenar2 in c("baseline_run", "baseline_migration2x_run", "infill_run", "infill_migration2x_run", 
                   "infill_noflood_run", "infill_noflood_migration2x_run", "noflood_run", 
                   "noflood_migration2x_run")) {
      pval <- t.test(grdf[grdf$scenario == scenar1, varb], grdf[grdf$scenario == scenar2, varb])$p.value
      var_mat[scenar1, scenar2] <- pval
      var_mat[var_mat > 0.05] <- NA
    }
  }
  return(var_mat)
}

sigmat_cs <- function(varb){
  var_mat <- matrix(0, ncol = 8, nrow = 8)
  rownames(var_mat) <- c("baseline_run", "baseline_migration2x_run", "infill_run", "infill_migration2x_run", 
                         "infill_nofire_run", "infill_nofire_migration2x_run", "nofire_run", 
                         "nofire_migration2x_run")
  colnames(var_mat) <- c("baseline_run", "baseline_migration2x_run", "infill_run", "infill_migration2x_run", 
                         "infill_nofire_run", "infill_nofire_migration2x_run", "nofire_run", 
                         "nofire_migration2x_run")
  
  for (scenar1 in c("baseline_run", "baseline_migration2x_run", "infill_run", "infill_migration2x_run", 
                    "infill_nofire_run", "infill_nofire_migration2x_run", "nofire_run", 
                    "nofire_migration2x_run")) {
    for (scenar2 in c("baseline_run", "baseline_migration2x_run", "infill_run", "infill_migration2x_run", 
                      "infill_nofire_run", "infill_nofire_migration2x_run", "nofire_run", 
                      "nofire_migration2x_run")) {
      pval <- t.test(csdf[csdf$scenario == scenar1, varb], csdf[csdf$scenario == scenar2, varb])$p.value
      var_mat[scenar1, scenar2] <- pval
      var_mat[var_mat > 0.05] <- NA
    }
  }
  return(var_mat)
}

gr_ag_mat <- sigmat_gr('ag_loss')
gr_forest_mat <- sigmat_gr('forest_loss')
gr_newurb <- sigmat_gr('newurb')
gr_risk <- sigmat_gr('risk_dev')

cs_ag_mat <- sigmat_cs('ag_loss')
cs_forest_mat <- sigmat_cs('forest_loss')
cs_newurb <- sigmat_cs('newurb')
cs_risk <- sigmat_cs('risk_dev')

t.test(grdf[grdf$scenario == 'infill_migration2x_run', 'newurb'], grdf[grdf$scenario == 'baseline_migration2x_run', 'newurb'])$p.value
