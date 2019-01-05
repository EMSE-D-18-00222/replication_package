library('data.table')
library('ggplot2')
library('gridExtra')
library('pscl')
library('dendextend')
library('fasttime')
library('stargazer')
library('GGally')
library('gplots')
library('truncnorm')
library('AUC')
library('ggthemes')

theme_Publication <- function(base_size=14, base_family="helvetica") {
  library(grid)
  library(ggthemes)
  (theme_foundation(base_size=base_size, base_family=base_family)
    + theme(plot.title = element_text(face = "bold",
                                      size = rel(1.2), hjust = 0.5),
            text = element_text(),
            panel.background = element_rect(colour = NA),
            plot.background = element_rect(colour = NA),
            panel.border = element_rect(colour = NA),
            axis.title = element_text(face = "bold",size = rel(1)),
            axis.title.y = element_text(angle=90,vjust =2),
            axis.title.x = element_text(vjust = -0.2),
            axis.text = element_text(), 
            axis.line = element_line(colour="black"),
            axis.ticks = element_line(),
            panel.grid.major = element_line(colour="#f0f0f0"),
            panel.grid.minor = element_blank(),
            legend.key = element_rect(colour = NA),
            legend.position = "bottom",
            legend.direction = "horizontal",
            legend.key.size= unit(0.2, "cm"),
            legend.margin = unit(0, "cm"),
            legend.title = element_text(face="italic"),
            plot.margin=unit(c(10,5,5,5),"mm"),
            strip.background=element_rect(colour="#f0f0f0",fill="#f0f0f0"),
            strip.text = element_text(face="bold")
    ))
  
}

scale_fill_Publication <- function(...){
  library(scales)
  discrete_scale("fill","Publication",manual_pal(values = c("#386cb0","#fdb462","#7fc97f","#ef3b2c","#662506","#a6cee3","#fb9a99","#984ea3","#ffff33")), ...)
  
}

scale_colour_Publication <- function(...){
  library(scales)
  discrete_scale("colour","Publication",manual_pal(values = c("#386cb0","#fdb462","#7fc97f","#ef3b2c","#662506","#a6cee3","#fb9a99","#984ea3","#ffff33")), ...)
  
}

plot_pred_obs <- function(model) {
  dt = data.table(obs=model$model$nonreply.indegree.response, pred=predict(model, type='response'))
  model.lm = lm(y ~ x,
                data=ggplot_build(ggplot(dt, aes(obs, pred)) + geom_smooth(method='lm'))$data[[1]][, c(1,2)])
  ggplot(dt, aes(obs, pred)) + 
    geom_point() +
    geom_smooth(aes(color='Trend Line'), method='lm') +
    geom_segment(aes(x=0, y=0, xend=max(dt$obs), yend=max(dt$obs), color='y = x'), linetype='dashed', size=2) +
    xlab('Observed Value') + ylab('Predicted Value') +
    scale_color_ptol('') +
    theme_minimal() +
    coord_fixed()
}


set_na_0 <- function(dt) {
  for(j in names(dt)) {
    data.table::set(dt, which(is.na(dt[[j]])), j, 0)
  }
}

min.max.dates <- fread('./data/data200_min_max_dates_6mo_perproject.csv')
min.max.dates[, max_date_minus := fastPOSIXct(max_date_minus, tz='GMT')]

commits.d <- fread('./data/data200_commit_data_splitmax_6mo.csv')
commits.d <- commits.d[login != '']
commits.d[, committer := 1]

adjacency.reply.d <- fread('./data/data200_cumulative_network_adjacency_list_reply_splitmax_6mo.csv')

user.reply.indegrees <- adjacency.reply.d[, .(sum(count)), by=.(to, project)]
setnames(user.reply.indegrees, c('to', 'V1'), c('login', 'reply.indegree'))
user.reply.indegrees[, poster := 1]
user.reply.outdegrees <- adjacency.reply.d[, .(sum(count)), by=.(from, project)]
setnames(user.reply.outdegrees, c('from', 'V1'), c('login', 'reply.outdegree'))
user.reply.outdegrees[, poster := 1]

adjacency.nonreply.d <- fread('./data/data200_cumulative_network_adjacency_list_nonreply_splitmax_6mo.csv')

user.nonreply.indegrees <- adjacency.nonreply.d[, .(sum(count)), by=.(to, project)]
setnames(user.nonreply.indegrees, c('to', 'V1'), c('login', 'nonreply.indegree'))
user.nonreply.indegrees[, poster := 1]
user.nonreply.outdegrees <- adjacency.nonreply.d[, .(sum(count)), by=.(from, project)]
setnames(user.nonreply.outdegrees, c('from', 'V1'), c('login', 'nonreply.outdegree'))
user.nonreply.outdegrees[, poster := 1]

setkey(commits.d, login, project)
setkey(user.reply.indegrees, login, project)
setkey(user.reply.outdegrees, login, project)
setkey(user.nonreply.indegrees, login, project)
setkey(user.nonreply.outdegrees, login, project)

dt <- merge(commits.d, user.reply.indegrees, all=T)
set_na_0(dt)
dt <- merge(dt, user.reply.outdegrees, all=T)
dt[, poster := poster.x]
dt[is.na(poster) & !is.na(poster.y), poster := poster.y]
dt[, poster.x := NULL]
dt[, poster.y := NULL]
set_na_0(dt)
dt <- merge(dt, user.nonreply.outdegrees, all=T)
dt[, poster := poster.x]
dt[is.na(poster) & !is.na(poster.y), poster := poster.y]
dt[, poster.x := NULL]
dt[, poster.y := NULL]
set_na_0(dt)
dt <- merge(dt, user.nonreply.indegrees, all=T)
dt[, poster := poster.x]
dt[is.na(poster) & !is.na(poster.y), poster := poster.y]
dt[, poster.x := NULL]
dt[, poster.y := NULL]
set_na_0(dt)

adjacency.nonreply.aftermax.d <- fread('./data/data200_cumulative_network_adjacency_list_nonreply_splitmax_6mo_after_max.csv')

user.nonreply.indegrees.aftermax <- adjacency.nonreply.aftermax.d[, .(sum(count)), by=.(to, project)]
setnames(user.nonreply.indegrees.aftermax, c('to', 'V1'), c('login', 'nonreply.indegree.aftermax'))

setkey(dt, login, project)
setkey(user.nonreply.indegrees.aftermax, login, project)

dt <- user.nonreply.indegrees.aftermax[dt]
# People with no aftermax mean no changes after the split date
dt[is.na(nonreply.indegree.aftermax), nonreply.indegree.aftermax := 0]
# Have to add since the aftermax value is ONLY from the split date to the end
dt[, nonreply.indegree.response := nonreply.indegree + nonreply.indegree.aftermax]


#####################################
load('./data/data200_daf.reply.dt_splitmax_6mo.RData')
setnames(daf.reply.dt, c('daf', 'dafprime'), c('daf.reply', 'dafprime.reply'))
load('./data/data200_maf.reply.dt_splitmax_6mo.RData')
setnames(maf.reply.dt, c('maf', 'mafprime'), c('maf.reply', 'mafprime.reply'))
load('./data/data200_daf.nonreply.dt_splitmax_6mo.RData') 
setnames(daf.nonreply.dt, c('daf', 'dafprime'), c('daf.nonreply', 'dafprime.nonreply'))
load('./data/data200_maf.nonreply.dt_splitmax_6mo.RData')
setnames(maf.nonreply.dt, c('maf', 'mafprime'), c('maf.nonreply', 'mafprime.nonreply'))
load('./data/data200_daf.dt_splitmax_6mo.RData') 

setkey(daf.reply.dt, login, project)
setkey(daf.nonreply.dt, login, project)
setkey(maf.reply.dt, login, project)
setkey(maf.nonreply.dt, login, project)
setkey(daf.dt, login, project)


mafdaf.dt <- merge(merge(daf.reply.dt, daf.nonreply.dt, all=T), merge(maf.reply.dt, maf.nonreply.dt, all=T), all=T)
mafdaf.dt <- merge(mafdaf.dt, daf.dt, all=T)
# set_na_0(mafdaf.dt)

setkey(dt, login, project)
setkey(mafdaf.dt, login, project)

dt <- mafdaf.dt[dt]

issue.comments.nobody <- fread('./data/data200_flatcomments_nobody.csv')
issue.comments.nobody[, datetime := fastPOSIXct(datetime, tz='GMT')]
setkey(issue.comments.nobody, project)
setkey(min.max.dates, project)
issue.comments.nobody <- min.max.dates[issue.comments.nobody]
issue.comments.nobody <- issue.comments.nobody[datetime <= max_date_minus]

issue.post.counts.dt <- issue.comments.nobody[, .(total.posts.login=.N), by=.(project, login)]

setkey(issue.post.counts.dt, login, project)
setkey(dt, login, project)

dt <- issue.post.counts.dt[dt]
# Filter out people with no posts and no commits. You have to have at least one in either
# set. The reason there's more is because of false positives - you might see a
# falsely detected @mention in e.g. javascript projects due to the usage of @.
dt <- dt[total.posts.login > 0 | commits > 0]

issue.post.counts.project.dt <- issue.comments.nobody[, .(total.posts.project=.N), by=.(project)]

setkey(issue.post.counts.project.dt, project)
setkey(dt, project)

dt <- issue.post.counts.project.dt[dt]

#####
# Get response value for maf nonreply
load('./data/data200_maf.nonreply.dt.RData')
setnames(maf.nonreply.dt, c('maf', 'mafprime'), c('maf.nonreply.response', 'mafprime.nonreply.response'))
setkey(maf.nonreply.dt, login, project)
setkey(dt, login, project)

dt <- maf.nonreply.dt[dt]
#####

####################
bugs <- fread('./data/data200_bugs_splitmax_6mo.csv')
# If we can't find the buggy github login 
# or track down the bug sha (which means we don't have commits that go that far),
# remove.
bugs <- bugs[bug_github_login != '' & bug_sha_full != '']

bugs.login <- bugs[, .(n.bugcommits.login=length(unique(bug_sha_full))), by=.(bug_github_login, project)]
setnames(bugs.login, 'bug_github_login', 'login')

setkey(dt, login, project)
setkey(bugs.login, login, project)
dt <- bugs.login[dt]

#########
dt[, project.size := sum(poster, committer), by=.(project)]
dt[committer == 1, person.type := 'Committer']
dt[poster == 1, person.type := 'Poster']
dt[committer == 1 & poster == 1, person.type := 'Both']
dt[, person.type := factor(person.type)]
dt[, person.type := relevel(person.type, ref='Poster')]
dt[, n.committers.project := sum(committer), by=.(project)]
dt[, n.posters.project := sum(poster), by=project]
# Get project owners
dt[, project_owner := sub('_.*', '', project)]
dt[, is.project.owner := login == project_owner]
dt[, is.top.committer := commits == max(commits), by=project]
dt[, is.top.committer.or.project.owner := is.project.owner | is.top.committer]
# dt[, sum(is.project.owner) == 0, by=project][V1 == T]

responsiveness <- fread('./data/data200_responsiveness_map_splitmax_6mo.csv')
setkey(responsiveness, login, project)
setkey(dt, login, project)
dt <- responsiveness[dt]
dt[is.na(responsiveness), responsiveness := 0]
# There are a few anomalies that don't make sense
# e.g. responsiveness > 0 when you have no indegree.
# This could be due to deleted accounts (renamed to ghost)
# This happens in very obs, so delete them
dt <- dt[!(nonreply.indegree == 0 & responsiveness > 0)]

# When dealing with data that's still being completed,
# need to filter out projects that we don't have data for
commits.projs <- unique(commits.d$project)
reply.projs <- unique(adjacency.reply.d$project)
nonreply.projs <- unique(adjacency.nonreply.d$project)
mafdaf.projs <- unique(mafdaf.dt$project)
bug.projs <- unique(bugs$project)

considered.projs <- intersect(intersect(intersect(commits.projs, reply.projs),
                                        intersect(nonreply.projs, mafdaf.projs)),
                              bug.projs)
# Detected by detect_incomplete_commits.py
incomplete.projs <- c('facebook_infer',
                      'dotnet_roslyn',
                      'phacility_phabricator',
                      'powerline_fonts',
                      'cocos2d_cocos2d-x',
                      'mattermost_platform',
                      'facebook_xctool',
                      'dotnet_corefx',
                      'facebook_folly',
                      'celery_celery')
# Found to be outliers
ignore.projs <- c('dotnet_coreclr')
# Found to be problems (e.g. bots)
ignore.logins <- c('ghost', 'dotnet-bot')
dt <- dt[project %in% considered.projs & !project %in% incomplete.projs & !project %in% ignore.projs &
           !login %in% ignore.logins]
#####################
# Get max date for each project based on issue comments to get an estimate of user age
issue.maxdates.dt <- issue.comments.nobody[, max(datetime), by=project]
setnames(issue.maxdates.dt, 'V1', 'maxdate')

setkey(issue.maxdates.dt)
setkey(dt, project)

dt <- issue.maxdates.dt[dt]
# In case we're missing anything
dt <- dt[!is.na(maxdate)]

user.data <- fread('data/data200_user_data.csv')
# We have some duplicates due to some issue with coordination within the parallel crawler
user.data <- unique(user.data, by=c('login'))
# Only look at users
user.data <- user.data[type == 'User']
user.data[, created_at := fastPOSIXct(created_at, tz='GMT')]

user.data.dt <- user.data[, .(created_at, followers, following, public_gists, public_repos, login)]
setkey(user.data.dt, login)
setkey(dt, login)

dt <- user.data.dt[dt]
dt[, login.gh.age.days := as.numeric(maxdate - created_at)]


dt[, social.outdegree := reply.outdegree + nonreply.outdegree, by=.(login, project)]
dt[, social.indegree := reply.indegree + nonreply.indegree, by=.(login, project)]

quantile_nozeroes <- function(value.list, top.perc) {
  if(all(value.list == 0)) {
    rep(0, length(value.list))
  } else {
    quantile(sort(value.list[which(value.list > 0)]), probs=c(0, 1 - top.perc))[2]
  }
}

topn_level_nozeroes <- function(value.list, n) {
  value.list.sorted = sort(value.list[which(value.list > 0)], decreasing=T)
  if(all(value.list.sorted == 0)) {
    0L
  }
  else if(length(value.list.sorted) < n) {
    rep(value.list.sorted[length(value.list.sorted)], length(value.list))
  } else {
    value.list.sorted[n]
  }
}

dt[, top10perc.commits.project := quantile_nozeroes(commits, .1), by=project]
dt[, top10.commit.level.project := topn_level_nozeroes(commits, 10), by=project]
dt[, top10perc.nonreply.indegree.project := quantile_nozeroes(nonreply.indegree, .1), by=project]
dt[, top10.nonreply.indegree.level.project := topn_level_nozeroes(nonreply.indegree, 10), by=project]
dt[, is.top10perc.commits.project := commits >= top10perc.commits.project]
dt[, is.top10.commit.level.project := commits >= top10.commit.level.project]
dt[, is.top10perc.nonreply.indegree.project := nonreply.indegree >= top10perc.nonreply.indegree.project]
dt[, is.top10.nonreply.indegree.level.project := nonreply.indegree >= top10.nonreply.indegree.level.project]
dt.na0 <- copy(dt)
# Need to get rid of people that we can't get a date for
# This is fine since we filter for commits > 0 later
# and we expect to be "missing" data since we ran the user data only
# on those with commits > 0
dt.na0 <- dt.na0[!is.na(login.gh.age.days)]
# This can happen if we get commits for someone that are retroactively
# attributed to their github account, which could be created AFTER
# the project is created. This happens extremely rarely, so removing should be safe
dt.na0 <- dt.na0[login.gh.age.days >= 0]
dt.na0[, public_repos := as.numeric(public_repos)]
for(j in names(dt)) {
  data.table::set(dt.na0, which(is.na(dt.na0[[j]])), j, 0)
}
# Look for obvious outliers in nonreply.indegree
boxplot(dt.na0[nonreply.indegree.response > 0]$nonreply.indegree.response)
quantile(dt.na0[nonreply.indegree.response > 0]$nonreply.indegree.response, probs=c(0, 0.99))

dt.na0 <- dt.na0[nonreply.indegree.response <= quantile(dt.na0[nonreply.indegree.response > 0]$nonreply.indegree.response, probs=c(0, 0.99))[2]]

model.data <- copy(dt.na0[commits > 0][,.(nonreply.indegree.response,
                                          dafprime.reply,
                                          dafprime.nonreply,
                                          mafprime.nonreply,
                                          social.outdegree,
                                          n.bugcommits.login, 
                                          dafprime,
                                          commits,
                                          responsiveness,
                                          total.posts.project,
                                          nonreply.indegree)])

#########
g.visibility <- ggplot(model.data, aes(social.outdegree + 0.5, nonreply.indegree.response + 0.5)) +
  geom_point(size=1) +
  scale_x_log10() + scale_y_log10() +
  xlab("Social Outdegree") + ylab("") + theme_minimal() + scale_color_ptol()

g.expertise <- ggplot(model.data, aes(n.bugcommits.login + 0.5, nonreply.indegree.response + 0.5)) +
  geom_point(size=1) +
  scale_x_log10() + scale_y_log10() +
  xlab("Number of Buggy Commits") + ylab("") + theme_minimal() + scale_color_ptol()

g.productivity <- ggplot(model.data, aes(commits + 0.5, nonreply.indegree.response + 0.5)) +
  geom_point(size=1) +
  scale_x_log10() + scale_y_log10() +
  xlab("Commits") + ylab("") + theme_minimal() + scale_color_ptol()

g.responsiveness <- ggplot(model.data, aes(responsiveness + 0.5, nonreply.indegree.response + 0.5)) +
  geom_point(size=1) +
  scale_x_log10() + scale_y_log10() +
  xlab("User Responsiveness") + ylab("") + theme_minimal() + scale_color_ptol()

ggplot(model.data, aes(nonreply.indegree.response)) +
  geom_density() +
  xlab('Future @-mentions') + ylab('Density') + theme_minimal() + scale_color_ptol()

grid.arrange(g.visibility, g.expertise, g.productivity, g.responsiveness, left='Future @-mentions')

cor(log(model.data$social.outdegree + 0.5), log(model.data$nonreply.indegree.response + 0.5))
cor(log(model.data$n.bugcommits.login + 0.5), log(model.data$nonreply.indegree.response + 0.5))
cor(log(model.data$commits + 0.5), log(model.data$nonreply.indegree.response + 0.5))
cor(log(model.data$responsiveness + 0.5), log(model.data$nonreply.indegree.response + 0.5))

#############
mentions.nb.hurdle2 <- hurdle(nonreply.indegree.response ~ 
                             # Visibility
                             dafprime.reply +
                             dafprime.nonreply +
                             mafprime.nonreply +
                             log(social.outdegree + 0.5) +
                             # Expertise
                             log(n.bugcommits.login + 0.5) + 
                             dafprime +
                             is.top.committer.or.project.owner +
                             # Productivity
                             log(commits) +
                             # Responsiveness
                             log(responsiveness + 0.5) +
                             # Measures for controlling outside experience
                             log(login.gh.age.days) +
                             # Controls
                             person.type +
                             log(total.posts.project) +
                             log(nonreply.indegree + 0.5) 
                           |
                             # Visibility
                             dafprime.reply +
                             dafprime.nonreply +
                             log(social.outdegree + 0.5) +
                             # Expertise
                             log(n.bugcommits.login + 0.5) + 
                             dafprime +
                             is.top.committer.or.project.owner +
                             # Productivity
                             log(commits) +
                             # Measures for controlling outside experience
                             log(login.gh.age.days) + I(log(login.gh.age.days)^2) +
                             # Controls
                             person.type +
                             log(total.posts.project),
                           dist='negbin',
                           data=dt.na0[commits > 0])

summary(mentions.nb.hurdle2)
AIC(mentions.nb.hurdle2)
plot_pred_obs(mentions.nb.hurdle2)

count.covar.labels = c('OSSRHO',
                       'OSSKAPPA',
                       'ISSKAPPA',
                       'Log Social Outdegree',
                       'Log Number of Buggy Commits',
                       'DAF',
                       'Top Committer or Project Owner',
                       'Log Commits',
                       'Log User Responsiveness',
                       'Committer Only',
                       'Log Total Posts in Project',
                       'Log Observed @-Mention value',
                       'Intercept')
zero.covar.labels = c('OSSRHO',
                      'OSSKAPPA',
                      'Log Social Outdegree',
                      'Log Number of Buggy Commits',
                      'DAF',
                      'Top Committer or Project Owner',
                      'Log Commits',
                      'Committer Only',
                      'Log Total Posts in Project',
                      'Intercept')
stargazer(mentions.nb.hurdle2, zero.component=F,
          covariate.labels=count.covar.labels,
          dep.var.labels=c('Future @-mentions (6 months later)'))

stargazer(mentions.nb.hurdle2, zero.component=T,
          covariate.labels=zero.covar.labels,
          dep.var.labels=c('Future @-mentions (6 months later)'))

#############

mentions.nb.hurdle2.mse <- mean((predict(mentions.nb.hurdle2, type='response') - mentions.nb.hurdle2$model$nonreply.indegree.response)^2)
mentions.nb.hurdle2.mae <- mean(abs(predict(mentions.nb.hurdle2, type='response') - mentions.nb.hurdle2$model$nonreply.indegree.response))

#####################
# Outlier project
projects.gt200 <- dt.na0[commits > 0 & project != 'FreeCodeCampChina_freecodecamp.cn', .N, by=project][N > 200]

project.count.formula <- nonreply.indegree.response ~ 
  dafprime.reply +
  # dafprime.nonreply +
  log(n.bugcommits.login + 0.5) + 
  mafprime.nonreply +
  log(social.outdegree + 0.5) +
  log(commits + 0.5) +
  log(responsiveness + 0.5) +
  log(login.gh.age.days + 0.5)

project.count.valid.cols = c('dafprime.reply', 'n.bugcommits.login', 'mafprime.nonreply', 'social.outdegree',
                             'commits', 'responsiveness', 'login.gh.age.days')

project.zero.formula <- nonreply.indegree.response ~ 
  dafprime.reply +
  # dafprime.nonreply +
  log(n.bugcommits.login + 0.5) +
  log(social.outdegree + 0.5) +
  log(commits + 0.5) +
  log(login.gh.age.days + 0.5)

project.zero.valid.cols = c('dafprime.reply', 'n.bugcommits.login', 'social.outdegree', 'commits',
                            'login.gh.age.days')

# These projects don't fit (numerically 0 probabilities)
projects.gt200 <- projects.gt200[!project %in% c('google_WebFundamentals', 'elastic_logstash', 'spring-projects_spring-boot')]

lms <- list()
for(c.proj in projects.gt200$project) {
  lms[[c.proj]][['count']] <- glm(project.count.formula,
                                  family='poisson',
                                  data=dt.na0[commits > 0 & project == c.proj & nonreply.indegree.response > 0])
  lms[[c.proj]][['zero']] <- glm(project.zero.formula,
                                 family='binomial',
                                 data=dt.na0[commits > 0 & project == c.proj & nonreply.indegree.response <= 1])
}

# Check if any NA coefficients
count.coefs <- list()
zero.coefs <- list()
for(lm in lms) {
  count.coefs <- append(count.coefs, coef(lm[['count']]))
  zero.coefs <- append(zero.coefs, coef(lm[['zero']]))
}
any(is.na(count.coefs))
any(is.na(zero.coefs))

pseudo.r2s <- c()
lm.coefs.count.dt <- data.table()
lm.coefs.zero.dt <- data.table()
for(lm in lms) {
  pseudo.r2s <- append(pseudo.r2s, 1 - lm[['count']]$deviance / lm[['count']]$null.deviance)
  lm.coefs.count.dt <- rbind(lm.coefs.count.dt, data.table(t(coef(lm[['count']]))))
  
  lm.coefs.zero.dt <- rbind(lm.coefs.zero.dt, data.table(t(coef(lm[['zero']]))))
}
mean.pseudo.r2 <- mean(pseudo.r2s)
# Form an outgroup that consists of columns randomly shuffled and sampled

form_outgroup <- function(formula, fam, d) {
  as.data.table(t(coef(glm(formula, family=fam, data=d))))
}

form_outgroup_data <- function(d, valid.cols) {
  new.d = copy(d[, valid.cols, with=F])
  for(j in names(new.d)) {
    new.d[[j]] <- rtruncnorm(nrow(new.d), a=0, mean=mean(new.d[[j]]), sd=sd(new.d[[j]]))
  }
  new.d <- cbind(new.d, d[, .(nonreply.indegree.response)])
  nsamp = mean(d[, .N, by=project]$N)
  new.d[sample(nrow(new.d), nsamp)]
  
}

outgroup.count.d <- form_outgroup_data(dt.na0[commits > 0 & project %in% projects.gt200$project & nonreply.indegree.response > 0],
                                       project.count.valid.cols)
outgroup.zero.d <- form_outgroup_data(dt.na0[commits > 0 & project %in% projects.gt200$project & nonreply.indegree.response <= 1],
                                      project.zero.valid.cols)

lm.coefs.count.dt <- rbind(lm.coefs.count.dt, form_outgroup(project.count.formula, 'poisson', outgroup.count.d))

row.names(lm.coefs.count.dt) <- c(sub('_', '/', projects.gt200$project), 'OUTGROUP')
lm.coefs.count.dt.projcor.mat <- cor(t(lm.coefs.count.dt))
lm.coefs.count.dt.projcor.zscore.mat <- cor(t(lm.coefs.count.dt[, lapply(.SD, scale, center=T, scale=T)]))
rownames(lm.coefs.count.dt.projcor.mat) <- c(sub('_', '/', projects.gt200$project), 'OUTGROUP')
colnames(lm.coefs.count.dt.projcor.mat) <- c(sub('_', '/', projects.gt200$project), 'OUTGROUP')
rownames(lm.coefs.count.dt.projcor.zscore.mat) <- c(sub('_', '/', projects.gt200$project), 'OUTGROUP')
colnames(lm.coefs.count.dt.projcor.zscore.mat) <- c(sub('_', '/', projects.gt200$project), 'OUTGROUP')

lm.coefs.count.mat <- as.matrix(lm.coefs.count.dt)
rownames(lm.coefs.count.mat) <- c(sub('_', '/', projects.gt200$project), 'OUTGROUP')

lm.coefs.zero.dt <- rbind(lm.coefs.zero.dt, form_outgroup(project.zero.formula, 'binomial', outgroup.zero.d))

row.names(lm.coefs.zero.dt) <- c(sub('_', '/', projects.gt200$project), 'OUTGROUP')
lm.coefs.zero.dt.projcor.mat <- cor(t(lm.coefs.zero.dt))
lm.coefs.zero.dt.projcor.zscore.mat <- cor(t(lm.coefs.zero.dt[, lapply(.SD, scale, center=T, scale=T)]))
rownames(lm.coefs.zero.dt.projcor.mat) <- c(sub('_', '/', projects.gt200$project), 'OUTGROUP')
colnames(lm.coefs.zero.dt.projcor.mat) <- c(sub('_', '/', projects.gt200$project), 'OUTGROUP')
rownames(lm.coefs.zero.dt.projcor.zscore.mat) <- c(sub('_', '/', projects.gt200$project), 'OUTGROUP')
colnames(lm.coefs.zero.dt.projcor.zscore.mat) <- c(sub('_', '/', projects.gt200$project), 'OUTGROUP')

lm.coefs.zero.mat <- as.matrix(lm.coefs.zero.dt)
rownames(lm.coefs.zero.mat) <- c(sub('_', '/', projects.gt200$project), 'OUTGROUP')

# Ignore intercept
gplots::heatmap.2(lm.coefs.count.mat[-nrow(lm.coefs.count.mat), -1],
                  trace='none',
                  margin=c(15, 16),
                  col=colorRampPalette(c('yellow', 'white', 'green')),
                  sepwidth=c(0.01, 0.01),
                  colsep=1:ncol(lm.coefs.count.mat[-nrow(lm.coefs.count.mat), -1]),
                  rowsep=1:nrow(lm.coefs.count.mat[-nrow(lm.coefs.count.mat), -1]),
                  labCol = c(expression('OSS'['p']),
                             'Log Number of Buggy Commits',
                             expression('ISS'['k']),
                             'Log Social Outdegree',
                             'Log Commits', 'Log Responsiveness',
                             'Log User GitHub Age (Days)'),
                  cexCol=1.2)

gplots::heatmap.2(lm.coefs.zero.mat[-nrow(lm.coefs.zero.mat), -1],
                  trace='none',
                  margin=c(15, 15),
                  col=colorRampPalette(c('yellow', 'white', 'green')),
                  sepwidth=c(0.01, 0.01),
                  colsep=1:ncol(lm.coefs.zero.mat[-nrow(lm.coefs.zero.mat), -1]),
                  rowsep=1:nrow(lm.coefs.zero.mat[-nrow(lm.coefs.zero.mat), -1]),
                  labCol=c(expression('OSS'['p']),
                           'Log Number of Buggy Commits',
                           'Log Social Outdegree',
                           'Log Commits',
                           'Log User GitHub Age (Days)'),
                  cexCol=1.2)


##############

preds.mae.count <- list()
for(proj.i in names(lms)) {
  for(proj.j in names(lms)) {
    d = dt.na0[commits > 0 & project == proj.j & nonreply.indegree.response > 0] 
    pred.val = predict(lms[[proj.i]]$count, newdata=d, type='response')
    mae = mean(abs(pred.val - lms[[proj.j]]$count$model$nonreply.indegree.response))
    preds.mae.count[[proj.i]][[proj.j]] = mae
  }
}
preds.mae.count.mean <- matrix(NA, nrow=length(lms), ncol=length(lms),
                               dimnames=list(names(preds.mae.count),
                                             names(preds.mae.count)))
for(proj.i in names(lms)) {
  for(proj.j in names(lms)) {
    preds.mae.count.mean[proj.i, proj.j] <- (preds.mae.count[[proj.i]][[proj.j]] + preds.mae.count[[proj.j]][[proj.i]]) / 2
  }
}

rownames(preds.mae.count.mean) <- sub('_', '/', rownames(preds.mae.count.mean))
colnames(preds.mae.count.mean) <- sub('_', '/', colnames(preds.mae.count.mean))
gplots::heatmap.2(preds.mae.count.mean,
                  trace='none',
                  density='density',
                  margin=c(0,16),
                  col=colorRampPalette(c('red', 'yellow')),
                  sepwidth=c(0.001, 0.001),
                  colsep=1:ncol(preds.mae.count.mean),
                  rowsep=1:nrow(preds.mae.count.mean))


preds.mae.zero <- list()
for(proj.i in names(lms)) {
  for(proj.j in names(lms)) {
    d = dt.na0[commits > 0 & project == proj.j & nonreply.indegree.response <= 1] 
    pred.val = predict(lms[[proj.i]]$zero, newdata=d, type='response')
    roc.obj = roc(pred.val, as.factor(lms[[proj.j]]$zero$model$nonreply.indegree.response))
    auc.val = auc(roc.obj)
    preds.mae.zero[[proj.i]][[proj.j]] = auc.val
  }
}
preds.mae.zero.mean <- matrix(NA, nrow=length(lms), ncol=length(lms),
                              dimnames=list(names(preds.mae.zero),
                                            names(preds.mae.zero)))
for(proj.i in names(lms)) {
  for(proj.j in names(lms)) {
    preds.mae.zero.mean[proj.i, proj.j] <- (preds.mae.zero[[proj.i]][[proj.j]] + preds.mae.zero[[proj.j]][[proj.i]]) / 2
  }
}

rownames(preds.mae.zero.mean) <- sub('_', '/', rownames(preds.mae.zero.mean))
colnames(preds.mae.zero.mean) <- sub('_', '/', colnames(preds.mae.zero.mean))
gplots::heatmap.2(preds.mae.zero.mean,
                  trace='none',
                  density='density',
                  margin=c(0,16),
                  col=colorRampPalette(c('yellow', 'red')),
                  sepwidth=c(0.001, 0.001),
                  colsep=1:ncol(preds.mae.zero.mean),
                  rowsep=1:nrow(preds.mae.zero.mean))

