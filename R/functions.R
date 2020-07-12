# GP utility funcs
getDataJoined <- function(dataDir = "data/", join = "looser"){
  require(data.table)
  require(lubridate)

  dataCS <- getDataCS()
  dataPB <- getDataPB()

  # Inner join data sets based on email, isbn, and period (strict). Probably don't wanna do.
  suffix <- c(".cs", ".pb")
  setkey(dataCS, email, isbn, period)
  dataJoinedStrict <- merge(dataCS, dataPB,
                              by = c("email", "isbn", "period"),
                              suffixes = suffix)
  setcolorder(dataJoinedStrict, c("cs.id", "pb.id", "email", "period", "crsesec", "course"))
  dataJoinedStrict[,cs.count := .N, by = cs.id]
  dataJoinedStrict[,pb.count := .N, by = pb.id]
  dataJoinedStrict <- dataJoinedStrict[order(pb.count, email, cs.count, cs.id)]

  # Inner join  only on email and period (loose). Probably wanna do.
  dataJoinedLoose <- merge(dataCS, dataPB, by = c("email", "period"),
                             suffixes = suffix)[isbn.cs == isbn.pb,
                                                isbn.match := T][
                                                isbn.cs != isbn.pb,
                                                isbn.match := F][,
                                                email.count := .N, by = email
                                                                 ][,
                                                pb.count := .N, by = pb.id
                                                                   ][,
                                                cs.count := .N, by = cs.id
                                                                 ]

  # Inner join  only on email and period (looser)
  dataJoinedLooser <- merge(dataCS, dataPB, by = c("email"),
                             suffixes = suffix)[isbn.cs == isbn.pb,
                                                isbn.match := T][
                                                isbn.cs != isbn.pb,
                                                isbn.match := F][,
                                                email.count := .N, by = email
                                                                 ][,
                                                pb.count := .N, by = pb.id
                                                                   ][,
                                                cs.count := .N, by = cs.id
                                                                 ]
  # An even looser join allows for clean sample observations to join to price
  # beliefs observations as long as period.cs <= period.pb
  dataJoinedLooser  <- dataJoinedLooser[(period.cs == "fall12" & period.pb %in% c("fall12", "spring13"))
                                          | (period.cs == "spring13" & period.pb == "spring13")]


  setcolorder(dataJoinedLoose, c("cs.id", "pb.id", "email", "period", "crsesec", "course", "isbn.match", "isbn.cs", "isbn.pb", "major.pb", "major.cs", "freshdum.pb", "freshdum.cs"))
  setcolorder(dataJoinedLooser, c("cs.id", "pb.id", "email", "period.cs", "period.pb", "crsesec", "course", "isbn.match", "isbn.cs", "isbn.pb", "major.pb", "major.cs", "freshdum.pb", "freshdum.cs"))

  returnData  <- switch(join, "strict" = dataJoinedStrict, "loose" = dataJoinedLoose, "looser" = dataJoinedLooser)
  return(returnData)
}

getDataCS  <- function(dataDir = "data/", handleMultiClasses = T){
  require(data.table)
  dataCSRaw <- fread(paste0(dataDir, 'cleansample.csv'))

  # ID dataset
  dataCSRaw[,cs.id := .I]

  # Remove blank emails
  dataCS <- dataCSRaw[email != ""]

  # Create a period column for CS
  dataCS[fall12dum == 1, period := "fall12"][
    spring13dum == 1, period := "spring13"
  ]

  # CS: Get fracs of class (crsesec) with offered, and fieldcourse
  dataCS[, sec_fc_count := sum(fieldcourse), by = .(crsesec, period)][,
            sec_of_count := sum(offered), by = .(crsesec, period)][,
            ':=' (sec_fc_frac = sec_fc_count / .N, sec_of_frac = sec_of_count / .N), by = .(crsesec, period)]

  # Added to account for 3/6/2020 drawing in C48.
  if (handleMultiClasses == T){
    dataCS[, emailPeriodCount := .N, by=c("email", "period")]
    dataCS[, offeredSum := sum(offered), by=c("email", "period")]
    dataCS[, fieldCourseSum := sum(fieldcourse), by=c("email", "period")]
  }

  # Remove respondents with more than 10 semesters of college
  dataCS  <- dataCS[semesters <= 10]

  return(dataCS)
}

getDataPB  <- function(dataDir = "data/"){
  require(data.table)
  require(lubridate)

  dataPB <- fread(paste0(dataDir, 'pricebeliefs.csv'))

  # ID dataset
  dataPB[,pb.id := .I]

  # Put month and year into PB create a "period" column (fall12 or spring13)
  pbDate <- "%m/%d/%Y %H:%M"
  dataPB[, ':=' (startdate = as.POSIXct(startdate, format = pbDate),
                  enddate = as.POSIXct(enddate, format = pbDate))][,
    ':=' (startmonth = month(startdate), startyear = year(startdate),
          endmonth = month(enddate), endyear = year(enddate))][,
    datedelta := enddate - startdate][
    startyear == 2013 & startmonth == 4, period := "spring13"][
    startyear == 2012 & startmonth %in% c(11, 12), period := "fall12"
    ]

  # Create mu and sigma columns
  dataPB[,c("mu", "sigma", "mu_lnorm", "sigma_lnorm") := calcPBMuSigma(lb, newlb, ub, newub, newexp), by = pb.id]
  dataPB[,c("normmu", "normsigma", "normmu_lnorm", "normsigma_lnorm") := calcPBMuSigma(lb, newlb, ub, newub, newexpratio), by = pb.id]

  # Remove blank emails
  dataPB  <- dataPB[email != ""]


  # Remove respondents with more than 10 semesters of college
  dataPB  <- dataPB[semesters <= 10]

  # Remove respondents who give expectation of new price < 10% or > 150% of the new bookstore price.
  dataPB <- dataPB[newexpratio > .1 & newexpratio < 1.5]

  return(dataPB)
}

calcPBMuSigma <- function(lb, newlb, ub, newub, newexp){
  require(purrr)
  require(magrittr)

  sigmaNum <- (log(ub) - log(lb))
  sigmaDenom <- qnorm(1 - (newub / 100)) - qnorm(newlb / 100)

  sigma <- sigmaNum/sigmaDenom
  mu <- log(newexp) + log(lb) - sigma * qnorm(newlb / 100)
  mu_lnorm <- exp(mu + .5 * (sigma ^ 2))
  sigma_lnorm <- sqrt((exp(sigma ^ 2) - 1) * exp(2 * mu + (sigma ^ 2)))
  return(list(mu, sigma, mu_lnorm, sigma_lnorm))
}

# Example use. When mu and sigma calculations are finished use this style of func
# dpb[,c("beef", "pork") := tf(lb, lbnewp), by = pb.id]
tf <- function(lb, lbnewp){
  return(list("beef" = lb * lbnewp,
              "pork" = "it's pork time"
              ))
}

getData  <- function(dataType){
  rv <- switch(dataType, "cs" = getDataCS(),
               "pb" = getDataPB(),
               "strict" = getDataJoined(join = dataType),
               "loose" = getDataJoined(join = dataType),
               "looser" = getDataJoined(join = dataType))
  (rv)
}

## http://www.cookbook-r.com/Manipulating_data/Summarizing_data/
## Summarizes data.
## Gives count, mean, standard deviation, standard error of the mean, and confidence
## interval (default 95%).
##   data: a data frame.
##   measurevar: the name of a column that contains the variable to be summariezed
##   groupvars: a vector containing names of columns that contain grouping variables
##   na.rm: a boolean that indicates whether to ignore NA's
##   conf.interval: the percent range of the confidence interval (default is 95%)
summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE, conf.interval=.95) {
    require(doBy)
    require(data.table)

    # New version of length which can handle NA's: if na.rm==T, don't count them
    length2 <- function (x, na.rm=FALSE) {
        if (na.rm) sum(!is.na(x))
        else       length(x)
    }

    # Collapse the data
    formula <- as.formula(paste(measurevar, paste(groupvars, collapse=" + "), sep=" ~ "))
    datac <- summaryBy(formula, data=data, FUN=c(length2,mean,sd), na.rm=na.rm)

    # Rename columns
    names(datac)[ names(datac) == paste(measurevar, ".mean",    sep="") ] <- measurevar
    names(datac)[ names(datac) == paste(measurevar, ".sd",      sep="") ] <- "sd"
    names(datac)[ names(datac) == paste(measurevar, ".length2", sep="") ] <- "N"

    datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean

    # Confidence interval multiplier for standard error
    # Calculate t-statistic for confidence interval:
    # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
    ciMult <- qt(conf.interval/2 + .5, datac$N-1)
    datac$ci <- datac$se * ciMult

    return(as.data.table(datac))
}

# Funcs for drawing made in room C35 2/24/2020
# (1) binary treat/no treat
createTreatmentPlot <- function(dataType, data = NULL){
  require(ggplot2)
  require(magrittr)
  if (is.null(data)){
    data <- getData(dataType)
  }

  # Treatment and control column, then a compliant/non-compliant treatment grouping
  data[offered != 1, treatOrControl := "control"][
    offered == 1, treatOrControl := "treat"][
    offered == 1 & fieldcourse == 1, treatComplyOrNot := "comply"][
    offered == 1 & fieldcourse != 1, treatComplyOrNot := "nocomply"
    ]

  # Help our labels look nice
  data$treatComplyOrNot  <- factor(data$treatComplyOrNot, levels = c("nocomply", "comply"))

  (ggplot(data, aes(treatOrControl, fill = treatComplyOrNot)) +
    geom_bar() +
    # geom_text(stat='count', aes(label=..count..), vjust=-1) +
    geom_text(aes(label = scales::percent((..count..)/sum(..count..))),
             stat="count", vjust = 1.1) +
    labs(title = "Treat/No Treat breakdown", subtitle = paste0("W/ treatment compliance. N = ", nrow(data))) +
    theme_minimal()
  )
}

# (2a) Conditional on treat: # classes in treatment
createTreatmentPlotClasses  <- function(dataType, data = NULL){
  require(ggplot2)
  if (is.null(data)){
    data <- getData(dataType)
  }

  dataUniqueClass  <- switch(dataType,
    "cs" = unique(data[,.(crsesec, period, sec_fc_count, sec_fc_frac, sec_of_count, sec_of_frac)]),
    "loose" = unique(data[,.(crsesec, period, sec_fc_count, sec_fc_frac, sec_of_count, sec_of_frac)]),
    "looser" = unique(data[,.(crsesec, period.cs, sec_fc_count, sec_fc_frac, sec_of_count, sec_of_frac)])
    )

  dataUniqueClass[sec_of_frac == 0, treatOrControl := "control"][
                  sec_of_frac == 1, treatOrControl := "treat"][
                  sec_of_frac == 1 & sec_fc_frac == 1, treatComplyOrNot := "comply"][
                  sec_of_frac == 1 & sec_fc_frac == 0, treatComplyOrNot := "nocomply"]
  # Help our labels look nice
  dataUniqueClass$treatComplyOrNot  <- factor(dataUniqueClass$treatComplyOrNot, levels = c("nocomply", "comply"))

  (ggplot(dataUniqueClass, aes(treatOrControl, fill = treatComplyOrNot)) +
   geom_bar() +
    # geom_text(stat='count', aes(label=..count..), vjust=-1) +
    geom_text(aes(label = scales::percent((..count..)/sum(..count..))),
              stat="count", vjust = 1.1) +
   labs(title = "Treat/No Treat breakdown by class", subtitle = paste0("W/ treatment compliance. N (classes) = ", nrow(dataUniqueClass))) +
   theme_minimal()
  )
}

# (2b) Compare online price, bookstore price, their difference, proportion.
createTreatmentPlotClassesPriceDiff  <- function(dataType, data = NULL){
  require(ggplot2)
  require(magrittr)
  if (is.null(data)){
    data <- getData(dataType)
  }

  # Include, 'uncnewp', 'uncusedp', 'onlinenewp', 'onlineusedp'
  dataUniqueClass  <- switch(dataType,
    "cs" = unique(data[,.(crsesec, period, sec_fc_count, sec_fc_frac, sec_of_count, sec_of_frac, uncnewp, uncusedp, onlinenewp, onlineusedp)]),
    "loose" = unique(data[,.(crsesec, period, sec_fc_count, sec_fc_frac, sec_of_count, sec_of_frac, uncnewp, uncusedp, onlinenewp, onlineusedp)]),
    "looser" = unique(data[,.(crsesec, period.cs, sec_fc_count, sec_fc_frac, sec_of_count, sec_of_frac, uncnewp, uncusedp, onlinenewp, onlineusedp)])
  )

  dataUniqueClass[sec_of_frac == 0, treatOrControl := "control"][
                  sec_of_frac == 1, treatOrControl := "treat"][
                  sec_of_frac == 1 & sec_fc_frac == 1, treatComplyOrNot := "comply"][
                  sec_of_frac == 1 & sec_fc_frac == 0, treatComplyOrNot := "nocomply"]
  N  <- nrow(dataUniqueClass)


  uncnewpSummary <- summarySE(dataUniqueClass, measurevar="uncnewp", groupvars=c("treatOrControl", "treatComplyOrNot"))[, price.where := "uncnewp"] %>% setnames("uncnewp", "price")
  uncusedpSummary <- summarySE(dataUniqueClass, measurevar="uncusedp", groupvars=c("treatOrControl", "treatComplyOrNot"), na.rm = TRUE)[, price.where := "uncusedp"] %>% setnames("uncusedp", "price")
  onlinenewpSummary <- summarySE(dataUniqueClass, measurevar="onlinenewp", groupvars=c("treatOrControl", "treatComplyOrNot"))[, price.where := "onlinenewp"] %>% setnames("onlinenewp", "price")
  onlineusedpSummary <- summarySE(dataUniqueClass, measurevar="onlineusedp", groupvars=c("treatOrControl", "treatComplyOrNot"))[, price.where := "onlineusedp"] %>% setnames("onlineusedp", "price")

  priceSummary  <- rbind(uncnewpSummary, uncusedpSummary, onlinenewpSummary, onlineusedpSummary)


  (ggplot(priceSummary, aes(x=price.where, y=price)) +
    geom_bar(position=position_dodge(), stat="identity") +
    geom_errorbar(aes(ymin=price-ci, ymax=price+ci),
                  width=.2,                    # Width of the error bars
                  position=position_dodge(.9)) +
    geom_text(aes(label=round(price), vjust = -.5, hjust = -.8)) +
    labs(title = "Class Textbook price means by treatment group and textbook source/condition.", subtitle = paste0("95% CI | N = ", N, "")) +
    facet_wrap(treatOrControl ~ treatComplyOrNot) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
  )

}

# 3 Treat/Control in major class
createTreatmentPlotMajorClass  <- function(dataType, data = NULL){
  require(ggplot2)
  if (is.null(data)){
    data <- getData(dataType)
  }

  # Treatment and control column, then a compliant/non-compliant treatment grouping
  data[offered != 1, treatOrControl := "control"][
    offered == 1, treatOrControl := "treat"][
    offered == 1 & fieldcourse == 1, treatComplyOrNot := "comply"][
    offered == 1 & fieldcourse != 1, treatComplyOrNot := "nocomply"
    ]

  # Help labels look nice
  data$treatComplyOrNot  <- factor(data$treatComplyOrNot, levels = c("nocomply", "comply"))


  # Make major.cs column plotting friendly
  dataUniqueCS  <- switch(dataType,
    "cs" = data[major == 0, major.plot := "(0) Course not in major"][major == 1, major.plot := "(1) Course in major"],
    data[major.cs == 0, major.plot := "(0) Course not in major"][major.cs == 1, major.plot := "(1) Course in major"]
    )


  # There are duplicate clean sample (CS) IDs here but unique price belief ids. We want unique CS ids only since the "major" column for CS is binary
  # dataUniqueCS  <- unique(data[,.(cs.id, email, offered, treatOrControl, treatComplyOrNot, major.plot)])

  (ggplot(dataUniqueCS, aes(treatOrControl, fill = treatComplyOrNot)) +
    geom_bar() +
    # geom_text(stat='count', aes(label=..count..), vjust=-1) +
    geom_text(aes(label = scales::percent((..count..)/sum(..count..))),
        stat = "count", vjust = 1.1) +
    labs(title = "Treat/No Treat breakdown by whether Clean Sample Class is in Major",
         subtitle = paste0("W/ treatment compliance. N = ", nrow(dataUniqueCS))) +
    facet_wrap(. ~ major.plot) +
    theme_minimal())
}

# 4 Inexperience (freshman) vs experienced
createTreatmentPlotFreshman  <- function(dataType, data = NULL){
  require(ggplot2)
  if (is.null(data)){
    data <- getData(dataType)
  }

  # Treatment and control column, then a compliant/non-compliant treatment grouping
  data[offered != 1, treatOrControl := "control"][ offered == 1, treatOrControl := "treat"][
    offered == 1 & fieldcourse == 1, treatComplyOrNot := "comply"][
    offered == 1 & fieldcourse != 1, treatComplyOrNot := "nocomply"
    ]

  # Make freshdum column plotting friendly
  data  <- switch(dataType,
    "cs" = data[freshdum == 0, fresh.plot := "(0) 2nd, 3rd, 4th year"][freshdum == 1, fresh.plot := "(1) Freshman"],
    data[freshdum.pb == 0, fresh.plot := "(0) 2nd, 3rd, 4th year"][freshdum.pb == 1, fresh.plot := "(1) Freshman"]
    )

  # Helps our labels look nice
  data$treatComplyOrNot  <- factor(data$treatComplyOrNot, levels = c("nocomply", "comply"))

  (ggplot(data, aes(treatOrControl, fill = treatComplyOrNot)) +
    geom_bar() +
    # geom_text(stat='count', aes(label=..count..), vjust=-1) +
    geom_text(aes(label = scales::percent((..count..)/sum(..count..))),
              stat="count", vjust = -1) +
    labs(title = "Treat/No Treat breakdown by Freshman status",
         subtitle = paste0("W/ treatment compliance. N = ", nrow(data))) +
    facet_wrap(. ~ fresh.plot) +
    theme_minimal())
}

# Funcs for drawing made in room C48 3/6/2020

# This breakdown all the different levels we care about for the loose joined
# data
createLooseJoinDrilldownPlot <- function(){
  require(ggplot2)
  require(data.table)

  djLoose <- getDataJoined(join="loose")

  djLoose[fieldCourseSum >= 1, treatOrControl := "treat"]
  djLoose[fieldCourseSum == 0, treatOrControl := "control"]
  djLoose[freshdum.cs == 1, freshmanStatus := "freshman"]
  djLoose[freshdum.cs == 0, freshmanStatus := "sophOrHigher"]

  # Helps our labels look nice
  djLoose$freshmanStatus  <- factor(djLoose$freshmanStatus, levels = c("sophOrHigher", "freshman"))

  (ggplot(djLoose, aes(treatOrControl, fill = freshmanStatus)) +
    geom_bar() +
    geom_text(aes(label = scales::percent((..count..)/sum(..count..))),
                  stat="count", vjust = 1.1) +
    facet_wrap(. ~ period) +
    labs(title = "Treatment/Control breakdown by Freshman Status and Semester",
         subtitle = paste0("N = ", nrow(djLoose))) +
    theme_minimal())
}

# Test updates in beliefs from treatment from fall12 to spring13
createBeliefUpdateGroups <- function(){
  require(data.table)
  returnList = list()

  djLoose <- getDataJoined(join="loose")

  # Initial control group
  initial  <- djLoose[period == "fall12" & fieldCourseSum == 0, ':=' (initial = 1)][initial == 1]
  emails <- unique(initial$email)
  djLoose[period == "spring13" & email %chin% emails, after := 1]
  afterTreatment <- djLoose[after == 1 & fieldCourseSum >= 1]
  afterControl <- djLoose[after == 1 & fieldCourseSum == 0]

  returnList$initial <- initial
  returnList$afterTreatment <- afterTreatment
  returnList$afterControl <- afterControl
  returnList$after <- djLoose[after == 1]
  returnList$fall12 <- djLoose[period == "fall12"]
  returnList$spring13 <- djLoose[period == "spring13"]

  return(returnList)
}

# From meeting 3/12/2020 pb only freshman/upperclassman by semester breakdown
createPBOnlyBreakdownPlot <- function(){
  require(data.table)
  require(ggplot2)

  dataPB <- getDataPB()
  dj <- getDataJoined(join="loose")

  dataPBOnly <- dataPB[!(pb.id %in% dj$pb.id)]
  # print(nrow(dataPB))
  # print(nrow(dataPBOnly))

  N <- nrow(dataPBOnly)
  dataPBOnly[freshdum == 1, freshmanStatus := "freshman"]
  dataPBOnly[freshdum == 0, freshmanStatus := "sophOrHigher"]
  dataPBOnly$freshmanStatus  <- factor(dataPBOnly$freshmanStatus, levels = c("sophOrHigher", "freshman"))

  (ggplot(dataPBOnly, aes(freshmanStatus)) +
   geom_bar() +
   facet_wrap(. ~ period) +
   labs(title="Class Status by Semester",
        subtitle=paste0("Price Beliefs data not overlapping with Clean Sample data | N = ", nrow(dataPBOnly))) +
    geom_text(aes(label = ..count..),
             stat="count", vjust = 1.1) +
   theme_minimal()
  )
}

getColNames  <- function(pattern, table){
  return(table[,names(table)[grepl(pattern, names(table))],with=F])
}

getTTestGroup <- function(freshVal, treatVal, season, nonComply = T){
  require(data.table)

  dataCS <- getDataCS()

  group <- dataCS[freshdum == freshVal & treatme == treatVal,]

  # season can be "fall"(12) or "both" (fall12 and spring13)
  if (season == "fall"){
    group <- group[fall12dum == 1]
  }

  # Do we want to leave the non-compliant treatment group in the control?
  if (nonComply == F & treatVal == 0){
    group <- group[offered == 0]
  }

  return(group)
}

calc2STTest <- function(sample1Index, sample2Index, index1Name, index2Name, groupList, outcomeVar, conf = .95){
  require(dplyr)
  sample1 <- groupList[[sample1Index]][[outcomeVar]]
  sample2 <- groupList[[sample2Index]][[outcomeVar]]

  results <- t.test(sample1, sample2, conf.level = conf) %>%
    broom::tidy() %>%
    cbind(data.frame(group_compared = glue::glue("{index1Name} - {index2Name}"),
                     outcomeVar = outcomeVar
                     )) %>%
    dplyr::mutate(sig = case_when(p.value < (1 - conf) ~ "sig",
                                  TRUE ~ "nonsig")

    )

  return(results)
}

calc1STTest <- function(sample1Index, index1Name, groupList, outcomeVar){
  require(dplyr)
  sample1 <- groupList[[sample1Index]][[outcomeVar]]

  results <- t.test(sample1,
                    mu = .5 # Binary variable
                    ) %>%
    broom::tidy() %>%
    cbind(data.frame(group = index1Name,
                     outcomeVar = outcomeVar
                     )) %>%
    dplyr::mutate(sig = case_when(p.value < .05 ~ "sig",
                                  TRUE ~ "nonsig")

    )

  return(results)
}

getNormMuGroup <- function(freshVal, treatVal, season, dataType, nonComply = T){
  require(data.table)

  data <- switch(dataType,
                 PB = getDataPB(),
                 joined = getDataJoined(join = "loose")
                 )

  # Opt with price beliefs versions of who is a freshman
  if (dataType == "PB"){
    group <- data[freshdum == freshVal]
  } else if (dataType == "joined"){
    group <- data[freshdum.pb == freshVal]
    group <- data[treatme == treatVal]

    # Do we want to leave the non-compliant treatment group in the control?
    if (nonComply == F & treatVal == 0){
      group <- group[offered == 0]
    }
  }

  # season can be "fall"(12) or "both" (fall12 and spring13)
  if (season == "fall"){
    group <- group[period == "fall12"]
  }

  return(group)
}

# Function for doing t-tests with google doc specifications
# https://docs.google.com/spreadsheets/d/1JS-5_iv-WrfzifFsr_1CW5a12G5DfMAGYSs6r4ACfxs/edit#gid=0
calcSpec2STTest <- function(sample1Name, sample2Name, masterDf, outcomeVar){
  require(dplyr)
  sample1vec <- masterDf %>%
    filter(name == sample1Name) %>%
    pull(outcomeVar)
  sample2vec <- masterDf %>%
    filter(name == sample2Name) %>%
    pull(outcomeVar)

  results <- t.test(sample1vec, sample2vec) %>%
    broom::tidy() %>%
    cbind(data.frame(group_compared = glue::glue("{sample1Name} - {sample2Name}"),
                     outcomeVar = outcomeVar
                     )) %>%
    dplyr::mutate(sig = case_when(p.value < .05 ~ "sig",
                                  TRUE ~ "nonsig")

    )

  return(results)
}

getLNormDraws <- function(name, draws, spec, masterDf, mode = "write", startSeed = 26){
  require(dplyr)
  require(glue)
  require(readr)
  require(purrr)

  possibleFp  <- glue("data/lnorm_draws/spec_{spec}_{name}_{draws}.csv")
  if (file.exists(possibleFp)){
    if (mode == "write"){
      return()
    } else {
      return(read_csv(possibleFp))
    }
  }

  filterDf <- masterDf %>%
    dplyr::filter(name == !!name & spec == !!spec) %>%
    mutate(seed = startSeed:(nrow(.) + startSeed - 1)) %>%
    select(normmu_lnorm, normsigma_lnorm, seed)

  simluateLnorm <- function(normmu_lnorm, normsigma_lnorm, seed){
    set.seed(seed)
    obs <- rlnorm(draws, normmu_lnorm, normsigma_lnorm)
    rval <- list(obs = obs, normmu_lnorm = normmu_lnorm, normsigma_lnorm = normsigma_lnorm)
    return(rval)
  }

  returnDf  <- filterDf %>%
    pmap_dfr(.f = simluateLnorm)
  returnLen <- nrow(returnDf)
  filterLen <- nrow(filterDf)
  message(glue("Spec {spec} {name}, {draws} draws | {returnLen} obs for {filterLen} distributions"))

  if (mode == "write"){
    write_csv(returnDf, possibleFp)
  } else {
    return(returnDf)
  }
}

# Function to create all the required draws (save space)
calcAndCacheAllDraws <- function(masterDf){
  require(tidyr)
  require(purrr)
  require(dplyr)

  paramDf <- data.frame(name = c("Experienced Control", "Experienced Treated",
                                 "Inexperienced Control", "Inexperienced Treated")) %>%
    crossing(spec = c(1, 2), draws = c(100, 1000)) #, 10000))

  paramDf %>%
    pwalk(.f = getLNormDraws, masterDf = masterDf)
  list.files("data/lnorm_draws/")
}

compareDistributions <- function(sample1Name, sample2Name, masterDf, mode,
                                 spec, vecSize = 1000, draws = 100, outcomeVar = "normmu_lnorm",
                                 conf = .95, startSeed = 26){
  require(dplyr)
  require(kSamples)
  require(purrr)

  if (mode == "means"){
    sample1vec <- masterDf %>%
      filter(name == sample1Name, spec == !!spec) %>%
      pull(outcomeVar) %>%
      sample(size = vecSize, replace = T)
    sample2vec <- masterDf %>%
      filter(name == sample2Name, spec == !!spec) %>%
      pull(outcomeVar) %>%
      sample(size = vecSize, replace = T)

    results <- ad.test(sample1vec, sample2vec)
    rval <- results$ad[1,] %>% # Results from continuous population
      broom::tidy() %>%
      tidyr::pivot_wider(names_from = names, values_from = x) %>%
      rename(asympt.p.value = ` asympt. P-value`) %>%
      cbind(data.frame(group_compared = glue::glue("{sample1Name} ~ {sample2Name}"),
                       outcomeVar = outcomeVar,
                       spec = spec,
                       sample1Name = sample1Name,
                       sample1N = results$ns[1],
                       sample2Name = sample2Name,
                       sample2N = results$ns[2]
                       )) %>%
      mutate(sig = case_when(asympt.p.value < 1 - conf ~ "sig",
                             TRUE ~ "nonsig"))
  } else if (mode == "draws") {
    message(glue::glue("Comparing {sample1Name} ~ {sample2Name} | {draws} draws . . ."))

    sample1vec <- getLNormDraws(sample1Name, draws, spec, masterDf = NULL, mode = "read") %>%
      filter(!(is.infinite(obs) | is.nan(obs) | is.na(obs))) %>%
      pull(obs)
    sample2vec <- getLNormDraws(sample2Name, draws, spec, masterDf = NULL, mode = "read") %>%
      filter(!(is.infinite(obs) | is.nan(obs) | is.na(obs))) %>%
      pull(obs)

    results <- ad.test(sample1vec, sample2vec)
    rval <- results$ad[1,] %>% # Results from continuous population
      broom::tidy() %>%
      tidyr::pivot_wider(names_from = names, values_from = x) %>%
      rename(asympt.p.value = ` asympt. P-value`) %>%
      cbind(data.frame(group_compared = glue::glue("{sample1Name} ~ {sample2Name}"),
                       outcomeVar = "obs",
                       draws = draws,
                       spec = spec,
                       sample1Name = sample1Name,
                       sample1N = results$ns[1],
                       sample2Name = sample2Name,
                       sample2N = results$ns[2]
                       )) %>%
      mutate(sig = case_when(asympt.p.value < 1 - conf ~ "sig",
                             TRUE ~ "nonsig"))
  }
  return(rval)
}


# data.joined.looser  <- getDataJoined()
# data.joined.loose  <- getDataJoined(join = "loose")
# data.joined.strict  <- getDataJoined(join = "strict")
# data.cs <- getDataCS()# fread('data/cleansample.csv')
# data.pb <- getDataPB()# fread('data/pricebeliefs.csv')
# createTreatmentPlot()
# createTreatmentPlotClasses()
# createTreatmentPlotClassesPriceDiff()
# createTreatmentPlotMajorClass()
# createTreatmentPlotFreshman()
