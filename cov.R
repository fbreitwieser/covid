
library(magrittr)
library(tidyverse)
library(cowplot)
theme_set(theme_cowplot())

get_data <- function(type) {
  message(type)
  url <- sprintf("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_%s_global.csv", type)
  data <- read.csv(url, check.names = F, stringsAsFactors = F)

  covid <- reshape2::melt(data, id.vars=c("Province/State", "Country/Region", "Lat", "Long"), variable.name = "Date", value.name = "Count")
  covid$Country <- as.character(covid$`Country/Region`)
  covid$Country[covid$Country == "Korea, South"] <- "South Korea"
  covid$Country[covid$Country == "United Kingdom"] <- "UK"

  covid$Date <- as.Date(covid$Date, "%m/%d/%y")
  covid$Type <- type
  covid <- plyr::ddply(covid, c("Country", "Date", "Type"), function(x) {
    c(Count=sum(x$Count))
  })
  plyr::ddply(covid, c("Country"), function(x) {
    if (all(x$Count == x$Count[1])) {
      return(x[1, , drop=FALSE])
    }
    x <- subset(x, !is.na(Count))
    while(nrow(x) > 1 && (x[x$Date == max(x$Date), "Count"] == x[x$Date == max(x$Date) -1, "Count"])) {
      x <- x[x$Date != max(x$Date),]
    }
    x$RawCount <- x$Count
    x$CountF <- x$Count
    for (i in seq(from=2, to=nrow(x) - 1)) {
      x$CountF[i] <- 0.25 * x$Count[i-1] + 0.5*x$Count[i] + 0.25 * x$Count[i+1]
    }
    x$Count <- x$CountF
    x$CountF <- NULL
    x$MaxCount <- max(x$Count)
    x
  })
}

  dat <- rbind(get_data("confirmed"),
               get_data("deaths"),
               get_data("recovered"))

  hr <- function(x) {
    ifelse(x > 1000, sprintf("%.1fk", x/1000), as.character(x))
  }
  dat <- plyr::ddply(dat, "Country", function(x) {
    x$MaxCountC <- max(x$Count)
    x
  })


  ggplot(subset(dat, Country %in% dat$Country[dat$MaxCount > 5000 &
                                                !Country %in% c("China", "Cruise Ship") &
                                                dat$Type == "confirmed"] &
                  Date > Sys.Date() - 31)) +
    geom_line(aes(x=Date, y=Count, color = Type)) +
    facet_wrap(~Country, scales = "free_y")

  dat$Cat <- "Other"
  dat$Cat[dat$MaxCountC >= 5000] <- "over 5k cases"
  dat$Cat[dat$MaxCountC >= 1000 & dat$MaxCountC < 5000] <- "1k to 5k cases"
  dat$Cat[dat$MaxCountC >= 500 & dat$MaxCountC < 1000] <- "500 to 1k cases"
  dat$Cat[dat$Country == "Cruise Ship"] <- "Other"
  

  dat$Cat <- factor(dat$Cat, c("500 to 1k cases", "1k to 5k cases", "over 5k cases", "Other"))
  

  summary <- plyr::ddply(subset(dat, MaxCountC > 100), c("Country", "Cat"), function(y) {
    x <- y[y$Type == "confirmed", ]
    last_date = as.Date(max(x$Date))
    while (x[x$Date == last_date,"Count"] == x[x$Date == last_date - 1,"Count"]) {
      last_date = last_date - 1
    }
    sel = x$Date == last_date
    sel1 = x$Date == last_date - 1
    sel2 = x$Date == last_date - 2
    res <- data.frame(
      Date = last_date,
      Count = x[sel, "Count"],
      CountsB4 = x[sel1, "Count"],
      GrowthRate = x[sel, "Count"] / x[sel1, "Count"],
      GrowthRateB4 = x[sel1, "Count"] / x[sel2, "Count"])

    res$Summary <- sprintf("%s-%s †%s",
                           hr(max(y$Count[y$Type =="confirmed"])),
                           hr(max(y$Count[y$Type =="recovered"])),
                           hr(max(y$Count[y$Type =="deaths"])))

    res[['GrowthRateIsUp']] =  res[['GrowthRate']] > res[['GrowthRateB4']]
    res
  })

  death_summary <- plyr::ddply(subset(dat, MaxCountC > 100), c("Country", "Cat"), function(y) {
    x <- y[y$Type == "deaths", ]
    last_date = as.Date(max(x$Date))
    if (nrow(x) <= 1) {
      return(NULL)
    }
    while (x[x$Date == last_date,"Count"] == x[x$Date == last_date - 1,"Count"]) {
      last_date = last_date - 1
    }
    sel = x$Date == last_date
    sel1 = x$Date == last_date - 1
    sel2 = x$Date == last_date - 2
    res <- data.frame(
      Date = last_date,
      Count = x[sel, "Count"],
      CountsB4 = x[sel1, "Count"],
      GrowthRate = x[sel, "Count"] / x[sel1, "Count"],
      GrowthRateB4 = x[sel1, "Count"] / x[sel2, "Count"])

    res$Summary <- sprintf("%s-%s †%s",
                           hr(max(y$Count[y$Type =="confirmed"])),
                           hr(max(y$Count[y$Type =="recovered"])),
                           hr(max(y$Count[y$Type =="deaths"])))

    res[['GrowthRateIsUp']] =  res[['GrowthRate']] > res[['GrowthRateB4']]
    res
  })


  summary$Growth <- ifelse(abs(summary$GrowthRate - summary$GrowthRateB4) > 0.05,
                           ifelse(summary$GrowthRate > summary$GrowthRateB4, "+", "-"),
                           "")
  death_summary$Growth <- ifelse(abs(death_summary$GrowthRate - death_summary$GrowthRateB4) > 0.05,
                           ifelse(death_summary$GrowthRate > death_summary$GrowthRateB4, "+", "-"),
                           "")


  plot_f <- function(dat, summary_ = summary) {
    breaks <- unique(dat$Date)
    breaks <- breaks[seq(from=1, to=length(breaks), by=round(max(1, length(breaks)/7)))]
    ggplot(dat,
         aes(x=Date, y=Count, color = Country)) +
    cowplot::background_grid("y", size.major = 0.1) +
    geom_hline(yintercept = 0) +
    geom_vline(xintercept = Sys.Date(), size = 0.1, col = "gray") +
    geom_line(aes(linetype = Type), alpha = .75) +
    #geom_smooth(se = FALSE, alpha = .5, size = .5) +
    scale_x_date(limits = c(min(dat$Date), max(dat$Date) + (max(dat$Date) - min(dat$Date))/2),
                 breaks = breaks) +
    #scale_x_continuous("meh") +
    #  geom_hline() +
    ggrepel::geom_text_repel(
      data = subset(summary_, Country %in% dat$Country),
      aes(label = sprintf("%s %.0f%%%s\n%s",
                          Country,
                          100*(GrowthRate-1),
                          Growth,
                          Summary)),
      size = 4,
      direction = "y",
      nudge_x = 190,
      segment.color = '#cccccc',
      alpha = 0.75,
      #segment.color = NA,
      # Width of the line segments.
      segment.size = 0.1

    ) +
    theme(legend.position = "none",
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  }

rates <- plyr::ddply(subset(dat, Type == "confirmed" & MaxCountC > 250), c("Country", "Cat"), function(x) {
  data.frame(Date = x$Date[-1],
    Rate=(x$Count[-1] - x$Count[-nrow(x)])/x$Count[-1])
})
rates$Rate[is.nan(rates$Rate)] <- 1
rates[rates == Inf] <- 1

ggplot(subset(rates, Date>Sys.Date() - 16 & Cat != "Other"), aes(x=Date, y=Rate)) +
    geom_line(aes(col=Country), alpha = .8) +
  geom_smooth(alpha = .5, size = .5, fill="lightgray") +
  scale_y_continuous("Percent increase per day", labels = scales::percent) +
  #coord_cartesian(ylim=c(0, 1)) +
  cowplot::background_grid(major="y", minor="y") +
  facet_wrap(~Cat) + ggtitle("Growth rates of confirmed cases")

  plot_f(subset(dat, Cat != "Other" & Type == "confirmed" & Date > Sys.Date() - 8)) +
    facet_wrap(~Cat, scales = "free_y")

  plot_f(subset(dat, Cat != "Other" & Type == "confirmed" & Date > Sys.Date() - 20)) +
    facet_wrap(~Cat, scales = "free_y") + ggtitle("confirmed")

plot_f(subset(dat, Cat != "Other" & Type == "deaths" & Date > Sys.Date() - 20), death_summary) +
    facet_wrap(~Cat, scales = "free_y") + ggtitle("deaths")


death_rates <- plyr::ddply(subset(dat, Type == "deaths" & MaxCount > 100), c("Country", "Cat"), function(x) {
  data.frame(Date = x$Date[-1],
             Count = x$Count[-1],
             Rate=(x$Count[-1] - x$Count[-nrow(x)])/x$Count[-1])
})
#death_rates[is.na(death_rates$Rate)] <- 1
#death_rates[death_rates$Rate == Inf,] <- 1

ggplot(subset(death_rates, Cat != "Other"),
       aes(x=Count, y=Rate)) +
  cowplot::background_grid(major = "y") +
      geom_line(aes(col=Country), alpha = .8)  +
  geom_point(aes(col=Country), alpha = .8)  +
  coord_cartesian(ylim = c(0, 0.42)) +
#  stat_smooth(method = "lm", formula = y ~ x, size = 1) +
  facet_wrap(~Country, scales="free_x") + ggtitle("Death growth rates") +
  theme(legend.position="off")


ggplot(subset(death_rates, Date>Sys.Date() - 16 & Cat != "Other"), aes(x=Date, y=Rate)) +
  geom_line(aes(col=Country), alpha = .8) +
  #geom_smooth(alpha = .5, size = .5, fill="lightgray") +
  scale_y_continuous("Percent increase per day", labels = scales::percent) +
  #coord_cartesian(ylim=c(0, 1)) +
  cowplot::background_grid(major="y", minor="y") +
  facet_wrap(~Cat) + ggtitle("Growth rates of confirmed cases")



