
### Code for subsetting LakeLake high-frequency profiler data
# working with long-form data

# required packages
library(lubridate)
library(chron)
options(chron.year.abb = FALSE)
library(RColorBrewer)

# set working directories
in.dir<-"input directory here"  # directory containing data files in long format
out.dir<-"output directroy here"  # directory for summarised files and plots

## import files, combine into one dataframe
setwd(in.dir)
f<-list.files()

data <- read.csv(f[1])
for(i in 2:length(f)){
  fi<-read.csv(f[i])
  data<-rbind(data,fi)
}

# define datetimes with chron (good for manipulating datetimes)
data$date <- gsub(" .*$", "" ,data$pr_dt.chron)
data$time <- gsub("^.* ", "" ,data$pr_dt.chron)
data$pr_dt.chron <- chron(dates=data$date, times=data$time, format=c(dates="y-m-d", times="h:m:s"))
data$hour <- hours(data$pr_dt.chron)


## subsetting

# set criteria
MARS1.dates <- as.Date(x=c("2015-06-02", "2015-06-05", "2015-06-12", "2015-06-16", "2015-06-23", "2015-06-30", "2015-07-07", "2015-07-14"))
MARS1.doys <- yday(MARS1.dates)
desired.hours <- 6:18
desired.depths <- seq(0.5, 6.5, 0.5)

# subset
data.sub <- data[which(data$DOY %in% MARS1.doys &            # select days (could also do with dates)
                       data$hour %in% desired.hours &        # select hours
                       data$depth %in% desired.depths  ), ]  # select depths

# write subset
setwd(out.dir)
write.csv(data.sub, "MARS1_HFdata_subset")


## summarising

# simple mean of all times and depths.  Note you could add na.rm=T, but be aware missing data may bias the mean!
means <- aggregate(data.sub[, 6:22], by=list(data.sub$enc, data.sub$DOY, data.sub$date), FUN=mean)
colnames(means)[1:3] <- c("enc", "DOY", "date")



## plotting

col.list<- c(brewer.pal(9,"Blues")[3:9], brewer.pal(9,"Greens")[3:9], brewer.pal(9,"Reds")[3:9], rep("grey50",3))
pch.list<- c(rep(16,21), rep(21,3))

means$response <- log(means$PAR+1) # set response variable here

# write plot
setwd(out.dir)
png("PAR_summary.png", width=20, height=10, pointsize=12, units="cm", res=300)

par(mar=c(4.1,4.1,2.1,6.1), xpd=NA)
plot(response~DOY, data=means, xaxt='n', ylab="log(PAR+1)", xlab="Date", las=1, xlim=c(152,196), typ='n')
axis(1, at=seq(153,200,7), lab=format(x=strptime(paste("2015", seq(153,200,7)), format="%Y %j"), format="%d %b"))
legend("topright", inset=c(-0.15,-0.15), legend=substr(levels(means$enc)[1:21],1,2), pch=16, col=col.list[1:21], bty='n', cex=0.9)
for (i in 1:21) {
  means.sub <- means[means$enc==levels(means$enc)[i],]
  points(response~DOY, data=means.sub, pch=pch.list[i], col=col.list[i], type='b')
}

dev.off()





