#necessary packages

deps = c("summarytools","mmod","poppr","ape","magrittr",
         "pegas","hierfstat", "lme4","dplyr");



for (dep in deps){
  if (dep %in% installed.packages()[,"Package"] == FALSE){
    install.packages(as.character(dep), quiet=TRUE);
  }
  library(dep, verbose=FALSE, character.only=TRUE)
}

#setting up data
setwd("~/R_analysis")

url1 <- "https://figshare.com/ndownloader/files/39760984"
download.file(url1, destfile="data1.csv", 'libcurl')
df <- read.csv(file="data1.csv", sep=";")

df$Year <- as.factor(df$Year)
df$Sex <- as.factor(df$Sex)
df$Species <- as.factor(df$Species)
df$Location <- as.factor(df$Location)
df$Type <- as.factor(df$Type)
df$Subregion <- as.factor(df$Subregion)
df$Any <- as.factor(df$Any)
df$Y139C <- as.factor(df$Y139C)
df$L128S <- as.factor(df$L128S)
df$R33P <- as.factor(df$R33P)
df$zygosity <- as.factor(df$zygosity)

url2 <- "https://figshare.com/ndownloader/files/39760981"
download.file(url2, destfile="data2.csv", 'libcurl')
rat_pop <- read.genalex("data2.csv", ploidy=2, sep=";")


#descriptives

table(df$Species, df$Sex)

stby(
  list(
    x = df$Location, # Sex/Subregion and Any polymorphism/Y139C/..
    y = df$Y139C #L128S
  ),
  INDICES = df$Species, # for each Species
  FUN = ctable # ctable for cross-tabulation
)

#population genetic analysis
summary(rat_pop)
basic.stats(rat_pop)

toto <- summary(rat_pop)
baS <- basic.stats(rat_pop)

hw.test(rat_pop, B = 1000)
nanhwe.pop <- seppop(rat_pop) %>% lapply(hw.test, B = 1000)
nanhwe.pop


wc(rat_pop)
rat_poppr <- poppr(rat_pop)

#model 

df2 <- filter(df, Sex != "")
df2 <- droplevels(df2)
rat_any <- glmer(Any ~ Species + Sex + Type + Subregion + (1|Location), 
                       family=binomial, data = df2)
summary(rat_any)