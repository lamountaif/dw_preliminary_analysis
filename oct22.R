#require in programs

require(ggplot2)
require(gridExtra)
require(lubridate)
require(GGally)
require(RColorBrewer)
require(usethis)
require(ggpubr)
require(moments)


use_git_config(user.name = "lamountaif", 
               user.email = "lamountaif@vcu.edu")
usethis::git_default_branch_configure()


#read in csv
fhdw<- read.csv("FHDW.csv",
                stringsAsFactors = T)
ns <- read.csv("ninesweep.csv",
                 stringsAsFactors = T)
w7 <- read.csv("week7.csv",
               stringsAsFactors = T)
ct <- read.csv("predictor_data.csv",
               stringsAsFactors = T)
ct.2 <-data.frame(ct)



#Merging Nine sweep data with Flood Height and Frond Count

nsfd <- merge(fhdw,ns, by.x="Pool_ID",by.y="Pool_ID") 
#Nine Sweep & Flood height
nsfdw7 <- merge(nsfd,w7, by.x="Pool_ID",by.y="pool_id") 
#Nine sweep, flood height and frond count for all duckweed pools at week 7
final <- subset(nsfdw7,w7 != 0) 
#Nine sweep, flood height and frond count for duckweed pools at week 7 that still had dw




#Setting date to exclude duplicates of pools

nsfdw7$Date_Collected <- as.POSIXct(nsfdw7$Date_Collected)

nsfdw7$month <- month(nsfdw7$Date_Collected)

nsfdw7 <- subset(nsfdw7,month>7) # Nine sweep, flood height and frond count for all duckweed pools at week 7 



#creating predictor data set 

nsfd$Date_Collected <- as.POSIXct(nsfd$Date_Collected)
nsfd$month <- month(nsfd$Date_Collected)

f <- subset(nsfd,month>7) #data that has predictor variables 



#Creating min lip
nsfdw7$min_lip <- apply(nsfdw7[,17:20],1,FUN= min)

f$min_lip <- apply(f[,17:20],1,FUN= min)

f <- f[!duplicated(f),]

#correlation test!

ct.2$volume_ml <- NULL
ct.2$Pool_ID <- NULL
ct.2$sa_cm <- NULL
ct.2$w7 <- NULL
ggcorr(ct.2,
       palette = "red", name = "Correlation", 
       label = TRUE, label_color = "black")


#Plots for duckweed treatments by different values

ggplot(nsfd,
       aes(x = treatment,
           y = floodedat)) +
  geom_boxplot() +
  labs(x= "Treatment",
       y= "Flood Height (feet) ",
       title = "Flood Height By Treatment")

ggplot(nsfd,
       aes(x = shading,
           y = floodedat)) +
  geom_boxplot() +
  labs(x= "Shading",
       y= "Flood Height (feet) ",
       title = "Flood Height By Shade")
ggplot(nsfd,
       aes(x = treatment,
           y = Depth_Avg)) +
  geom_boxplot() +
  labs(x= "Treatment",
       y= "Depth (cm)",
       title = "Depth of Pool by Treatment")

ggplot(nsfdw7,
       aes(x = Depth_Avg,
           y = w7)) +
  geom_point() 
labs(x= "Treatment",
     y= "Depth (cm)",
     title = "Depth of Pool by Treatment")

#histograms of all variables

hist(ct$volume..L.,
     xlab = "Volume (L)",
     main = "Volume Diversity amongst Pools")

hist(ct$depth_avg,
     xlab = "Average Depth (cm)",
     main = "Depth Diversity amongst Pools")  

hist(ct$FloodedAt,
     xlab = "Flood Height (ft)",
     main = "Flood height of Pools") 

hist(ct$sa,
     xlab = "Surface Area (cm)",
     main = "Surface Area amongst Pools")  

hist(nsfdw7$min_lip,
     xlab = "Min Lip (cm)",
     main = "Minimum Lip Height Frequency") 

#trying to transform flood height data

ggdensity(ct, x = "FloodedAt", fill = "lightgray", title = "Flood Heights") +
  scale_x_continuous(limits = c(0,20)) +
  stat_overlay_normal_density(color = "red", linetype = "dashed")

skewness(ct$FloodedAt, na.rm = TRUE) #1.477

ct$FloodedAt <- log10(ct$FloodedAt)

ggdensity(ct, x = "FloodedAt", fill = "lightgray", title = "Flood Height log 10") +
  scale_x_continuous(limits = c(0.5,1.5)) + 
  stat_overlay_normal_density(color = "red", linetype = "dashed")

skewness(ct$FloodedAt, na.rm = TRUE) #0.823

#trying to transform depth data

ggdensity(ct, x = "depth_avg", fill = "lightgray", title = "Depth (cm)") +
  scale_x_continuous(limits = c(0,75)) +
  stat_overlay_normal_density(color = "red", linetype = "dashed")

skewness(ct$depth_avg, na.rm = TRUE) #0.7370244

ct$depth_avg <- sqrt(ct$depth_avg)

ggdensity(ct, x = "depth_avg", fill = "lightgray", title = "Depth sqrt") +
  stat_overlay_normal_density(color = "red", linetype = "dashed")

skewness(ct$depth_avg, na.rm = TRUE) #0.25

#trying to transform surface area data

ggdensity(ct, x = "sa_m", fill = "lightgray", title = " Surface Area") +
  scale_x_continuous(limits = c(0,250)) +
  stat_overlay_normal_density(color = "red", linetype = "dashed")

skewness(ct$sa_m, na.rm = TRUE) #2.550033

ct$sa_m <- 1/(ct$sa_m)

ggdensity(ct, x = "sa_m", fill = "lightgray", title = "Inverse Average SA ") +
  stat_overlay_normal_density(color = "red", linetype = "dashed")

skewness(ct$sa_m, na.rm = TRUE) #0.49

#trying to transform volume data

ggdensity(ct, x = "volume_L", fill = "lightgray", title = "Volume") +
  scale_x_continuous(limits = c(0,1100)) +
  stat_overlay_normal_density(color = "red", linetype = "dashed")

skewness(ct$volume_L, na.rm = TRUE) #2.715302

ct$volume_L <- log10(ct$volume_L)

ggdensity(ct, x = "volume_L", fill = "lightgray", title = "Log 10 Volume ") +
  stat_overlay_normal_density(color = "red", linetype = "dashed")

skewness(ct$volume_L, na.rm = TRUE) #-0.24

#trying to transform minimum lip data

ggdensity(ct, x = "min_lip", fill = "lightgray", title = "Minimum Lip") +
  scale_x_continuous(limits = c(0,20)) +
  stat_overlay_normal_density(color = "red", linetype = "dashed")

skewness(ct$min_lip, na.rm = TRUE) #3.096287

ct$min_lip <- (ct$min_lip + 0.0001)

ct$min_lip <- sqrt(ct$min_lip)

ggdensity(ct, x = "min_lip", fill = "lightgray", title = "Transformed Minimum Lip ") +
  stat_overlay_normal_density(color = "red", linetype = "dashed")

skewness(ct$min_lip, na.rm = TRUE) #-0.24









