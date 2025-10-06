
#SCript for excersise 4, PRA2026, 10/2025
#s.badin@student.maastrichtuniversity.nl








#__________________________________Setup________________________
rm(list = ls())
# dev.off()
library(multcomp)


#----------data
climbers <- read.csv("data/climb/climber_df.csv")            # Climbers 
routes <- read.csv("data/climb/routes_rated.csv")            # Routes 
grades <- read.csv("data/climb/grades_conversion_table.csv") # Conversion table

#----------find common countries
common_countries <- intersect(
  toupper(unique(routes$country)),
  toupper(unique(climbers$country))
)

#-----------y axis ticks and French labels
y_ticks <- grades$grade_id[
  grades$grade_id >= 25 & grades$grade_id <= 80
]
y_labels <- grades$grade_fra[
  grades$grade_id >= 25 & grades$grade_id <= 80
]
#reduced ticks
y_labels <- y_labels[seq(1, length(y_labels), by = 4)]
y_ticks <- y_ticks[seq(1, length(y_ticks), by = 4)]












#__________________________________Climbers________________________

#----------filter data by common country
climbers_filtered <- subset(climbers, toupper(country) %in% common_countries)


#----------Statistics
#set country as factor and FRA as reference for Dunnett
climbers_filtered$country <- factor(climbers_filtered$country)
climbers_filtered$country <- relevel(climbers_filtered$country, ref = "FRA")

#ANOVA
model_c <- aov(grades_mean ~ country, data = climbers_filtered)

#Dunnetts
dunnett_c <- glht(model_c, linfct = mcp(country = "Dunnett"))
sum_c <- summary(dunnett_c)
  #extract values
coef_c <- sum_c$test$coef
pval_c <- sum_c$test$pvalues

#determine significance per country
sig_c <- pval_c < 0.05
names(sig_c) <- names(coef_c)

#Shading 
  #comppute difference numerically
abs_effect_c <- abs(coef_c)
max_effect_c <- max(abs_effect_c[sig_c], na.rm = TRUE)
  #initialize colors (as white)
cols_c <- rep("white", length(levels(climbers_filtered$country)))
names(cols_c) <- levels(climbers_filtered$country)
  #shade significant countries
for (nm in names(coef_c)) {
  country <- sub(" - FRA", "", nm)
  if (sig_c[nm]) {
    cols_c[country] <- gray(1 - abs_effect_c[nm] / max_effect_c)
  }
}




#----------Plotting
#save
png("climbers_mean_grades_1080p.png", width = 1920, height = 1080, res = 150)

#margins
par(mar = c(4.5, 4.5, 8, 2))

#plot
cplot <- boxplot(grades_mean ~ country,
                 data = climbers_filtered,
                 las = 2,
                 col = cols_c,
                 border = "grey30",
                 ylab = "Grade",
                 xlab = "Country",
                 yaxt = "n",
                 cex.names = 0.85)

#y axis
axis(2, at = y_ticks, labels = y_labels, las = 1)

#top axis
  #sum the counts
grades_sum <- tapply(climbers$grades_count, climbers$country, sum, na.rm = TRUE)
grades_sum_ordered <- grades_sum[cplot$names]
  #create the axis
axis(3, at = 1:length(grades_sum_ordered),
     labels = grades_sum_ordered,
     las = 2, cex.axis = 0.9, col.axis = "blue2", line = 0)
  #label
mtext("Climber counts", side = 3, line = 3.5, col = "blue2")

#title
mtext("Mean Grades of Climbers per Country",
      side = 3, line = 5.5, cex = 1.5, font = 2)
#close to save
dev.off()














#__________________________________Routes________________________


#----------Filtering
  #by common country
routes_filtered <- subset(routes, toupper(country) %in% common_countries)
routes_filtered$country_upper <- toupper(routes_filtered$country)

  #count for top axis and stats
routes_count <- table(routes_filtered$country_upper)

  #by sample size
valid_countries <- names(routes_count[routes_count >= 30])
invalid_countries <- names(routes_count[routes_count < 30])
routes_valid <- subset(routes_filtered, country_upper %in% valid_countries)


#----------Statistics
#factor and reference
routes_valid$country <- factor(routes_valid$country_upper)
routes_valid$country <- relevel(routes_valid$country, ref = "FRA")

#ANOVA & Dunnett
model_r <- aov(grade_mean ~ country, data = routes_valid)
dunnett_r <- glht(model_r, linfct = mcp(country = "Dunnett"))
sum_r <- summary(dunnett_r)
  #extract results
coef_r <- sum_r$test$coef
pval_r <- sum_r$test$pvalues
sig_r  <- pval_r < 0.05
names(sig_r) <- names(coef_r)
    #clean up
comp_names <- gsub(" - FRA", "", names(coef_r))
comp_names <- gsub("FRA - ", "", comp_names)

#build data-frame for graphing
stats_df <- data.frame(
  country = comp_names,
  coef = coef_r,
  pval = pval_r,
  sig = sig_r,
  stringsAsFactors = FALSE
)

#compute shading values
abs_effect_r <- abs(stats_df$coef)
max_effect_r <- max(abs_effect_r[stats_df$sig], na.rm = TRUE)






#----------Plotting
#assign colors
all_countries <- sort(unique(routes_filtered$country_upper))
cols_r <- rep("lightcoral", length(all_countries))
names(cols_r) <- all_countries

#apply shading
for (country in valid_countries) {
  if (country %in% stats_df$country) {
    if (stats_df$sig[stats_df$country == country]) {
      shade <- 1 - abs_effect_r[stats_df$country == country] / max_effect_r
      cols_r[country] <- gray(shade)
    } else {
      cols_r[country] <- "white"
    }
  } else {
    cols_r[country] <- "white"
  }
}

#put FRA first
routes_filtered$country_upper <- factor(routes_filtered$country_upper,
                                        levels = c("FRA", sort(setdiff(unique(routes_filtered$country_upper), "FRA"))))

#save
png("routes_mean_grades_1080p.png", width = 1920, height = 1080, res = 150)


#margins
par(mar = c(4.5, 4.5, 7, 2))

#empty boxplot
rplot <- boxplot(grade_mean ~ country_upper,
                 data = routes_filtered,
                 plot = FALSE)

#match colors
box_cols <- cols_r[rplot$names]

#draw plot
boxplot(grade_mean ~ country_upper,
        data = routes_filtered,
        las = 2,
        col = box_cols,
        border = "grey30",
        ylab = "Grade",
        xlab = "Country",
        yaxt = "n",
        cex.names = 0.85)

#y axis
axis(2, at = y_ticks, labels = y_labels, las = 1)

#top axis
routes_count_ordered <- routes_count[rplot$names]
axis(3, at = 1:length(routes_count_ordered),
     labels = routes_count_ordered,
     las = 2, cex.axis = 0.9,
     col.axis = "blue2",
     line = 0)
  #top label
mtext("Route counts", side = 3, line = 3, col = "blue2")

#title
mtext("Mean Grades of Climbing Routes per Country",
      side = 3, line = 4.5, cex = 1.5, font = 2)

#close to save
dev.off()
