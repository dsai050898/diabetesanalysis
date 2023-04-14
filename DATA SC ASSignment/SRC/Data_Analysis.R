# Import required libraries 
library(tidyverse)
library(formattable)
# cleaned data
cleaned_df <- read.csv("C:/Users/dsai0/OneDrive/Desktop/DATA SC ASSignment/Clean DATA/diabetes_cleaned.csv")

#setting seed for req work
set.seed(35)

#random sample of 25 observations
sample_df <- cleaned_df[sample(nrow(diabetes_df), 25, replace = FALSE), ]

# top five in sample data
head(sample_df)

# end five values in sample data
tail(sample_df)

#number of rows in sample data
nrow(sample_df)

#number of columns in sample data
ncol(sample_df)

#mean glucose of the sample data
glucose_mean_sample <- mean(sample_df$Glucose)


glucose_mean_sample


write(glucose_mean_sample, file = "C:/Users/dsai0/OneDrive/Desktop/DATA SC ASSignment/RESULTS/glucose_mean_sample.txt")

#max glucose of the sample data
gl_max_samp <- max(sample_df$Glucose)

gl_max_samp

write(gl_max_samp, file = "C:/Users/dsai0/OneDrive/Desktop/DATA SC ASSignment/RESULTS/gl_max_samp.txt")

#max glucose of the glucose
gl_max_popul <- max(diabetes_df$Glucose)
gl_max_popul

write(gl_max_popul, file = "C:/Users/dsai0/OneDrive/Desktop/DATA SC ASSignment/RESULTS/gl_max_popul.txt")

#mean glucose of the Glucose
gl_mean_popul <- mean(diabetes_df$Glucose)

gl_mean_popul

write(gl_mean_popul, file = "C:/Users/dsai0/OneDrive/Desktop/DATA SC ASSignment/RESULTS/gl_mean_popul.txt")

# Barplot for  mean glucose

counts <- c(glucose_mean_sample, gl_mean_popul)
counts


labels <- c("sampl", "Popul")

barplot(counts, names.arg = labels, main="Comparision Mean Glucose",
        xlab="Glucose", ylab = "Count", col=c("black","red"))

library(formattable)

#For pie chart mean glucose

percentge <- percent(counts/sum(counts))

pie(counts, labels = labels, main = "Comparison of Mean Glucose", col=c("black","red"))

legend("topleft", legend = labels, cex = 1.0, fill = c("black","red"))

# barplot for comparisn of max glucose

counts <- c(gl_max_samp, gl_max_popul)
counts

labels <- c("Sampl", "Popul")

barplot(counts, names.arg = labels, main="Compa of Max Glucos",
        xlab="Glucose", ylab = "Count", col=c("black","red"))

library(formattable)

# pie chart for comparisn of max glucose
percentge <- percent(counts/sum(counts))

pie(counts, labels = labels, main = "Compari of Max Glucos", col=c("black","red"))

legend("topleft", legend = labels, cex = 1.0, fill = c("black","red"))

          

#########################################################################
                       # PART - B #

#Finding the 98th percentile of BMI

BMI_populat_percent <- quantile(diabetes_df$BMI, 0.98)

BMI_populat_percent

write(BMI_populat_percent , file = "C:/Users/dsai0/OneDrive/Desktop/DATA SC ASSignment/RESULTS/BMI_populat_percent.txt")

# for sample percent of BMI 
BMI_sampl_percent <- quantile(sample_df$BMI, 0.98)

BMI_sampl_percent

write(BMI_sampl_percent , file = "C:/Users/dsai0/OneDrive/Desktop/DATA SC ASSignment/RESULTS/BMI_sampl_percent.txt")

# for barplot btw bmi sample vs bmi population percent
counts <- c(BMI_sampl_percent, BMI_populat_percent)
counts

labels <- c("Sampl", "Popul")

barplot(counts, names.arg = labels, main="Comparisn of 98th Percentile of BMI",
        xlab="98th Percentile of BMI", ylab = "Count", col=c("violet","gray"))

# for pie chart btw bmi sample vs bmi population percent

counts <- c(BMI_sampl_percent, BMI_populat_percent)
counts

percentge <- percent(counts/sum(counts))

pie(counts, labels = labels, main = "Comparisn of 98th Percentile of BMI", col=c("violet","gray"))

legend("topleft", legend = labels, cex = 1.0 , fill = c("violet","gray"))


#########################################################################
                          # PART - C #
#setting seed for req work
set.seed(2)

#bootstrap samples and its size for req data
required_sample <- 500
required_sample_size <- 150

#for loop to loop over number of bootstraps

for (i in 1:required_sample_size) {
  sample_indices <- sample(nrow(diabetes_df), size = required_sample, replace = TRUE)
  sample_data <- diabetes_df[sample_indices, ]
  
}
# end five values of sample data
tail(sample_data )

# head five values for sample data
head(sample_data )
# no of rows in sample data
nrow(sample_data)

# no of col in sample data
ncol(sample_data)

#sample of blood pressure for sample data
bp_sampledata_sample <- sd(sample_data$BloodPressure)

bp_sampledata_sample

write(bp_sampledata_sample, file = "C:/Users/dsai0/OneDrive/Desktop/DATA SC ASSignment/RESULTS/bp_sampledata_sample.txt")

#mean of blood pressure for sample data

bp_mean_sampl <- mean(sample_data$BloodPressure)

bp_mean_sampl

write(bp_mean_sampl, file = "C:/Users/dsai0/OneDrive/Desktop/DATA SC ASSignment/RESULTS/bp_mean_sampl.txt")

#percentile for blood pressure for sample data

bp_percentil_sampl <- quantile(sample_data$BloodPressure, 1.0)

bp_percentil_sampl

write(bp_percentil_sampl, file = "C:/Users/dsai0/OneDrive/Desktop/DATA SC ASSignment/RESULTS/bp_percentil_sampl.txt")

#mean of blood pressure for population data

bp_mean_popul <- mean(diabetes_df$BloodPressure)

bp_mean_popul

write(bp_mean_popul, file = "C:/Users/dsai0/OneDrive/Desktop/DATA SC ASSignment/RESULTS/bp_mean_popul.txt")

#sample data of blood pressure forpopulation

bp_sampledata_population <- sd(diabetes_df$BloodPressure)

bp_sampledata_population

write(bp_sampledata_population, file = "C:/Users/dsai0/OneDrive/Desktop/DATA SC ASSignment/RESULTS/bp_sampledata_population.txt")


# percent poulation  for BP
bp_percent_popul <- quantile(diabetes_df$BloodPressure, 1.0)

bp_percent_popul

write(bp_percent_popul, file = "C:/Users/dsai0/OneDrive/Desktop/DATA SC ASSignment/RESULTS/bp_percent_popul.txt")

# gen counts and pie chart btw mean sample and  mean sample
counts <- c(bp_mean_sampl, bp_mean_popul)
counts

library (formattable)

percentge <- percent(counts/sum(counts))

pie(counts, labels = labels, main = "Comparisn of Mean of BP", col=c("blue","green"))

legend("topleft", legend = labels, cex = 1.0, fill = c("blue","green"))

# gen counts and barplot for mean sample and mean population)
counts <- c(bp_mean_sampl, bp_mean_popul)
counts

library (formattable)

labels <- c("Sampl", "Popul")

barplot(counts, names.arg = labels, main="Comparisn of Mean of BP",
        xlab="Average", ylab = "Count", col=c("blue","green"))


# bar chart for standard deviation of blood pressure for sample and population data
counts <- c(bp_sampledata_sample, bp_sampledata_population)
counts

labels <- c("Sampl", "Popul")


barplot(counts, names.arg = labels, main="Comparisn of Standard Deviation of BP",
        xlab="Standard Deviation", ylab = "Count", col=c("maroon","navy"))

# piecharts btw sampledata vs sampledata population

counts <- c(bp_sampledata_sample, bp_sampledata_population)
counts

percentge <- percent(counts/sum(counts))

pie(counts, labels = labels, main = "Comparision of Standard Deviation of Blood Pressure", col=c("maroon","navy"))

legend("topleft", legend = labels, cex = 1.0, fill = c("maroon","navy"))


# bar chart for percentile of blood pressure for sample and population data

counts <- c(bp_percentil_sampl, bp_percent_popul)
counts

labels <- c("Sampl", "Popul")


barplot(counts, names.arg = labels, main="Comparisn of Percentile of BP",
        xlab="Percentile", ylab = "Count", col=c("orange","yellow"))

# pie chart btw  percentile sample vs percent population
counts <- c(bp_percentil_sampl, bp_percent_popul)
counts

percentge <- percent(counts/sum(counts))

pie(counts, labels = labels, main = "Comparisn of Percentile of BP", col=c("orange","yellow"))

legend("topleft", legend = labels, cex = 1.0, fill = c("orange","yellow"))



