setwd("C:\\Users\\powertec\\Desktop\\LAB8")
getwd()

#1
data<-read.table("Data - Lab 8.txt",header=TRUE)
fix(data)
attach(data)
popmn<-mean(Nicotine)
popvar<-var(Nicotine)

#2
samples<-c()
n<-c()
for(i in 1:30){
  s<-sample(Nicotine,5,replace=TRUE)
  samples<-cbind(samples,s)
  n<-c(n,paste('s',i))
}
colnames(samples)<-n
s.means<-apply(samples,2,mean)
s.vars<-apply(samples,2,var)

#3
samplemean<-mean(s.means)
samplevars<-var(s.means)

#4
popmn
samplemean

#5
truevar=popvar/5
samplevars

#EXERCISE
#1
weights <- c(2.7, 2.4, 2.6, 3.1, 2.3, 2.8, 2.1, 2.5, 2.9, 2.2)

pop_mean <- mean(weights)

pop_sd <- sd(weights) * sqrt((length(weights)-1)/length(weights))

cat("Population mean =", pop_mean, "\n")
cat("Population SD   =", pop_sd, "\n")

#2
set.seed(123)  
n_samples <- 25
sample_size <- 6

sample_means <- numeric(n_samples)
sample_sds   <- numeric(n_samples)

for (i in 1:n_samples) {
  s <- sample(weights, size = sample_size, replace = TRUE)
  sample_means[i] <- mean(s)
  sample_sds[i]   <- sd(s)   
}


samples_df <- data.frame(
  sample_index = 1:n_samples,
  sample_mean  = round(sample_means, 4),
  sample_sd    = round(sample_sds, 4)
)

print(samples_df)

#3
mean_of_sample_means <- mean(sample_means)

sd_of_sample_means <- sd(sample_means)

theoretical_se <- pop_sd / sqrt(sample_size)

cat("Mean of sample means =", mean_of_sample_means, "\n")
cat("SD of sample means   =", sd_of_sample_means, "\n")
cat("Theoretical SE       =", theoretical_se, "\n")