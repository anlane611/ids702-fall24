## Generate data for HW1 2024

Y <- rnorm(100000, mean=20, sd=5)
X1 <- sample(1:5,1000,prob=rep(0.2,5), replace=TRUE)
X2 <- ifelse(Y<32,1,0)

dat <- data.frame(Y,X1,X2)



## Simulate each sampling scheme

SRSmeans <- data.frame(means=NA) #create an empty dataframe to store the sample means
for(i in 1:1000){
  set.seed(i) #ensure that we have a different sample each time
  SRS.sample <- population |> slice(sample(1:nrow(population),size=10))
  SRSmeans[i,1] <- SRS.sample |> summarise(mean(Y))
}

ggplot(SRSmeans, aes(x=means)) +
  geom_histogram()+
  labs(title="_____________")

myclustermeans <- c()
for(i in 1:1000){
  set.seed(i)
  myclusters <- sample(unique(dat$X3),size=2)
  mysam <- filter(dat, X3 %in% myclusters)
  myclustermeans[i] <- mean(mysam$X1)
}

mystratmeans <- c()
for(i in 1:1000){
  set.seed(i)
  dat.strat <- dat |>
    group_by(X4) |>
    slice(sample(1:n(),size=100)) |>
    ungroup()
  mystratmeans[i] <- mean(dat.strat$X1)
}

mystratmeans_eq <- c()
for(i in 1:1000){
  set.seed(i)
  dat.strat <- dat |>
    group_by(X3) |>
    slice(sample(1:n(),size=100)) |>
    ungroup()
  mystratmeans_eq[i] <- mean(dat.strat$X1)
}

mymultimeans <- c()
for(i in 1:1000){
  set.seed(i)
  myclusters <- sample(unique(dat$X3),size=2)
  mysam <- filter(dat, X3 %in% myclusters)
  mystratdat <- mysam |> group_by(X3) |> 
    slice(sample(1:n(),size=100)) |>
    ungroup()
  mymultimeans[i] <- mean(mystratdat$X1)
}

hist(mymeans)