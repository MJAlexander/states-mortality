library(tidyverse)
library(rstan)
library(here)

# load in data
ds <- read_rds(here("data","births_2017_sample.RDS"))
head(ds)
ds <- ds %>% 
  rename(birthweight = dbwt, gest = combgest) %>% 
  mutate(preterm = ifelse(gest<32, "Y", "N")) %>% 
  filter(ilive=="Y",gest< 99, birthweight<9.999)

# Calculate scaled gestational length
log_gest_c <- (log(ds$gest) - mean(log(ds$gest)))/sd(log(ds$gest))
N <- length(log_gest_c)

# load in model output for Model 1

load(here("output", "mod1.Rda"))

# extract the samples
fit <- extract(mod1)

nsims <- dim(fit$beta)[1]

beta0_s <- fit$beta[,1]
beta1_s <- fit$beta[,2]
sigma_s <- fit$sigma

# empty matrix to store samples
log_weight_rep <- matrix(NA, nrow = N, ncol = nsims)

# do the thing
for(i in 1:N){
  log_weight_rep[i,] <- rnorm(nsims, mean = beta0_s+beta1_s*log_gest_c[i], sd = sigma_s)
}


# data easier to plot
colnames(log_weight_rep) <- 1:nsims

dr <- as_tibble(log_weight_rep)
dr <- dr %>% bind_cols(i = 1:N, log_weight_obs = log(ds$birthweight))

dr <- dr %>% 
  pivot_longer(`1`:`1000`, names_to = "sim", values_to = "log_weight_rep")

# plot densities for 100 samples

set.seed(176)
sims_to_sample <- sample(1:nsims, 100)

dr %>% 
  filter(sim %in% sims_to_sample) %>% 
  ggplot(aes(log_weight_rep, group = sim)) + 
  geom_density(alpha = 0.2, aes(color = "y_rep")) + 
  geom_density(data = ds %>% mutate(sim = 1), 
               aes(x = log(birthweight), col = "y")) + 
  scale_color_manual(name = "", 
                     values = c("y" = "darkblue", 
                                "y_rep" = "lightblue")) + 
  ggtitle("Distribution of observed and replicated birthweights") + 
  theme_bw(base_size = 16)



