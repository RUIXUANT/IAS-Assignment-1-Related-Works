comm = read.csv("Comm 20k-37.5k.csv")

cont_model_comm = lm(log(comm$CommInsure) ~ factor(comm$Floor) + log(comm$Excess) + log(comm$Content), data = comm)
summary(cont_model_comm)
# plot(cont_model_comm)

comm$Floor = factor(comm$Floor)
comm$Excess = factor(comm$Excess, levels = c(750, 100, 200, 300, 500, 1000, 2000, 5000))
comm$Content = factor(comm$Content, levels = c(25000, 20000, 22500, 27500, 30000, 37500))
fac_model_comm = lm(log(comm$CommInsure) ~ comm$Floor+comm$Excess+comm$Content, data = comm)
summary(fac_model_comm)
# plot(fac_model_comm)

# ilogit = function(x) exp(x)/(1+exp(x))
# range01 <- function(x){(x-min(x))/(max(x)-min(x))}
# logit_model_comm = glm(range01(comm$CommInsure) ~ comm$Floor+comm$Excess+comm$Content, family = Gamma(link = 'logit'),data = comm)
# summary(logit_model_comm)

suncp = read.csv("Suncorp 20k-37.5k.csv")

cont_model_suncp = lm(log(suncp$Suncorp) ~ factor(suncp$Floor) + log(suncp$Excess) + log(suncp$Content), data = suncp)
summary(cont_model_suncp)

suncp$Floor = factor(suncp$Floor)
suncp$Excess = factor(suncp$Excess, levels = c(750, 200, 400, 600, 1000, 2000))
suncp$Content = factor(suncp$Content, levels = c(25000, 20000, 22500, 27500, 30000, 37500))
model_suncp = lm(log(suncp$Suncorp) ~ suncp$Floor+suncp$Excess+suncp$Content, data = suncp)
summary(model_suncp)
# plot(model_suncp)
