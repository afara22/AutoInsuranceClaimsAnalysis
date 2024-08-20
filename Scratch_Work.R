# view frequencies of AGE and PAID variable
ggplot(autoClaims, aes(x = AGE)) +
  geom_histogram(binwidth = 5, 
                 fill = "lightblue", 
                 color = "black") +
  labs(title = "Distribution of Age", x = "Age", y = "Frequency")

ggplot(autoClaims, aes(x = PAID)) +
  geom_histogram(binwidth = 500, 
                 fill = "lightgreen", 
                 color = "black") +
  labs(title = "Distribution of PAID", x = "Paid", y = "Frequency")

# data is skewed for both! lets take log of each
# distribution of log AGE frequency
ggplot(autoClaims, aes(x = log(AGE))) +
  geom_histogram(binwidth = .05, 
                 fill = "brown", 
                 color = "black") +
  labs(title = "Distribution of logAGE", 
       x = "logAGE", 
       y = "Frequency")

# distribution of log PAID frequency
ggplot(autoClaims, aes(x = log(PAID))) +
  geom_histogram(binwidth = .05, 
                 fill = "orange", 
                 color = "black") +
  labs(title = "Distribution of logPAID", 
       x = "logPAID", 
       y = "Frequency")

# add logAGE to new data frame
autoClaims_new <- autoClaims %>% 
  mutate(logAGE = log(AGE), logPaid = log(PAID))

view(autoClaims_new)


# scatter plot of PAID vs logAGE with regression line
ggplot(autoClaims_new, aes(x = logAGE, 
                           y = logPaid)) +
  geom_point() + 
  labs(title = "LOG Claims Paid vs Log(AGE) Scatter Plot", 
       x = "Log(Age)",
       y = "log(Claims)") +
  geom_smooth(method = "lm", color = "red")

# summary of linear regression model
model <- lm(logPaid ~ logAGE, data = autoClaims_new)  
summary(model)  
