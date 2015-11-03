bribe_sta <- aggregate(amount ~ states, df, sum)
bribe_sta <- bribe_sta[-c(1),]
bribe_sta <- bribe_sta %>% arrange(desc(amount))
