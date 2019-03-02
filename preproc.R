
library(comtradr)
library(reshape)

q1_AG2 <- ct_search(reporters = "all", 
                    partners = "World", 
                    trade_direction = "export",
                    start_date = 2010,
                    end_date = 2010,
                    commod_codes = "AG2")

x <- data.frame(q1_AG2$reporter,q1_AG2$commodity_code,q1_AG2$trade_value_usd)
y <- cast(x, q1_AG2.reporter ~ q1_AG2.commodity_code)

write.csv(y , file = "raw_data.csv")
