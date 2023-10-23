
# Load data

library(readr)
library(plm)
library(readxl)

Export<- read_excel("Export_PE_CH.xlsx")
str(Export)
View(Export)
country=pdata.frame(Export,index=c("country","year"))
x11()
coplot(cmer ~ country|year, type="l",data=country )

df$country <- as.factor(df$country)
