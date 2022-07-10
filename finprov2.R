install.packages("RSelenium")
install.packages("tidyverse")
install.packages("netstat")

library(RSelenium)
library(tidyverse)
library(netstat)


rs_driver_object <-rsDriver(
  browser="chrome",
  chromever="99.0.4844.51",
  verbose=F,
  port=free_port()
)

remDr <- rs_driver_object$client

remDr$open()
remDr$navigate("https://www.unigo.com/colleges/by-state/connecticut")

remDr$findElement(using = "css", ".title-one a")$clickElement()

#remDr$findElement(using = "css", "body > div.container-fluid.full-width-container.college-landing-page-main-container > div > div > section > div.centered-content-row > div > div.col-md-9 > div:nth-child(1) > div > div > div > div.row.accordion-container > div:nth-child(1) > div > div:nth-child(2) > button")$clickElement()

e <- remDr$findElement(using = "css selector", ".college_detail_supplemental_information_container:nth-child(1) tr:nth-child(1) td+ td")

e$getElementText()

remDr$close()
