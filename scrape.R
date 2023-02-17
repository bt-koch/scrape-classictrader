# -----------------------------------------------------------------------------.
# initialization ----
# -----------------------------------------------------------------------------.
rm(list=ls()); gc()
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(rvest)) install.packages("rvest", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")

# objects
data <- data.table()

# functions
get_vehicle_information <- function(link){
  page <- read_html(link)
  
  info <- page %>% html_nodes(".vehicle-details-accordion-content") %>% html_table()
  tab_1 <- info[[1]] %>% as.data.table() %>% transpose() %>% setnames(as.character(.[1,])) %>% .[-1,]
  tab_2 <- info[[2]] %>% as.data.table() %>% transpose() %>% setnames(as.character(.[1,])) %>% .[-1,]
  
  price <- page %>% html_nodes(".filter-by-currency-code") %>% html_text()
  
  if(length(price) == 0){
    price <- page %>% html_nodes(".vehicle-ad-price-on-request") %>% html_text() %>% unique()
    if(length(price == 1)){tab_3 <- data.table(price = NA)}
    
  } else {
    price <- price[str_detect(price, "EUR")] %>% gsub(",", "", .) %>% 
      str_extract_all(pattern = "[\\.0-9e-]+", simplify = T) %>% unique() %>%
      as.numeric()
    if(length(price) != 1){stop(link)}
    tab_3 <- data.table(price = price)
  }
  
  tab <- cbind(ds1=tab_1, ds2=tab_2, tab_3)
  return(tab)
}

# -----------------------------------------------------------------------------.
# scrape data ----
# -----------------------------------------------------------------------------.
cat("\nScrape data from classic-trader.com...")

# scrape data
base_link <- "https://www.classic-trader.com/uk/cars/search?fulltext=&pagination%5Bpage%5D="

# get number of pages we want to scrape
pages <- read_html(paste0(base_link, 1)) %>%
  html_nodes(".pager-text") %>%
  html_text() %>%
  str_extract_all(pattern = "[\\.0-9e-]+", simplify = T) %>%
  as.numeric()

for(page in pages[1]:pages[2]){
  
  # create link for specific web page
  link <- paste0(base_link, page)
  
  # get all items of page
  vehicle_links <- read_html(link) %>%
    html_nodes(".result-name") %>%
    html_attr("href") %>%
    file.path("https://www.classic-trader.com", .)
  
  # scrape data from links
  vehicles <- sapply(vehicle_links, FUN = get_vehicle_information)
  
  # bind rows to temporary data.table
  temp_dt <- rbindlist(vehicles, fill = T)
  
  # bind temporary data.table to permanent data.table
  data <- rbind(data, temp_dt, fill = T)
  
  # track progress
  cat('\rPage', page, "of", pages[2], "scraped")
  flush.console()
  
}

cat("\nScraping successfully completed!")

# -----------------------------------------------------------------------------.
# manipulate data ----
# -----------------------------------------------------------------------------.

# filter relevant data and perform simple manipulations
data <- data %>%
  select(
    ID = `ds2.Vehicle-ID:`,
    manufacturer = ds1.Make,
    model = ds1.Model,
    model_name = `ds1.Model name`,
    year = `ds1.Year of manufacture`,
    mileage = `ds1.Mileage (read)`,
    body_style = `ds1.Body style`,
    power = `ds1.Power (kw/hp)`,
    ccm = `ds1.Cubic capacity (ccm)`,
    cylinders = ds1.Cylinders,
    steering = ds1.Steering,
    transmission = ds1.Transmission,
    drive = ds1.Drive,
    fuel = ds1.Fuel,
    color = `ds1.Exterior colour`,
    condition = `ds1.Condition category`,
    price = price
  ) %>%
  mutate(
    # extract power in kw as integer
    power = as.integer(str_extract(power, pattern = "[\\.0-9e-]+")),
    # extract mileage and convert to km as numeric
    mileage_num = gsub(",", "", mileage),
    mileage_num = as.integer(str_extract(mileage_num, pattern = "[\\.0-9e-]+")),
    mileage_unit = str_extract(mileage, pattern = "[a-zA-Z]+"),
    mileage_unit = ifelse(mileage_unit == "km", 1, 1.60934),
    mileage = mileage_num*mileage_unit,
    # extract cubic capacity (ccm) as integer
    ccm = gsub(",", "", ccm),
    ccm = as.integer(str_extract(ccm, pattern = "[\\.0-9e-]+")),
    # create decade
    decade = (as.numeric(year)-1)-(as.numeric(year)-1)%%10,
    # year as factor
    year = as.factor(year),
    # decade as factor
    decade = as.factor(decade),
    # number of cylinders as factor
    cylinders = as.factor(cylinders),
    # manufacturer as factor
    manufacturer = as.factor(manufacturer),
    # model as factor
    model = as.factor(model),
    # body style as factor
    body_style = as.factor(body_style),
    # steering as factor
    steering = as.factor(steering),
    # trainsmission as factor
    transmission = as.factor(transmission),
    # drive as factor
    drive = as.factor(drive),
    # fuel as factor
    fuel = as.factor(fuel)
  ) %>%
  select(
    # clean up
    -c(mileage_num, mileage_unit)
  )

# -----------------------------------------------------------------------------.
# save data ----
# -----------------------------------------------------------------------------.
fwrite(data, "classictrader_newdata.csv")
