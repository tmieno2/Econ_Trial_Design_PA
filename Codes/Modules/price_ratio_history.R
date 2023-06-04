## ==============================================================
##                          Preparation                        =
## ==============================================================

rm(list = ls())

# === Packages ===#
library(data.table)
library(tidyverse)
library(dplyr)
library(magrittr)
library(stringr)
library(ggplot2)
library(here)
options(stringsAsFactors = FALSE)

# === Set working directory ===#
setwd(here())



# /*===========================================================
#' # Load csv data
# /*===========================================================
# ! Define field and plot characteristics here

#=== corn price ===#
NASS_corn_price <- read.csv(here("Shared/Data/price_data/NASS_corn_price.csv")) %>% 
    data.table() %>% 
    .[, .(Year, Period, Value)] %>% 
    .[Period!="MARKETING YEAR", ] %>% 
    #=== $/bushel ===
    setnames("Value", "pCorn") %>% 
    print()

#=== N fertilizer price (2002-2014) ===#
NASS_fertilizer_price <- read.csv(here("Shared/Data/price_data/NASS_fertilizer_price_yearly.csv")) %>% 
    data.table() %>% 
    .[, .(Year, Data.Item, Value)] %>% 
    setnames(c("Data.Item", "Value"), c("N_type", "pN_index")) %>% 
    .[str_detect(N_type, "ANHYDROUS AMMONIA"), N_type := "ANHYD"] %>% 
    .[str_detect(N_type, "28%"), N_type := "UAN28"] %>% 
    .[str_detect(N_type, "32%"), N_type := "UAN32"] %>% 
    print()

#=== N fertilizer price index ===#
NASS_fertilizer_index <- read.csv(here("Shared/Data/price_data/NASS_fertilizer_index.csv")) %>% 
    data.table() %>% 
    .[, .(Year, Period, Value)] %>% 
    setnames("Value", "pN_index") %>% 
    print()

#=== reference N fertilizer price ===#
DTN_weekly <- read.csv(here("Shared/Data/price_data/DTN Fertilizer Trends_weekly.csv")) %>% 
    data.table() %>% 
    .[, Year := substr(Date.Range, nchar(Date.Range)-3, nchar(Date.Range))] %>% 
    .[, Week := substr(Date.Range, 1, nchar(Date.Range)-5)] %>% 
    .[, Month := substr(Date.Range, 1, 3)] %>% 
    .[, Date.Range := NULL] %>% 
    .[, Year := as.numeric(Year)] %>% 
    .[, ANHYD := as.numeric(ANHYD)] %>% 
    .[, UAN28 := as.numeric(UAN28)] %>% 
    .[, UAN32 := as.numeric(UAN32)] %>% 
    #=== convert to $/lb ===
    .[, ANHYD_lb := ANHYD / (2000*0.82)] %>% 
    .[, UAN28_lb := UAN28 / (2000*0.28)] %>% 
    .[, UAN32_lb := UAN32 / (2000*0.34)] %>% 
    print()

#=== check unit conversion ===#
convert_df <- 
    data.table(
    fertilizer = c("ANHYD", "UAN28", "UAN32"),
    dollar_per_ton = c(1523, 622, 698),
    dollar_per_lb = c(0.93, 1.11, 1.09)
    ) %>% 
    rbind(
        data.table(
            fertilizer = c("ANHYD", "UAN28", "UAN32"),
            dollar_per_ton = c(1520, 610, 706),
            dollar_per_lb = c(0.93, 1.09, 1.10)
        )
    ) %>%
    rbind(
        data.table(
            fertilizer = c("ANHYD", "UAN28", "UAN32"),
            dollar_per_ton = c(492, 235, 278),
            dollar_per_lb = c(0.3, 0.42, 0.43)
        )
    ) %>% 
    rbind(
        data.table(
            fertilizer = c("ANHYD", "UAN28", "UAN32"),
            dollar_per_ton = c(565, 265, 304),
            dollar_per_lb = c(0.34, 0.47, 0.48)
        )
    ) %>% 
    .[, convert := dollar_per_ton / dollar_per_lb] %>% 
    .[, mean_convert := mean(convert), by = fertilizer] %>% 
    #=== my calculation ===
    .[fertilizer=="ANHYD", my_per_lb := dollar_per_ton / (2000*0.82)] %>% 
    .[fertilizer=="UAN28", my_per_lb := dollar_per_ton / (2000*0.28)] %>% 
    .[fertilizer=="UAN32", my_per_lb := dollar_per_ton / (2000*0.32)] %>% 
    print()

# => decimals made some differences in conversion coefficients 
# => my conversion formula should be OK


# /*===========================================================
#' # Retrieve monthly N price data
# /*===========================================================

#=== Reference prices: 2022 Jan and Feb prices
N_price_2022 <- DTN_weekly %>% 
    #=== 2022 Jan and Feb prices
    .[Year=="2022", ] %>% 
    .[order(Year, Month), ] %>% 
    print()
N_price_2022Jan <- N_price_2022[Month=="Jan", 
                                lapply(.SD, mean), 
                                .SDcols = c("ANHYD_lb","UAN28_lb","UAN32_lb")
                                ] %>% 
    rowMeans()
N_price_2022Feb <- N_price_2022[Month=="Feb", 
                                lapply(.SD, mean), 
                                .SDcols = c("ANHYD_lb","UAN28_lb","UAN32_lb")
] %>% 
    rowMeans()

#*********************************************
#* Not sure how the total price "index" was calculated. I cannot get
#* the $/lb prices that are exactly proportional to the index.
N_price_2022Jan / N_price_2022Feb
NASS_fertilizer_index[Year==2022 & Period =="JAN", pN_index] / 
    NASS_fertilizer_index[Year==2022 & Period =="FEB", pN_index]
#*********************************************

#=== turn price index to $/lb
pN_index_2022Jan <- NASS_fertilizer_index[Year==2022 & Period =="JAN", pN_index]
NASS_fertilizer_index <- NASS_fertilizer_index %>% 
    #=== use 2022 Jan as the reference price ===
    .[, pN := N_price_2022Jan / pN_index_2022Jan * pN_index] %>% 
    print()


#=== compare: NASS 2002-2014 data
NASS_fertilizer_price_wide <- NASS_fertilizer_price %>% 
    dcast(Year ~ N_type, value.var = "pN_index") %>% 
    #=== convert to $/lb ===
    .[, ANHYD_lb := ANHYD / (2000*0.82)] %>% 
    .[, UAN28_lb := UAN28 / (2000*0.28)] %>% 
    .[, UAN32_lb := UAN32 / (2000*0.34)] %>% 
    .[, c("ANHYD", "UAN28", "UAN32") := NULL] %>% 
    print()


#=== DTN weekly data to monthly
DTN_monthly <- DTN_weekly %>% 
    dplyr::select(Year, Month, ANHYD_lb, UAN28_lb, UAN32_lb) %>% 
    .[, lapply(.SD, mean), .SDcols = c("ANHYD_lb","UAN28_lb","UAN32_lb"),
      by = .(Year, Month)] %>% 
    .[, Month := toupper(Month)] %>% 
    setnames("Month", "Period") %>% 
    merge(.,     
          data.table(
              Period = c("JAN", "FEB", "MAR", "APR", "MAY", "JUN",
                         "JUL", "AUG", "SEP", "OCT", "NOV", "DEC"),
              month = c(1,2,3,4,5,6,7,8,9,10,11,12)),
          by = "Period"
    ) %>% 
    .[, time := Year + month / 12] %>% 
    print()


# /*===========================================================
#' # Price ratio history
# /*===========================================================
#* price ratio: N price divided by corn price

#=== Full history retrieved by index: 2022 Jan and Feb prices as reference
price_ratio_data <- NASS_corn_price %>% 
    .[NASS_fertilizer_index, on = c("Year", "Period")] %>% 
    #=== price ratio
    .[, pRatio := pN / pCorn] %>% 
    print()


#=== graph of historical trend
month_data <- copy(price_ratio_data) %>% 
    merge(.,     
          data.table(
              Period = c("JAN", "FEB", "MAR", "APR", "MAY", "JUN",
                         "JUL", "AUG", "SEP", "OCT", "NOV", "DEC"),
              month = c(1,2,3,4,5,6,7,8,9,10,11,12)),
          by = "Period"
    ) %>% 
    .[, time := Year + month / 12] %>% 
    # .[, month := 1:.N, by = .(Year)] %>% 
    # .[, month := as.character(month)] %>% 
    # .[nchar(month)==1, month := paste0("0", month)] %>% 
    # .[, time := paste0(Year, month) %>% as.numeric()] %>% 
    print()
ggplot(data = month_data) +
    geom_line( aes(x = time, y = pCorn), color = "green4") +
    scale_y_continuous(
        name = "Corn price ($/bushel)"
    )
color_legend <- c("NASS index" = "red",
                  "ANHYD" = "purple",
                  "UAN28" = "blue", 
                  "UAN32" = "green4")
ggplot() +
    geom_line(data = month_data, aes(x = time, y = pN, color = "NASS index")) +
    geom_line(data = NASS_fertilizer_price_wide, aes(x = Year, y = ANHYD_lb, color = "ANHYD")) +
    geom_line(data = NASS_fertilizer_price_wide, aes(x = Year, y = UAN28_lb, color = "UAN28")) +
    geom_line(data = NASS_fertilizer_price_wide, aes(x = Year, y = UAN32_lb, color = "UAN32")) +
    geom_line(data = DTN_monthly, aes(x = time, y = ANHYD_lb), color = "purple") +
    geom_line(data = DTN_monthly, aes(x = time, y = UAN28_lb), color = "blue") +
    geom_line(data = DTN_monthly, aes(x = time, y = UAN32_lb), color = "green4") +
    scale_y_continuous(name = "N price ($/lb)") +
    scale_color_manual(values = color_legend)
ggsave(here("Shared/Data/price_data/N_price_history.png"),
       height = 4, width = 8.5, units = "in")



#=== It appears the index retrieved N prices are too high

#=== Part of history: Merge NASS and DTN data
NASS_fertilizer_price_wide <- expand_grid(
    NASS_fertilizer_price_wide,
    data.frame(
        Period = c("JAN", "FEB", "MAR", "APR", "MAY", "JUN",
                   "JUL", "AUG", "SEP", "OCT", "NOV", "DEC"),
        month = c(1,2,3,4,5,6,7,8,9,10,11,12))
    ) %>% 
    data.table() %>% 
    .[, time := Year + month / 12]

N_price_frac <- rbind(NASS_fertilizer_price_wide, DTN_monthly)

price_ratio_data <- NASS_corn_price %>% 
    merge(., N_price_frac, by = c("Year", "Period")) %>% 
    #=== to long format
    melt(id.vars = c("Year", "Period", "month", "time", "pCorn")) %>% 
    setnames("value", "pN") %>% 
    #=== price ratio
    .[, pRatio := pN / pCorn] %>% 
    print()

# monthly distribution
price_ratio_data$pRatio %>% quantile(na.rm=TRUE)
price_ratio_data$pRatio %>% quantile(c(0.05, 0.5, 0.95),na.rm=TRUE)
# yearly distribution
pRatio_yearly <- price_ratio_data[, .(pRatio = mean(pRatio)), by = Year]$pRatio
pRatio_yearly %>% quantile(na.rm=TRUE)
pRatio_yearly %>% quantile(c(0.05, 0.5, 0.95),na.rm=TRUE)

ggplot(data = price_ratio_data) +
    geom_line(aes(x = time, y = pRatio, color = variable)) +
    ylab("N-Corn price ratio (in $/lb)") 
ggsave(here("Shared/Data/price_data/price_ratio_history_lb.png"),
       height = 4, width = 8.5, units = "in")

#=== convert price unit to $/kg
price_ratio_data <- price_ratio_data %>% 
    .[, pCorn := pCorn / 25.4] %>% 
    .[, pN := pN / 0.453592] %>%
    .[, pRatio := pN / pCorn] %>% 
    print()
ggplot(data = price_ratio_data) +
    geom_line(aes(x = time, y = pRatio, color = variable)) +
    ylab("N-Corn price ratio (in $/kg)") 
ggsave(here("Shared/Data/price_data/price_ratio_history.png"),
       height = 4, width = 8.5, units = "in")


#=== corn and N prices together

#=== full history
color_legend <- c("N fertilzer" = "red",
                  "Corn" = "green4")
sec_scale <- 10
ggplot(data = month_data, aes(x = time)) +
    geom_line( aes(y = pCorn, color = "Corn")) +
    geom_line( aes(y = pN * sec_scale, color = "N fertilzer")) +
    scale_y_continuous(
        # 1st axis
        name = "Corn price ($/bushel)",
        # 2nd axis
        sec.axis = sec_axis(~ . / sec_scale, name = "N price ($/lb)")
    ) +
    scale_color_manual(values = color_legend, name = "price") +
    theme_classic()

#=== 2002-2021
color_legend <- c("N fertilzer" = "red",
                  "Corn" = "green4")
sec_scale <- 0.15
g1 <- 
    price_ratio_data %>% 
    .[, .(pN = mean(pN)),
      by = .(time, pCorn)
      ] %>% 
    ggplot(data = ., aes(x = time)) +
    geom_line( aes(y = pCorn, color = "Corn")) +
    geom_line( aes(y = pN * sec_scale, color = "N fertilzer")) +
    scale_y_continuous(
        # 1st axis
        name = "Corn price ($/bushel)",
        # 2nd axis
        sec.axis = sec_axis(~ . / sec_scale, name = "N price ($/lb)")
    ) +
    scale_color_manual(values = color_legend, name = "price") +
    theme_classic()
color_legend <- c("N / Corn" = "black")
g2 <- 
    price_ratio_data %>% 
    .[, .(pN = mean(pN)),
      by = .(time, pCorn)
    ] %>% 
    .[, pRatio := pN / pCorn] %>% 
    ggplot(data = ., aes(x = time)) +
    geom_line( aes(y = pRatio, color = "N / Corn")) +
    scale_y_continuous(
        # 1st axis
        name = "N / Corn price ratio ($/kg) ($/kg)^-1",
        # 2nd axis
        sec.axis = sec_axis(~ . / sec_scale, name = "")
    ) +
    scale_color_manual(values = color_legend, name = "") +
    theme_classic()
library(ggpubr)
ggarrange(g1, g2, ncol = 1)
ggsave(here("Shared/Data/price_data/comb_price_history.png"),
       height = 8, width = 8.5, units = "in")


#* save the price ratio data
saveRDS(price_ratio_data, here("Shared/Data/price_ratio_data.rds"))
