setwd("D:/2019/bulk_predict/Bulk-TCE-forecast/ver2")

##### Data 불러오기 #####

file_list <- list.files(pattern=".xlsx")
file_path <- paste0(getwd(),"/",file_list)

library(data.table)
library(readxl)

Baltic_TC <- as.data.table(read_excel(path = file_list[1],
                                      col_names = TRUE))

Cape_FFA <- as.data.table(read_excel(path = file_list[2],
                                     sheet = "Cape",
                                     col_names = TRUE,
                                     col_types = c("date", "text", "numeric", "text")))

Pana_FFA <- as.data.table(read_excel(path = file_list[2],
                                     sheet = "Pana",
                                     col_names = TRUE,
                                     col_types = c("date", "text", "numeric", "text")))

Supra_FFA <- as.data.table(read_excel(path = file_list[2],
                                      sheet = "Supra",                                 
                                      col_names = TRUE,
                                      col_types = c("date", "text", "numeric", "text")))

FFA <- Cape_FFA[,.(Date = ArchiveDate, 
                   month = rep(c("CURMON", "1MON"),4972/2),
                   Cape = RouteAverage,
                   Pana = Pana_FFA[,RouteAverage],
                   Supra = Supra_FFA[,RouteAverage],
                   Description = FFADescription)]

Time_Charter <- as.data.table(read_excel(path = file_list[3], 
                                         skip = 1,
                                         col_names = TRUE))

# Date 형식 변환
Baltic_TC[,Date := as.Date(Date)]
FFA[,Date := as.Date(Date)]
Time_Charter[,Date := as.Date(Date)]

# data 통합 #
Cape <- Baltic_TC[,.(Date, Cape_5TC)]
Pana <- Baltic_TC[,.(Date, Pana_4TC)]
Supra <- Baltic_TC[,.(Date, Supra_6TC)]

A <- Time_Charter[,.(year = year(Date), week = week(Date), 
                     Cape_6m, Cape_5y,
                     Pana_6m, Pana_5y, 
                     Supra_6m, Supra_5y)]

B <- Baltic_TC[,.(Date, year = year(Date), week = week(Date))]

C <- B[A,
       .(Date, 
         Cape_6m, Cape_5y,
         Pana_6m, Pana_5y, 
         Supra_6m, Supra_5y),
       on = .(year, week)]

Cape <- Cape[C,,on = .(Date)][-1,c(1:4)]
Pana <- Pana[C,,on = .(Date)][-1,c(1, 2, 5, 6)]
Supra <- Supra[C,,on = .(Date)][-1,c(1, 2, 7, 8)]

Cape <- Cape[FFA[month == "CURMON",.(Date, FFA_CURMON = Cape)], ,on = .(Date),]
Cape <- Cape[FFA[month == "1MON",.(Date, "FFA_1MON" = Cape)], on=.(Date), nomatch = 0]
Cape <- Cape[!is.na(Cape_5TC)]

Pana <- Pana[FFA[month == "CURMON",.(Date, FFA_CURMON = Pana)], ,on = .(Date),]
Pana <- Pana[FFA[month == "1MON",.(Date, "FFA_1MON" = Pana)], on=.(Date), nomatch = 0]
Pana <- Pana[!is.na(Cape_5TC)]

Supra <- Supra[FFA[month == "CURMON",.(Date, FFA_CURMON = Supra)], ,on = .(Date),]
Supra <- Supra[FFA[month == "1MON",.(Date, "FFA_1MON" = Supra)], on=.(Date), nomatch = 0]
Supra <- Supra[!is.na(Cape_5TC)]


####dadsf





