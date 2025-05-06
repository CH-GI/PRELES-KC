
source("main/functions.R")
library(ncdf4)

co2_file <- nc_open("main/20220101-C3S-L2_CO2_midtrop-GHG_PRODUCTS-IASI-METOPB-NLIS-DAILY-v10.1.nc")
print(co2_file)

print(get_fapar_at_point("main/12121212.nc",120,120))




library(ncdf4)
file <- nc_open("main/12121212.nc")  # 请替换为你的 VPD 文件路径
print(file)

















