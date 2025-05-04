Sys.setenv("LD_LIBRARY_PATH" = "/usr/lib/x86_64-linux-gnu")

Sys.setenv("PKG_CXXFLAGS" = "-I/usr/include")
Sys.setenv("PKG_LIBS" = "-lnetcdf")
library(Rcpp)
Rcpp::sourceCpp(file = "/home/cheng/project/PRELES-KC/main/nc_read.cpp")
read_nc_header_c("D:\A_PDE\R\KC\PRELES-KC\main\FAPAR.nc")
abc(1,2)

