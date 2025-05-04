#include <Rcpp.h>
#include <netcdf.h>

// [[Rcpp::export]]
Rcpp::List read_netCDF(std::string file_path) {
    int ncid, varid;
    int retval;
    size_t len;
    
    // 打开NetCDF文件
    retval = nc_open(file_path.c_str(), NC_NOWRITE, &ncid);
    if (retval != NC_NOERR) {
        Rcpp::stop("Error opening NetCDF file");
    }
    
    // 获取变量ID
    retval = nc_inq_varid(ncid, "your_variable_name", &varid); // 替换为你的变量名
    if (retval != NC_NOERR) {
        Rcpp::stop("Error finding variable in NetCDF file");
    }

    // 获取变量的维度
    retval = nc_inq_dimlen(ncid, 0, &len); // 假设你只有一个维度
    if (retval != NC_NOERR) {
        Rcpp::stop("Error getting dimension length");
    }

    // 读取变量数据
    std::vector<float> data(len);
    retval = nc_get_var_float(ncid, varid, data.data());
    if (retval != NC_NOERR) {
        Rcpp::stop("Error reading variable data");
    }

    // 关闭NetCDF文件
    retval = nc_close(ncid);
    if (retval != NC_NOERR) {
        Rcpp::stop("Error closing NetCDF file");
    }

    return Rcpp::List::create(Rcpp::Named("data") = data);
}
