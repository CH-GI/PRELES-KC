#include <jdbc/cppconn/statement.h>
#include <jdbc/cppconn/resultset.h>
#include <jdbc/cppconn/exception.h>
#include "jdbc/mysql_driver.h"
#include <windows.h>
int main() {
    std::string sql1 = "create database testmysql";//sql1用于创建testmysql数据库
    SetConsoleOutputCP(CP_UTF8);
    try
    {
        sql::mysql::MySQL_Driver* driver = sql::mysql::get_mysql_driver_instance();
        sql::Connection* con;
        sql::Statement* stmt;
        sql::ResultSet* res;
        con = driver->connect("tcp://localhost:3306", "root", "2145");
        stmt = con->createStatement();
        stmt->execute(sql1);
        delete stmt;
        delete con;
    }
    catch (sql::SQLException& sqle)
    {
        std::cout << "数据库连接出错啦，你是不是密码或者用户名写错了?或者你的数据库名称或者表名称写错了?" << std::endl;
    }
    return 0;
}