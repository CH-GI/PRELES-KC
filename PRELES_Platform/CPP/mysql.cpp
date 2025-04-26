#include <jdbc/cppconn/statement.h>
#include <jdbc/cppconn/resultset.h>
#include <jdbc/cppconn/exception.h>
#include "jdbc/mysql_driver.h"
#include <windows.h>
int main() {
    std::string sql1 = "create database testmysql";//sql1���ڴ���testmysql���ݿ�
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
        std::cout << "���ݿ����ӳ����������ǲ�����������û���д����?����������ݿ����ƻ��߱�����д����?" << std::endl;
    }
    return 0;
}