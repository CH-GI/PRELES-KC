// deepseek_stream.cpp
#include <Rcpp.h>
#include <curl/curl.h>
#include <string>
#include <sstream>

using namespace Rcpp;

// 回调函数用于处理流式数据
static size_t WriteCallback(void* contents, size_t size, size_t nmemb, void* userp) {
    size_t realsize = size * nmemb;
    std::string* stream = (std::string*)userp;
    stream->append((char*)contents, realsize);

    // 检查是否包含完整消息块
    size_t pos;
    while ((pos = stream->find("\n")) != std::string::npos) {
        std::string line = stream->substr(0, pos);
        stream->erase(0, pos + 1);

        // 如果检测到数据块，处理并输出
        if (line.find("data:") != std::string::npos) {
            Rcpp::Function callback("stream_callback");
            callback(line);
        }
    }

    return realsize;
}

// [[Rcpp::export]]
void call_deepseek_api_stream(const std::string& gpp, const std::string& et, const std::string& sw,
    const std::string& api_key, const std::string& api_url) {
    CURL* curl;
    CURLcode res;
    std::string readBuffer;

    curl = curl_easy_init();
    if (curl) {
        // 构造请求体
        std::stringstream ss;
        ss << "{"
            << "\"model\":\"deepseek-chat\","
            << "\"messages\":["
            << "{\"role\":\"system\",\"content\":\"你是一个经验丰富、专业的森林生态学家\"},"
            << "{\"role\":\"user\",\"content\":\"用中文分析森林生态系统数据：GPP=" << gpp
            << ", ET=" << et << ", SW=" << sw << "。要求：1) 分点说明 2) 指出潜在问题 3) 给出管理建议\"}"
            << "],"
            << "\"temperature\":0.3,"
            << "\"stream\":true"
            << "}";

        struct curl_slist* headers = NULL;
        headers = curl_slist_append(headers, "Content-Type: application/json");
        headers = curl_slist_append(headers, ("Authorization: Bearer " + api_key).c_str());

        curl_easy_setopt(curl, CURLOPT_URL, api_url.c_str());
        curl_easy_setopt(curl, CURLOPT_HTTPHEADER, headers);
        curl_easy_setopt(curl, CURLOPT_POSTFIELDS, ss.str().c_str());
        curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, WriteCallback);
        curl_easy_setopt(curl, CURLOPT_WRITEDATA, &readBuffer);

        res = curl_easy_perform(curl);
        if (res != CURLE_OK) {
            Rcpp::Function callback("stream_callback");
            callback("{\"error\":\"API请求失败\"}");
        }

        curl_slist_free_all(headers);
        curl_easy_cleanup(curl);
    }
}