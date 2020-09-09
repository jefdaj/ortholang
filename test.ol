proper = curl_date "2020-09-09" "https://haxx.se/curl"
today = curl "today" "https://haxx.se/curl"
cached = curl "cached" "https://haxx.se/curl"
fail = curl "this is not a valid date" "https://haxx.se/curl"
result = [cached, fail, today, proper]
