module main;

import std.stdio;
import std.file;
import std.string;
import std.regex;
import std.conv;
import std.algorithm;
import std.array;

// Структура для одной записи access.log
struct AccessLog {
    string ip;
    string datetime;
    string method;
    string path;
    int    status;
    long   bytesSent;
}

// Парсим одну строку access.log с помощью regex
AccessLog* parseLine(string line) {
    enum pattern = `^(\S+)\s+\S+\s+\S+\s+\[([^\]]+)\]\s+"(\S+)\s+([^"]+?)\s+(\S+)"\s+(\d{3})\s+(\S+)\s+"([^"]*)"\s+"([^"]*)"`; 

    auto m = matchFirst(line, regex(pattern));

    if (m.empty) {
        return null;
    }

    AccessLog log;
    log.ip       = m.captures[1].to!string;
    log.datetime = m.captures[2].to!string;
    log.method   = m.captures[3].to!string;
    log.path     = m.captures[4].to!string;
    log.status   = m.captures[6].to!int;

    string bytesStr = m.captures[7].to!string;
    if (bytesStr == "-" || bytesStr.length == 0) {
        log.bytesSent = 0;
    } else {
        log.bytesSent = bytesStr.to!long;
    }

    // 🔧 dòng quan trọng
    return new AccessLog(
        log.ip,
        log.datetime,
        log.method,
        log.path,
        log.status,
        log.bytesSent
    );
}

void main() {
    string filename = "input.txt";

    if (!exists(filename)) {
        writeln("ERROR: file ", filename, " not found");
        return;
    }

    auto lines = readText(filename).splitLines();

    AccessLog[] logs;
    foreach (line; lines) {
        auto entry = parseLine(line);
        if (entry !is null) {
            logs ~= *entry;
        }
    }

    if (logs.length == 0) {
        writeln("No valid access.log lines found.");
        return;
    }

    // ---- Агрегация статистики ----
    int[string]   countByMethod;
    int[int]      countByStatus;
    int[string]   countByPath;
    int[string]   countByIP;
    long totalBytes = 0;

    foreach (ref e; logs) {
        countByMethod[e.method] += 1;
        countByStatus[e.status] += 1;
        countByPath[e.path]     += 1;
        countByIP[e.ip]         += 1;
        totalBytes              += e.bytesSent;
    }

    // ---- Вывод результата ----
    writeln("=== ACCESS.LOG SUMMARY ===\n");

    writeln("Total requests: ", logs.length);
    writeln("Total bytes sent: ", totalBytes);
    writeln();

    writeln("=== BY HTTP METHOD ===");
    foreach (mtd, cnt; countByMethod) {
        writeln("  ", mtd, ": ", cnt);
    }
    writeln();

    writeln("=== BY STATUS CODE ===");
    // Сортировка по коду статуса
    auto statusCodes = countByStatus.keys.array;
    sort(statusCodes);
    foreach (code; statusCodes) {
        writeln("  ", code, ": ", countByStatus[code]);
    }
    writeln();

    writeln("=== TOP 5 PATHS ===");
    auto pathsSorted = countByPath.byKeyValue.array;
    sort!((a, b) => a.value > b.value)(pathsSorted);
    foreach (i, kv; pathsSorted) {
        if (i >= 5) break;
        writeln("  ", kv.key, " -> ", kv.value, " hits");
    }
    writeln();

    writeln("=== TOP 5 IPs ===");
    auto ipsSorted = countByIP.byKeyValue.array;
    sort!((a, b) => a.value > b.value)(ipsSorted);
    foreach (i, kv; ipsSorted) {
        if (i >= 5) break;
        writeln("  ", kv.key, " -> ", kv.value, " requests");
    }
}
