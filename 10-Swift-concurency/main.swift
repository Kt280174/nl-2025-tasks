import Foundation

// --------------------- Structures ---------------------

struct LogRecord {
    let timestamp: Date
    let level: String
    let module: String
    let message: String
}

struct ChunkStats {
    var countByLevel: [String: Int] = [:]
    var countByModule: [String: Int] = [:]
    var countByHour: [Int: Int] = [:]
    var messageFrequency: [String: Int] = [:]

    mutating func merge(with other: ChunkStats) {
        for (k,v) in other.countByLevel { countByLevel[k, default: 0] += v }
        for (k,v) in other.countByModule { countByModule[k, default: 0] += v }
        for (k,v) in other.countByHour   { countByHour[k, default: 0] += v }
        for (msg,v) in other.messageFrequency {
            messageFrequency[msg, default: 0] += v
        }
    }
}

// --------------------- Parsing ---------------------

let formatter: DateFormatter = {
    let f = DateFormatter()
    f.dateFormat = "yyyy-MM-dd HH:mm:ss"
    f.locale = Locale(identifier: "en_US_POSIX")
    return f
}()

func parseLine(_ line: String) -> LogRecord? {
    let pattern = #"\[(.*?)\] \[(.*?)\] \[(.*?)\] (.*)"#
    guard let regex = try? NSRegularExpression(pattern: pattern) else { return nil }

    if let m = regex.firstMatch(in: line, range: NSRange(line.startIndex..., in: line)) {
        let ts = String(line[Range(m.range(at: 1), in: line)!])
        let level = String(line[Range(m.range(at: 2), in: line)!])
        let module = String(line[Range(m.range(at: 3), in: line)!])
        let msg = String(line[Range(m.range(at: 4), in: line)!])

        guard let date = formatter.date(from: ts) else { return nil }
        return LogRecord(timestamp: date, level: level, module: module, message: msg)
    }
    return nil
}

// ----------------------- Worker -----------------------

func analyzeChunk(_ logs: [LogRecord]) -> ChunkStats {
    var stats = ChunkStats()

    for log in logs {
        stats.countByLevel[log.level, default: 0] += 1
        stats.countByModule[log.module, default: 0] += 1

        let hour = Calendar.current.component(.hour, from: log.timestamp)
        stats.countByHour[hour, default: 0] += 1

        stats.messageFrequency[log.message, default: 0] += 1
    }

    return stats
}

func chunk<T>(_ array: [T], parts: Int) -> [[T]] {
    if parts <= 1 { return [array] }
    let size = max(1, array.count / parts)
    var result: [[T]] = []
    var start = 0

    while start < array.count {
        let end = min(start + size, array.count)
        result.append(Array(array[start..<end]))
        start = end
    }
    return result
}

// ------------------------ MAIN (TOP LEVEL) ------------------------

let file = "/uploads/input.txt"

guard let content = try? String(contentsOfFile: file, encoding: .utf8) else {
    print("Cannot read \(file)")
    exit(0)
}

let lines = content.split(separator: "\n").map(String.init)
let logs = lines.compactMap(parseLine)

if logs.isEmpty {
    print("No valid logs found.")
    exit(0)
}

let workers = 4
let chunks = chunk(logs, parts: workers)

var globalStats = ChunkStats()

// Vì top-level không hỗ trợ async/await → dùng Task + group.wait()
let group = DispatchGroup()
let mergeQueue = DispatchQueue(label: "merge")

for ch in chunks {
    group.enter()
    DispatchQueue.global().async {
        let partial = analyzeChunk(ch)
        mergeQueue.sync {
            globalStats.merge(with: partial)
        }
        group.leave()
    }
}

group.wait()

// ---------------- OUTPUT ----------------

print("=== LEVEL SUMMARY ===")
for (lvl, cnt) in globalStats.countByLevel {
    print("  \(lvl): \(cnt)")
}

print("\n=== MODULE SUMMARY ===")
for (mod, cnt) in globalStats.countByModule {
    print("  \(mod): \(cnt)")
}

print("\n=== HOURLY TRAFFIC ===")
for h in globalStats.countByHour.keys.sorted() {
    print("  Hour \(h): \(globalStats.countByHour[h]!) logs")
}

print("\n=== TOP 5 MESSAGES ===")
let top = globalStats.messageFrequency
    .sorted { $0.value > $1.value }
    .prefix(5)

for (msg, cnt) in top {
    print("  [\(cnt)x] \(msg)")
}
