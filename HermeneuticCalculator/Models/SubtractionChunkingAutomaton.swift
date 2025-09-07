import Foundation
import Darwin // For log

// MARK: - Strategy A: Chunking Backwards (by Known Part)

struct ChunkingAutomatonA {
    let m: Int, s: Int, base: Int
    private var currentValue: Int = 0
    private var sRemaining: Int = 0
    private(set) var history: [String] = []

    init(m: Int, s: Int, base: Int = 10) {
        self.m = m; self.s = s; self.base = base
    }

    mutating func run() {
        currentValue = m
        sRemaining = s
        history.append("Strategy A: Start at \(m), subtract chunks of \(s).")

        while sRemaining > 0 {
            let power = floor(log(Double(sRemaining)) / log(Double(base)))
            let powerValue = Int(pow(Double(base), power))
            let chunk = (sRemaining / powerValue) * powerValue

            history.append("Subtract chunk: \(currentValue) - \(chunk) = \(currentValue - chunk)")
            currentValue -= chunk
            sRemaining -= chunk
        }
        history.append("Finished. Result: \(currentValue)")
    }
}

// MARK: - Strategy B: Chunking Forwards (from Known Part)

struct ChunkingAutomatonB {
    let m: Int, s: Int, base: Int
    private var currentValue: Int = 0
    private var distance: Int = 0
    private(set) var history: [String] = []

    init(m: Int, s: Int, base: Int = 10) {
        self.m = m; self.s = s; self.base = base
    }

    mutating func run() {
        currentValue = s
        distance = 0
        history.append("Strategy B: Start at \(s), add chunks to get to \(m).")

        while currentValue < m {
            let remaining = m - currentValue
            var chunk = 0

            // Heuristic: find strategic chunk to next base
            let nextBase = ((currentValue / base) + 1) * base
            let k = nextBase - currentValue

            if k > 0 && k <= remaining {
                chunk = k
                history.append("Add strategic chunk to reach base: \(currentValue) + \(chunk) = \(currentValue + chunk)")
            } else {
                let power = floor(log(Double(remaining)) / log(Double(base)))
                let powerValue = Int(pow(Double(base), power))
                chunk = (remaining / powerValue) * powerValue
                chunk = chunk > 0 ? chunk : remaining
                history.append("Add large chunk: \(currentValue) + \(chunk) = \(currentValue + chunk)")
            }
            currentValue += chunk
            distance += chunk
        }
        history.append("Finished. Result (distance): \(distance)")
    }
}

// MARK: - Strategy C: Chunking Backwards (to Known Part)

struct ChunkingAutomatonC {
    let m: Int, s: Int, base: Int
    private var currentValue: Int = 0
    private var distance: Int = 0
    private(set) var history: [String] = []

    init(m: Int, s: Int, base: Int = 10) {
        self.m = m; self.s = s; self.base = base
    }

    mutating func run() {
        currentValue = m
        distance = 0
        history.append("Strategy C: Start at \(m), subtract chunks to get to \(s).")

        while currentValue > s {
            let remaining = currentValue - s
            var chunk = 0

            // Heuristic: find strategic chunk to previous base
            let prevBase = (currentValue / base) * base
            let k = currentValue - prevBase

            if k > 0 && k <= remaining {
                chunk = k
                 history.append("Subtract strategic chunk to reach base: \(currentValue) - \(chunk) = \(currentValue - chunk)")
            } else {
                let power = floor(log(Double(remaining)) / log(Double(base)))
                let powerValue = Int(pow(Double(base), power))
                chunk = (remaining / powerValue) * powerValue
                chunk = chunk > 0 ? chunk : remaining
                history.append("Subtract large chunk: \(currentValue) - \(chunk) = \(currentValue - chunk)")
            }
            currentValue -= chunk
            distance += chunk
        }
        history.append("Finished. Result (distance): \(distance)")
    }
}
