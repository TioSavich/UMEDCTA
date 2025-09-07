import Foundation

struct SubtractionSlidingAutomaton {
    let m: Int
    let s: Int
    let base: Int

    private var k: Int = 0
    private var m_adj: Int = 0
    private var s_adj: Int = 0

    private(set) var history: [String] = []
    private(set) var jumps: [Jump] = []

    init(m: Int, s: Int, base: Int = 10) {
        self.m = m
        self.s = s
        self.base = base
    }

    mutating func run() -> Int {
        guard s <= m else {
            history.append("Error: Subtrahend cannot be greater than Minuend.")
            return 0
        }

        // Calculate K
        let targetBase = (s > 0 && s % base != 0) ? ((s / base) + 1) * base : s
        k = targetBase - s
        history.append("K needed to reach base is \(k).")

        // Record the "slide" as two simultaneous jumps
        jumps.append(Jump(from: s, to: s + k, type: .one))
        jumps.append(Jump(from: m, to: m + k, type: .one))

        // Adjust
        s_adj = s + k
        m_adj = m + k
        history.append("Sliding both by +\(k). New problem: \(m_adj) - \(s_adj).")

        // Subtract
        let result = m_adj - s_adj
        history.append("Perform Subtraction: \(m_adj) - \(s_adj) = \(result).")

        return result
    }
}
