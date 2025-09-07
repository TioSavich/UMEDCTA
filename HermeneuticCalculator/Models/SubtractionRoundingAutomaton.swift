import Foundation

struct SubtractionRoundingAutomaton {
    let m: Int
    let s: Int
    let base: Int

    private var k_m: Int = 0
    private var k_s: Int = 0
    private var m_rounded: Int = 0
    private var s_rounded: Int = 0
    private var tempResult: Int = 0

    private(set) var history: [String] = []

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

        // Round M
        k_m = m % base
        m_rounded = m - k_m
        history.append("Round M down: \(m) -> \(m_rounded). (K_M = \(k_m)).")

        // Round S
        k_s = s % base
        s_rounded = s - k_s
        history.append("Round S down: \(s) -> \(s_rounded). (K_S = \(k_s)).")

        // Subtract
        tempResult = m_rounded - s_rounded
        history.append("Intermediate Subtraction: \(m_rounded) - \(s_rounded) = \(tempResult).")

        // Adjust M
        let prev = tempResult
        tempResult += k_m
        history.append("Adjust for M (Add K_M): \(prev) + \(k_m) = \(tempResult).")

        // Adjust S
        let prev2 = tempResult
        tempResult -= k_s
        history.append("Adjust for S (Subtract K_S): \(prev2) - \(k_s) = \(tempResult).")

        return tempResult
    }
}
