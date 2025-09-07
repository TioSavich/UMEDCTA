import Foundation

struct SubtractionDecompositionAutomaton {
    let m: Int
    let s: Int
    let base: Int

    private var r_t: Int = 0
    private var r_o: Int = 0
    private let s_t: Int
    private let s_o: Int

    private(set) var history: [String] = []

    init(m: Int, s: Int, base: Int = 10) {
        self.m = m
        self.s = s
        self.base = base
        self.s_t = s / base
        self.s_o = s % base
    }

    mutating func run() -> Int {
        guard s <= m else {
            history.append("Error: Subtrahend cannot be greater than Minuend.")
            return 0
        }

        // Init
        r_t = m / base
        r_o = m % base
        history.append("Decompose M (\(r_t)T+\(r_o)O) and S (\(s_t)T+\(s_o)O).")

        // Subtract Bases
        let initial_r_t = r_t
        r_t -= s_t
        history.append("Subtract Bases: \(initial_r_t)T - \(s_t)T = \(r_t)T.")

        // Check and Decompose
        if r_o < s_o {
            history.append("Insufficient Ones (\(r_o) < \(s_o)). Need decomposition.")
            if r_t > 0 {
                r_t -= 1
                r_o += base
                history.append("Decomposed 1 Ten. New state: \(r_t)T, \(r_o)O.")
            } else {
                history.append("Error: Cannot decompose further.")
                return 0
            }
        }

        // Subtract Ones
        let prev_o = r_o
        r_o -= s_o
        history.append("Subtract Ones: \(prev_o)O - \(s_o)O = \(r_o)O.")

        // Final result
        let result = r_t * base + r_o
        history.append("Accept. Final Result: \(result).")
        return result
    }
}
