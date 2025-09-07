import Foundation

// Conversion to Groups Other than Bases (CGOB)
struct DivisionCGOBAAutomaton {
    let t: Int // Dividend
    let s: Int // Divisor
    let base: Int

    private var t_bases: Int = 0
    private var t_ones: Int = 0
    private var quotient: Int = 0
    private var remainder: Int = 0

    private(set) var history: [String] = []

    init(t: Int, s: Int, base: Int = 10) {
        self.t = t
        self.s = s
        self.base = base
    }

    mutating func run() -> Int {
        guard s > 0 else {
            history.append("Error: Divisor must be positive.")
            return 0
        }

        // Decompose T
        t_bases = t / base
        t_ones = t % base
        history.append("Initialize: \(t)/\(s). Decompose T: \(t_bases) Bases + \(t_ones) Ones.")

        // Analyze Base
        let s_in_b = base / s
        let r_in_b = base % s
        history.append("Analyze Base: One Base (\(base)) = \(s_in_b) group(s) of \(s) + Remainder \(r_in_b).")

        // Process Bases
        let q_from_bases = t_bases * s_in_b
        let r_from_bases = t_bases * r_in_b
        quotient += q_from_bases
        remainder += r_from_bases
        history.append("Process \(t_bases) Bases: Yields \(q_from_bases) groups and \(r_from_bases) remainder.")

        // Combine Remainders
        let initial_rem = remainder
        remainder += t_ones
        history.append("Combine Remainders: \(initial_rem) (from Bases) + \(t_ones) (from Ones) = \(remainder).")

        // Process Remainder
        let q_from_r = remainder / s
        let r_final = remainder % s
        quotient += q_from_r
        remainder = r_final
        history.append("Process Remainder: Yields \(q_from_r) additional group(s).")

        history.append("Finished. Total Quotient = \(quotient). Final Remainder = \(remainder).")
        return quotient
    }
}
