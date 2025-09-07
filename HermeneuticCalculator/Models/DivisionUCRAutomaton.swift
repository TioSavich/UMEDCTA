import Foundation

// Using Commutative Reasoning (UCR), also known as Dealing in Rounds
struct DivisionUCRAutomaton {
    let e: Int // Total items (Dividend)
    let g: Int // Number of groups (Divisor)

    private var t: Int = 0 // Accumulated total
    private var q: Int = 0 // Items per group (Quotient)

    private(set) var history: [String] = []

    init(e: Int, g: Int) {
        self.e = e
        self.g = g
    }

    mutating func run() -> Int {
        guard g > 0 else {
            history.append("Error: Cannot divide by zero.")
            return 0
        }

        history.append("Initialize: E=\(e), G=\(g).")

        while t < e {
            t += g
            q += 1
            history.append("Distribute round \(q). Total distributed: \(t).")
        }

        if t == e {
            history.append("Total reached. Problem solved. Output Q=\(q).")
            return q
        } else {
            history.append("Error: Accumulated total exceeded E. Not perfectly divisible.")
            return 0 // Or handle remainder
        }
    }
}
