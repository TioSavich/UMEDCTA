import Foundation

struct DivisionInverseDistributiveAutomaton {
    let t: Int // Dividend
    let s: Int // Divisor
    let knownFactsDB: [Int: [(Int, Int)]]

    private var remaining: Int
    private var totalQuotient: Int = 0
    private var kb: [(Int, Int)] = []

    private(set) var history: [String] = []

    init(t: Int, s: Int, knownFacts: [Int: [(Int, Int)]]? = nil) {
        self.t = t
        self.s = s
        self.remaining = t

        if let knownFacts = knownFacts {
            self.knownFactsDB = knownFacts
        } else {
            // Create a default knowledge base
            var defaultDB = [Int: [(Int, Int)]]()
            for i in 1...12 {
                defaultDB[i] = [(i*1, 1), (i*2, 2), (i*5, 5), (i*10, 10)]
            }
            self.knownFactsDB = defaultDB
        }
    }

    mutating func run() -> Int {
        guard s > 0 else {
            history.append("Error: Divisor must be positive.")
            return 0
        }

        if let facts = knownFactsDB[s] {
            kb = facts.sorted { $0.0 > $1.0 }
        }
        history.append("Initialize: \(t) / \(s). Loaded known facts for \(s).")

        while remaining > 0 {
            var foundFact = false
            for (multiple, factor) in kb {
                if multiple <= remaining {
                    history.append("Found known multiple: \(multiple) (\(factor) x \(s)).")
                    remaining -= multiple
                    totalQuotient += factor
                    history.append("Applied fact. Subtracted \(multiple). Added \(factor) to Quotient. Remaining: \(remaining)")
                    foundFact = true
                    break
                }
            }
            if !foundFact {
                history.append("Cannot decompose further with known facts. Remainder: \(remaining).")
                break
            }
        }

        history.append("Decomposition complete. Total Quotient = \(totalQuotient).")
        return totalQuotient
    }
}
