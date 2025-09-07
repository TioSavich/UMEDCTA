import Foundation

struct MultiplicationDistributiveAutomaton {
    let n: Int
    let s: Int
    let base: Int

    private var s1: Int = 0
    private var s2: Int = 0
    private var p1: Int = 0
    private var p2: Int = 0

    private(set) var history: [String] = []

    init(n: Int, s: Int, base: Int = 10) {
        self.n = n
        self.s = s
        self.base = base
    }

    private func heuristicSplit(value: Int) -> (Int, Int) {
        let easyNumbers = [base, base / 2, 2, 1].filter { $0 > 0 }
        for easyNum in easyNumbers {
            if value > easyNum {
                return (easyNum, value - easyNum)
            }
        }
        return (value, 0)
    }

    mutating func run() -> Int {
        history.append("Inputs: \(n) x \(s).")

        (s1, s2) = heuristicSplit(value: s)

        if s2 > 0 {
            history.append("Split S (\(s)) into \(s1) + \(s2).")
        } else {
            history.append("S (\(s)) is easy. No split needed.")
        }

        // Calculate P1
        var counter = n
        history.append("Initializing calculation of P1 (\(n) x \(s1)).")
        while counter > 0 {
            p1 += s1
            counter -= 1
        }
        history.append("P1 complete. P1 = \(p1).")

        // Calculate P2
        if s2 > 0 {
            counter = n
            history.append("Initializing calculation of P2 (\(n) x \(s2)).")
            while counter > 0 {
                p2 += s2
                counter -= 1
            }
            history.append("P2 complete. P2 = \(p2).")
        }

        let total = p1 + p2
        history.append("Summing partials: \(p1) + \(p2) = \(total).")
        return total
    }
}
