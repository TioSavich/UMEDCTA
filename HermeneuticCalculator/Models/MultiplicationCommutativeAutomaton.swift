import Foundation

struct MultiplicationCommutativeAutomaton {
    var a: Int
    var b: Int
    let base: Int

    private var groups: Int = 0
    private var itemsPerGroup: Int = 0
    private var total: Int = 0

    private(set) var history: [String] = []

    init(a: Int, b: Int, base: Int = 10) {
        self.a = a
        self.b = b
        self.base = base
    }

    private func heuristic(groups: Int, items: Int) -> Int {
        var difficulty = 0
        let isEasyItem = (items == 1) || (items == base) || (base % 2 == 0 && items == base / 2)
        if !isEasyItem {
            difficulty += 100
        }
        difficulty += groups
        return difficulty
    }

    mutating func run() -> Int {
        history.append("Inputs: \(a) x \(b).")

        let h_ab = heuristic(groups: a, items: b)
        let h_ba = heuristic(groups: b, items: a)

        history.append("Evaluating: H(\(a)x\(b))=\(h_ab) vs H(\(b)x\(a))=\(h_ba).")

        if h_ba < h_ab {
            history.append("Heuristic suggests commuting is easier.")
            groups = b
            itemsPerGroup = a
        } else {
            history.append("Heuristic suggests original is optimal or equal.")
            groups = a
            itemsPerGroup = b
        }
        history.append("Repackaged as \(groups) x \(itemsPerGroup).")

        // Iterative calculation
        var counter = groups
        while counter > 0 {
            total += itemsPerGroup
            counter -= 1
            history.append("Iterate: Added \(itemsPerGroup). Total = \(total).")
        }

        history.append("Calculation complete. Result = \(total).")
        return total
    }
}
