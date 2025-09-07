import Foundation

// Conversion to Bases and Ones (CBO) for Multiplication
struct MultiplicationCBOAutomaton {
    let n: Int
    let s: Int
    let base: Int

    private var groups: [Int]
    private var sourceIdx: Int = 0
    private var targetIdx: Int = 0

    private(set) var history: [String] = []

    init(n: Int, s: Int, base: Int = 10) {
        self.n = n
        self.s = s
        self.base = base
        self.groups = Array(repeating: s, count: n)
    }

    mutating func run() -> Int {
        guard n > 0 else { return 0 }

        history.append("Initialize \(n) groups of \(s). State: \(groups)")

        // Select source
        sourceIdx = n - 1
        history.append("Selected Group \(sourceIdx + 1) as the source.")

        // Loop
        targetIdx = 0
        while true {
            if groups[sourceIdx] == 0 {
                history.append("Source group is empty. Redistribution complete. State: \(groups)")
                break
            }
            if targetIdx >= n {
                history.append("All groups checked. Redistribution complete. State: \(groups)")
                break
            }

            if targetIdx != sourceIdx {
                if groups[targetIdx] < base {
                    groups[sourceIdx] -= 1
                    groups[targetIdx] += 1
                    history.append("Transferred 1 from Group \(sourceIdx + 1) to Group \(targetIdx + 1). State: \(groups)")

                    if groups[targetIdx] == base {
                        targetIdx += 1
                    }
                } else {
                    targetIdx += 1
                }
            } else {
                targetIdx += 1
            }
        }

        // Finalize
        let total = groups.reduce(0, +)
        let bases = groups.filter { $0 >= base }.count
        let ones = total - (bases * base)
        history.append("Final Tally: \(bases) Bases + \(ones) Ones = \(total).")
        return total
    }
}
