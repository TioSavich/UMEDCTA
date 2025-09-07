import Foundation

struct DivisionDealingByOnesAutomaton {
    let t: Int // Total
    let n: Int // Number of groups

    private var remaining: Int
    private var groups: [Int]
    private var currentIdx: Int = 0

    private(set) var history: [String] = []

    init(t: Int, n: Int) {
        self.t = t
        self.n = n
        self.remaining = t
        self.groups = Array(repeating: 0, count: n)
    }

    mutating func run() -> Int {
        guard n > 0 else {
            history.append("Error: Cannot divide by zero.")
            return 0
        }

        history.append("Initialize: \(t) items to deal into \(n) groups.")

        while remaining > 0 {
            groups[currentIdx] += 1
            remaining -= 1
            history.append("Dealt 1 item to Group \(currentIdx + 1). State: \(groups)")
            currentIdx = (currentIdx + 1) % n
        }

        let result = n > 0 ? groups[0] : 0
        history.append("Dealing complete. Result: \(result) per group.")
        return result
    }
}
