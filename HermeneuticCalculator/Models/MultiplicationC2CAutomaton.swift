import Foundation

struct MultiplicationC2CAutomaton {
    let n: Int // Number of groups
    let s: Int // Size of groups

    private var g: Int = 0 // Group counter
    private var i: Int = 0 // Item counter
    private var t: Int = 0 // Total counter

    private(set) var history: [String] = []

    init(n: Int, s: Int) {
        self.n = n
        self.s = s
    }

    mutating func run() -> Int {
        history.append("Inputs: \(n) groups of \(s). Initialize counters.")

        while g < n {
            history.append("Starting Group \(g + 1).")
            i = 0
            while i < s {
                i += 1
                t += 1
                history.append("Count: \(t). (Item \(i) in Group \(g + 1)).")
            }
            history.append("Group \(g + 1) finished.")
            g += 1
        }

        history.append("All groups counted. Result = \(t).")
        return t
    }
}
