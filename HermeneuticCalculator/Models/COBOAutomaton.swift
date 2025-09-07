import Foundation

struct COBOAutomaton {
    let a: Int
    let b: Int
    let baseUnit: Int

    // Registers for internal computation
    private var sum: Int = 0
    private var baseCounter: Int = 0
    private var oneCounter: Int = 0

    // State
    private enum State {
        case start, initialize, addBases, addOnes, accept, error
    }
    private var state: State = .start

    // Output
    private(set) var history: [String] = []
    private(set) var jumps: [Jump] = []
    private(set) var finalSum: Int = 0

    init(a: Int, b: Int, base: Int = 10) {
        self.a = a
        self.b = b
        self.baseUnit = base
    }

    private mutating func recordHistory(interpretation: String) {
        history.append(interpretation)
    }

    private mutating func recordJump(from: Int, to: Int, type: Jump.JumpType) {
        let jump = Jump(from: from, to: to, type: type)
        jumps.append(jump)
    }

    private mutating func transition(to nextState: State) {
        state = nextState
    }

    mutating func run() {
        while state != .accept && state != .error {
            switch state {
            case .start:
                executeStart()
            case .initialize:
                executeInitialize()
            case .addBases:
                executeAddBases()
            case .addOnes:
                executeAddOnes()
            case .accept, .error:
                break
            }
        }
        finalSum = sum
    }

    private mutating func executeStart() {
        recordHistory(interpretation: "Start: Problem is \(a) + \(b).")
        transition(to: .initialize)
    }

    private mutating func executeInitialize() {
        sum = a
        baseCounter = b / baseUnit
        oneCounter = b % baseUnit

        let interpretation = "Initialize: Start at \(sum). Decompose \(b) into \(baseCounter) base(s) of \(baseUnit) and \(oneCounter) one(s)."
        recordHistory(interpretation: interpretation)
        transition(to: .addBases)
    }

    private mutating func executeAddBases() {
        if baseCounter > 0 {
            let prevSum = sum
            sum += baseUnit
            baseCounter -= 1

            recordJump(from: prevSum, to: sum, type: .base)
            let interpretation = "Count on by base: \(prevSum) + \(baseUnit) = \(sum)."
            recordHistory(interpretation: interpretation)
            // Stay in the same state
        } else {
            recordHistory(interpretation: "All bases added. Transition to adding ones.")
            transition(to: .addOnes)
        }
    }

    private mutating func executeAddOnes() {
        if oneCounter > 0 {
            let prevSum = sum
            sum += 1
            oneCounter -= 1

            recordJump(from: prevSum, to: sum, type: .one)
            let interpretation = "Count on by one: \(prevSum) + 1 = \(sum)."
            recordHistory(interpretation: interpretation)
            // Stay in the same state
        } else {
            recordHistory(interpretation: "All ones added. Final result is \(sum).")
            transition(to: .accept)
        }
    }
}
