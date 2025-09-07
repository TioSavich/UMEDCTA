import Foundation

// Rearranging to Make Bases (RMB)
struct RMBAutomaton {
    let a: Int
    let b: Int
    let baseUnit: Int

    // MARK: - Registers
    private var sumA: Int
    private var sumB: Int
    private var k: Int = 0
    private var aTemp: Int = 0
    private var bTemp: Int = 0

    // MARK: - State
    private enum State {
        case start, calcK, decomposeB, recombine, accept, error
    }
    private var state: State = .start

    // MARK: - Output
    private(set) var history: [String] = []
    private(set) var jumps: [Jump] = []
    private(set) var finalSum: Int = 0

    init(a: Int, b: Int, base: Int = 10) {
        // Heuristic: Apply the strategy to the larger number.
        self.sumA = max(a, b)
        self.sumB = min(a, b)
        self.a = self.sumA // Keep original values for history
        self.b = self.sumB
        self.baseUnit = base
    }

    // MARK: - State Machine
    mutating func run() {
        if state == .start {
            transition(to: .calcK)
        }

        while state != .accept && state != .error {
            switch state {
            case .calcK:
                executeCalcK()
            case .decomposeB:
                executeDecomposeB()
            case .recombine:
                executeRecombine()
            default:
                transition(to: .error)
            }
        }
        finalSum = sumA + sumB
    }

    // MARK: - Private Methods
    private mutating func recordHistory(_ interpretation: String) {
        history.append(interpretation)
    }

    private mutating func recordJump(from: Int, to: Int, type: Jump.JumpType) {
        jumps.append(Jump(from: from, to: to, type: type))
    }

    private mutating func transition(to nextState: State) {
        state = nextState
    }

    // MARK: - State Executors
    private mutating func executeCalcK() {
        let targetBase = ((sumA / baseUnit) + 1) * baseUnit

        if aTemp == 0 {
            aTemp = sumA
            k = 0
            recordHistory("Initialize K calc: Start counting up from \(sumA) to Target Base (\(targetBase)).")
        }

        if aTemp < targetBase {
            aTemp += 1
            k += 1
            // This is an internal thought process, so we don't add to public history
        } else if aTemp == targetBase {
            recordHistory("K needed to reach base is \(k).")
            transition(to: .decomposeB)
        }
    }

    private mutating func executeDecomposeB() {
        let kNeeded = k

        if k > 0 && bTemp == 0 {
            bTemp = sumB
             recordHistory("Initialize B decomp: Start counting down K (\(k)) from B (\(sumB)).")
        }

        if k > 0 && bTemp > 0 {
            bTemp -= 1
            k -= 1
            // Internal thought process
        } else if k == 0 {
            let prevSumA = sumA
            sumA = aTemp // sumA is now the target base
            sumB = bTemp // sumB is the remainder

            // Record the jump for the part that was moved from B to A
            recordJump(from: prevSumA, to: sumA, type: .one)

            recordHistory("Decomposition Complete. Transferred \(kNeeded). New state: A=\(sumA), B=\(sumB).")
            transition(to: .recombine)
        } else if k > 0 && bTemp == 0 {
            recordHistory("Strategy Failed: B (\(b)) is too small to provide K (\(kNeeded)).")
            transition(to: .error)
        }
    }

    private mutating func executeRecombine() {
        let result = sumA + sumB

        // Record the final jump
        if sumB > 0 {
            recordJump(from: sumA, to: result, type: .base)
        }

        recordHistory("Recombine: Combine rearranged numbers: \(sumA) + \(sumB) = \(result).")
        transition(to: .accept)
    }
}
