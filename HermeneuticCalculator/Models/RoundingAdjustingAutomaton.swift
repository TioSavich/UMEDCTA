import Foundation

struct RoundingAdjustingAutomaton {
    let a: Int
    let b: Int
    let baseUnit: Int

    // MARK: - Registers
    private var target: Int
    private var other: Int
    private var k: Int = 0
    private var aRounded: Int = 0
    private var tempSum: Int = 0
    private var result: Int = 0

    // Internal registers for subroutines
    private var internalCounter: Int = 0
    private var internalValue: Int = 0

    // MARK: - State
    private enum State {
        case start, calcK, add, adjust, accept, error
    }
    private var state: State = .start

    // MARK: - Output
    private(set) var history: [String] = []
    private(set) var jumps: [Jump] = []
    private(set) var finalSum: Int = 0
    private(set) var startValue: Int = 0

    init(a: Int, b: Int, base: Int = 10) {
        self.a = a
        self.b = b
        self.baseUnit = base

        // Heuristic: Apply the strategy to the number closer to the next base.
        let aRem = a != 0 ? a % base : 0
        let bRem = b != 0 ? b % base : 0

        if aRem >= bRem {
            self.target = a
            self.other = b
        } else {
            self.target = b
            self.other = a
        }
        self.startValue = self.target
    }

    // MARK: - State Machine
    mutating func run() {
        while state != .accept && state != .error {
            switch state {
            case .start: executeStart()
            case .calcK: executeCalcK()
            case .add: executeAdd()
            case .adjust: executeAdjust()
            case .accept, .error: break
            }
        }
        finalSum = result
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
        internalCounter = 0
        internalValue = 0
    }

    // MARK: - State Executors
    private mutating func executeStart() {
        recordHistory("Inputs: \(a), \(b). Target for rounding: \(target)")
        transition(to: .calcK)
    }

    private mutating func executeCalcK() {
        let nextBase = (target == 0 || target % baseUnit == 0) ? target : ((target / baseUnit) + 1) * baseUnit

        if internalValue == 0 {
            internalValue = target
            k = 0
        }

        if internalValue < nextBase {
            internalValue += 1
            k += 1
        } else {
            aRounded = nextBase
            recordHistory("K needed is \(k). Target rounded to \(aRounded).")
            recordJump(from: target, to: aRounded, type: .one) // First jump
            transition(to: .add)
        }
    }

    private mutating func executeAdd() {
        if internalCounter == 0 && internalValue == 0 {
            tempSum = aRounded
            internalValue = other / baseUnit // BaseCounter for COBO
            internalCounter = other % baseUnit // OneCounter for COBO
        }

        if internalValue > 0 {
            tempSum += baseUnit
            internalValue -= 1
            return
        }

        if internalCounter > 0 {
            tempSum += 1
            internalCounter -= 1
            return
        }

        recordHistory("\(aRounded) + \(other) = \(tempSum).")
        recordJump(from: aRounded, to: tempSum, type: .base) // Second jump
        transition(to: .adjust)
    }

    private mutating func executeAdjust() {
        if internalCounter == 0 {
            result = tempSum
            internalCounter = k // Count down K times
        }

        if internalCounter > 0 {
            result -= 1
            internalCounter -= 1
        } else {
            recordHistory("Subtracted K (\(k)). Final Result: \(result).")
            recordJump(from: tempSum, to: result, type: .one) // Third (backward) jump
            transition(to: .accept)
        }
    }
}
