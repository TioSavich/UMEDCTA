import Foundation

// This file will contain the logic for both COBO (Missing Addend) and CBBO (Take Away) subtraction strategies.

// MARK: - COBO (Missing Addend)

struct COBOMissingAddendAutomaton {
    let m: Int // Minuend (Whole)
    let s: Int // Subtrahend (Known Part)
    let baseUnit: Int

    private var currentValue: Int = 0
    private var distance: Int = 0

    private enum State { case start, addBases, addOnes, accept, error }
    private var state: State = .start

    private(set) var history: [String] = []
    private(set) var jumps: [Jump] = []

    init(m: Int, s: Int, base: Int = 10) {
        self.m = m
        self.s = s
        self.baseUnit = base
    }

    mutating func run() {
        guard s <= m else {
            history.append("Error: Subtrahend cannot be greater than Minuend.")
            state = .error
            return
        }

        executeStart()

        while state != .accept && state != .error {
            switch state {
            case .addBases: executeAddBases()
            case .addOnes: executeAddOnes()
            default: break
            }
        }
    }

    private mutating func executeStart() {
        currentValue = s
        distance = 0
        history.append("COBO (Missing Addend): Start at \(s), count up to \(m).")
        transition(to: .addBases)
    }

    private mutating func executeAddBases() {
        if currentValue + baseUnit <= m {
            let prev = currentValue
            currentValue += baseUnit
            distance += baseUnit
            jumps.append(Jump(from: prev, to: currentValue, type: .base))
            history.append("Add Base (+\(baseUnit)): \(currentValue). Distance: \(distance)")
        } else {
            history.append("Next base overshoots. Switching to ones.")
            transition(to: .addOnes)
        }
    }

    private mutating func executeAddOnes() {
        if currentValue < m {
            let prev = currentValue
            currentValue += 1
            distance += 1
            jumps.append(Jump(from: prev, to: currentValue, type: .one))
            history.append("Add One (+1): \(currentValue). Distance: \(distance)")
        } else {
            history.append("Target reached. Result (Distance) = \(distance).")
            transition(to: .accept)
        }
    }

    private mutating func transition(to nextState: State) {
        state = nextState
    }
}


// MARK: - CBBO (Take Away)

struct CBBOTakeAwayAutomaton {
    let m: Int // Minuend (Whole)
    let s: Int // Subtrahend (Known Part)
    let baseUnit: Int

    private var currentValue: Int = 0
    private var baseCounter: Int = 0
    private var oneCounter: Int = 0

    private enum State { case start, subBases, subOnes, accept, error }
    private var state: State = .start

    private(set) var history: [String] = []
    private(set) var jumps: [Jump] = []

    init(m: Int, s: Int, base: Int = 10) {
        self.m = m
        self.s = s
        self.baseUnit = base
    }

    mutating func run() {
        guard s <= m else {
            history.append("Error: Subtrahend cannot be greater than Minuend.")
            state = .error
            return
        }

        executeStart()

        while state != .accept && state != .error {
            switch state {
            case .subBases: executeSubBases()
            case .subOnes: executeSubOnes()
            default: break
            }
        }
    }

    private mutating func executeStart() {
        currentValue = m
        baseCounter = s / baseUnit
        oneCounter = s % baseUnit
        history.append("CBBO (Take Away): Start at \(m). Decompose \(s) into \(baseCounter) bases and \(oneCounter) ones.")
        transition(to: .subBases)
    }

    private mutating func executeSubBases() {
        if baseCounter > 0 {
            let prev = currentValue
            currentValue -= baseUnit
            baseCounter -= 1
            jumps.append(Jump(from: prev, to: currentValue, type: .base))
            history.append("Subtract Base (-\(baseUnit)): \(currentValue).")
        } else {
            history.append("Bases finished. Switching to ones.")
            transition(to: .subOnes)
        }
    }

    private mutating func executeSubOnes() {
        if oneCounter > 0 {
            let prev = currentValue
            currentValue -= 1
            oneCounter -= 1
            jumps.append(Jump(from: prev, to: currentValue, type: .one))
            history.append("Subtract One (-1): \(currentValue).")
        } else {
            history.append("Subtraction finished. Result = \(currentValue).")
            transition(to: .accept)
        }
    }

    private mutating func transition(to nextState: State) {
        state = nextState
    }
}
