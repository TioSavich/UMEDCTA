import Foundation

struct ChunkingAutomaton {
    let a: Int
    let b: Int
    let baseUnit: Int

    // MARK: - Registers
    private var sum: Int = 0
    private var basesRemaining: Int = 0
    private var onesRemaining: Int = 0
    private var k: Int = 0 // Strategic gap for ones

    // Internal registers for iteration
    private var internalSumTemp: Int = 0
    private var targetBase: Int = 0

    // MARK: - State
    private enum State {
        case start, initialize, addBaseChunk, initOnesChunk, initK, loopK, addOnesChunk, accept, error
    }
    private var state: State = .start

    // MARK: - Output
    private(set) var history: [String] = []
    private(set) var jumps: [Jump] = []
    private(set) var finalSum: Int = 0

    init(a: Int, b: Int, base: Int = 10) {
        self.a = a
        self.b = b
        self.baseUnit = base
    }

    // MARK: - State Machine runner

    mutating func run() {
        while state != .accept && state != .error {
            switch state {
            case .start: executeStart()
            case .initialize: executeInitialize()
            case .addBaseChunk: executeAddBaseChunk()
            case .initOnesChunk: executeInitOnesChunk()
            case .initK: executeInitK()
            case .loopK: executeLoopK()
            case .addOnesChunk: executeAddOnesChunk()
            case .accept, .error: break
            }
        }
        finalSum = sum
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
        if nextState == .initOnesChunk || nextState == .accept {
            k = 0
            internalSumTemp = 0
        }
    }

    // MARK: - State Executors

    private mutating func executeStart() {
        recordHistory("Inputs: A=\(a), B=\(b)")
        transition(to: .initialize)
    }

    private mutating func executeInitialize() {
        sum = a
        basesRemaining = (b / baseUnit) * baseUnit
        onesRemaining = b % baseUnit
        recordHistory("Initialize Sum to \(sum). Decompose B: \(basesRemaining) + \(onesRemaining).")
        transition(to: .addBaseChunk)
    }

    private mutating func executeAddBaseChunk() {
        if basesRemaining > 0 {
            let chunk = basesRemaining
            let prevSum = sum
            sum += chunk
            basesRemaining = 0
            recordJump(from: prevSum, to: sum, type: .base)
            recordHistory("Add Base Chunk (+\(chunk)). Sum = \(sum).")
        } else {
            recordHistory("No bases to add.")
        }
        transition(to: .initOnesChunk)
    }

    private mutating func executeInitOnesChunk() {
        if onesRemaining > 0 {
            recordHistory("Begin strategic chunking of remaining ones (\(onesRemaining)).")
            transition(to: .initK)
        } else {
            recordHistory("All ones added. Accepting.")
            transition(to: .accept)
        }
    }

    private mutating func executeInitK() {
        k = 0
        internalSumTemp = sum

        if sum > 0 && sum % baseUnit != 0 {
            targetBase = ((sum / baseUnit) + 1) * baseUnit
        } else {
            targetBase = sum
        }

        recordHistory("Calculating K: Counting from \(sum) to \(targetBase).")
        transition(to: .loopK)
    }

    private mutating func executeLoopK() {
        if internalSumTemp < targetBase {
            internalSumTemp += 1
            k += 1
            // We don't record history for each step of this internal calculation
            // to keep the main history clean.
        } else {
            recordHistory("K needed to reach base is \(k).")
            transition(to: .addOnesChunk)
        }
    }

    private mutating func executeAddOnesChunk() {
        if onesRemaining >= k && k > 0 {
            let chunk = k
            let prevSum = sum
            sum += chunk
            onesRemaining -= chunk
            recordJump(from: prevSum, to: sum, type: .one)
            recordHistory("Add Strategic Chunk (+\(chunk)) to make base. Sum = \(sum).")
        } else if onesRemaining > 0 {
            let chunk = onesRemaining
            let prevSum = sum
            sum += chunk
            onesRemaining = 0
            recordJump(from: prevSum, to: sum, type: .one)
            recordHistory("Add Remaining Chunk (+\(chunk)). Sum = \(sum).")
        }

        transition(to: .initOnesChunk)
    }
}
