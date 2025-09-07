import Foundation

// Represents a single jump on the number line
struct Jump: Identifiable {
    let id = UUID()
    let from: Int
    let to: Int
    let type: JumpType

    enum JumpType {
        case base, one
    }
}
