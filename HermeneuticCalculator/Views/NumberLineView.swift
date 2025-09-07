import SwiftUI

struct NumberLineView: View {
    let startValue: Int
    let jumps: [Jump]

    // Constants for drawing
    private let numberLineY: CGFloat = 150
    private let baseArcHeight: CGFloat = -40 // Negative for "up"
    private let oneArcHeight: CGFloat = -20  // Negative for "up"
    private let tickHeight: CGFloat = 10
    private let marginLeft: CGFloat = 40
    private let marginRight: CGFloat = 40

    var body: some View {
        Canvas { context, size in
            let allValues = [startValue] + jumps.map { $0.from } + jumps.map { $0.to }
            let minVal = allValues.min() ?? 0
            let maxVal = allValues.max() ?? 1

            let availableWidth = size.width - marginLeft - marginRight
            let numericRange = CGFloat(maxVal - minVal)

            // Avoid division by zero if there's no range
            guard numericRange > 0 else { return }

            let scale = availableWidth / numericRange

            // Function to convert a numeric value to an X coordinate
            func valueToX(_ value: Int) -> CGFloat {
                return marginLeft + CGFloat(value - minVal) * scale
            }

            // Draw the main number line
            let lineY = numberLineY
            let lineStart = CGPoint(x: marginLeft, y: lineY)
            let lineEnd = CGPoint(x: size.width - marginRight, y: lineY)
            context.stroke(Path { path in
                path.move(to: lineStart)
                path.addLine(to: lineEnd)
            }, with: .color(.primary), lineWidth: 2)

            // Draw arrowhead
            context.stroke(Path { path in
                path.move(to: CGPoint(x: lineEnd.x - 8, y: lineEnd.y - 5))
                path.addLine(to: lineEnd)
                path.move(to: CGPoint(x: lineEnd.x - 8, y: lineEnd.y + 5))
                path.addLine(to: lineEnd)
            }, with: .color(.primary), lineWidth: 2)

            // Draw ticks and labels for all unique points
            let uniqueValues = Set(allValues)
            for value in uniqueValues {
                 drawTickAndLabel(at: value, in: context, size: size, valueToX: valueToX)
            }

            // Draw jumps
            for jump in jumps {
                drawJump(jump, in: context, size: size, valueToX: valueToX)
            }
        }
        .frame(height: 250)
    }

    private func drawTickAndLabel(at value: Int, in context: GraphicsContext, size: CGSize, valueToX: (Int) -> CGFloat) {
        let x = valueToX(value)
        let y = numberLineY

        context.stroke(Path { path in
            path.move(to: CGPoint(x: x, y: y - tickHeight / 2))
            path.addLine(to: CGPoint(x: x, y: y + tickHeight / 2))
        }, with: .color(.primary), lineWidth: 2)

        context.draw(Text("\(value)"), at: CGPoint(x: x, y: y + 20))
    }

    private func drawJump(_ jump: Jump, in context: GraphicsContext, size: CGSize, valueToX: (Int) -> CGFloat) {
        let x1 = valueToX(jump.from)
        let x2 = valueToX(jump.to)
        let y = numberLineY

        let arcHeight = jump.type == .base ? baseArcHeight : oneArcHeight
        let controlPoint = CGPoint(x: (x1 + x2) / 2, y: y + arcHeight)

        let arcPath = Path { path in
            path.move(to: CGPoint(x: x1, y: y))
            path.addQuadCurve(to: CGPoint(x: x2, y: y), control: controlPoint)
        }
        context.stroke(arcPath, with: .color(.blue), lineWidth: 1.5)

        let angle = atan2(y - controlPoint.y, x2 - controlPoint.x)
        let arrowLength: CGFloat = 6
        let arrowAngle: CGFloat = .pi / 6

        let p1 = CGPoint(x: x2 - arrowLength * cos(angle - arrowAngle), y: y - arrowLength * sin(angle - arrowAngle))
        let p2 = CGPoint(x: x2 - arrowLength * cos(angle + arrowAngle), y: y - arrowLength * sin(angle + arrowAngle))

        context.stroke(Path { path in
            path.move(to: p1)
            path.addLine(to: CGPoint(x: x2, y: y))
            path.addLine(to: p2)
        }, with: .color(.blue), lineWidth: 1.5)

        let jumpValue = jump.to - jump.from
        let labelText = jumpValue >= 0 ? "+\(jumpValue)" : "\(jumpValue)"
        context.draw(Text(labelText).font(.caption).foregroundColor(.blue), at: CGPoint(x: controlPoint.x, y: controlPoint.y - 10))
    }
}

struct NumberLineView_Previews: PreviewProvider {
    static var previews: some View {
        let sampleJumps = [
            Jump(from: 46, to: 50, type: .one),
            Jump(from: 50, to: 87, type: .base),
            Jump(from: 87, to: 83, type: .one)
        ]
        NumberLineView(startValue: 46, jumps: sampleJumps)
            .padding()
    }
}
