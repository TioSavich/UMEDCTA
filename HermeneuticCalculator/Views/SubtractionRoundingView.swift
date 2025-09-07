import SwiftUI

struct SubtractionRoundingView: View {
    @State private var mString = "84"
    @State private var sString = "29"
    @State private var automaton: SubtractionRoundingAutomaton?

    var body: some View {
        VStack {
            HStack {
                TextField("Minuend", text: $mString)
                Text("-")
                TextField("Subtrahend", text: $sString)
            }
            .padding()
            .textFieldStyle(RoundedBorderTextFieldStyle())

            Button("Calculate") {
                guard let m = Int(mString), let s = Int(sString) else { return }
                var newAutomaton = SubtractionRoundingAutomaton(m: m, s: s)
                _ = newAutomaton.run()
                self.automaton = newAutomaton
            }

            // Custom visualization would go here

            List(automaton?.history ?? [], id: \.self) { Text($0) }
        }
        .navigationTitle("Sub: Rounding")
    }
}
