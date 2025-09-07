import SwiftUI

struct SubtractionDecompositionView: View {
    @State private var mString = "45"
    @State private var sString = "27"
    @State private var automaton: SubtractionDecompositionAutomaton?

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
                var newAutomaton = SubtractionDecompositionAutomaton(m: m, s: s)
                _ = newAutomaton.run()
                self.automaton = newAutomaton
            }

            // Custom visualization would go here

            List(automaton?.history ?? [], id: \.self) { Text($0) }
        }
        .navigationTitle("Sub: Decomposition")
    }
}
