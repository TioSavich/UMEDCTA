import SwiftUI

struct SubtractionSlidingView: View {
    @State private var mString = "73"
    @State private var sString = "47"
    @State private var automaton: SubtractionSlidingAutomaton?

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
                var newAutomaton = SubtractionSlidingAutomaton(m: m, s: s)
                _ = newAutomaton.run()
                self.automaton = newAutomaton
            }

            if let automaton = automaton, !automaton.jumps.isEmpty {
                NumberLineView(startValue: automaton.s, jumps: automaton.jumps)
            }

            List(automaton?.history ?? [], id: \.self) { Text($0) }
        }
        .navigationTitle("Sub: Sliding")
    }
}
