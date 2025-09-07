import SwiftUI

struct DivisionInverseDistributiveView: View {
    @State private var tString = "56"
    @State private var sString = "8"
    @State private var automaton: DivisionInverseDistributiveAutomaton?

    // This would be loaded from a database or user input in a real app
    private let studentKnowledge: [Int: [(Int, Int)]] = [
        8: [(40, 5), (16, 2), (8, 1)]
    ]

    var body: some View {
        VStack {
            HStack {
                TextField("Dividend", text: $tString)
                Text("÷")
                TextField("Divisor", text: $sString)
            }
            .padding()
            .textFieldStyle(RoundedBorderTextFieldStyle())

            Button("Calculate") {
                guard let t = Int(tString), let s = Int(sString) else { return }
                var newAutomaton = DivisionInverseDistributiveAutomaton(t: t, s: s, knownFacts: studentKnowledge)
                _ = newAutomaton.run()
                self.automaton = newAutomaton
            }

            if let automaton = automaton {
                 GroupsAndItemsView(groups: automaton.t / automaton.s, itemsPerGroup: automaton.s)
            }

            List(automaton?.history ?? [], id: \.self) { Text($0) }
        }
        .navigationTitle("Div: Inv Distribute")
    }
}
