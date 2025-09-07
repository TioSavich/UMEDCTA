import SwiftUI

struct MultiplicationC2CView: View {
    @State private var nString = "3"
    @State private var sString = "6"
    @State private var automaton: MultiplicationC2CAutomaton?

    var body: some View {
        VStack {
            HStack {
                TextField("Groups", text: $nString)
                Text("x")
                TextField("Size", text: $sString)
            }
            .padding()
            .textFieldStyle(RoundedBorderTextFieldStyle())

            Button("Calculate") {
                guard let n = Int(nString), let s = Int(sString) else { return }
                var newAutomaton = MultiplicationC2CAutomaton(n: n, s: s)
                _ = newAutomaton.run()
                self.automaton = newAutomaton
            }

            if let automaton = automaton {
                GroupsAndItemsView(groups: automaton.n, itemsPerGroup: automaton.s)
            }

            List(automaton?.history ?? [], id: \.self) { Text($0) }
        }
        .navigationTitle("Mult: C2C")
    }
}
