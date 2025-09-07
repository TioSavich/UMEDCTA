import SwiftUI

struct MultiplicationDistributiveView: View {
    @State private var nString = "5"
    @State private var sString = "7"
    @State private var automaton: MultiplicationDistributiveAutomaton?

    var body: some View {
        VStack {
            HStack {
                TextField("N", text: $nString)
                Text("x")
                TextField("S", text: $sString)
            }
            .padding()
            .textFieldStyle(RoundedBorderTextFieldStyle())

            Button("Calculate") {
                guard let n = Int(nString), let s = Int(sString) else { return }
                var newAutomaton = MultiplicationDistributiveAutomaton(n: n, s: s)
                _ = newAutomaton.run()
                self.automaton = newAutomaton
            }

            if let automaton = automaton {
                VStack {
                    GroupsAndItemsView(groups: automaton.n, itemsPerGroup: automaton.s)
                    Text("is decomposed into")
                    // This is a simplified visualization. A more complex one could show the two separate groups.
                    GroupsAndItemsView(groups: automaton.n, itemsPerGroup: automaton.s)
                }
            }

            List(automaton?.history ?? [], id: \.self) { Text($0) }
        }
        .navigationTitle("Mult: Distribute")
    }
}
