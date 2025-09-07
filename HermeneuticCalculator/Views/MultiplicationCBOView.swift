import SwiftUI

struct MultiplicationCBOView: View {
    @State private var nString = "7"
    @State private var sString = "9"
    @State private var automaton: MultiplicationCBOAutomaton?

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
                var newAutomaton = MultiplicationCBOAutomaton(n: n, s: s)
                _ = newAutomaton.run()
                self.automaton = newAutomaton
            }

            if let automaton = automaton {
                GroupsAndItemsView(groups: automaton.n, itemsPerGroup: automaton.s)
            }

            List(automaton?.history ?? [], id: \.self) { Text($0) }
        }
        .navigationTitle("Mult: CBO")
    }
}
