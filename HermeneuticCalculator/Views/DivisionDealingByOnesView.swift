import SwiftUI

struct DivisionDealingByOnesView: View {
    @State private var tString = "12"
    @State private var nString = "4"
    @State private var automaton: DivisionDealingByOnesAutomaton?

    var body: some View {
        VStack {
            HStack {
                TextField("Total", text: $tString)
                Text("÷")
                TextField("Groups", text: $nString)
            }
            .padding()
            .textFieldStyle(RoundedBorderTextFieldStyle())

            Button("Calculate") {
                guard let t = Int(tString), let n = Int(nString) else { return }
                var newAutomaton = DivisionDealingByOnesAutomaton(t: t, n: n)
                _ = newAutomaton.run()
                self.automaton = newAutomaton
            }

            if let automaton = automaton {
                GroupsAndItemsView(groups: automaton.n, itemsPerGroup: automaton.t / automaton.n)
            }

            List(automaton?.history ?? [], id: \.self) { Text($0) }
        }
        .navigationTitle("Div: Dealing Ones")
    }
}
