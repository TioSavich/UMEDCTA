import SwiftUI

struct DivisionUCRView: View {
    @State private var eString = "56"
    @State private var gString = "8"
    @State private var automaton: DivisionUCRAutomaton?

    var body: some View {
        VStack {
            HStack {
                TextField("Total", text: $eString)
                Text("÷")
                TextField("Groups", text: $gString)
            }
            .padding()
            .textFieldStyle(RoundedBorderTextFieldStyle())

            Button("Calculate") {
                guard let e = Int(eString), let g = Int(gString) else { return }
                var newAutomaton = DivisionUCRAutomaton(e: e, g: g)
                _ = newAutomaton.run()
                self.automaton = newAutomaton
            }

            if let automaton = automaton {
                GroupsAndItemsView(groups: automaton.g, itemsPerGroup: automaton.e / automaton.g)
            }

            List(automaton?.history ?? [], id: \.self) { Text($0) }
        }
        .navigationTitle("Div: Dealing Rounds")
    }
}
