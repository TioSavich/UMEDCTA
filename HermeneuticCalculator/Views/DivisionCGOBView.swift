import SwiftUI

struct DivisionCGOBView: View {
    @State private var tString = "32"
    @State private var sString = "8"
    @State private var automaton: DivisionCGOBAAutomaton?

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
                var newAutomaton = DivisionCGOBAAutomaton(t: t, s: s)
                _ = newAutomaton.run()
                self.automaton = newAutomaton
            }

            if let automaton = automaton {
                GroupsAndItemsView(groups: automaton.t / automaton.s, itemsPerGroup: automaton.s)
            }

            List(automaton?.history ?? [], id: \.self) { Text($0) }
        }
        .navigationTitle("Div: CGOB")
    }
}
