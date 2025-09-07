import SwiftUI

struct MultiplicationCommutativeView: View {
    @State private var aString = "10"
    @State private var bString = "7"
    @State private var automaton: MultiplicationCommutativeAutomaton?

    var body: some View {
        VStack {
            HStack {
                TextField("A", text: $aString)
                Text("x")
                TextField("B", text: $bString)
            }
            .padding()
            .textFieldStyle(RoundedBorderTextFieldStyle())

            Button("Calculate") {
                guard let a = Int(aString), let b = Int(bString) else { return }
                var newAutomaton = MultiplicationCommutativeAutomaton(a: a, b: b)
                _ = newAutomaton.run()
                self.automaton = newAutomaton
            }

            if let automaton = automaton {
                GroupsAndItemsView(groups: automaton.a, itemsPerGroup: automaton.b)
            }

            List(automaton?.history ?? [], id: \.self) { Text($0) }
        }
        .navigationTitle("Mult: Commute")
    }
}
