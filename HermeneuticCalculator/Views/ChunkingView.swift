import SwiftUI

struct ChunkingView: View {
    @State private var addend1String = "46"
    @State private var addend2String = "37"

    // State for the automaton's output
    @State private var history: [String] = []
    @State private var jumps: [Jump] = []
    @State private var finalSum: Int?

    var body: some View {
        VStack(spacing: 20) {
            HStack {
                TextField("Addend 1", text: $addend1String)
                    .textFieldStyle(RoundedBorderTextFieldStyle())
                    .keyboardType(.numberPad)

                Text("+")

                TextField("Addend 2", text: $addend2String)
                    .textFieldStyle(RoundedBorderTextFieldStyle())
                    .keyboardType(.numberPad)
            }
            .padding(.horizontal)

            Button("Calculate") {
                // Hide the keyboard
                UIApplication.shared.sendAction(#selector(UIResponder.resignFirstResponder), to: nil, from: nil, for: nil)

                // Perform calculation
                calculate()
            }
            .padding()
            .background(Color.blue)
            .foregroundColor(.white)
            .cornerRadius(8)

            Divider()

            // Show visualization only if there are jumps
            if !jumps.isEmpty, let startValue = Int(addend1String), let endValue = finalSum {
                NumberLineView(startValue: startValue, jumps: jumps, finalValue: endValue)
            }

            // Show history
            List {
                ForEach(history, id: \.self) { step in
                    Text(step)
                }
            }
            .listStyle(InsetGroupedListStyle())

            Spacer()
        }
        .navigationTitle("Add: Chunking")
        .padding(.vertical)
    }

    private func calculate() {
        guard let a = Int(addend1String), let b = Int(addend2String) else {
            history = ["Please enter valid numbers."]
            jumps = []
            finalSum = nil
            return
        }

        var automaton = ChunkingAutomaton(a: a, b: b)
        automaton.run()

        // Update the state with the results from the automaton
        history = automaton.history
        jumps = automaton.jumps
        finalSum = automaton.finalSum
    }
}

struct ChunkingView_Previews: PreviewProvider {
    static var previews: some View {
        NavigationView {
            ChunkingView()
        }
    }
}
