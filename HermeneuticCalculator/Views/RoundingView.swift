import SwiftUI

struct RoundingView: View {
    @State private var addend1String = "46"
    @State private var addend2String = "37"

    // State for the automaton's output
    @State private var history: [String] = []
    @State private var automaton: RoundingAdjustingAutomaton?

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
            if let automaton = automaton, !automaton.jumps.isEmpty {
                NumberLineView(startValue: automaton.startValue, jumps: automaton.jumps)
            }

            // Show history
            VStack {
                Text("Calculation History")
                    .font(.headline)
                List {
                    ForEach(automaton?.history ?? [], id: \.self) { step in
                        Text(step)
                    }
                }
                .listStyle(InsetGroupedListStyle())
            }

            Spacer()
        }
        .navigationTitle("Add: Rounding")
        .padding(.vertical)
    }

    private func calculate() {
        guard let a = Int(addend1String), let b = Int(addend2String) else {
            self.automaton = nil
            // Or show an error message in history
            return
        }

        var newAutomaton = RoundingAdjustingAutomaton(a: a, b: b)
        newAutomaton.run()
        self.automaton = newAutomaton
    }
}

struct RoundingView_Previews: PreviewProvider {
    static var previews: some View {
        NavigationView {
            RoundingView()
        }
    }
}
