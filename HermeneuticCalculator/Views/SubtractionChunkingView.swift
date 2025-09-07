import SwiftUI

struct SubtractionChunkingView: View {

    enum Strategy: String, CaseIterable, Identifiable {
        case a = "A (Backwards by Part)"
        case b = "B (Forwards from Part)"
        case c = "C (Backwards to Part)"
        var id: Self { self }
    }

    @State private var minuendString = "400"
    @State private var subtrahendString = "294"
    @State private var selectedStrategy: Strategy = .a

    @State private var history: [String] = []

    var body: some View {
        VStack(spacing: 20) {

            Picker("Strategy", selection: $selectedStrategy) {
                ForEach(Strategy.allCases) { strategy in
                    Text(strategy.rawValue).font(.caption)
                }
            }
            .pickerStyle(SegmentedPickerStyle())
            .padding(.horizontal)

            HStack {
                TextField("Minuend", text: $minuendString)
                    .textFieldStyle(RoundedBorderTextFieldStyle())
                    .keyboardType(.numberPad)

                Text("-")

                TextField("Subtrahend", text: $subtrahendString)
                    .textFieldStyle(RoundedBorderTextFieldStyle())
                    .keyboardType(.numberPad)
            }
            .padding(.horizontal)

            Button("Calculate") {
                UIApplication.shared.sendAction(#selector(UIResponder.resignFirstResponder), to: nil, from: nil, for: nil)
                calculate()
            }
            .padding()
            .background(Color.blue)
            .foregroundColor(.white)
            .cornerRadius(8)

            Divider()

            VStack {
                Text("Calculation History")
                    .font(.headline)
                List {
                    ForEach(history, id: \.self) { step in
                        Text(step)
                    }
                }
                .listStyle(InsetGroupedListStyle())
            }

            Spacer()
        }
        .navigationTitle("Sub: Chunking")
        .padding(.vertical)
    }

    private func calculate() {
        guard let m = Int(minuendString), let s = Int(subtrahendString) else {
            history = ["Please enter valid numbers."]
            return
        }

        if s > m {
            history = ["Error: Subtrahend cannot be greater than Minuend."]
            return
        }

        switch selectedStrategy {
        case .a:
            var automaton = ChunkingAutomatonA(m: m, s: s)
            automaton.run()
            history = automaton.history
        case .b:
            var automaton = ChunkingAutomatonB(m: m, s: s)
            automaton.run()
            history = automaton.history
        case .c:
            var automaton = ChunkingAutomatonC(m: m, s: s)
            automaton.run()
            history = automaton.history
        }
    }
}

struct SubtractionChunkingView_Previews: PreviewProvider {
    static var previews: some View {
        NavigationView {
            SubtractionChunkingView()
        }
    }
}
