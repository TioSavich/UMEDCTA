import SwiftUI

struct SubtractionCOBOView: View {

    enum Strategy: String, CaseIterable, Identifiable {
        case cobo = "COBO (Missing Addend)"
        case cbbo = "CBBO (Take Away)"
        var id: Self { self }
    }

    @State private var minuendString = "94"
    @State private var subtrahendString = "65"
    @State private var selectedStrategy: Strategy = .cobo

    @State private var history: [String] = []
    @State private var jumps: [Jump] = []
    @State private var startValue: Int?

    var body: some View {
        VStack(spacing: 20) {

            Picker("Strategy", selection: $selectedStrategy) {
                ForEach(Strategy.allCases) { strategy in
                    Text(strategy.rawValue)
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

            if !jumps.isEmpty, let start = startValue {
                NumberLineView(startValue: start, jumps: jumps)
            }

            VStack {
                Text("Calculation History").font(.headline)
                List {
                    ForEach(history, id: \.self) { step in
                        Text(step)
                    }
                }
                .listStyle(InsetGroupedListStyle())
            }

            Spacer()
        }
        .navigationTitle("Sub: COBO / CBBO")
        .padding(.vertical)
    }

    private func calculate() {
        guard let m = Int(minuendString), let s = Int(subtrahendString) else {
            history = ["Please enter valid numbers."]; jumps = []; startValue = nil
            return
        }

        if s > m {
            history = ["Error: Subtrahend cannot be greater than Minuend."]; jumps = []; startValue = nil
            return
        }

        switch selectedStrategy {
        case .cobo:
            var automaton = COBOMissingAddendAutomaton(m: m, s: s)
            automaton.run()
            history = automaton.history
            jumps = automaton.jumps
            startValue = s
        case .cbbo:
            var automaton = CBBOTakeAwayAutomaton(m: m, s: s)
            automaton.run()
            history = automaton.history
            jumps = automaton.jumps
            startValue = m
        }
    }
}

struct SubtractionCOBOView_Previews: PreviewProvider {
    static var previews: some View {
        NavigationView {
            SubtractionCOBOView()
        }
    }
}
