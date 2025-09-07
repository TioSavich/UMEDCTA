import SwiftUI

struct MenuView: View {

    // MARK: - Strategy Definitions

    struct Strategy: Identifiable {
        let id = UUID()
        let name: String
        let destination: AnyView
    }

    let additionStrategies: [Strategy]
    let subtractionStrategies: [Strategy]
    let multiplicationStrategies: [Strategy]
    let divisionStrategies: [Strategy]

    init() {
        additionStrategies = [
            Strategy(name: "Add: COBO", destination: AnyView(COBOView())),
            Strategy(name: "Add: Chunking", destination: AnyView(ChunkingView())),
            Strategy(name: "Add: RMB", destination: AnyView(RMBView())),
            Strategy(name: "Add: Rounding", destination: AnyView(RoundingView()))
        ]

        subtractionStrategies = [
            Strategy(name: "Sub: COBO / CBBO", destination: AnyView(SubtractionCOBOView())),
            Strategy(name: "Sub: Chunking", destination: AnyView(SubtractionChunkingView())),
            Strategy(name: "Sub: Decomposition", destination: AnyView(SubtractionDecompositionView())),
            Strategy(name: "Sub: Sliding", destination: AnyView(SubtractionSlidingView())),
            Strategy(name: "Sub: Rounding", destination: AnyView(SubtractionRoundingView()))
        ]

        multiplicationStrategies = [
            Strategy(name: "Mult: C2C", destination: AnyView(MultiplicationC2CView())),
            Strategy(name: "Mult: Commute", destination: AnyView(MultiplicationCommutativeView())),
            Strategy(name: "Mult: Distribute", destination: AnyView(MultiplicationDistributiveView())),
            Strategy(name: "Mult: CBO", destination: AnyView(MultiplicationCBOView()))
        ]

        divisionStrategies = [
            Strategy(name: "Div: Dealing Ones", destination: AnyView(DivisionDealingByOnesView())),
            Strategy(name: "Div: Dealing Rounds (UCR)", destination: AnyView(DivisionUCRView())),
            Strategy(name: "Div: Inv Distribute", destination: AnyView(DivisionInverseDistributiveView())),
            Strategy(name: "Div: CGOB", destination: AnyView(DivisionCGOBView()))
        ]
    }

    // MARK: - Body

    var body: some View {
        NavigationView {
            List {
                Section(header: Text("Addition Strategies")) {
                    ForEach(additionStrategies) { strategy in
                        NavigationLink(destination: strategy.destination) {
                            Text(strategy.name)
                        }
                    }
                }

                Section(header: Text("Subtraction Strategies")) {
                    ForEach(subtractionStrategies) { strategy in
                        NavigationLink(destination: strategy.destination) {
                            Text(strategy.name)
                        }
                    }
                }

                Section(header: Text("Multiplication Strategies")) {
                    ForEach(multiplicationStrategies) { strategy in
                        NavigationLink(destination: strategy.destination) {
                            Text(strategy.name)
                        }
                    }
                }

                Section(header: Text("Division Strategies")) {
                    ForEach(divisionStrategies) { strategy in
                        NavigationLink(destination: strategy.destination) {
                            Text(strategy.name)
                        }
                    }
                }
            }
            .listStyle(GroupedListStyle())
            .navigationTitle("Hermeneutic Calculator")
        }
    }
}

// MARK: - Previews

struct MenuView_Previews: PreviewProvider {
    static var previews: some View {
        MenuView()
    }
}
