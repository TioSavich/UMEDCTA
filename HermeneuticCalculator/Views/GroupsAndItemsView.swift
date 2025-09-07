import SwiftUI

struct GroupsAndItemsView: View {
    let groups: Int
    let itemsPerGroup: Int

    // Configuration
    private let dotSize: CGFloat = 10
    private let dotSpacing: CGFloat = 5
    private let groupSpacing: CGFloat = 15

    var body: some View {
        VStack(alignment: .leading, spacing: groupSpacing) {
            ForEach(0..<groups, id: \.self) { _ in
                HStack(spacing: dotSpacing) {
                    ForEach(0..<itemsPerGroup, id: \.self) { _ in
                        Circle()
                            .frame(width: dotSize, height: dotSize)
                            .foregroundColor(.blue)
                    }
                }
            }
        }
        .padding()
        .background(Color.gray.opacity(0.1))
        .cornerRadius(10)
    }
}

struct GroupsAndItemsView_Previews: PreviewProvider {
    static var previews: some View {
        GroupsAndItemsView(groups: 3, itemsPerGroup: 6)
    }
}
