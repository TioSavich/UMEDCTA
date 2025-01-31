(function() {
    const elementId = getElementId('dealingDiagram');
    const { graph, paper } = initializeDiagram(elementId);
    const states = createBasicStateDiagram(elementId, 'q0');

    window.runDealingAutomaton = function() {
        let outputDiv = document.getElementById('dealingOutput');
        let totalItems = parseInt(document.getElementById('dealTotalItems').value);
        let groupSize = parseInt(document.getElementById('dealGroupSize').value);
        updateStateHighlight(elementId, 'q0');

        let steps = '';
        let groupsFormed = 0;
        let itemsRemaining = totalItems;

        steps += 'Dealing items into groups:<br>';
        updateStateHighlight(elementId, 'q1');

        while (itemsRemaining >= groupSize) {
            itemsRemaining -= groupSize;
            groupsFormed += 1;
            steps += 'Formed group ' + groupsFormed + ', items remaining: ' + itemsRemaining + '<br>';
        }

        if (itemsRemaining > 0) {
            steps += '<br>Items left without a full group: ' + itemsRemaining;
        }

        updateStateHighlight(elementId, 'q2');
        steps += '<br><br><strong>Total Groups Formed:</strong> ' + groupsFormed;
        outputDiv.innerHTML = steps;
        typesetMath();
    };

    // Initialize with starting state
    updateStateHighlight(elementId, 'q0');
    document.getElementById('dealingOutput').innerHTML = "<strong>Current Values:</strong> 15 / 3<br>";
    typesetMath();
})();