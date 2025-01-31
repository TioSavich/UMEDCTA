(function() {
    const elementId = getElementId('divisionDiagram');
    const { graph, paper } = initializeDiagram(elementId);
    const states = createBasicStateDiagram(elementId, 'q0');

    window.runDivisionAutomaton = function() {
        let outputDiv = document.getElementById('divisionOutput');
        let totalItems = parseInt(document.getElementById('divTotalItems').value);
        let groupSize = parseInt(document.getElementById('divGroupSize').value);

        let groups = Math.floor(totalItems / groupSize);
        let remainder = totalItems % groupSize;
        updateStateHighlight(elementId, 'q0');

        let steps = '';
        steps += '<strong>Total Items:</strong> ' + totalItems + '<br>';
        steps += '<strong>Group Size:</strong> ' + groupSize + '<br>';

        updateStateHighlight(elementId, 'q1');
        steps += '<br><strong>Number of Full Groups:</strong> ' + groups + '<br>';

        if (remainder > 0) {
            steps += 'Remaining Items: ' + remainder + '<br>';
        } else {
            steps += 'No Remaining Items.<br>';
        }

        updateStateHighlight(elementId, 'q2');
        outputDiv.innerHTML = steps;
        typesetMath();
    };

    // Initialize with starting state
    updateStateHighlight(elementId, 'q0');
    document.getElementById('divisionOutput').innerHTML = "<strong>Current Values:</strong> 24 / 4<br>";
    typesetMath();
})();