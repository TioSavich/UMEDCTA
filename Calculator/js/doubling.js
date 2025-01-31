(function() {
    const elementId = getElementId('doublingDiagram');
    createBasicStateDiagram(elementId, 'q0');

    window.runDoublingAutomaton = function() {
        let outputDiv = document.getElementById('doublingOutput');
        let groups = parseInt(document.getElementById('doubleGroups').value);
        let itemsPerGroup = parseInt(document.getElementById('doubleItems').value);

        let steps = '';
        let total = itemsPerGroup;
        let currentGroups = 1;
        updateStateHighlight(elementId, 'q0');

        steps += 'Starting with ' + itemsPerGroup + ' items for 1 group.<br>';
        updateStateHighlight(elementId, 'q1');

        while (currentGroups * 2 <= groups) {
            total *= 2;
            currentGroups *= 2;
            steps += 'Doubled to ' + total + ' items for ' + currentGroups + ' groups.<br>';
        }

        if (currentGroups < groups) {
            let remainingGroups = groups - currentGroups;
            let remainingTotal = remainingGroups * itemsPerGroup;
            total += remainingTotal;
            steps += 'Adding remaining ' + remainingGroups + ' groups: ' + remainingTotal + '<br>';
        }

        updateStateHighlight(elementId, 'q2');
        steps += '<br><strong>Final Total:</strong> ' + total;
        outputDiv.innerHTML = steps;
        typesetMath();
    };
})();