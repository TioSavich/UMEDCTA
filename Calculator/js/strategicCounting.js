(function() {
    const elementId = getElementId('strategicDiagram');
    createBasicStateDiagram(elementId, 'q0');

    window.runStrategicCountingAutomaton = function() {
        let outputDiv = document.getElementById('strategicOutput');
        let groups = parseInt(document.getElementById('stratGroups').value);
        let itemsPerGroup = parseInt(document.getElementById('stratItems').value);

        let steps = '';
        updateStateHighlight(elementId, 'q0');
        
        let total = 0;
        let remainingGroups = groups;

        steps += '<strong>Strategic Addition:</strong><br>';
        while (remainingGroups >= 2) {
            let partialSum = itemsPerGroup * 2;
            total += partialSum;
            remainingGroups -= 2;
            steps += 'Adding ' + itemsPerGroup + ' + ' + itemsPerGroup + ' = ' + partialSum + ', <strong>Total so far:</strong> ' + total + '<br>';
        }
        if (remainingGroups === 1) {
            total += itemsPerGroup;
            steps += 'Adding remaining ' + itemsPerGroup + ', <strong>Total so far:</strong> ' + total + '<br>';
        }
        updateStateHighlight(elementId, 'q1');
        updateStateHighlight(elementId, 'q_accept');

        steps += '<br><strong>Final Total:</strong> ' + total;
        outputDiv.innerHTML = steps;
        typesetMath();
    };
})();