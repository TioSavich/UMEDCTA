(function() {
    const elementId = getElementId('inverseDiagram');
    const { graph, paper } = initializeDiagram(elementId);
    const states = createBasicStateDiagram(elementId, 'q0');

    window.runInverseDistributiveAutomaton = function() {
        let outputDiv = document.getElementById('inverseOutput');
        let total = parseInt(document.getElementById('invTotal').value);
        let divisor = parseInt(document.getElementById('invDivisor').value);

        let steps = '';
        updateStateHighlight(elementId, 'q0');

        steps += '<strong>Breaking down ' + total + ' into manageable parts:</strong><br>';

        // Break total into multiples of divisor
        let multiples = [];
        let remainingTotal = total;

        while (remainingTotal >= divisor * 10) {
            multiples.push(divisor * 10);
            remainingTotal -= divisor * 10;
        }
        if (remainingTotal > 0) {
            multiples.push(remainingTotal);
        }

        updateStateHighlight(elementId, 'q1');
        let quotient = 0;
        steps += 'Parts: ' + multiples.join(', ') + '<br><br>';

        steps += '<strong>Calculating partial quotients:</strong><br>';
        multiples.forEach(part => {
            let partialQuotient = part / divisor;
            quotient += partialQuotient;
            steps += part + ' ÷ ' + divisor + ' = ' + partialQuotient + '<br>';
        });

        updateStateHighlight(elementId, 'q2');
        steps += '<br><strong>Total Quotient:</strong> ' + quotient;
        outputDiv.innerHTML = steps;
        typesetMath();
    };

    // Initialize with starting state
    updateStateHighlight(elementId, 'q0');
    document.getElementById('inverseOutput').innerHTML = "<strong>Current Values:</strong> 84 / 6<br>";
    typesetMath();
})();