(function() {
    const elementId = getElementId('trialDiagram');
    const { graph, paper } = initializeDiagram(elementId);
    const states = createBasicStateDiagram(elementId, 'q0');

    window.runStrategicTrialAutomaton = function() {
        let outputDiv = document.getElementById('trialOutput');
        let total = parseInt(document.getElementById('trialTotal').value);
        let divisor = parseInt(document.getElementById('trialDivisor').value);
        updateStateHighlight(elementId, 'q0');

        let steps = '';
        let guess = Math.floor(total / divisor);
        let product = divisor * guess;

        steps += 'Initial Guess: ' + guess + '<br>';
        steps += divisor + ' * ' + guess + ' = ' + product + '<br>';
        updateStateHighlight(elementId, 'q1');

        if (product === total) {
            steps += 'Exact match found. Quotient: ' + guess;
        } else {
            while (product < total) {
                guess += 1;
                product = divisor * guess;
                steps += '<br>Trying ' + guess + ': ' + divisor + ' * ' + guess + ' = ' + product;
            }
            if (product === total) {
                steps += '<br>Exact match found. Quotient: ' + guess;
            } else {
                steps += '<br>' + product + ' exceeds total. Quotient is ' + (guess - 1);
            }
        }
        updateStateHighlight(elementId, 'q2');

        outputDiv.innerHTML = steps;
        typesetMath();
    };

    // Initialize with starting state
    updateStateHighlight(elementId, 'q0');
    document.getElementById('trialOutput').innerHTML = "<strong>Current Values:</strong> 56 / 7<br>";
    typesetMath();
})();