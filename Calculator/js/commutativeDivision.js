(function() {
    const elementId = getElementId('commDivDiagram');
    createBasicStateDiagram(elementId, 'q0');

    window.runCommutativeDivisionAutomaton = function() {
        let outputDiv = document.getElementById('commDivOutput');
        let total = parseInt(document.getElementById('commDivTotal').value);
        let divisor = parseInt(document.getElementById('commDivDivisor').value);

        let steps = '';
        updateStateHighlight(elementId, 'q0');

        steps += 'Using commutative property of multiplication:<br>';
        updateStateHighlight(elementId, 'q1');

        let quotient = total / divisor;
        if (Number.isInteger(quotient)) {
            steps += divisor + ' * ' + quotient + ' = ' + total + '<br>';
            steps += 'Therefore, ' + total + ' ÷ ' + divisor + ' = ' + quotient;
        } else {
            steps += 'No integer quotient exists.<br>';
            steps += 'Result: ' + quotient.toFixed(2);
        }

        updateStateHighlight(elementId, 'q2');
        outputDiv.innerHTML = steps;
        typesetMath();
    };
})();