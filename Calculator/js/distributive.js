(function() {
    const elementId = getElementId('drDiagram');
    createBasicStateDiagram(elementId, 'q0');

    window.runDRAutomaton = function() {
        let outputDiv = document.getElementById('drOutput');
        let groups = parseInt(document.getElementById('drGroups').value);
        let itemsPerGroup = parseInt(document.getElementById('drItems').value);

        let steps = '';
        updateStateHighlight(elementId, 'q0');

        let tens = Math.floor(itemsPerGroup / 10) * 10;
        let ones = itemsPerGroup % 10;

        steps += '<strong>Breaking apart items per group:</strong><br>';
        steps += itemsPerGroup + ' = ' + tens + ' + ' + ones + '<br>';
        updateStateHighlight(elementId, 'q1');

        let partial1 = groups * tens;
        let partial2 = groups * ones;

        steps += '<br><strong>Computing Partial Products:</strong><br>';
        steps += groups + ' * ' + tens + ' = ' + partial1 + '<br>';
        steps += groups + ' * ' + ones + ' = ' + partial2 + '<br>';

        updateStateHighlight(elementId, 'q2');
        let total = partial1 + partial2;
        steps += '<br><strong>Total:</strong> ' + total;

        outputDiv.innerHTML = steps;
        typesetMath();
    };
})();

function typesetMath() {
    MathJax.Hub.Queue(["Typeset", MathJax.Hub]);
}