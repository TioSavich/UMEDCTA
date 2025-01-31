(function() {
    const elementId = getElementId('commuteDiagram');
    const { graph, paper } = initializeDiagram(elementId);
    const states = createBasicStateDiagram(elementId, 'q0');

    window.runCommutativeAutomaton = function() {
        let outputDiv = document.getElementById('commuteOutput');
        let a = parseInt(document.getElementById('commuteA').value);
        let b = parseInt(document.getElementById('commuteB').value);

        let steps = '';
        updateStateHighlight(elementId, 'q0');

        let total = a * b;
        steps += '<strong>Original Arrangement:</strong><br>';
        steps += `${a} groups of ${b} items<br>`;
        steps += `${a} × ${b} = ${total}<br>`;

        updateStateHighlight(elementId, 'q1');
        steps += '<br><strong>Commuted Arrangement:</strong><br>';
        steps += `${b} groups of ${a} items<br>`;
        steps += `${b} × ${a} = ${total}`;

        updateStateHighlight(elementId, 'q2');
        outputDiv.innerHTML = steps;
        typesetMath();
    };

    // Initialize with starting state
    updateStateHighlight(elementId, 'q0');
    document.getElementById('commuteOutput').innerHTML = "<strong>Current Values:</strong> 3 * 5<br>";
    typesetMath();
})();