// JavaScript code for Conversion to Bases and Ones (CBO)

(function() {
    const elementId = getElementId('cboDiagram');
    const { graph, paper } = initializeDiagram(elementId);
    const states = createBasicStateDiagram(elementId, 'q0');

    window.runCBOAutomaton = function() {
        let outputDiv = document.getElementById('cboOutput');
        let groups = parseInt(document.getElementById('cboGroups').value);
        let itemsPerGroup = parseInt(document.getElementById('cboItems').value);

        let steps = '';
        updateStateHighlight(elementId, 'q0');

        let total = groups * itemsPerGroup;
        let tens = Math.floor(total / 10);
        let ones = total % 10;

        steps += `<strong>Converting ${total} to Bases and Ones:</strong><br>`;
        updateStateHighlight(elementId, 'q1');
        steps += `${tens} tens and ${ones} ones<br>`;
        updateStateHighlight(elementId, 'q2');

        outputDiv.innerHTML = steps;
        typesetMath();
    };
})();