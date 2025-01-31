// Get diagram element ID for this automaton
const elementId = 'c2cDiagram';

// Initialize the basic state diagram
createBasicStateDiagram(elementId, 'q0');

function runC2CAutomaton() {
    let outputDiv = document.getElementById('c2cOutput');
    let groups = parseInt(document.getElementById('c2cGroups').value);
    let items = parseInt(document.getElementById('c2cItems').value);
    
    let steps = '';
    let total = 0;
    
    // Start at initial state
    updateStateHighlight(elementId, 'q0');
    steps += 'Starting count...<br>';
    
    // Process groups
    updateStateHighlight(elementId, 'q1');
    for (let i = 1; i <= groups; i++) {
        total += items;
        steps += `Group ${i}: Added ${items}, Total: ${total}<br>`;
    }
    
    // Final state
    updateStateHighlight(elementId, 'q2');
    steps += `<br><strong>Final Total:</strong> ${total}`;
    
    outputDiv.innerHTML = steps;
    typesetMath();
}

// Initial setup
document.addEventListener('DOMContentLoaded', function() {
    updateStateHighlight(elementId, 'q0');
    document.getElementById('c2cOutput').innerHTML = "<strong>Current Values:</strong> 3 * 4<br>";
    typesetMath();
});