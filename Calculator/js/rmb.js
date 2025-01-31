// JavaScript code for Rearranging to Make Bases (RMB) with improved output and interactive diagram

function runRMBAutomaton() {
    let outputDiv = document.getElementById('rmbOutput');
    let a1 = parseInt(document.getElementById('addend1').value);
    let a2 = parseInt(document.getElementById('addend2').value);

    let steps = '';
    steps += '<strong>Initial Addends:</strong> ' + a1 + ' + ' + a2 + '<br>';

    let onesNeeded = 10 - (a1 % 10);
    if (onesNeeded === 10) onesNeeded = 0;

    steps += '<strong>Ones needed to make ' + a1 + ' a full base (multiple of 10):</strong> ' + onesNeeded + '<br>';

    let currentState = 'q0';
    updateStateDiagram(currentState);

    if (a2 >= onesNeeded && onesNeeded > 0) {
        currentState = 'q1';
        updateStateDiagram(currentState);

        let adjustedA1 = a1 + onesNeeded;
        let adjustedA2 = a2 - onesNeeded;
        steps += '<strong>Move ' + onesNeeded + ' ones from ' + a2 + ' to ' + a1 + ' to make ' + adjustedA1 + '</strong><br>';
        steps += '<strong>Adjusted Addends:</strong> ' + adjustedA1 + ' + ' + adjustedA2 + '<br>';
        currentState = 'q4';
        updateStateDiagram(currentState);

        let sum = adjustedA1 + adjustedA2;
        steps += '<strong>Sum:</strong> ' + sum;
        currentState = 'q_accept';
        updateStateDiagram(currentState);
    } else {
        steps += '<strong>Not enough ones in second addend to make first addend a full base or no need to rearrange.</strong><br>';
        let sum = a1 + a2;
        steps += '<strong>Sum without rearranging:</strong> ' + sum;
        currentState = 'q_accept';
        updateStateDiagram(currentState);
    }

    outputDiv.innerHTML = steps;
    typesetMath();
}

// State Diagram using JointJS
let graph = new joint.dia.Graph();
let paper = new joint.dia.Paper({
    el: document.getElementById('rmbDiagram'),
    model: graph,
    width: 600,
    height: 400,
    gridSize: 1,
    interactive: false
});

// Define states
let states = {};

states.q0 = new joint.shapes.fsa.State({
    position: { x: 250, y: 20 },
    size: { width: 70, height: 70 },
    attrs: { text: { text: 'q0' } }
});
states.q1 = new joint.shapes.fsa.State({
    position: { x: 250, y: 100 },
    size: { width: 70, height: 70 },
    attrs: { text: { text: 'q1' } }
});
states.q2 = new joint.shapes.fsa.State({
    position: { x: 250, y: 180 },
    size: { width: 70, height: 70 },
    attrs: { text: { text: 'q2' } }
});
states.q3 = new joint.shapes.fsa.State({
    position: { x: 250, y: 260 },
    size: { width: 70, height: 70 },
    attrs: { text: { text: 'q3' } }
});
states.q4 = new joint.shapes.fsa.State({
    position: { x: 250, y: 340 },
    size: { width: 70, height: 70 },
    attrs: { text: { text: 'q4' } }
});
states.q_accept = new joint.shapes.fsa.State({
    position: { x: 250, y: 420 },
    size: { width: 70, height: 70 },
    attrs: { text: { text: 'q_accept' } }
});

// Add states to the graph
graph.addCells([states.q0, states.q1, states.q2, states.q3, states.q4, states.q_accept]);

// Define transitions
function link(source, target, label) {
    return new joint.shapes.fsa.Arrow({
        source: { id: source.id },
        target: { id: target.id },
        labels: [{ position: 0.5, attrs: { text: { text: label, 'font-size': 12 } } }]
    });
}

let transitions = [
    link(states.q0, states.q1, 'Read A, B'),
    link(states.q1, states.q2, 'Determine ones'),
    link(states.q2, states.q3, 'Compute needed ones'),
    link(states.q3, states.q4, 'Check B\'s availability'),
    link(states.q4, states.q_accept, 'Output sum')
];

graph.addCells(transitions);

// Highlight current state
function updateStateDiagram(currentState) {
    // Reset all states to default style
    Object.values(states).forEach(state => {
        state.attr('body/fill', '#FFFFFF');
        state.attr('text/fill', '#000000');
    });
    // Highlight current state
    if (states[currentState]) {
        states[currentState].attr('body/fill', '#00FF00');
        states[currentState].attr('text/fill', '#000000');
    }
}
 function typesetMath() {
        MathJax.Hub.Queue(["Typeset",MathJax.Hub]);
    }
updateStateDiagram('q0');
document.getElementById('rmbOutput').innerHTML = '<strong>Current Addends:</strong> 8 + 5<br>';
typesetMath();