(function() {
    const elementId = getElementId('subRoundingDiagram');
    createBasicStateDiagram(elementId, 'q0');

    window.runSubtractionRoundingAutomaton = function() {
        let outputDiv = document.getElementById('subRoundingOutput');
        let minuend = parseInt(document.getElementById('roundSubMinuend').value);
        let subtrahend = parseInt(document.getElementById('roundSubSubtrahend').value);

        let steps = '';
        updateStateHighlight(elementId, 'q0');

        // Decide which number to round
        let remainderSub = subtrahend % 10;
        let adjustment = remainderSub;
        updateStateHighlight(elementId, 'q1');

        // Round subtrahend down
        let roundedSub = subtrahend - adjustment;
        steps += '<strong>Rounded Subtrahend down to ' + roundedSub + ' (subtracted ' + adjustment + ')</strong><br>';

        // Perform subtraction
        updateStateHighlight(elementId, 'q2');

        let preliminaryDifference = minuend - roundedSub;
        steps += '<strong>Preliminary Difference:</strong> ' + minuend + ' - ' + roundedSub + ' = ' + preliminaryDifference + '<br>';

        // Adjust the result
        let finalDifference = preliminaryDifference + adjustment;
        updateStateHighlight(elementId, 'q3');
        updateStateHighlight(elementId, 'q_accept');

        steps += '<strong>Adjusting by adding ' + adjustment + ' to ' + preliminaryDifference + '</strong><br>';
        steps += '<strong>Final Difference:</strong> ' + finalDifference;

        outputDiv.innerHTML = steps;
        typesetMath();
    };
})();

let graph9 = new joint.dia.Graph();
let paper9 = new joint.dia.Paper({
    el: document.getElementById('subRoundingDiagram'),
    model: graph9,
    width: 600,
    height: 400,
    gridSize: 1,
    interactive: false
});

// Define states
let states9 = {};

states9.q0 = new joint.shapes.fsa.State({
    position: { x: 50, y: 50 },
    size: { width: 70, height: 70 },
    attrs: { text: { text: 'q0' } }
});
states9.q1 = new joint.shapes.fsa.State({
    position: { x: 200, y: 50 },
    size: { width: 70, height: 70 },
    attrs: { text: { text: 'q1' } }
});
states9.q2 = new joint.shapes.fsa.State({
    position: { x: 350, y: 50 },
    size: { width: 70, height: 70 },
    attrs: { text: { text: 'q2' } }
});
states9.q3 = new joint.shapes.fsa.State({
    position: { x: 500, y: 50 },
    size: { width: 70, height: 70 },
    attrs: { text: { text: 'q3' } }
});
states9.q_accept = new joint.shapes.fsa.State({
    position: { x: 500, y: 250 },
    size: { width: 70, height: 70 },
    attrs: { text: { text: 'q_accept' } }
});

// Add states to the graph
graph9.addCells([states9.q0, states9.q1, states9.q2, states9.q3, states9.q_accept]);

// Define transitions
function link9(source, target, label) {
    return new joint.shapes.fsa.Arrow({
        source: { id: source.id },
        target: { id: target.id },
        labels: [{ position: 0.5, attrs: { text: { text: label, 'font-size': 12 } } }]
    });
}

let transitions9 = [
    link9(states9.q0, states9.q1, 'Read A, B'),
    link9(states9.q1, states9.q2, 'Determine Rounding Strategy'),
    link9(states9.q2, states9.q3, 'Compute Partial Sum'),
    link9(states9.q3, states9.q_accept, 'Compute Adjustment')
];

graph9.addCells(transitions9);

// Highlight current state
function updateStateHighlight(elementId, currentState) {
    // Reset all states to default style
    Object.values(states9).forEach(state => {
        state.attr('body/fill', '#FFFFFF');
        state.attr('text/fill', '#000000');
    });
    // Highlight current state
    if (states9[currentState]) {
        states9[currentState].attr('body/fill', '#00FF00');
        states9[currentState].attr('text/fill', '#000000');
    }
}

function typesetMath() {
    MathJax.Hub.Queue(["Typeset", MathJax.Hub]);
}

updateStateHighlight('subRoundingDiagram', 'q0');
document.getElementById('subRoundingOutput').innerHTML = "<strong>Current Values:</strong> 83 - 37<br>";
typesetMath();