    // JavaScript code for Subtraction Chunking by Bases and Ones
    let graph7 = new joint.dia.Graph();
    let paper7 = new joint.dia.Paper({
        el: document.getElementById('subChunkDiagram'),
        model: graph7,
        width: 600,
        height: 400,
        gridSize: 1,
        interactive: false
    });

    // Define states
    let states7 = {};

    states7.q0 = new joint.shapes.fsa.State({
        position: { x: 50, y: 50 },
        size: { width: 70, height: 70 },
        attrs: { text: { text: 'q0' } }
    });
    states7.q1 = new joint.shapes.fsa.State({
        position: { x: 200, y: 50 },
        size: { width: 70, height: 70 },
        attrs: { text: { text: 'q1' } }
    });
    states7.q2 = new joint.shapes.fsa.State({
        position: { x: 350, y: 50 },
        size: { width: 70, height: 70 },
        attrs: { text: { text: 'q2' } }
    });
       states7.q3 = new joint.shapes.fsa.State({
        position: { x: 500, y: 50 },
        size: { width: 70, height: 70 },
        attrs: { text: { text: 'q3' } }
    });
    
       states7.q_accept = new joint.shapes.fsa.State({
        position: { x: 500, y: 250 },
        size: { width: 70, height: 70 },
        attrs: { text: { text: 'q_accept' } }
    });
    // Add states to the graph
    graph7.addCells([states7.q0, states7.q1, states7.q2, states7.q3, states7.q_accept]);

    // Define transitions
    function link7(source, target, label) {
        return new joint.shapes.fsa.Arrow({
            source: { id: source.id },
            target: { id: target.id },
            labels: [{ position: 0.5, attrs: { text: { text: label, 'font-size': 12 } } }]
        });
    }

    let transitions7 = [
        link7(states7.q0, states7.q1, 'Read A, B'),
        link7(states7.q1, states7.q2, 'Split Subtrahend'),
                link7(states7.q2, states7.q3, 'Subtract Bases'),
                        link7(states7.q3, states7.q_accept, 'Subtract Ones')
    ];

    graph7.addCells(transitions7);

        // Highlight current state
    function updateStateDiagram7(currentState) {
        // Reset all states to default style
        Object.values(states7).forEach(state => {
            state.attr('body/fill', '#FFFFFF');
            state.attr('text/fill', '#000000');
        });
        // Highlight current state
        if (states7[currentState]) {
            states7[currentState].attr('body/fill', '#00FF00');
            states7[currentState].attr('text/fill', '#000000');
        }
    }
    function runSubtractionChunkingAutomaton() {
        let outputDiv = document.getElementById('subChunkingOutput');
        let minuend = parseInt(document.getElementById('chunkMinuend').value);
        let subtrahend = parseInt(document.getElementById('chunkSubtrahend').value);

        let steps = '';
                updateStateDiagram7('q0');

        // Split subtrahend into bases and ones
        let tensSub = Math.floor(subtrahend / 10) * 10;
        let onesSub = subtrahend % 10;

        steps += '<strong>Splitting Subtrahend:</strong><br>';
        steps += subtrahend + ' = ' + tensSub + ' + ' + onesSub + '<br>';
            updateStateDiagram7('q1');
        // Subtract bases
        let currentDifference = minuend;
        steps += '<br><strong>Subtracting Bases:</strong><br>';
        currentDifference -= tensSub;
        steps += minuend + ' - ' + tensSub + ' = ' + currentDifference + '<br>';
            updateStateDiagram7('q2');

        // Subtract ones
        steps += '<br><strong>Subtracting Ones:</strong><br>';
        currentDifference -= onesSub;
        steps += (currentDifference + onesSub) + ' - ' + onesSub + ' = ' + currentDifference + '<br>';
                    updateStateDiagram7('q3');
          updateStateDiagram7('q_accept');


        steps += '<br><strong>Final Difference:</strong> ' + currentDifference;

        outputDiv.innerHTML = steps;
         typesetMath();
    }
    
     function typesetMath() {
        MathJax.Hub.Queue(["Typeset",MathJax.Hub]);
    }
            updateStateDiagram7('q0');
             document.getElementById('subChunkingOutput').innerHTML = "<strong>Current Values:</strong> 83 - 37<br>";
            typesetMath();