let graph8 = new joint.dia.Graph();
    let paper8 = new joint.dia.Paper({
        el: document.getElementById('countBackDiagram'),
        model: graph8,
        width: 600,
        height: 400,
        gridSize: 1,
        interactive: false
    });

    // Define states
    let states8 = {};

    states8.q0 = new joint.shapes.fsa.State({
        position: { x: 50, y: 50 },
        size: { width: 70, height: 70 },
        attrs: { text: { text: 'q0' } }
    });
    states8.q1 = new joint.shapes.fsa.State({
        position: { x: 200, y: 50 },
        size: { width: 70, height: 70 },
        attrs: { text: { text: 'q1' } }
    });
    states8.q2 = new joint.shapes.fsa.State({
        position: { x: 350, y: 50 },
        size: { width: 70, height: 70 },
        attrs: { text: { text: 'q2' } }
    });
       states8.q3 = new joint.shapes.fsa.State({
        position: { x: 500, y: 50 },
        size: { width: 70, height: 70 },
        attrs: { text: { text: 'q3' } }
    });
      states8.q_accept = new joint.shapes.fsa.State({
        position: { x: 500, y: 250 },
        size: { width: 70, height: 70 },
        attrs: { text: { text: 'q_accept' } }
    });

    // Add states to the graph
    graph8.addCells([states8.q0, states8.q1, states8.q2, states8.q3, states8.q_accept]);

    // Define transitions
    function link8(source, target, label) {
        return new joint.shapes.fsa.Arrow({
            source: { id: source.id },
            target: { id: target.id },
            labels: [{ position: 0.5, attrs: { text: { text: label, 'font-size': 12 } } }]
        });
    }

    let transitions8 = [
        link8(states8.q0, states8.q1, 'Read A, B'),
        link8(states8.q1, states8.q2, 'Count Up by Tens'),
                link8(states8.q2, states8.q3, 'Count Up by Ones'),
                            link8(states8.q3, states8.q_accept, 'Total Difference')
    ];

    graph8.addCells(transitions8);

        // Highlight current state
    function updateStateDiagram8(currentState) {
        // Reset all states to default style
        Object.values(states8).forEach(state => {
            state.attr('body/fill', '#FFFFFF');
            state.attr('text/fill', '#000000');
        });
        // Highlight current state
        if (states8[currentState]) {
            states8[currentState].attr('body/fill', '#00FF00');
            states8[currentState].attr('text/fill', '#000000');
        }
    }
    function runCountingBackAutomaton() {
        let outputDiv = document.getElementById('countBackOutput');
        let minuend = parseInt(document.getElementById('countMinuend').value);
        let subtrahend = parseInt(document.getElementById('countSubtrahend').value);

        let steps = '';
                updateStateDiagram8('q0');


        if (minuend < subtrahend) {
            steps += 'Error: Minuend must be greater than or equal to Subtrahend.';
            outputDiv.textContent = steps;
            typesetMath();

            return;
        }

        let totalDifference = 0;

        let current = subtrahend;
        steps += 'Starting from Subtrahend: ' + current + '<br>';

        // Count up by tens
           updateStateDiagram8('q1');

        let nextTen = Math.ceil(current / 10) * 10;
        if (nextTen === current) nextTen += 10;

        
steps += '<br><strong>Counting up by tens:</strong><br>';
        while (nextTen <= minuend) {
            let increment = nextTen - current;
            totalDifference += increment;
            steps += current + ' + ' + increment + ' = ' + nextTen + '<br>';
            current = nextTen;
            nextTen += 10;
        }

        // If current is less than minuend, count up by ones
        if (current < minuend) {
            let increment = minuend - current;
            totalDifference += increment;
            steps += '<br><strong>Counting up by ones:</strong><br>';
            steps += current + ' + ' + increment + ' = ' + minuend + '<br>';
        }
                 updateStateDiagram8('q2');
            updateStateDiagram8('q3');
                      updateStateDiagram8('q_accept');


        steps += '<br><strong>Total Difference:</strong> ' + totalDifference;

        outputDiv.innerHTML = steps;
         typesetMath();

    }
       function typesetMath() {
        MathJax.Hub.Queue(["Typeset",MathJax.Hub]);
    }
            updateStateDiagram8('q0');
    document.getElementById('countBackOutput').innerHTML = "<strong>Current Values:</strong> 83 - 37<br>";
     typesetMath();