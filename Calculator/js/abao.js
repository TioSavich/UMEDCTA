    // JavaScript code for Adding Bases and Adding Ones (ABAO)
    
    let graph6 = new joint.dia.Graph();
    let paper6 = new joint.dia.Paper({
        el: document.getElementById('abaoDiagram'),
        model: graph6,
        width: 600,
        height: 400,
        gridSize: 1,
        interactive: false
    });

    // Define states
    let states6 = {};

    states6.q0 = new joint.shapes.fsa.State({
        position: { x: 50, y: 50 },
        size: { width: 70, height: 70 },
        attrs: { text: { text: 'q0' } }
    });
    states6.q1 = new joint.shapes.fsa.State({
        position: { x: 200, y: 50 },
        size: { width: 70, height: 70 },
        attrs: { text: { text: 'q1' } }
    });
    states6.q2 = new joint.shapes.fsa.State({
        position: { x: 350, y: 50 },
        size: { width: 70, height: 70 },
        attrs: { text: { text: 'q2' } }
    });
       states6.q3 = new joint.shapes.fsa.State({
        position: { x: 500, y: 50 },
        size: { width: 70, height: 70 },
        attrs: { text: { text: 'q3' } }
    });
    states6.q4 = new joint.shapes.fsa.State({
        position: { x: 500, y: 250 },
        size: { width: 70, height: 70 },
        attrs: { text: { text: 'q4' } }
    });
       states6.q_accept = new joint.shapes.fsa.State({
        position: { x: 350, y: 250 },
        size: { width: 70, height: 70 },
        attrs: { text: { text: 'q_accept' } }
    });

    // Add states to the graph
    graph6.addCells([states6.q0, states6.q1, states6.q2, states6.q3, states6.q4, states6.q_accept]);

    // Define transitions
    function link6(source, target, label) {
        return new joint.shapes.fsa.Arrow({
            source: { id: source.id },
            target: { id: target.id },
            labels: [{ position: 0.5, attrs: { text: { text: label, 'font-size': 12 } } }]
        });
    }

    let transitions6 = [
        link6(states6.q0, states6.q1, 'Read A, B'),
        link6(states6.q1, states6.q2, 'Split Numbers'),
        link6(states6.q2, states6.q3, 'Add Bases'),
                link6(states6.q3, states6.q4, 'Add Ones'),
                        link6(states6.q4, states6.q_accept, 'Combine Sums')
    ];

    graph6.addCells(transitions6);

        // Highlight current state
    function updateStateDiagram6(currentState) {
        // Reset all states to default style
        Object.values(states6).forEach(state => {
            state.attr('body/fill', '#FFFFFF');
            state.attr('text/fill', '#000000');
        });
        // Highlight current state
        if (states6[currentState]) {
            states6[currentState].attr('body/fill', '#00FF00');
            states6[currentState].attr('text/fill', '#000000');
        }
    }
    function runABAOAutomaton() {
        let outputDiv = document.getElementById('abaoOutput');
        let a1 = parseInt(document.getElementById('abaoAddend1').value);
        let a2 = parseInt(document.getElementById('abaoAddend2').value);

        let steps = '';
                updateStateDiagram6('q0');
                

        // Split both addends into bases and ones
        let tensA1 = Math.floor(a1 / 10) * 10;
        let onesA1 = a1 % 10;
        let tensA2 = Math.floor(a2 / 10) * 10;
        let onesA2 = a2 % 10;
        
            updateStateDiagram6('q1');
        steps += '<strong>Splitting Addends:</strong><br>';
        steps += a1 + ' = ' + tensA1 + ' + ' + onesA1 + '<br>';
        steps += a2 + ' = ' + tensA2 + ' + ' + onesA2 + '<br>';
                
                    updateStateDiagram6('q2');

        // Add bases
        let baseSum = tensA1 + tensA2;
        steps += '<br><strong>Adding Bases:</strong><br>';
        steps += tensA1 + ' + ' + tensA2 + ' = ' + baseSum + '<br>';
        
                updateStateDiagram6('q3');


        // Add ones
        let onesSum = onesA1 + onesA2;
        steps += '<br><strong>Adding Ones:</strong><br>';
        steps += onesA1 + ' + ' + onesA2 + ' = ' + onesSum + '<br>';
        
          updateStateDiagram6('q4');


        // Check for carry-over from ones
        if (onesSum >= 10) {
            steps += '<br><strong>Handling Carry-Over:</strong><br>';
            baseSum += 10;
            onesSum -= 10;
            steps += 'Carry over 10 to bases, new base sum: ' + baseSum + ', remaining ones: ' + onesSum + '<br>';
        }

        // Combine partial sums
        let finalSum = baseSum + onesSum;
        steps += '<br><strong>Final Sum:</strong><br>';
        steps += baseSum + ' + ' + onesSum + ' = ' + finalSum;
        
        updateStateDiagram6('q_accept');
        

        outputDiv.innerHTML = steps;
                 typesetMath();

    }
        function typesetMath() {
        MathJax.Hub.Queue(["Typeset",MathJax.Hub]);
    }
            updateStateDiagram6('q0');
          document.getElementById('abaoOutput').innerHTML = "<strong>Current Addends:</strong> 65 + 25<br>";
          typesetMath();