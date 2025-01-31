  // JavaScript code for Chunking by Bases and Ones
    
  let graph5 = new joint.dia.Graph();
  let paper5 = new joint.dia.Paper({
      el: document.getElementById('chunkDiagram'),
      model: graph5,
      width: 600,
      height: 400,
      gridSize: 1,
      interactive: false
  });

  // Define states
  let states5 = {};

  states5.q0 = new joint.shapes.fsa.State({
      position: { x: 50, y: 50 },
      size: { width: 70, height: 70 },
      attrs: { text: { text: 'q0' } }
  });
  states5.q1 = new joint.shapes.fsa.State({
      position: { x: 200, y: 50 },
      size: { width: 70, height: 70 },
      attrs: { text: { text: 'q1' } }
  });
  states5.q2 = new joint.shapes.fsa.State({
      position: { x: 350, y: 50 },
      size: { width: 70, height: 70 },
      attrs: { text: { text: 'q2' } }
  });
     states5.q3 = new joint.shapes.fsa.State({
      position: { x: 500, y: 50 },
      size: { width: 70, height: 70 },
      attrs: { text: { text: 'q3' } }
  });
  
     states5.q_accept = new joint.shapes.fsa.State({
      position: { x: 500, y: 250 },
      size: { width: 70, height: 70 },
      attrs: { text: { text: 'q_accept' } }
  });
  // Add states to the graph
  graph5.addCells([states5.q0, states5.q1, states5.q2, states5.q3, states5.q_accept]);

  // Define transitions
  function link5(source, target, label) {
      return new joint.shapes.fsa.Arrow({
          source: { id: source.id },
          target: { id: target.id },
          labels: [{ position: 0.5, attrs: { text: { text: label, 'font-size': 12 } } }]
      });
  }

  let transitions5 = [
      link5(states5.q0, states5.q1, 'Read A, B'),
      link5(states5.q1, states5.q2, 'Add Tens'),
              link5(states5.q2, states5.q3, 'Add Ones Strategically'),
              link5(states5.q3, states5.q_accept, 'Output Sum')

  ];

  graph5.addCells(transitions5);

      // Highlight current state
  function updateStateDiagram5(currentState) {
      // Reset all states to default style
      Object.values(states5).forEach(state => {
          state.attr('body/fill', '#FFFFFF');
          state.attr('text/fill', '#000000');
      });
      // Highlight current state
      if (states5[currentState]) {
          states5[currentState].attr('body/fill', '#00FF00');
          states5[currentState].attr('text/fill', '#000000');
      }
  }
  
  function runChunkingAutomaton() {
      let outputDiv = document.getElementById('chunkingOutput');
      let a1 = parseInt(document.getElementById('chunkAddend1').value);
      let a2 = parseInt(document.getElementById('chunkAddend2').value);

      let steps = '';
      steps += '<strong>Starting Value:</strong> ' + a1 + '<br>';
              updateStateDiagram5('q0');

      // Extract tens and ones from second addend
      let tens = Math.floor(a2 / 10) * 10;
      let ones = a2 % 10;
      
      updateStateDiagram5('q1');

      let currentSum = a1;
      steps += '<br><strong>Adding tens in one chunk:</strong><br>';
      currentSum += tens;
      steps += a1 + ' + ' + tens + ' = ' + currentSum + '<br>';
updateStateDiagram5('q2');
      steps += '<br><strong>Adding ones strategically:</strong><br>';
      if (ones > 0) {
          // Check if adding ones crosses a base (e.g., from 76 to 80)
          let toNextBase = 10 - (currentSum % 10);
          if (ones >= toNextBase) {
              steps += 'Adding ' + toNextBase + ' to reach next base:<br>';
              currentSum += toNextBase;
              steps += '<strong>Intermediate Sum:</strong> ' + currentSum + '<br>';
              let remainingOnes = ones - toNextBase;
              if (remainingOnes > 0) {
                  steps += 'Adding remaining ones: ' + remainingOnes + '<br>';
                  currentSum += remainingOnes;
                  steps += '<strong>Final Sum:</strong> ' + currentSum;
              } else {
                  steps += '<strong>Final Sum:</strong> ' + currentSum;
              }
          } else {
              steps += 'Adding ones: ' + ones + '<br>';
              currentSum += ones;
              steps += '<strong>Final Sum:</strong> ' + currentSum;
          }
      } else {
          steps += 'No ones to add. <strong>Final Sum:</strong> ' + currentSum;
      }
      
             updateStateDiagram5('q3');
                             updateStateDiagram5('q_accept');



      outputDiv.innerHTML = steps;
        typesetMath();
  }
   function typesetMath() {
      MathJax.Hub.Queue(["Typeset",MathJax.Hub]);
  }
       updateStateDiagram5('q0');
           document.getElementById('chunkingOutput').innerHTML = "<strong>Current Values:</strong> 46 + 37<br>";
        typesetMath();