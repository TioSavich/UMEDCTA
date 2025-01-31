 // JavaScript code for Counting On by Bases and Then Ones (COBO)
    
 let graph4 = new joint.dia.Graph();
 let paper4 = new joint.dia.Paper({
     el: document.getElementById('coboDiagram'),
     model: graph4,
     width: 600,
     height: 400,
     gridSize: 1,
     interactive: false
 });

 // Define states
 let states4 = {};

 states4.q0 = new joint.shapes.fsa.State({
     position: { x: 50, y: 50 },
     size: { width: 70, height: 70 },
     attrs: { text: { text: 'q0' } }
 });
 states4.q1 = new joint.shapes.fsa.State({
     position: { x: 200, y: 50 },
     size: { width: 70, height: 70 },
     attrs: { text: { text: 'q1' } }
 });
 states4.q2 = new joint.shapes.fsa.State({
     position: { x: 350, y: 50 },
     size: { width: 70, height: 70 },
     attrs: { text: { text: 'q2' } }
 });
    states4.q3 = new joint.shapes.fsa.State({
     position: { x: 500, y: 50 },
     size: { width: 70, height: 70 },
     attrs: { text: { text: 'q3' } }
 });
 
    states4.q_accept = new joint.shapes.fsa.State({
     position: { x: 500, y: 250 },
     size: { width: 70, height: 70 },
     attrs: { text: { text: 'q_accept' } }
 });
 // Add states to the graph
 graph4.addCells([states4.q0, states4.q1, states4.q2, states4.q3, states4.q_accept]);

 // Define transitions
 function link4(source, target, label) {
     return new joint.shapes.fsa.Arrow({
         source: { id: source.id },
         target: { id: target.id },
         labels: [{ position: 0.5, attrs: { text: { text: label, 'font-size': 12 } } }]
     });
 }

 let transitions4 = [
     link4(states4.q0, states4.q1, 'Read A, B'),
     link4(states4.q1, states4.q2, 'Compute Tens'),
     link4(states4.q2, states4.q3, 'Add Tens'),
     link4(states4.q3, states4.q_accept, 'Add Ones')
 ];

 graph4.addCells(transitions4);

     // Highlight current state
 function updateStateDiagram4(currentState) {
     // Reset all states to default style
     Object.values(states4).forEach(state => {
         state.attr('body/fill', '#FFFFFF');
         state.attr('text/fill', '#000000');
     });
     // Highlight current state
     if (states4[currentState]) {
         states4[currentState].attr('body/fill', '#00FF00');
         states4[currentState].attr('text/fill', '#000000');
     }
 }
 function runCOBOAutomaton() {
     let outputDiv = document.getElementById('coboOutput');
     let a1 = parseInt(document.getElementById('coboAddend1').value);
     let a2 = parseInt(document.getElementById('coboAddend2').value);

     let steps = '';
     steps += '<strong>Starting Value:</strong> ' + a1 + '<br>';
             updateStateDiagram4('q0');

     // Extract tens and ones from second addend
     let tens = Math.floor(a2 / 10);
     let ones = a2 % 10;

     let currentSum = a1;
     steps += '<br><strong>Adding tens one by one:</strong><br>';
            updateStateDiagram4('q1');

     for (let i = 1; i <= tens; i++) {
         currentSum += 10;
         steps += 'Step ' + i + ': ' + currentSum + '<br>';
     }
     updateStateDiagram4('q2');
     steps += '<br><strong>Adding ones one by one:</strong><br>';
     for (let i = 1; i <= ones; i++) {
         currentSum += 1;
         steps += 'Step ' + i + ': ' + currentSum + '<br>';
     }
    updateStateDiagram4('q3');
    updateStateDiagram4('q_accept');

     steps += '<br><strong>Final Sum:</strong> ' + currentSum;

     outputDiv.innerHTML = steps;
      typesetMath();
 }
       function typesetMath() {
     MathJax.Hub.Queue(["Typeset",MathJax.Hub]);
 }
      updateStateDiagram4('q0');
 
 document.getElementById('coboOutput').innerHTML = "<strong>Current Addends:</strong> 46 + 37<br>";
  typesetMath();