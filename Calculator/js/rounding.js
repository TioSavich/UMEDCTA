    // JavaScript code for Rounding and Adjusting
    function runRoundingAutomaton() {
        let outputDiv = document.getElementById('roundingOutput');
        let a1 = parseInt(document.getElementById('roundAddend1').value);
        let a2 = parseInt(document.getElementById('roundAddend2').value);

        let steps = '';
        steps += 'Initial Addends: ' + a1 + ' + ' + a2 + '<br>';

        // Decide which addend to round
        // For simplicity, we'll round the addend that requires the smallest adjustment
        let remainderA1 = a1 % 10;
        let remainderA2 = a2 % 10;

        let adjustmentA1 = remainderA1 === 0 ? 0 : 10 - remainderA1;
        let adjustmentA2 = remainderA2 === 0 ? 0 : 10 - remainderA2;

        if (adjustmentA1 <= adjustmentA2) {
            // Round a1
            let roundedA1 = a1 + adjustmentA1;
            let preliminarySum = roundedA1 + a2;
            let finalSum = preliminarySum - adjustmentA1;

            steps += 'Rounded ' + a1 + ' up to ' + roundedA1 + ' (added ' + adjustmentA1 + ')<br>';
            steps += 'Preliminary Sum: ' + roundedA1 + ' + ' + a2 + ' = ' + preliminarySum + '<br>';
            steps += 'Adjusting by subtracting ' + adjustmentA1 + ' from ' + preliminarySum + '<br>';
            steps += 'Final Sum: ' + preliminarySum + ' - ' + adjustmentA1 + ' = ' + finalSum;
        } else {
            // Round a2
            let roundedA2 = a2 + adjustmentA2;
            let preliminarySum = a1 + roundedA2;
            let finalSum = preliminarySum - adjustmentA2;

            steps += 'Rounded ' + a2 + ' up to ' + roundedA2 + ' (added ' + adjustmentA2 + ')<br>';
            steps += 'Preliminary Sum: ' + a1 + ' + ' + roundedA2 + ' = ' + preliminarySum + '<br>';
            steps += 'Adjusting by subtracting ' + adjustmentA2 + ' from ' + preliminarySum + '<br>';
            steps += 'Final Sum: ' + preliminarySum + ' - ' + adjustmentA2 + ' = ' + finalSum;
        }

        outputDiv.innerHTML = steps;
          typesetMath();
    }
    function typesetMath() {
        MathJax.Hub.Queue(["Typeset",MathJax.Hub]);
    }