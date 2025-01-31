    // JavaScript code for the Counting Automaton
    let stack = ['#', 'D0'];

    function runCountingAutomaton() {
        let outputDiv = document.getElementById('countingOutput');
        // Simulate q_count state
        incrementDigit();
        // Output the current count
        outputDiv.textContent = 'Current Count: ' + getStackNumber();
        typesetMath();
    }

    function incrementDigit() {
        let i = stack.length - 1; // Start from the top of the stack
        let carry = true;
        while (i >= 0 && carry) {
            let digit = stack[i];
            if (digit.startsWith('D')) {
                let n = parseInt(digit.substring(1));
                if (n < 9) {
                    n += 1;
                    stack[i] = 'D' + n;
                    carry = false;
                } else {
                    stack[i] = 'D0';
                    carry = true;
                }
            } else if (digit === '#') {
                // Need to push a new digit onto the stack
                stack.splice(i + 1, 0, 'D1');
                carry = false;
            }
            i--;
        }
    }

    function getStackNumber() {
        let numStr = '';
        for (let i = 1; i < stack.length; i++) {
            numStr += stack[i].substring(1);
        }
        return numStr;
    }
    function typesetMath() {
        MathJax.Hub.Queue(["Typeset",MathJax.Hub]);
    }
    
    // Initialize output
    document.getElementById('countingOutput').textContent = 'Current Count: ' + getStackNumber();
    typesetMath();