// scripts.js

// Function for Rearranging to Make Bases (RMB)
function runRMBAutomaton() {
    const outputDiv = document.getElementById('rmbOutput');
    const a1 = parseInt(document.getElementById('addend1').value);
    const a2 = parseInt(document.getElementById('addend2').value);

    let steps = `Initial Addends: ${a1} + ${a2}<br>`;

    const onesNeeded = 10 - (a1 % 10);
    steps += `Ones needed to make ${a1} a full base (multiple of 10): ${onesNeeded}<br>`;

    if (a2 >= onesNeeded && onesNeeded > 0) {
        const adjustedA1 = a1 + onesNeeded;
        const adjustedA2 = a2 - onesNeeded;
        steps += `Move ${onesNeeded} ones from ${a2} to ${a1} to make ${adjustedA1}<br>`;
        steps += `Adjusted Addends: ${adjustedA1} + ${adjustedA2}<br>`;
        const sum = adjustedA1 + adjustedA2;
        steps += `Sum: ${sum}`;
    } else {
        steps += `Not enough ones in second addend to make first addend a full base or no need to rearrange.<br>`;
        const sum = a1 + a2;
        steps += `Sum without rearranging: ${sum}`;
    }

    outputDiv.innerHTML = steps;
}

// Function for Rounding and Adjusting
function runRoundingAutomaton() {
    const outputDiv = document.getElementById('roundingOutput');
    const a1 = parseInt(document.getElementById('roundAddend1').value);
    const a2 = parseInt(document.getElementById('roundAddend2').value);

    let steps = `Initial Addends: ${a1} + ${a2}<br>`;

    // Determine which addend to round based on the smallest adjustment
    const remainderA1 = a1 % 10;
    const remainderA2 = a2 % 10;

    const adjustmentA1 = remainderA1 === 0 ? 0 : 10 - remainderA1;
    const adjustmentA2 = remainderA2 === 0 ? 0 : 10 - remainderA2;

    steps += `Adjustment needed for A1: ${adjustmentA1}<br>`;
    steps += `Adjustment needed for A2: ${adjustmentA2}<br>`;

    if (adjustmentA1 <= adjustmentA2 && adjustmentA1 > 0) {
        // Round a1
        const roundedA1 = a1 + adjustmentA1;
        steps += `Rounded A1 up to: ${roundedA1} (added ${adjustmentA1})<br>`;
        // Compute preliminary sum
        const preliminarySum = roundedA1 + a2;
        steps += `Preliminary Sum: ${roundedA1} + ${a2} = ${preliminarySum}<br>`;
        // Adjust the sum
        const finalSum = preliminarySum - adjustmentA1;
        steps += `Final Sum: ${preliminarySum} - ${adjustmentA1} = ${finalSum}<br>`;
    } else if (adjustmentA2 > 0) {
        // Round a2
        const roundedA2 = a2 + adjustmentA2;
        steps += `Rounded A2 up to: ${roundedA2} (added ${adjustmentA2})<br>`;
        // Compute preliminary sum
        const preliminarySum = a1 + roundedA2;
        steps += `Preliminary Sum: ${a1} + ${roundedA2} = ${preliminarySum}<br>`;
        // Adjust the sum
        const finalSum = preliminarySum - adjustmentA2;
        steps += `Final Sum: ${preliminarySum} - ${adjustmentA2} = ${finalSum}<br>`;
    } else {
        // No need to rearrange
        steps += `No need to rearrange.<br>`;
        const sum = a1 + a2;
        steps += `Sum without rearranging: ${sum}<br>`;
    }

    outputDiv.innerHTML = steps;
}

// Function for Counting On by Bases and Then Ones (COBO)
function runCOBOAutomaton() {
    const outputDiv = document.getElementById('coboOutput');
    const a1 = parseInt(document.getElementById('coboAddend1').value);
    const a2 = parseInt(document.getElementById('coboAddend2').value);

    let steps = `Starting Value: ${a1}<br>`;

    // Extract tens and ones from second addend
    const tens = Math.floor(a2 / 10);
    const ones = a2 % 10;

    steps += `<br>Adding Tens:<br>`;

    let currentSum = a1;
    for (let i = 1; i <= tens; i++) {
        currentSum += 10;
        steps += `Step ${i}: ${currentSum - 10} + 10 = ${currentSum}<br>`;
    }

    steps += `<br>Adding Ones:<br>`;
    for (let i = 1; i <= ones; i++) {
        currentSum += 1;
        steps += `Step ${i}: ${currentSum - 1} + 1 = ${currentSum}<br>`;
    }

    steps += `<br>Final Sum: ${currentSum}`;

    outputDiv.innerHTML = steps;
}

// Function for Coordinating Two Counts by Ones (C2C)
function runC2CAutomaton() {
    const outputDiv = document.getElementById('c2cOutput');
    const groups = parseInt(document.getElementById('c2cGroups').value);
    const itemsPerGroup = parseInt(document.getElementById('c2cItems').value);

    let steps = '';
    let total = 0;

    steps += `Counting items in each group:<br>`;
    for (let i = 1; i <= groups; i++) {
        steps += `Group ${i}:<br>`;
        for (let j = 1; j <= itemsPerGroup; j++) {
            total += 1;
            steps += `Counted item ${j}, Total so far: ${total}<br>`;
        }
    }
    steps += `<br>Final Total: ${total}`;

    outputDiv.innerHTML = steps;
}

// Function for Rearranging to Make Bases (RMB) Automaton
function runRMBAutomaton_JS() {
    const outputDiv = document.getElementById('rmbOutput');
    const a1 = parseInt(document.getElementById('addend1').value);
    const a2 = parseInt(document.getElementById('addend2').value);

    let steps = `Initial Addends: ${a1} + ${a2}<br>`;

    const onesNeeded = 10 - (a1 % 10);
    steps += `Ones needed to make ${a1} a full base (multiple of 10): ${onesNeeded}<br>`;

    if (a2 >= onesNeeded && onesNeeded > 0) {
        const adjustedA1 = a1 + onesNeeded;
        const adjustedA2 = a2 - onesNeeded;
        steps += `Move ${onesNeeded} ones from ${a2} to ${a1} to make ${adjustedA1}<br>`;
        steps += `Adjusted Addends: ${adjustedA1} + ${adjustedA2}<br>`;
        const sum = adjustedA1 + adjustedA2;
        steps += `Sum: ${sum}`;
    } else {
        steps += `Not enough ones in second addend to make first addend a full base or no need to rearrange.<br>`;
        const sum = a1 + a2;
        steps += `Sum without rearranging: ${sum}`;
    }

    outputDiv.innerHTML = steps;
}

// Function for Commutative Packaging Automaton
function runCommutativeAutomaton() {
    const outputDiv = document.getElementById('commuteOutput');
    const a = parseInt(document.getElementById('commuteA').value);
    const b = parseInt(document.getElementById('commuteB').value);

    const total = a * b;

    let steps = '';
    steps += `Original Multiplication: ${a} \u00D7 ${b} = ${total}<br>`;
    steps += `Interpreted as ${a} groups of ${b} items.<br><br>`;

    steps += `Reorganizing:<br>`;
    steps += `Switching factors to ${b} \u00D7 ${a}<br>`;
    steps += `Interpreted as ${b} groups of ${a} items.<br><br>`;

    steps += `Total items remain the same: ${total}`;

    outputDiv.innerHTML = steps;
}

// Function for Division Conversion Automaton
function runDivisionAutomaton() {
    const outputDiv = document.getElementById('divisionOutput');
    const totalItems = parseInt(document.getElementById('divTotalItems').value);
    const groupSize = parseInt(document.getElementById('divGroupSize').value);

    const groups = Math.floor(totalItems / groupSize);
    const remainder = totalItems % groupSize;

    let steps = '';
    steps += `Total Items: ${totalItems}<br>`;
    steps += `Group Size: ${groupSize}<br><br>`;
    steps += `Number of Full Groups: ${groups}<br>`;
    if (remainder > 0) {
        steps += `Remaining Items: ${remainder}<br>`;
    } else {
        steps += `No Remaining Items.<br>`;
    }

    outputDiv.innerHTML = steps;
}

// Additional functions for other automata can be added here following the same pattern
