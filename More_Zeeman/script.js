document.addEventListener('DOMContentLoaded', () => {

    // --- MATRIX AUTOMATON CLASS ---
    // Manages the state and rendering of the growing matrix.
    class MatrixAutomaton {
        constructor(initialSize = 2) {
            this.gridContainer = document.getElementById('matrix-grid');
            this.matrix = this.createInitialMatrix(initialSize);
            this.maxSize = 8;
            this.render();
        }

        // Creates the starting matrix with alternating M and W
        createInitialMatrix(size) {
            const matrix = [];
            for (let i = 0; i < size; i++) {
                matrix.push([]);
                for (let j = 0; j < size; j++) {
                    matrix[i].push((i + j) % 2 === 0 ? 'M' : 'W');
                }
            }
            return matrix;
        }

        // The core logic for updating the matrix based on Cantorian diagonalization
        grow(finalCharacter) {
            let baseMatrix = this.matrix;
            let newSize = this.matrix.length + 1;

            // If matrix is at max size, create a "ripple" effect by shifting it
            if (this.matrix.length >= this.maxSize) {
                newSize = this.maxSize;
                const shifted = [];
                // Create a new matrix from the bottom-right (size-1)x(size-1) section
                for (let i = 1; i < this.matrix.length; i++) {
                    shifted.push(this.matrix[i].slice(1));
                }
                baseMatrix = shifted;
            }

            const n = baseMatrix.length;
            const newMatrix = Array(newSize).fill(null).map(() => Array(newSize).fill(null));

            // 1. Copy base matrix (either the original or the shifted one)
            for (let i = 0; i < n; i++) {
                for (let j = 0; j < n; j++) {
                    newMatrix[i][j] = baseMatrix[i][j];
                }
            }

            // 2. Get diagonal of the base matrix and negate it
            const diagonal = [];
            for (let i = 0; i < n; i++) {
                diagonal.push(baseMatrix[i][i]);
            }
            const negatedDiagonal = diagonal.map(char => (char === 'M' ? 'W' : 'M'));

            // 3. Fill the last row and column with the negated diagonal
            for (let i = 0; i < n; i++) {
                newMatrix[n][i] = negatedDiagonal[i]; // New row
                newMatrix[i][n] = negatedDiagonal[i]; // New column
            }

            // 4. Fill the final corner with the new character from the Zeeman machine
            newMatrix[n][n] = finalCharacter;

            this.matrix = newMatrix;
            this.render();
        }

        // Renders the matrix data into the DOM
        render() {
            this.gridContainer.innerHTML = '';
            const size = this.matrix.length;
            this.gridContainer.style.gridTemplateColumns = `repeat(${size}, 40px)`;

            for (let i = 0; i < size; i++) {
                for (let j = 0; j < size; j++) {
                    const cell = document.createElement('div');
                    cell.classList.add('matrix-cell');
                    cell.textContent = this.matrix[i][j] || '';
                    this.gridContainer.appendChild(cell);
                }
            }
        }
    }

    // --- ZEEMAN CATASTROPHE MACHINE CLASS ---
    // Manages the physics simulation and rendering of the Zeeman machine.
    class ZeemanMachine {
        constructor(matrixAutomaton) {
            this.canvas = document.getElementById('zeeman-canvas');
            this.ctx = this.canvas.getContext('2d');
            this.automaton = matrixAutomaton;

            // DOM Elements
            this.statusEl = document.getElementById('status-value');
            this.timeSpeedSlider = document.getElementById('time-speed');

            // Parameters
            this.width = this.canvas.width;
            this.height = this.canvas.height;
            this.wheelRadius = 60;
            this.wheelCenter = { x: this.width / 2, y: this.height / 2 - 50 };
            this.fixedAnchor = { x: this.width / 2, y: 50 };
            this.controlPoint = { x: this.width / 2, y: this.height - 100 };
            this.cuspOrigin = { x: this.width / 2, y: 450 }; // Center point for the cusp and axes

            // Physics state
            this.angle = Math.PI / 2;
            this.angularVelocity = 0;
            this.damping = 0.95;
            
            // Set a slower default speed and update the slider to match
            this.timeSpeed = 0.4;
            this.timeSpeedSlider.value = this.timeSpeed;
            
            // Machine state
            this.isStable = true;
            this.lastStableOutput = null;

            this.bindEvents();
            this.loop();
        }

        bindEvents() {
            this.canvas.addEventListener('mousemove', e => {
                const rect = this.canvas.getBoundingClientRect();
                this.controlPoint.x = e.clientX - rect.left;
                this.controlPoint.y = e.clientY - rect.top;
            });
            this.timeSpeedSlider.addEventListener('input', e => {
                this.timeSpeed = parseFloat(e.target.value);
            });
        }

        // The main physics and state update method
        update() {
            // Simplified potential energy calculation to find torque
            const attachmentPoint = {
                x: this.wheelCenter.x + this.wheelRadius * Math.cos(this.angle),
                y: this.wheelCenter.y + this.wheelRadius * Math.sin(this.angle)
            };

            const toFixed = { x: this.fixedAnchor.x - attachmentPoint.x, y: this.fixedAnchor.y - attachmentPoint.y };
            const toControl = { x: this.controlPoint.x - attachmentPoint.x, y: this.controlPoint.y - attachmentPoint.y };

            // Torque is related to the cross product of the force vectors and the radius vector
            const torque = (toFixed.x + toControl.x) * Math.sin(this.angle) - (toFixed.y + toControl.y) * Math.cos(this.angle);
            
            // Apply torque to velocity
            this.angularVelocity += (torque / 5000) * this.timeSpeed; // Scaled for effect
            this.angularVelocity *= this.damping; // Apply damping
            this.angle += this.angularVelocity * this.timeSpeed; // Update angle

            // --- State Management ---
            const wasStable = this.isStable;
            this.isStable = Math.abs(this.angularVelocity) < 0.001;

            if (this.isStable) {
                this.statusEl.textContent = 'Stable';
                // The output character is determined by the wheel's orientation
                const currentOutput = this.angle > Math.PI / 2 && this.angle < 3 * Math.PI / 2 ? 'W' : 'M';
                
                // If the machine JUST became stable, trigger the automaton
                if (!wasStable) {
                    this.automaton.grow(currentOutput);
                }
                this.lastStableOutput = currentOutput;

            } else {
                this.statusEl.textContent = 'Superimposed';
                this.lastStableOutput = null; // In a superimposed state, there is no output
            }
        }

        // The main rendering method
        draw() {
            this.ctx.clearRect(0, 0, this.width, this.height);
            
            // --- Draw Axes and Cusp ---
            this.ctx.save();
            this.ctx.translate(this.cuspOrigin.x, this.cuspOrigin.y);

            // Draw u-v axes
            this.ctx.beginPath();
            this.ctx.moveTo(-this.width / 2, 0);
            this.ctx.lineTo(this.width / 2, 0);
            this.ctx.moveTo(0, -100);
            this.ctx.lineTo(0, 100);
            this.ctx.strokeStyle = '#7f8c8d';
            this.ctx.lineWidth = 1;
            this.ctx.stroke();

            // Draw axis labels
            this.ctx.font = '16px sans-serif';
            this.ctx.fillStyle = '#34495e';
            this.ctx.textAlign = 'center';
            this.ctx.fillText('u', 0, 120);
            this.ctx.textAlign = 'left';
            this.ctx.fillText('v', this.width / 2 - 20, 5);

            // Draw astroid cusp using a parametric equation
            this.ctx.beginPath();
            const cuspSize = 50;
            const steps = 100;
            for (let i = 0; i <= steps; i++) {
                const t = (i / steps) * 2 * Math.PI;
                const x = cuspSize * Math.pow(Math.cos(t), 3);
                const y = cuspSize * Math.pow(Math.sin(t), 3);
                if (i === 0) {
                    this.ctx.moveTo(x, y);
                } else {
                    this.ctx.lineTo(x, y);
                }
            }
            this.ctx.strokeStyle = '#34495e';
            this.ctx.lineWidth = 2;
            this.ctx.stroke();
            this.ctx.restore();


            // Draw fixed anchor
            this.ctx.beginPath();
            this.ctx.arc(this.fixedAnchor.x, this.fixedAnchor.y, 8, 0, 2 * Math.PI);
            this.ctx.fillStyle = '#c0392b';
            this.ctx.fill();

            // Draw wheel
            this.ctx.beginPath();
            this.ctx.arc(this.wheelCenter.x, this.wheelCenter.y, this.wheelRadius, 0, 2 * Math.PI);
            this.ctx.fillStyle = '#ecf0f1';
            this.ctx.fill();
            this.ctx.strokeStyle = '#7f8c8d';
            this.ctx.lineWidth = 3;
            this.ctx.stroke();

            // Draw the character 'M' and rotate it with the wheel, regardless of state
            this.ctx.save();
            this.ctx.font = 'bold 60px sans-serif';
            this.ctx.fillStyle = '#2c3e50';
            this.ctx.textAlign = 'center';
            this.ctx.textBaseline = 'middle';
            
            // Translate context to the wheel's center
            this.ctx.translate(this.wheelCenter.x, this.wheelCenter.y);
            // Rotate the context by the wheel's angle (add PI/2 to make it upright at the top)
            this.ctx.rotate(this.angle + Math.PI / 2);
            
            this.ctx.fillText('M', 0, 0);
            
            this.ctx.restore();

            // Draw attachment point and elastic bands
            const attachmentPoint = {
                x: this.wheelCenter.x + this.wheelRadius * Math.cos(this.angle),
                y: this.wheelCenter.y + this.wheelRadius * Math.sin(this.angle)
            };
            this.ctx.beginPath();
            this.ctx.moveTo(this.fixedAnchor.x, this.fixedAnchor.y);
            this.ctx.lineTo(attachmentPoint.x, attachmentPoint.y);
            this.ctx.strokeStyle = '#c0392b'; // Red
            this.ctx.stroke();

            this.ctx.beginPath();
            this.ctx.moveTo(attachmentPoint.x, attachmentPoint.y);
            this.ctx.lineTo(this.controlPoint.x, this.controlPoint.y);
            this.ctx.strokeStyle = '#2980b9'; // Blue
            this.ctx.stroke();
            
            // Draw control point
            this.ctx.beginPath();
            this.ctx.arc(this.controlPoint.x, this.controlPoint.y, 8, 0, 2 * Math.PI);
            this.ctx.fillStyle = '#2980b9';
            this.ctx.fill();
        }

        // The animation loop
        loop() {
            this.update();
            this.draw();
            requestAnimationFrame(() => this.loop());
        }
    }

    // --- INITIALIZATION ---
    const matrixAutomaton = new MatrixAutomaton();
    const zeemanMachine = new ZeemanMachine(matrixAutomaton);
});
