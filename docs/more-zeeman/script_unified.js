document.addEventListener('DOMContentLoaded', () => {

    // --- MATRIX AUTOMATON CLASS ---
    class MatrixAutomaton {
        constructor() {
            this.gridContainer = document.getElementById('matrix-grid');
            this.matrix = [['M']]; // Start with single M
            this.maxSize = 8;
            this.render();
        }

        // Cantorian diagonalization
        grow(finalCharacter) {
            let baseMatrix = this.matrix;
            let newSize = this.matrix.length + 1;

            // Ripple effect when at max size
            if (this.matrix.length >= this.maxSize) {
                newSize = this.maxSize;
                const shifted = [];
                for (let i = 1; i < this.matrix.length; i++) {
                    shifted.push(this.matrix[i].slice(1));
                }
                baseMatrix = shifted;
            }

            const n = baseMatrix.length;
            const newMatrix = Array(newSize).fill(null).map(() => Array(newSize).fill(null));

            // Copy base matrix
            for (let i = 0; i < n; i++) {
                for (let j = 0; j < n; j++) {
                    newMatrix[i][j] = baseMatrix[i][j];
                }
            }

            // Get diagonal and negate it
            const diagonal = [];
            for (let i = 0; i < n; i++) {
                diagonal.push(baseMatrix[i][i]);
            }
            const negatedDiagonal = diagonal.map(char => (char === 'M' ? 'W' : 'M'));

            // Fill last row and column with negated diagonal
            for (let i = 0; i < n; i++) {
                newMatrix[n][i] = negatedDiagonal[i];
                newMatrix[i][n] = negatedDiagonal[i];
            }

            // Fill corner with new character from Zeeman machine
            newMatrix[n][n] = finalCharacter;

            this.matrix = newMatrix;
            this.render();
        }

        render() {
            this.gridContainer.innerHTML = '';
            const size = this.matrix.length;
            this.gridContainer.style.gridTemplateColumns = `repeat(${size}, 50px)`;

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

    // --- ZEEMAN CATASTROPHE MACHINE CLASS (WITH PROPER PHYSICS) ---
    class ZeemanMachine {
        constructor(matrixAutomaton) {
            this.canvas = document.getElementById('zeeman-canvas');
            this.ctx = this.canvas.getContext('2d');
            this.automaton = matrixAutomaton;

            // DOM Elements
            this.statusEl = document.getElementById('status-value');
            this.timeSpeedSlider = document.getElementById('time-speed');
            this.springKSlider = document.getElementById('spring-k');
            this.naturalLengthSlider = document.getElementById('natural-length');
            this.kValueDisplay = document.getElementById('k-value');
            this.l0ValueDisplay = document.getElementById('l0-value');
            this.timeValueDisplay = document.getElementById('time-value');

            // Canvas parameters
            this.width = this.canvas.width;
            this.height = this.canvas.height;
            this.wheelRadius = 60;
            this.attachmentRadius = 50; // R
            this.wheelCenter = { x: this.width / 2, y: this.height / 2 };
            this.fixedAnchor = { x: this.width / 2, y: 50 };

            // Physics parameters (user-controllable)
            this.springK = 2.0; // Spring constant
            this.L0 = 100; // Natural length
            this.timeSpeed = 1.0;

            // Physics state
            this.angle = Math.PI / 2; // Start pointing down ('M')
            this.previousAngle = this.angle;

            // Interaction state
            this.isDragging = false;
            this.controlPoint = { x: this.width / 2 + 100, y: this.height / 2 + 100 };

            // Machine state
            this.isStable = true;
            this.currentStability = 1.0;

            this.bindEvents();
            this.loop();
        }

        bindEvents() {
            // Control point dragging
            this.canvas.addEventListener('mousedown', e => {
                const rect = this.canvas.getBoundingClientRect();
                const mx = e.clientX - rect.left;
                const my = e.clientY - rect.top;
                const dist = Math.sqrt(Math.pow(mx - this.controlPoint.x, 2) + Math.pow(my - this.controlPoint.y, 2));
                if (dist < 15) {
                    this.isDragging = true;
                }
            });

            window.addEventListener('mousemove', e => {
                if (this.isDragging) {
                    const rect = this.canvas.getBoundingClientRect();
                    this.controlPoint.x = Math.max(0, Math.min(this.width, e.clientX - rect.left));
                    this.controlPoint.y = Math.max(0, Math.min(this.height, e.clientY - rect.top));
                }
            });

            window.addEventListener('mouseup', () => {
                this.isDragging = false;
            });

            // Parameter sliders
            this.timeSpeedSlider.addEventListener('input', e => {
                this.timeSpeed = parseFloat(e.target.value);
                this.timeValueDisplay.textContent = this.timeSpeed.toFixed(1);
            });

            this.springKSlider.addEventListener('input', e => {
                this.springK = parseFloat(e.target.value);
                this.kValueDisplay.textContent = this.springK.toFixed(1);
            });

            this.naturalLengthSlider.addEventListener('input', e => {
                this.L0 = parseFloat(e.target.value);
                this.l0ValueDisplay.textContent = this.L0.toFixed(0);
            });
        }

        // Get attachment point on wheel rim
        getAttachmentPoint(angle) {
            return {
                x: this.wheelCenter.x + this.attachmentRadius * Math.cos(angle),
                y: this.wheelCenter.y + this.attachmentRadius * Math.sin(angle)
            };
        }

        // Calculate potential energy using Hooke's Law: E = 0.5*k*(L-L0)^2
        calculateEnergy(angle) {
            const P = this.getAttachmentPoint(angle);

            // Calculate current lengths
            const L1 = Math.sqrt(Math.pow(P.x - this.fixedAnchor.x, 2) + Math.pow(P.y - this.fixedAnchor.y, 2));
            const L2 = Math.sqrt(Math.pow(P.x - this.controlPoint.x, 2) + Math.pow(P.y - this.controlPoint.y, 2));

            // Energy only if stretched beyond L0
            const E1 = (L1 > this.L0) ? 0.5 * this.springK * Math.pow(L1 - this.L0, 2) : 0;
            const E2 = (L2 > this.L0) ? 0.5 * this.springK * Math.pow(L2 - this.L0, 2) : 0;

            return E1 + E2;
        }

        // Calculate gradient (torque) - dE/dAngle
        calculateGradient(angle) {
            const R = this.attachmentRadius;

            // Coordinates relative to wheel center
            const Fx = this.fixedAnchor.x - this.wheelCenter.x;
            const Fy = this.fixedAnchor.y - this.wheelCenter.y;
            const Cx = this.controlPoint.x - this.wheelCenter.x;
            const Cy = this.controlPoint.y - this.wheelCenter.y;

            // Attachment point relative to center
            const Px = R * Math.cos(angle);
            const Py = R * Math.sin(angle);

            // Calculate lengths
            const L1 = Math.sqrt(Math.pow(Px - Fx, 2) + Math.pow(Py - Fy, 2));
            const L2 = Math.sqrt(Math.pow(Px - Cx, 2) + Math.pow(Py - Cy, 2));

            let Term1 = 0;
            if (L1 > this.L0 && L1 > 0.001) {
                Term1 = this.springK * (1 - this.L0 / L1) * (Fx * Math.sin(angle) - Fy * Math.cos(angle));
            }

            let Term2 = 0;
            if (L2 > this.L0 && L2 > 0.001) {
                Term2 = this.springK * (1 - this.L0 / L2) * (Cx * Math.sin(angle) - Cy * Math.cos(angle));
            }

            return R * (Term1 + Term2);
        }

        // Estimate stability (second derivative) using central differences
        estimateStability(angle) {
            const dA = 0.01;
            const gradientPlus = this.calculateGradient(angle + dA);
            const gradientMinus = this.calculateGradient(angle - dA);
            return (gradientPlus - gradientMinus) / (2 * dA);
        }

        // Update disc state using gradient descent (finds local minimum - hysteresis)
        updateState() {
            let newAngle = this.angle;
            const learningRate = 0.001;
            const maxIterations = 250;
            const convergenceThreshold = 0.05;
            let stability = 0;

            // Gradient descent loop
            for (let i = 0; i < maxIterations; i++) {
                const gradient = this.calculateGradient(newAngle);

                if (Math.abs(gradient) < convergenceThreshold) {
                    stability = this.estimateStability(newAngle);
                    if (stability > 0) break; // Found stable minimum
                }

                let step = learningRate * gradient;
                const maxStep = 0.1;
                step = Math.max(-maxStep, Math.min(maxStep, step));

                newAngle -= step;
                newAngle = (newAngle + 4 * Math.PI) % (2 * Math.PI);
            }

            if (stability <= 0) {
                stability = this.estimateStability(newAngle);
            }

            // Detect catastrophe (snap)
            let angleDiff = newAngle - this.angle;
            while (angleDiff > Math.PI) angleDiff -= 2 * Math.PI;
            while (angleDiff < -Math.PI) angleDiff += 2 * Math.PI;

            let snapped = Math.abs(angleDiff) > 0.6;

            this.angle = newAngle;
            this.currentStability = stability / 5000; // Normalize for visualization
            this.currentStability = Math.max(0, Math.min(1, this.currentStability));

            return snapped;
        }

        update() {
            const wasStable = this.isStable;
            const snapped = this.updateState();

            // Determine current output character
            const currentOutput = (this.angle > Math.PI / 2 && this.angle < 3 * Math.PI / 2) ? 'W' : 'M';

            // Check stability
            this.isStable = this.currentStability > 0.3;

            if (this.isStable) {
                this.statusEl.textContent = 'Stable - ' + currentOutput;
                // Trigger matrix growth on catastrophe
                if (snapped && !wasStable) {
                    this.automaton.grow(currentOutput);
                }
            } else {
                this.statusEl.textContent = 'Superimposed (Indeterminate)';
            }
        }

        draw() {
            this.ctx.clearRect(0, 0, this.width, this.height);

            const P = this.getAttachmentPoint(this.angle);

            // Calculate lengths for visualization
            const L1 = Math.sqrt(Math.pow(P.x - this.fixedAnchor.x, 2) + Math.pow(P.y - this.fixedAnchor.y, 2));
            const L2 = Math.sqrt(Math.pow(P.x - this.controlPoint.x, 2) + Math.pow(P.y - this.controlPoint.y, 2));

            // Draw elastic bands
            this.ctx.lineWidth = 3;

            // Band to fixed anchor
            this.ctx.beginPath();
            this.ctx.moveTo(this.fixedAnchor.x, this.fixedAnchor.y);
            this.ctx.lineTo(P.x, P.y);
            if (L1 <= this.L0) {
                this.ctx.setLineDash([5, 5]); // Dashed if slack
                this.ctx.strokeStyle = '#999';
            } else {
                this.ctx.setLineDash([]);
                this.ctx.strokeStyle = '#c0392b'; // Red
            }
            this.ctx.stroke();

            // Band to control point
            this.ctx.beginPath();
            this.ctx.moveTo(this.controlPoint.x, this.controlPoint.y);
            this.ctx.lineTo(P.x, P.y);
            if (L2 <= this.L0) {
                this.ctx.setLineDash([5, 5]); // Dashed if slack
                this.ctx.strokeStyle = '#999';
            } else {
                this.ctx.setLineDash([]);
                this.ctx.strokeStyle = '#2980b9'; // Blue
            }
            this.ctx.stroke();
            this.ctx.setLineDash([]);

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

            // Draw 'M' character rotating with wheel
            this.ctx.save();
            this.ctx.font = 'bold 60px sans-serif';
            this.ctx.fillStyle = this.isStable ? '#2c3e50' : 'rgba(44, 62, 80, 0.3)';
            this.ctx.textAlign = 'center';
            this.ctx.textBaseline = 'middle';
            this.ctx.translate(this.wheelCenter.x, this.wheelCenter.y);
            this.ctx.rotate(this.angle + Math.PI / 2); // +90° to make M upright

            // Apply blur if unstable
            if (!this.isStable) {
                this.ctx.filter = `blur(${(1 - this.currentStability) * 5}px)`;
            }

            this.ctx.fillText('M', 0, 0);
            this.ctx.restore();

            // Draw attachment point
            this.ctx.beginPath();
            this.ctx.arc(P.x, P.y, 6, 0, 2 * Math.PI);
            this.ctx.fillStyle = '#555';
            this.ctx.fill();

            // Draw control point
            this.ctx.beginPath();
            this.ctx.arc(this.controlPoint.x, this.controlPoint.y, 12, 0, 2 * Math.PI);
            this.ctx.fillStyle = '#2980b9';
            this.ctx.fill();
            this.ctx.strokeStyle = this.isDragging ? '#fff' : '#333';
            this.ctx.lineWidth = 2;
            this.ctx.stroke();
        }

        loop() {
            this.update();
            this.draw();
            requestAnimationFrame(() => this.loop());
        }
    }

    // --- ACOUSTIC VISUALIZATION CLASS ---
    class AcousticVisualization {
        constructor(zeemanMachine) {
            this.zeeman = zeemanMachine;
            this.svg = document.getElementById('acoustic-svg');
            this.svgNS = "http://www.w3.org/2000/svg";

            // Parameters
            this.width = 500;
            this.height = 300;
            this.numParticles = 200;
            this.particleRadius = 2;
            this.speakerWidth = 40;
            this.speakerHeight = 150;
            this.speakerX = 60;
            this.speakerY = (this.height - this.speakerHeight) / 2;
            this.chamberEndX = this.width - 20;

            // Wave propagation
            this.waveSpeed = 6;
            this.historySeconds = 1.5;
            this.fps = 60;
            this.history = new Array(Math.ceil(this.fps * this.historySeconds)).fill(0);
            this.historyIndex = 0;
            this.maxDisplacement = 30;
            this.sensitivityScale = 15;
            this.smoothedDisplacement = 0;
            this.smoothingFactor = 0.2;

            this.particles = [];
            this.pistonRect = null;

            this.init();
        }

        init() {
            this.svg.innerHTML = '';
            this.particles = [];

            // Draw walls
            const drawWall = (y) => {
                const wall = document.createElementNS(this.svgNS, 'line');
                wall.setAttribute('x1', this.speakerX);
                wall.setAttribute('y1', y);
                wall.setAttribute('x2', this.chamberEndX);
                wall.setAttribute('y2', y);
                wall.setAttribute('stroke', '#999');
                wall.setAttribute('stroke-width', '2');
                this.svg.appendChild(wall);
            };
            drawWall(this.speakerY);
            drawWall(this.speakerY + this.speakerHeight);

            // Draw piston
            this.pistonRect = document.createElementNS(this.svgNS, 'rect');
            this.pistonRect.setAttribute('x', this.speakerX - this.speakerWidth);
            this.pistonRect.setAttribute('y', this.speakerY);
            this.pistonRect.setAttribute('width', this.speakerWidth);
            this.pistonRect.setAttribute('height', this.speakerHeight);
            this.pistonRect.setAttribute('fill', '#aaa');
            this.pistonRect.setAttribute('stroke', '#666');
            this.pistonRect.setAttribute('stroke-width', '2');
            this.svg.appendChild(this.pistonRect);

            // Create air particles in a grid
            const particleGroup = document.createElementNS(this.svgNS, 'g');
            this.svg.appendChild(particleGroup);

            const startX = this.speakerX + 5;
            const endX = this.chamberEndX - 5;
            const startY = this.speakerY + 5;
            const endY = this.speakerY + this.speakerHeight - 5;

            const area = (endX - startX) * (endY - startY);
            const density = this.numParticles / area;
            const spacing = Math.sqrt(1 / density);

            const numX = Math.floor((endX - startX) / spacing);
            const numY = Math.floor((endY - startY) / spacing);

            for (let i = 0; i <= numX; i++) {
                for (let j = 0; j <= numY; j++) {
                    if (this.particles.length >= this.numParticles) break;

                    const baseX = startX + i * spacing + (Math.random() - 0.5) * spacing * 0.4;
                    const baseY = startY + j * spacing + (Math.random() - 0.5) * spacing * 0.4;

                    const circle = document.createElementNS(this.svgNS, 'circle');
                    circle.setAttribute('cx', baseX.toFixed(1));
                    circle.setAttribute('cy', baseY.toFixed(1));
                    circle.setAttribute('r', this.particleRadius);
                    circle.setAttribute('fill', '#667eea');
                    circle.setAttribute('opacity', '0.7');
                    particleGroup.appendChild(circle);

                    this.particles.push({ element: circle, baseX, baseY });
                }
            }
        }

        animate() {
            // Calculate angular velocity (change in angle)
            let dTheta = this.zeeman.angle - this.zeeman.previousAngle;
            while (dTheta > Math.PI) dTheta -= 2 * Math.PI;
            while (dTheta < -Math.PI) dTheta += 2 * Math.PI;

            // Target displacement based on velocity
            const targetDisplacement = this.maxDisplacement * Math.tanh(dTheta * this.sensitivityScale);

            // Smooth the displacement
            this.smoothedDisplacement += (targetDisplacement - this.smoothedDisplacement) * this.smoothingFactor;

            // Store in history for wave propagation
            this.history[this.historyIndex] = this.smoothedDisplacement;
            this.historyIndex = (this.historyIndex + 1) % this.history.length;

            // Move piston
            this.pistonRect.setAttribute('x', (this.speakerX - this.speakerWidth + this.smoothedDisplacement).toFixed(2));

            // Calculate piston face position
            const pistonFaceX = this.speakerX + this.smoothedDisplacement;

            // Update particle positions (wave propagation)
            for (const p of this.particles) {
                const distance = p.baseX - this.speakerX;
                const timeDelay = distance / this.waveSpeed;
                let lookBackIndex = Math.round(this.historyIndex - 1 - timeDelay);
                lookBackIndex = (lookBackIndex % this.history.length + this.history.length) % this.history.length;

                const historicalDisplacement = this.history[lookBackIndex] || 0;
                const damping = Math.exp(-0.003 * distance);
                const currentX = p.baseX + historicalDisplacement * damping;

                // Clamp to stay within chamber
                const clampedX = Math.max(
                    pistonFaceX + this.particleRadius,
                    Math.min(this.chamberEndX - this.particleRadius, currentX)
                );
                p.element.setAttribute('cx', clampedX.toFixed(2));
            }

            // Update previousAngle for next frame
            this.zeeman.previousAngle = this.zeeman.angle;
        }
    }

    // --- INITIALIZATION ---
    const matrixAutomaton = new MatrixAutomaton();
    const zeemanMachine = new ZeemanMachine(matrixAutomaton);
    const acousticViz = new AcousticVisualization(zeemanMachine);

    // Animation loop for acoustic visualization
    function animateAll() {
        acousticViz.animate();
        requestAnimationFrame(animateAll);
    }
    animateAll();
});
