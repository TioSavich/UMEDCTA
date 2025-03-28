"use strict";

// --- Constants ---
const SVG_NS = "http://www.w3.org/2000/svg";

// ZCM Logical Space & Mapping
// Define the logical range for u and v that the user interacts with visually.
// Let's center it around the typical bifurcation point location.
const LOGICAL_U_MIN = -1.0; // Roughly corresponds to original mY range around Oy
const LOGICAL_U_MAX = 3.0;  // Need enough range to explore cusp
const LOGICAL_V_MIN = -1.0;
const LOGICAL_V_MAX = 1.0;
const LOGICAL_WIDTH = LOGICAL_V_MAX - LOGICAL_V_MIN;
const LOGICAL_HEIGHT = LOGICAL_U_MAX - LOGICAL_U_MIN;

// SVG ViewBox and Scaling for ZCM
const ZCM_SVG_VIEWBOX_X = -200;
const ZCM_SVG_VIEWBOX_Y = -250;
const ZCM_SVG_VIEWBOX_WIDTH = 400;
const ZCM_SVG_VIEWBOX_HEIGHT = 400;
// Calculate scaling factors to map logical units to SVG units
const ZCM_SCALE_X = ZCM_SVG_VIEWBOX_WIDTH / LOGICAL_WIDTH;  // SVG units per logical V unit
const ZCM_SCALE_Y = ZCM_SVG_VIEWBOX_HEIGHT / LOGICAL_HEIGHT; // SVG units per logical U unit
// Define where logical (u=0, v=0) sits in SVG coordinates
const ZCM_SVG_ORIGIN_X = 0;
const ZCM_SVG_ORIGIN_Y = 100; // Place logical u=0 lower down visually

// ZCM Physics constants & parameters implicitly used in getVt
// const ZCM_PHYSICS_LA = 1.40407148348; // Location offset from original - not directly used if u,v are absolute
const ZCM_WHEEL_RADIUS_LOGICAL = 0.5; // Implicit radius in getVt formula

// Speaker SVG & Wave
const SPEAKER_SVG_VIEWBOX_WIDTH = 550;
const SPEAKER_SVG_VIEWBOX_HEIGHT = 350;
const SPEAKER_BASE_X = 60;
const SPEAKER_WIDTH = 30;
const SPEAKER_HEIGHT = 120;
const SPEAKER_Y = (SPEAKER_SVG_VIEWBOX_HEIGHT - SPEAKER_HEIGHT) / 2;
const CHAMBER_END_X = 500;
const CHAMBER_WIDTH = CHAMBER_END_X - SPEAKER_BASE_X;
const NUM_AIR_PARTICLES = 200;
const PARTICLE_RADIUS = 2;
const MAX_SPEAKER_DISPLACEMENT = 25;

// Modify these constants to control speed and feedback behavior
const SENSITIVITY_SCALE = 4; // Reduced from 5 - How dTheta => displacement
const RECOIL_FACTOR = 0.15; // Reduced from 0.25 - How displacement => v feedback
const WAVE_SPEED = 3;   // Significantly slower - pixels per frame
const DAMPING_FACTOR = 0.0015; // Reduced from 0.002 - How wave amplitude decays
const FEEDBACK_DECAY = 0.9; // Increased from 0.85 - Controls how quickly feedback effect decays

// Time scaling factors (new)
const TIME_SCALE = 0.08; // Slow down all motion dramatically - key parameter for dialog pacing
const CATASTROPHE_BOOST = 10.0; // How much faster catastrophes happen than normal movement

const FPS = 60;
const HISTORY_SECONDS = (CHAMBER_WIDTH / WAVE_SPEED) / FPS + 0.3; // Store history long enough for wave + buffer
const SPEAKER_HISTORY = new Array(Math.ceil(FPS * HISTORY_SECONDS)).fill(0);

// --- State Variables ---
let zcmSvg, zcmSvgWrapper, speakerSvg, speakerSvgWrapper;
let zcmCoordsDisplay, speakerInfoDisplay;
let targetU = 1.5, targetV = 0.0; // Initial position (logical) - outside cusp
let actualU = targetU, actualV = targetV; // Position after feedback (logical)
let theta = 0.0; // ZCM wheel angle (radians, CCW from -Y axis)
let previousTheta = theta;
let isDragging = false;
let pointerId = null; // For tracking specific pointer events

// ZCM SVG Element References
let zcmWheelGroup, wheelRadiusLine; // Removed wheelPinCircle as it's implicit
let spring1Line, spring2Line;
let targetControlPointCircle, actualControlPointCircle;
let fixedAnchorCircle;
let cuspPathElement;

// Speaker SVG Element References
let pistonRect;
let airParticles = []; // Array of { element: SVGcircle, baseX: num, baseY: num }
let historyIndex = 0;
let previousSpeakerDisplacement = 0;

// Add new state variables for smooth transition tracking
let targetThetaActual = 0; // The real target theta from physics
let currentThetaDisplayed = 0; // Smoothed display value that transitions to target
let isInCatastrophe = false; // Flag to track if we're in a catastrophe transition
let catastropheTimer = 0;    // Timer to track catastrophe progress
let lastTargetU = 0, lastTargetV = 0; // Track target position changes
let moveSpeed = 0.05;        // How fast the control point visually moves
let dragTimer = 0;           // Track how long since last drag event

// --- SVG Helper ---
function createSvgElement(type) {
    return document.createElementNS(SVG_NS, type);
}

// --- Coordinate Mapping ---
function mapPointerToLogical(clientX, clientY) {
    const svgRect = zcmSvgWrapper.getBoundingClientRect();
    const svgX = clientX - svgRect.left;
    const svgY = clientY - svgRect.top;

    // Convert clientX/Y to SVG viewBox coordinates
    const svgPoint = zcmSvg.createSVGPoint();
    svgPoint.x = svgX;
    svgPoint.y = svgY;
    let svgCoord;
    try {
        const CTM = zcmSvg.getScreenCTM();
        if(!CTM) return; // Guard against missing CTM
        svgCoord = svgPoint.matrixTransform(CTM.inverse());
    } catch (e) {
        console.error("Error getting screen CTM:", e);
        return; // Exit if matrix is non-invertible
    }


    // Map SVG coordinates to logical u, v
    // V maps to horizontal (svgCoord.x), U maps to vertical (svgCoord.y)
    const logicalV = (svgCoord.x - ZCM_SVG_ORIGIN_X) / ZCM_SCALE_X;
    // SVG Y increases downwards, logical U increases upwards
    const logicalU = -(svgCoord.y - ZCM_SVG_ORIGIN_Y) / ZCM_SCALE_Y;

    // Clamp to defined logical boundaries
    targetV = Math.max(LOGICAL_V_MIN, Math.min(LOGICAL_V_MAX, logicalV));
    targetU = Math.max(LOGICAL_U_MIN, Math.min(LOGICAL_U_MAX, logicalU));
}

function mapLogicalToSvg(u, v) {
    // Map logical u, v to SVG coordinates
    const svgX = v * ZCM_SCALE_X + ZCM_SVG_ORIGIN_X;
    // SVG Y increases downwards, logical U increases upwards
    const svgY = -u * ZCM_SCALE_Y + ZCM_SVG_ORIGIN_Y;
    return { x: svgX, y: svgY };
}

// --- Physics Calculations (Full Implementations from ZCM.html) ---

/**
 * Calculates the derivative of the ZCM potential V with respect to theta.
 * Based on the potential implicit in the original ZCM.html bset/getTheta.
 * Assumes parameters like wheel radius=0.5, fixed spring dist=2 are embedded.
 * @param {number} theta_calc Current angle (radians, CCW from -Y)
 * @param {number} u_calc Current 'u' control parameter (vertical logical coord)
 * @param {number} v_calc Current 'v' control parameter (horizontal logical coord)
 * @returns {number} dV/dtheta
 */
function getVt(theta_calc, u_calc, v_calc) {
    var c = Math.cos(theta_calc);
    var s = Math.sin(theta_calc);
    
    // Term 1: Fixed spring potential contribution
    var rf_sq = 17 / 4 - 2 * c; 
    if (rf_sq < 1e-9) rf_sq = 1e-9; // Prevent division by zero
    var rf = Math.sqrt(rf_sq);
    var ft = 2 * s; // = d(rf_sq)/dtheta
    
    // Term 2: Movable spring potential contribution
    var rg_sq = u_calc * (u_calc + c) + v_calc * (v_calc - s) + 0.25;
    if (rg_sq < 1e-9) rg_sq = 1e-9; // Prevent division by zero
    var rg = Math.sqrt(rg_sq);
    var gt = -u_calc * s - v_calc * c; // = d(rg_sq)/dtheta
    
    // Combine terms from potential V = (rf-1)^2 + (rg-1)^2
    var rfp = 1 - 1 / rf;
    var rgp = 1 - 1 / rg;
    var Vt = rfp * ft + rgp * gt;
    
    return Vt;
}

/**
 * Finds the stable angle theta by minimizing the potential V using gradient descent.
 * @param {number} theta0 Initial guess for theta (radians, CCW from -Y)
 * @param {number} u_calc Current 'u' control parameter
 * @param {number} v_calc Current 'v' control parameter
 * @returns {number} Stable theta value (radians, CCW from -Y)
 */
function getTheta(theta0, u_calc, v_calc) {
    let currentTheta = theta0;
    const ACC_theta = 1e-5; // Convergence tolerance
    const maxIter = 400;    // Safety break
    let dt_factor = 0.08;   // Step size multiplier
    const min_dt_factor = 1e-4; // Minimum step size
    
    for (let j = 0; j < maxIter; j++) {
        let Vt = getVt(currentTheta, u_calc, v_calc);
        
        if (isNaN(Vt)) {
            console.warn("Vt is NaN, resetting theta search slightly");
            // Try nudging theta if calculation fails
            currentTheta = theta0 + (Math.random() - 0.5) * 0.1;
            Vt = getVt(currentTheta, u_calc, v_calc);
            if(isNaN(Vt)) return theta0; // Give up if still NaN
        }
        
        if (Math.abs(Vt) < ACC_theta) {
             break; // Converged
        }
        
        // Gradient descent step
        currentTheta -= dt_factor * Vt;
        
        // Adaptive step size - if we're getting worse, slow down
        let nextVt = getVt(currentTheta, u_calc, v_calc);
        if (!isNaN(nextVt) && Vt * nextVt < 0 && dt_factor > min_dt_factor) {
           dt_factor *= 0.7; // Reduce step size if sign flips (overshot minimum)
        }
        
        // Keep theta in a consistent range [-PI, PI]
        if (currentTheta > Math.PI) currentTheta -= 2 * Math.PI;
        else if (currentTheta < -Math.PI) currentTheta += 2 * Math.PI;
    }
    
    // Final normalization to [-PI, PI]
    if (currentTheta > Math.PI) currentTheta -= 2 * Math.PI;
    else if (currentTheta < -Math.PI) currentTheta += 2 * Math.PI;
    
    return currentTheta;
}

// --- SVG Setup Functions ---
function setupZcmSvg() {
    zcmSvg = document.getElementById('zcm-svg');
    zcmSvgWrapper = document.getElementById('zcm-svg-wrapper');
    zcmCoordsDisplay = document.getElementById('zcm-coords');
    const wheelRadiusSvg = ZCM_WHEEL_RADIUS_LOGICAL * Math.min(ZCM_SCALE_X, ZCM_SCALE_Y); // Use average scale approx

    // Background elements
    const defs = createSvgElement('defs');
    // Marker for pin?
    zcmSvg.appendChild(defs);

    // Axes
    const xAxis = createSvgElement('line');
    xAxis.setAttribute('x1', ZCM_SVG_VIEWBOX_X);
    xAxis.setAttribute('y1', ZCM_SVG_ORIGIN_Y);
    xAxis.setAttribute('x2', ZCM_SVG_VIEWBOX_X + ZCM_SVG_VIEWBOX_WIDTH);
    xAxis.setAttribute('y2', ZCM_SVG_ORIGIN_Y);
    xAxis.setAttribute('stroke', '#ccc');
    xAxis.setAttribute('stroke-width', '1');
    zcmSvg.appendChild(xAxis);

    const yAxis = createSvgElement('line');
    yAxis.setAttribute('x1', ZCM_SVG_ORIGIN_X);
    yAxis.setAttribute('y1', ZCM_SVG_VIEWBOX_Y);
    yAxis.setAttribute('x2', ZCM_SVG_ORIGIN_X);
    yAxis.setAttribute('y2', ZCM_SVG_VIEWBOX_Y + ZCM_SVG_VIEWBOX_HEIGHT);
    yAxis.setAttribute('stroke', '#ccc');
    yAxis.setAttribute('stroke-width', '1');
    zcmSvg.appendChild(yAxis);

     // Axis Labels
    const uLabel = createSvgElement('text');
    uLabel.setAttribute('x', ZCM_SVG_ORIGIN_X);
    uLabel.setAttribute('y', ZCM_SVG_VIEWBOX_Y + ZCM_SVG_VIEWBOX_HEIGHT - 15);
    uLabel.setAttribute('text-anchor', 'middle');
    uLabel.setAttribute('font-size', '10');
    uLabel.textContent = 'u (Vertical)';
    zcmSvg.appendChild(uLabel);

    const vLabel = createSvgElement('text');
    vLabel.setAttribute('x', ZCM_SVG_VIEWBOX_X + ZCM_SVG_VIEWBOX_WIDTH - 10);
    vLabel.setAttribute('y', ZCM_SVG_ORIGIN_Y);
    vLabel.setAttribute('text-anchor', 'end');
    vLabel.setAttribute('alignment-baseline', 'middle');
    vLabel.setAttribute('font-size', '10');
    vLabel.textContent = 'v (Horizontal)';
    zcmSvg.appendChild(vLabel);


    // Unit Circle (logical radius 1)
    const unitCircleLogicalRadius = 1.0;
    const unitCircleSvgRadius = unitCircleLogicalRadius * Math.min(ZCM_SCALE_X, ZCM_SCALE_Y); // Approx scale
    const unitCircle = createSvgElement('circle');
    unitCircle.setAttribute('cx', ZCM_SVG_ORIGIN_X);
    unitCircle.setAttribute('cy', ZCM_SVG_ORIGIN_Y);
    unitCircle.setAttribute('r', unitCircleSvgRadius.toFixed(1));
    unitCircle.setAttribute('fill', 'none');
    unitCircle.setAttribute('stroke', '#ddd');
    unitCircle.setAttribute('stroke-width', '1');
    unitCircle.setAttribute('stroke-dasharray', '4 4');
    zcmSvg.appendChild(unitCircle);

    // Cusp Lines (Parametric: u = 3*t^2, v = 2*t^3, scaled around bifurcation point)
    // Bifurcation point origin in ZCM.html seems near u=la, v=0.
    // The standard cusp 27u^2 = 4v^3 has its point at u=0, v=0.
    // Let's draw the standard cusp relative to our logical (0,0).
    cuspPathElement = createSvgElement('path');
    let cuspPathData = "M";
    const cuspPoints = 50;
    for (let i = -cuspPoints; i <= cuspPoints; i++) {
        const t = (i / cuspPoints) * 1.0; // Parameter range (adjust scale factor 1.0 if needed)
        // Standard cusp centered at (0,0) in logical space
        const cusp_v = 2 * t * t * t; // V is horizontal
        const cusp_u = 3 * t * t;     // U is vertical
        const svgP = mapLogicalToSvg(cusp_u, cusp_v);
        cuspPathData += `${svgP.x.toFixed(1)},${svgP.y.toFixed(1)} L`;
    }
    cuspPathData = cuspPathData.slice(0, -2); // Remove trailing " L"
    // Need second branch u=3t^2, v=-2t^3
     let cuspPathData2 = "M";
     for (let i = -cuspPoints; i <= cuspPoints; i++) {
        const t = (i / cuspPoints) * 1.0;
        const cusp_v = -2 * t * t * t;
        const cusp_u = 3 * t * t;
        const svgP = mapLogicalToSvg(cusp_u, cusp_v);
         cuspPathData2 += `${svgP.x.toFixed(1)},${svgP.y.toFixed(1)} L`;
    }
     cuspPathData2 = cuspPathData2.slice(0, -2);

    // Combine paths (optional: draw as two separate paths)
    // For filling, need to connect them: Draw one, then reverse the other
    // cuspPathData += cuspPathData2.split('L').reverse().join('L').replace('M','L') + " Z";
    // Let's just draw lines for now
    cuspPathElement.setAttribute('d', cuspPathData);
    cuspPathElement.setAttribute('fill', 'none'); //'rgba(255, 0, 0, 0.05)');
    cuspPathElement.setAttribute('stroke', 'rgba(200, 0, 0, 0.5)');
    cuspPathElement.setAttribute('stroke-width', '1.5');
    zcmSvg.appendChild(cuspPathElement);
    // Add second path element for the other branch
    const cuspPathElement2 = cuspPathElement.cloneNode();
    cuspPathElement2.setAttribute('d', cuspPathData2);
    zcmSvg.appendChild(cuspPathElement2);


    // Dynamic Elements
    zcmWheelGroup = createSvgElement('g');
    zcmWheelGroup.setAttribute('id', 'zcm-wheel-group');
    // Center the group at the logical origin's SVG position
    zcmWheelGroup.setAttribute('transform', `translate(${ZCM_SVG_ORIGIN_X}, ${ZCM_SVG_ORIGIN_Y}) rotate(0)`);
    zcmSvg.appendChild(zcmWheelGroup);

    const wheelDisc = createSvgElement('circle');
    wheelDisc.setAttribute('cx', 0); // Center within the group
    wheelDisc.setAttribute('cy', 0);
    wheelDisc.setAttribute('r', wheelRadiusSvg.toFixed(1));
    wheelDisc.setAttribute('fill', '#ffddaa'); // Lighter orange
    wheelDisc.setAttribute('stroke', 'black');
    wheelDisc.setAttribute('stroke-width', '1');
    zcmWheelGroup.appendChild(wheelDisc);

    // Reference line pointing UP initially (along +Y in group coords)
    // This aligns with theta=0 being CCW from -Y axis (i.e., pointing down)
    wheelRadiusLine = createSvgElement('line');
    wheelRadiusLine.setAttribute('x1', 0);
    wheelRadiusLine.setAttribute('y1', 0);
    wheelRadiusLine.setAttribute('x2', 0); // Points UP
    wheelRadiusLine.setAttribute('y2', -wheelRadiusSvg.toFixed(1)); // To top edge
    wheelRadiusLine.setAttribute('stroke', 'black');
    wheelRadiusLine.setAttribute('stroke-width', '2');
    zcmWheelGroup.appendChild(wheelRadiusLine);

    // Springs - outside the rotating group
    spring1Line = createSvgElement('line');
    spring1Line.setAttribute('stroke', 'black');
    spring1Line.setAttribute('stroke-width', '3');
    spring1Line.setAttribute('opacity', '0.7');
    zcmSvg.appendChild(spring1Line);

    spring2Line = createSvgElement('line');
    spring2Line.setAttribute('stroke', 'black');
    spring2Line.setAttribute('stroke-width', '3');
    spring2Line.setAttribute('opacity', '0.7');
    zcmSvg.appendChild(spring2Line);

    // Fixed Anchor - Logical position (e.g., u=2.0, v=0 relates to fixed spring dist 2)
    const fixedAnchorLogical = { u: 2.0, v: 0 }; // Matches implicit parameter in potential?
    const fixedAnchorSvg = mapLogicalToSvg(fixedAnchorLogical.u, fixedAnchorLogical.v);
    fixedAnchorCircle = createSvgElement('circle');
    fixedAnchorCircle.setAttribute('cx', fixedAnchorSvg.x);
    fixedAnchorCircle.setAttribute('cy', fixedAnchorSvg.y);
    fixedAnchorCircle.setAttribute('r', '6');
    fixedAnchorCircle.setAttribute('fill', '#aaa');
    fixedAnchorCircle.setAttribute('stroke', 'black');
    fixedAnchorCircle.setAttribute('stroke-width', '1');
    zcmSvg.appendChild(fixedAnchorCircle);

    // Control Points
    const initialControlSvg = mapLogicalToSvg(actualU, actualV);
    targetControlPointCircle = createSvgElement('circle');
    targetControlPointCircle.setAttribute('cx', initialControlSvg.x);
    targetControlPointCircle.setAttribute('cy', initialControlSvg.y);
    targetControlPointCircle.setAttribute('r', '8');
    targetControlPointCircle.setAttribute('fill', 'rgba(0, 100, 255, 0.3)');
    targetControlPointCircle.setAttribute('stroke', 'rgba(0, 100, 255, 0.8)');
    targetControlPointCircle.setAttribute('stroke-width', '1');
    targetControlPointCircle.style.pointerEvents = 'none'; // Don't interfere with drag
    zcmSvg.appendChild(targetControlPointCircle);

    actualControlPointCircle = createSvgElement('circle');
    actualControlPointCircle.setAttribute('cx', initialControlSvg.x);
    actualControlPointCircle.setAttribute('cy', initialControlSvg.y);
    actualControlPointCircle.setAttribute('r', '6');
    actualControlPointCircle.setAttribute('fill', 'red');
    actualControlPointCircle.setAttribute('stroke', 'black');
    actualControlPointCircle.setAttribute('stroke-width', '1');
    actualControlPointCircle.style.pointerEvents = 'none'; // Don't interfere with drag
    zcmSvg.appendChild(actualControlPointCircle);
}

function setupSpeakerSvg() {
    speakerSvg = document.getElementById('speaker-svg');
    speakerSvgWrapper = document.getElementById('speaker-svg-wrapper');
    speakerInfoDisplay = document.getElementById('speaker-info');

    // Chamber Walls
    const chamberRect = createSvgElement('rect');
    chamberRect.setAttribute('x', SPEAKER_BASE_X);
    chamberRect.setAttribute('y', SPEAKER_Y);
    chamberRect.setAttribute('width', CHAMBER_WIDTH);
    chamberRect.setAttribute('height', SPEAKER_HEIGHT);
    chamberRect.setAttribute('fill', 'none');
    chamberRect.setAttribute('stroke', '#666');
    chamberRect.setAttribute('stroke-width', '2');
    speakerSvg.appendChild(chamberRect);

    // Piston
    pistonRect = createSvgElement('rect');
    pistonRect.setAttribute('x', SPEAKER_BASE_X - SPEAKER_WIDTH); // Initial position
    pistonRect.setAttribute('y', SPEAKER_Y);
    pistonRect.setAttribute('width', SPEAKER_WIDTH);
    pistonRect.setAttribute('height', SPEAKER_HEIGHT);
    pistonRect.setAttribute('fill', '#888');
    pistonRect.setAttribute('stroke', '#333');
    pistonRect.setAttribute('stroke-width', '1');
    speakerSvg.appendChild(pistonRect);

    // Air Particles
    const particleGroup = createSvgElement('g');
    particleGroup.setAttribute('id', 'air-particles');
    speakerSvg.appendChild(particleGroup);

    airParticles = [];
    const startX = SPEAKER_BASE_X + PARTICLE_RADIUS + 5;
    const endX = CHAMBER_END_X - PARTICLE_RADIUS - 5;
    const startY = SPEAKER_Y + PARTICLE_RADIUS + 5;
    const endY = SPEAKER_Y + SPEAKER_HEIGHT - PARTICLE_RADIUS - 5;
    const numX = Math.ceil(Math.sqrt(NUM_AIR_PARTICLES * (endX - startX) / (endY - startY)));
    const numY = Math.ceil(NUM_AIR_PARTICLES / numX);
    const stepX = (endX - startX) / Math.max(1, numX - 1);
    const stepY = (endY - startY) / Math.max(1, numY - 1);

    for (let i = 0; i < numX; i++) {
        for (let j = 0; j < numY; j++) {
             if (airParticles.length >= NUM_AIR_PARTICLES) break;
             let baseX = startX + i * stepX + (Math.random() - 0.5) * stepX * 0.3;
             let baseY = startY + j * stepY + (Math.random() - 0.5) * stepY * 0.3;

             let circle = createSvgElement('circle');
             circle.setAttribute('cx', baseX.toFixed(1));
             circle.setAttribute('cy', baseY.toFixed(1));
             circle.setAttribute('r', PARTICLE_RADIUS);
             circle.setAttribute('fill', '#369');
             particleGroup.appendChild(circle);
             airParticles.push({ element: circle, baseX: baseX, baseY: baseY });
        }
         if (airParticles.length >= NUM_AIR_PARTICLES) break;
    }
}


// --- Event Handlers ---
function handlePointerDown(e) {
    if (!isDragging && e.button === 0) { // Only main button
        isDragging = true;
        pointerId = e.pointerId;
        zcmSvgWrapper.setPointerCapture(pointerId);
        mapPointerToLogical(e.clientX, e.clientY);
        e.preventDefault();
    }
}

function handlePointerMove(e) {
    if (isDragging && e.pointerId === pointerId) {
        mapPointerToLogical(e.clientX, e.clientY);
        e.preventDefault();
    }
}

function handlePointerUp(e) {
    if (isDragging && e.pointerId === pointerId) {
        isDragging = false;
        zcmSvgWrapper.releasePointerCapture(pointerId);
        pointerId = null;
    }
}
function handlePointerLeave(e) {
     if (isDragging && e.pointerId === pointerId) {
        isDragging = false;
        zcmSvgWrapper.releasePointerCapture(pointerId);
        pointerId = null;
    }
}

// --- Animation Loop ---
function animate() {
    // Track if user is actively moving the control point
    if (isDragging) {
        dragTimer = 0; // Reset timer when dragging
    } else {
        dragTimer += 1;
    }
    
    // Only move position if user is dragging or recently stopped (for momentum)
    const shouldUpdatePosition = isDragging || dragTimer < 30;

    // 1. Smooth transition of control point position
    if (shouldUpdatePosition) {
        // Calculate distance to move
        const distU = targetU - actualU;
        const distV = targetV - actualV;
        
        // Scale target movement speed
        const dragSpeed = isDragging ? moveSpeed * 2 : moveSpeed;
        
        // Apply smooth movement
        if (Math.abs(distU) > 0.001) {
            actualU += distU * dragSpeed;
        } else {
            actualU = targetU;
        }
        
        if (Math.abs(distV) > 0.001) {
            actualV += distV * dragSpeed;
        } else {
            actualV = targetV;
        }
    }
    
    // 2. Calculate Feedback with smoother decay - now using actual feedback
    const feedbackForce = RECOIL_FACTOR * previousSpeakerDisplacement * FEEDBACK_DECAY;
    
    // Apply feedback to v (emotional dimension)
    const feedbackV = actualV - feedbackForce;
    actualV = Math.max(LOGICAL_V_MIN, Math.min(LOGICAL_V_MAX, feedbackV));
    
    // 3. Calculate ZCM State (Theta) using full physics
    targetThetaActual = getTheta(targetThetaActual, actualU, actualV);
    
    // 4. Handle smooth or catastrophic transition to new theta
    let absDeltaTheta = Math.abs(targetThetaActual - currentThetaDisplayed);
    // Handle wrap-around for comparison
    if (absDeltaTheta > Math.PI) absDeltaTheta = 2 * Math.PI - absDeltaTheta;
    
    // Detect catastrophe
    if (absDeltaTheta > 0.3 && !isInCatastrophe) {
        // Start catastrophe transition
        isInCatastrophe = true;
        catastropheTimer = 0;
    }
    
    // Update displayed theta based on transition type
    if (isInCatastrophe) {
        // Fast catastrophic transition
        catastropheTimer += TIME_SCALE * CATASTROPHE_BOOST;
        
        // Use ease-out transition curve for catastrophe
        const progress = 1 - Math.exp(-5 * catastropheTimer);
        
        if (progress >= 0.99) {
            // Transition complete
            currentThetaDisplayed = targetThetaActual;
            isInCatastrophe = false;
        } else {
            // Calculate shortest path to target theta (handle wrap around)
            let deltaTheta = targetThetaActual - currentThetaDisplayed;
            if (deltaTheta > Math.PI) deltaTheta -= 2 * Math.PI;
            if (deltaTheta < -Math.PI) deltaTheta += 2 * Math.PI;
            
            currentThetaDisplayed += deltaTheta * progress;
            
            // Keep in range
            if (currentThetaDisplayed > Math.PI) currentThetaDisplayed -= 2 * Math.PI;
            if (currentThetaDisplayed < -Math.PI) currentThetaDisplayed += 2 * Math.PI;
        }
    } else {
        // Smooth gradual transition (much slower)
        let deltaTheta = targetThetaActual - currentThetaDisplayed;
        if (deltaTheta > Math.PI) deltaTheta -= 2 * Math.PI;
        if (deltaTheta < -Math.PI) deltaTheta += 2 * Math.PI;
        
        // Apply slow time scale to normal movement
        currentThetaDisplayed += deltaTheta * TIME_SCALE;
        
        // Keep in range
        if (currentThetaDisplayed > Math.PI) currentThetaDisplayed -= 2 * Math.PI;
        if (currentThetaDisplayed < -Math.PI) currentThetaDisplayed += 2 * Math.PI;
    }
    
    // 5. Calculate change in theta for display and sound
    let dTheta = currentThetaDisplayed - previousTheta;
    if (dTheta > Math.PI) dTheta -= 2 * Math.PI;
    else if (dTheta <= -Math.PI) dTheta += 2 * Math.PI;
    
    // 6. Categorize change type with more nuance
    const absDTheta = Math.abs(dTheta);
    let changeType = "none";
    if (absDTheta > 0.05) changeType = "major_catastrophe";
    else if (absDTheta > 0.01) changeType = "minor_catastrophe";
    else if (absDTheta > 0.002) changeType = "significant_change";
    else changeType = "smooth_dialog";
    
    // 7. Calculate Speaker Displacement with smoother response curve
    const speakerDisplacement = MAX_SPEAKER_DISPLACEMENT * Math.tanh(dTheta * SENSITIVITY_SCALE);
    
    // Store in history for wave propagation
    SPEAKER_HISTORY[historyIndex] = speakerDisplacement;
    historyIndex = (historyIndex + 1) % SPEAKER_HISTORY.length;
    
    // 8. Update Visual Feedback - Change spring appearance based on change type
    if (changeType === "major_catastrophe") {
        // Major catastrophe - red flash and thicker spring
        spring2Line.setAttribute('stroke', '#f44');
        spring2Line.setAttribute('stroke-width', '4');
        spring2Line.setAttribute('opacity', '0.9');
        setTimeout(() => {
            spring2Line.setAttribute('stroke', 'black');
            spring2Line.setAttribute('stroke-width', '3');
            spring2Line.setAttribute('opacity', '0.7');
        }, 500);
    } else if (changeType === "minor_catastrophe") {
        // Minor catastrophe - orange flash
        spring2Line.setAttribute('stroke', '#f90');
        spring2Line.setAttribute('stroke-width', '3.5');
        spring2Line.setAttribute('opacity', '0.8');
        setTimeout(() => {
            spring2Line.setAttribute('stroke', 'black');
            spring2Line.setAttribute('stroke-width', '3');
            spring2Line.setAttribute('opacity', '0.7');
        }, 300);
    } else if (changeType === "significant_change") {
        // Significant but non-catastrophic change - subtle green flash
        spring2Line.setAttribute('stroke', '#4a4');
        spring2Line.setAttribute('opacity', '0.75');
        setTimeout(() => {
            spring2Line.setAttribute('stroke', 'black');
            spring2Line.setAttribute('opacity', '0.7');
        }, 200);
    }
    
    // 9. Update ZCM SVG Elements
    const actualControlSvg = mapLogicalToSvg(actualU, actualV);
    actualControlPointCircle.setAttribute('cx', actualControlSvg.x.toFixed(1));
    actualControlPointCircle.setAttribute('cy', actualControlSvg.y.toFixed(1));
    
    const targetControlSvg = mapLogicalToSvg(targetU, targetV);
    targetControlPointCircle.setAttribute('cx', targetControlSvg.x.toFixed(1));
    targetControlPointCircle.setAttribute('cy', targetControlSvg.y.toFixed(1));
    
    // Wheel rotation based on displayed theta, not raw physics theta
    const angleDeg = -currentThetaDisplayed * (180 / Math.PI);
    zcmWheelGroup.setAttribute('transform',
        `translate(${ZCM_SVG_ORIGIN_X}, ${ZCM_SVG_ORIGIN_Y}) rotate(${angleDeg.toFixed(1)})`
    );
    
    // Calculate pin position in absolute SVG coordinates for springs
    const wheelRadiusSvg = ZCM_WHEEL_RADIUS_LOGICAL * Math.min(ZCM_SCALE_X, ZCM_SCALE_Y);
    const pinWorldX = ZCM_SVG_ORIGIN_X + wheelRadiusSvg * Math.sin(currentThetaDisplayed);
    const pinWorldY = ZCM_SVG_ORIGIN_Y - wheelRadiusSvg * Math.cos(currentThetaDisplayed);
    
    const fixedAnchorSvg = {
        x: parseFloat(fixedAnchorCircle.getAttribute('cx')),
        y: parseFloat(fixedAnchorCircle.getAttribute('cy'))
    };
    
    // Update springs
    spring1Line.setAttribute('x1', fixedAnchorSvg.x.toFixed(1));
    spring1Line.setAttribute('y1', fixedAnchorSvg.y.toFixed(1));
    spring1Line.setAttribute('x2', pinWorldX.toFixed(1));
    spring1Line.setAttribute('y2', pinWorldY.toFixed(1));
    
    spring2Line.setAttribute('x1', actualControlSvg.x.toFixed(1));
    spring2Line.setAttribute('y1', actualControlSvg.y.toFixed(1));
    spring2Line.setAttribute('x2', pinWorldX.toFixed(1));
    spring2Line.setAttribute('y2', pinWorldY.toFixed(1));
    
    // 10. Update Speaker SVG Elements - slow down the wave propagation
    const pistonFaceX = SPEAKER_BASE_X + speakerDisplacement;
    pistonRect.setAttribute('x', (pistonFaceX - SPEAKER_WIDTH).toFixed(1));
    
    // 11. Update Air Particles with slower wave propagation
    const currentHistoryIndex = historyIndex;
    for(let i = 0; i < airParticles.length; i++) {
        const p = airParticles[i];
        const distance = p.baseX - SPEAKER_BASE_X;
        const timeDelayFrames = distance / WAVE_SPEED;
        
        // Look back in history to find the displacement that affects this particle
        let lookBackIndex = Math.round(currentHistoryIndex - 1 - timeDelayFrames);
        lookBackIndex = (lookBackIndex % SPEAKER_HISTORY.length + SPEAKER_HISTORY.length) % SPEAKER_HISTORY.length;
        
        const historicalDisplacement = SPEAKER_HISTORY[lookBackIndex] || 0;
        
        // Apply distance-based damping
        const damping = Math.exp(-DAMPING_FACTOR * distance);
        
        // Calculate new position with constraints
        const currentX = p.baseX + historicalDisplacement * damping;
        const clampedX = Math.max(
            pistonFaceX + PARTICLE_RADIUS,
            Math.min(CHAMBER_END_X - PARTICLE_RADIUS, currentX)
        );
        
        p.element.setAttribute('cx', clampedX.toFixed(1));
        
        // Improved color variation based on wave compression/decompression
        if (Math.abs(historicalDisplacement) > 0) {
            // Scale the intensity based on displacement
            const intensity = Math.min(255, 100 + Math.abs(historicalDisplacement * 4));
            
            // Color particles differently based on compression vs decompression
            let color;
            if (historicalDisplacement > 8) {
                // Strong compression - bright blue
                color = `rgb(100, 140, ${intensity})`;
            } else if (historicalDisplacement > 0) {
                // Mild compression - light blue
                color = `rgb(100, ${100 + intensity/3}, ${intensity})`;
            } else if (historicalDisplacement < -8) {
                // Strong decompression - purple
                color = `rgb(${intensity}, 100, ${intensity})`;
            } else if (historicalDisplacement < 0) {
                // Mild decompression - light purple
                color = `rgb(${100 + intensity/3}, 100, ${intensity})`;
            } else {
                // Default - standard blue
                color = '#369';
            }
            p.element.setAttribute('fill', color);
        } else {
            p.element.setAttribute('fill', '#369'); // Default blue
        }
    }
    
    // 12. Update Information Displays with more detailed information
    zcmCoordsDisplay.textContent = `u: ${actualU.toFixed(2)}, v: ${actualV.toFixed(2)} | θ: ${currentThetaDisplayed.toFixed(2)} | dθ: ${dTheta.toFixed(3)}`;
    
    // More descriptive display text with nuanced change categories
    let displacementText = `Displacement: ${speakerDisplacement.toFixed(1)}`;
    
    // Add detailed dialog state description
    if (changeType === "major_catastrophe") {
        displacementText += " | Major Shift (Strong Catastrophe)";
    } else if (changeType === "minor_catastrophe") {
        displacementText += " | Perspective Shift (Minor Catastrophe)";
    } else if (changeType === "significant_change") {
        displacementText += " | Notable Change (Continuous)";
    } else {
        displacementText += " | Smooth Dialog";
    }
    
    speakerInfoDisplay.textContent = displacementText;
    
    // 13. Store State for Next Frame
    previousTheta = currentThetaDisplayed;
    previousSpeakerDisplacement = speakerDisplacement;
    
    // 14. Continue animation loop
    requestAnimationFrame(animate);
}

// --- Initialization ---
window.onload = () => {
    // Basic check for SVG support
    if (!document.createElementNS || !document.createElementNS(SVG_NS, "svg").createSVGRect) {
        document.body.innerHTML = "<p>Sorry, your browser does not support SVG, which is required for this visualization.</p>";
        return;
    }

    try {
        setupZcmSvg();
        setupSpeakerSvg();

        // Add pointer event listeners to the wrapper for better hit area
        zcmSvgWrapper.addEventListener('pointerdown', handlePointerDown);
        zcmSvgWrapper.addEventListener('pointermove', handlePointerMove);
        zcmSvgWrapper.addEventListener('pointerup', handlePointerUp);
        zcmSvgWrapper.addEventListener('pointercancel', handlePointerUp);
        zcmSvgWrapper.addEventListener('pointerleave', handlePointerLeave);

        // Prevent default touch actions like scrolling/zooming on the ZCM SVG area
         zcmSvgWrapper.addEventListener('touchstart', (e) => { e.preventDefault(); }, { passive: false });
         zcmSvgWrapper.addEventListener('touchmove', (e) => { e.preventDefault(); }, { passive: false });


        // Start animation
        requestAnimationFrame(animate);
    } catch (error) {
        console.error("Initialization failed:", error);
        // Display a user-friendly error message
        const container = document.querySelector('.visualization-container') || document.body;
        container.innerHTML = `<p style="color: red; border: 1px solid red; padding: 10px;">Error initializing visualization: ${error.message}. Please ensure your browser is up-to-date.</p>`;
    }
};