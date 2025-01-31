let graphs = {};
let papers = {};
let stateObjects = {};
let countStack = [0];

// Define diagram mappings first
const diagramUpdaters = {
    4: 'coboDiagram',
    5: 'chunkDiagram',
    6: 'abaoDiagram',
    7: 'subChunkDiagram',
    8: 'countBackDiagram',
    9: 'subRoundingDiagram',
    10: 'c2cDiagram',
    11: 'strategicDiagram',
    12: 'doublingDiagram',
    13: 'cboDiagram',
    14: 'drDiagram',
    15: 'commuteDiagram',
    16: 'divisionDiagram',
    17: 'inverseDiagram',
    18: 'dealingDiagram',
    19: 'trialDiagram',
    20: 'commDivDiagram'
};

// Add this to manage automaton IDs
const automatonIds = new Map();

// Add this function to get unique elementId
function getElementId(basename) {
    if (!automatonIds.has(basename)) {
        automatonIds.set(basename, basename);
    }
    return automatonIds.get(basename);
}

// Core functions
function getStackNumber() {
    return parseInt(countStack.join('')) || 0;
}

function initializeDiagram(elementId) {
    if (!document.getElementById(elementId)) {
        console.warn(`Element ${elementId} not found`);
        return null;
    }

    // Clear existing graph if it exists
    if (graphs[elementId]) {
        graphs[elementId].clear();
        delete graphs[elementId];
        delete papers[elementId];
        delete stateObjects[elementId];
    }

    const graph = new joint.dia.Graph();
    const paper = new joint.dia.Paper({
        el: document.getElementById(elementId),
        model: graph,
        width: 600,
        height: 200,
        gridSize: 1,
        background: {
            color: '#fafafa'
        }
    });

    graphs[elementId] = graph;
    papers[elementId] = paper;
    stateObjects[elementId] = {};

    return { graph, paper };
}

function createState(graph, elementId, x, y, label, isInitial = false, isFinal = false) {
    const state = new joint.shapes.fsa.State({
        position: { x, y },
        size: { width: 60, height: 60 },
        attrs: {
            circle: {
                fill: '#ffffff',
                stroke: isInitial ? '#2C3E50' : '#2C3E50',
                'stroke-width': isInitial ? 3 : 1
            },
            text: {
                text: label,
                fill: '#000000',
                'font-size': 14
            }
        }
    });

    if (isFinal) {
        state.attr('circle', { 'double': true });
    }

    graph.addCell(state);
    if (!stateObjects[elementId]) {
        stateObjects[elementId] = {};
    }
    stateObjects[elementId][label] = state;
    return state;
}
function createTransition(graph, source, target, label) {
    const transition = new joint.shapes.fsa.Arrow({
        source: { id: source.id },
        target: { id: target.id },
        labels: [{ position: .5, attrs: { text: { text: label } } }],
        attrs: {
            '.marker-target': { fill: '#2C3E50', stroke: '#2C3E50' },
            '.connection': { stroke: '#2C3E50' }
        }
    });
    
    graph.addCell(transition);
    return transition;
}

function createBasicStateDiagram(elementId, activeState = 'q0') {
    const { graph, paper } = initializeDiagram(elementId);
    if (!graph) return null;

    const states = {};
    
    // Create states
    states.q0 = createState(graph, elementId, 50, 50, 'q0', true);
    states.q1 = createState(graph, elementId, 200, 50, 'q1');
    states.q2 = createState(graph, elementId, 350, 50, 'q2', false, true);
    
    // Create transitions
    createTransition(graph, states.q0, states.q1, 'process');
    createTransition(graph, states.q1, states.q2, 'complete');

    // Store states in global object
    stateObjects[elementId] = states;
    
    // Highlight active state
    if (states[activeState]) {
        states[activeState].attr('circle/fill', '#e8f0f8');
    }
    
    return states;
}

// State management functions
function updateStateDiagram(stateId) {
    return createBasicStateDiagram('rmbDiagram', stateId);
}

// Add these helper functions
function getStates(elementId) {
    return stateObjects[elementId] || {};
}

function getState(elementId, stateId) {
    const states = getStates(elementId);
    return states[stateId];
}

function updateStateHighlight(elementId, currentState) {
    const states = getStates(elementId);
    
    // Reset all states to default
    Object.values(states).forEach(state => {
        if (state && state.attr) {
            state.attr('circle/fill', '#ffffff');
        }
    });
    
    // Highlight current state
    if (states[currentState] && states[currentState].attr) {
        states[currentState].attr('circle/fill', '#e8f0f8');
    }
}

// Modify existing updateStateDiagram functions
Object.entries(diagramUpdaters).forEach(([num, elementId]) => {
    window[`updateStateDiagram${num}`] = function(stateId) {
        updateStateHighlight(elementId, stateId);
    };
});

// Create all updateStateDiagram functions
Object.entries(diagramUpdaters).forEach(([num, elementId]) => {
    window[`updateStateDiagram${num}`] = function(stateId) {
        return createBasicStateDiagram(elementId, stateId);
    };
});

function initializeAllDiagrams() {
    // Initialize RMB diagram first
    createBasicStateDiagram('rmbDiagram', 'q0');
    
    // Initialize all other diagrams
    Object.values(diagramUpdaters).forEach(elementId => {
        createBasicStateDiagram(elementId, 'q0');
    });
}

// Export functions to global scope
window.getStackNumber = getStackNumber;
window.initializeAllDiagrams = initializeAllDiagrams;
window.createBasicStateDiagram = createBasicStateDiagram;
window.updateStateDiagram = updateStateDiagram;
window.getStates = getStates;
window.getState = getState;
window.updateStateHighlight = updateStateHighlight;
