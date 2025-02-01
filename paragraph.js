console.log("script.js loaded");

document.addEventListener('DOMContentLoaded', function () {
    // Centralized Data Structure
    const hegelData = {
        paragraphs: {},
        concepts: {},
    };

    // Populate hegelData from the HTML content (modified from paragraph-dot-file.html)
    const paragraphAnalysesHTML = document.querySelectorAll('.paragraph-analysis');
    paragraphAnalysesHTML.forEach(analysis => {
        const paragraphNumber = parseInt(analysis.dataset.paragraph);
        hegelData.paragraphs[paragraphNumber] = {
            text: analysis.querySelector('h3').textContent, // You might want to extract more text content here
            concepts: [],
            dot: "", // Will be populated later
        };

        // Extract concepts from conceptual-changes
        const conceptChanges = analysis.querySelectorAll('.conceptual-changes li strong');
        conceptChanges.forEach(concept => {
            const conceptName = concept.textContent.trim().replace(/:$/, ''); // Remove trailing colon
            hegelData.paragraphs[paragraphNumber].concepts.push(conceptName);

            // Initialize concept in hegelData.concepts if it doesn't exist
            if (!hegelData.concepts[conceptName]) {
                hegelData.concepts[conceptName] = [];
            }
        });
    });

    // Populate DOT data for each paragraph (modified from paragraph-dot-file.html)
    const dotCodeBlocks = document.querySelectorAll('.paragraph-dot-file pre code.language-dot');
    dotCodeBlocks.forEach(codeBlock => {
        const dotCode = codeBlock.textContent.trim();
        const match = dotCode.match(/digraph Paragraph(\d+)/);
        if (match) {
            const paragraphNumber = parseInt(match[1]);
            if (hegelData.paragraphs[paragraphNumber]) {
                hegelData.paragraphs[paragraphNumber].dot = dotCode;

                // Extract concepts from the DOT code
                const conceptMatches = dotCode.matchAll(/label="(.+?):\\n/g); // Assuming concept labels start with the concept name followed by a colon
                for (const conceptMatch of conceptMatches) {
                    const conceptName = conceptMatch[1].trim();
                    if (!hegelData.concepts[conceptName]) {
                        hegelData.concepts[conceptName] = [];
                    }
                    if (!hegelData.concepts[conceptName].includes(paragraphNumber)) {
                        hegelData.concepts[conceptName].push(paragraphNumber);
                    }
                }
            }
        }
    });

    // Populate concept evolution data
    const evolutionDotCodeBlocks = document.querySelectorAll('.evolution-dot-file pre code.language-dot');
    evolutionDotCodeBlocks.forEach(codeBlock => {
        const dotCode = codeBlock.textContent.trim();
        const conceptNameMatch = dotCode.match(/digraph (\w+)Evolution/);
        if (conceptNameMatch) {
            const conceptName = conceptNameMatch[1];
            if (!hegelData.concepts[conceptName]) {
                hegelData.concepts[conceptName] = [];
            }

            const paragraphMatches = dotCode.matchAll(/"(\w+)_Para(\d+)"/g);
            for (const match of paragraphMatches) {
                const paragraphNumber = parseInt(match[2]);
                if (!hegelData.concepts[conceptName].includes(paragraphNumber)) {
                    hegelData.concepts[conceptName].push(paragraphNumber);
                }
            }
        }
    });

    console.log("Hegel Data:", hegelData); // Log the populated data structure

    // UI Elements
    const conceptSelect = document.getElementById('concept-select');
    const graphContainer = document.getElementById('graph-container');
    const paragraphAnalysesContainer = document.getElementById('paragraph-analyses');
    const searchInput = document.getElementById('search-input');
    const searchButton = document.getElementById('search-button');
    
    let panZoomInstance = null; // Initialize panZoomInstance here

    // Populate concept select
    for (const concept in hegelData.concepts) {
        const option = document.createElement('option');
        option.value = concept;
        option.textContent = concept;
        conceptSelect.appendChild(option);
    }

    // Generate Paragraph Analyses
    function generateParagraphAnalyses() {
        paragraphAnalysesContainer.innerHTML = '';
        for (const paragraphNumber in hegelData.paragraphs) {
            const paragraphData = hegelData.paragraphs[paragraphNumber];
            const analysisDiv = document.createElement('div');
            analysisDiv.classList.add('paragraph-analysis');
            analysisDiv.id = `paragraph-${paragraphNumber}`;
            analysisDiv.innerHTML = `<h3>${paragraphData.text}</h3>`;

            const conceptsList = document.createElement('ul');
            paragraphData.concepts.forEach(concept => {
                const listItem = document.createElement('li');
                listItem.textContent = concept;
                listItem.addEventListener('click', () => {
                    highlightConceptInGraph(concept);
                    // Optionally, filter the graph to only show this concept
                    // renderGraph(generateFilteredDotGraph(concept));
                });
                conceptsList.appendChild(listItem);
            });
            analysisDiv.appendChild(conceptsList);

            // Add DOT code display (optional)
            const dotCodeDisplay = document.createElement('pre');
            dotCodeDisplay.innerHTML = `<code class="language-dot">${paragraphData.dot}</code>`;
            analysisDiv.appendChild(dotCodeDisplay);
            hljs.highlightElement(dotCodeDisplay.firstChild); // Highlight the DOT code

            paragraphAnalysesContainer.appendChild(analysisDiv);
        }
    }

    // Generate .dot graph for the entire dataset or a filtered subset
    function generateDotGraph(filterConcept = null) {
        let dot = 'digraph HegelPhenomenology {\n';
        dot += '    rankdir=LR;\n';
        dot += '    node [shape=box, fontname="Arial", style=filled, fillcolor=white];\n';
        dot += '    edge [fontname="Arial"];\n';
        dot += '    labelloc=t;\n';
        dot += '    label="Hegel\'s Phenomenology of Spirit";\n';

        for (const paragraphNumber in hegelData.paragraphs) {
            const paragraphData = hegelData.paragraphs[paragraphNumber];
            
            // Skip paragraphs that don't contain the filterConcept
            if (filterConcept && !paragraphData.concepts.includes(filterConcept)) continue;

            const label = `Paragraph ${paragraphNumber}\\n${paragraphData.concepts.join('\\n')}`;
            dot += `    "${paragraphNumber}" [label="${label}"];\n`;
        }

        // Add edges based on concept evolution
        for (const concept in hegelData.concepts) {
            const paragraphs = hegelData.concepts[concept].sort((a, b) => a - b);
            for (let i = 0; i < paragraphs.length - 1; i++) {
                 // Skip edges that don't involve the filterConcept
                if (filterConcept && !hegelData.paragraphs[paragraphs[i]].concepts.includes(filterConcept)) continue;
                if (filterConcept && !hegelData.paragraphs[paragraphs[i+1]].concepts.includes(filterConcept)) continue;
                
                dot += `    "${paragraphs[i]}" -> "${paragraphs[i + 1]}" [label="${concept}"];\n`;
            }
        }

        dot += '}\n';
        return dot;
    }

// Generate a filtered .dot graph for a specific concept
function generateFilteredDotGraph(concept) {
    let dot = 'digraph HegelPhenomenology {\n';
    dot += '    rankdir=LR;\n';
    dot += '    node [shape=box, fontname="Arial", style=filled, fillcolor=white];\n';
    dot += '    edge [fontname="Arial"];\n';
    dot += '    labelloc=t;\n';
    dot += `    label="Hegel's Phenomenology of Spirit - ${concept}";\n`;

    // Add nodes for paragraphs related to the concept
    const conceptParagraphs = hegelData.concepts[concept].sort((a, b) => a - b);
    conceptParagraphs.forEach(paragraphNumber => {
        const paragraphData = hegelData.paragraphs[paragraphNumber];
        const label = `Paragraph ${paragraphNumber}\\n${paragraphData.concepts.join('\\n')}`;
        dot += `    "${paragraphNumber}" [label="${label}"];\n`;
    });

    // Add edges for the selected concept
    for (let i = 0; i < conceptParagraphs.length - 1; i++) {
        dot += `    "${conceptParagraphs[i]}" -> "${conceptParagraphs[i + 1]}" [label="${concept}"];\n`;
    }

    dot += '}\n';
    return dot;
}
    // Render the graph using Viz.js and enable pan/zoom
    function renderGraph(dot) {
        if (!dot) {
            console.error('No DOT graph provided');
            return;
        }

        const viz = new Viz();
        viz.renderSVGElement(dot)
            .then(svg => {
                graphContainer.innerHTML = ''; // Clear existing content

                // Set initial size and attributes
                svg.setAttribute('width', '100%');
                svg.setAttribute('height', '800px'); // Increased height
                svg.setAttribute('preserveAspectRatio', 'xMidYMid meet');
                graphContainer.appendChild(svg);

               // Initialize pan-zoom
                if (typeof svgPanZoom !== 'undefined') {
                    if (panZoomInstance) {
                        panZoomInstance.destroy();
                    }
                    panZoomInstance = svgPanZoom(svg, {
                        zoomEnabled: true,
                        controlIconsEnabled: true,
                        fit: true,
                        center: true,
                        minZoom: 0.1,
                        maxZoom: 10
                    });

                    // Resize handler
                    const resizeObserver = new ResizeObserver(() => {
                        panZoomInstance.resize();
                        panZoomInstance.fit();
                        panZoomInstance.center();
                    });

                    resizeObserver.observe(graphContainer);
                }

                // Add interactivity to nodes
                svg.querySelectorAll('.node').forEach(node => {
                    const nodeId = node.querySelector('title').textContent;

                    // Highlight paragraph on node click
                    node.addEventListener('click', () => {
                        const paragraphElement = document.getElementById(nodeId);
                        if (paragraphElement) {
                            // Remove previous highlights
                            document.querySelectorAll('.paragraph-analysis.highlight').forEach(el => {
                                el.classList.remove('highlight');
                            });

                            // Highlight the clicked paragraph
                            paragraphElement.classList.add('highlight');

                            // Scroll to the paragraph
                            paragraphElement.scrollIntoView({ behavior: 'smooth', block: 'start' });
                        }
                    });

                    // Highlight node on hover
                    node.addEventListener('mouseover', () => {
                        node.style.opacity = '0.8';
                    });
                    node.addEventListener('mouseout', () => {
                        node.style.opacity = '1';
                    });
                });
            })
            .catch(error => {
                console.error('Error rendering graph:', error);
                graphContainer.innerHTML = `
                    <div class="error-message">
                        <h3>Error rendering graph</h3>
                        <pre>${error.message}</pre>
                    </div>`;
            });
    }

    // Highlight concept in the graph
    function highlightConceptInGraph(concept) {
        const svg = graphContainer.querySelector('svg');
        if (svg) {
            // Reset all node colors
            svg.querySelectorAll('.node').forEach(node => {
                node.style.fill = '';
            });

            // Highlight nodes related to the concept
            const conceptParagraphs = hegelData.concepts[concept];
            svg.querySelectorAll('.node').forEach(node => {
                const title = node.querySelector('title').textContent;
                const paragraphNumber = parseInt(title);
                if (conceptParagraphs.includes(paragraphNumber)) {
                    node.style.fill = 'yellow'; // Highlight color
                }
            });
        }
    }

    // Highlight paragraphs based on search query
    function highlightParagraphs(query) {
        const paragraphs = document.querySelectorAll('.paragraph-analysis');
        paragraphs.forEach(paragraph => {
            paragraph.classList.remove('highlight');
            if (paragraph.textContent.toLowerCase().includes(query.toLowerCase())) {
                paragraph.classList.add('highlight');
            }
        });
    }

    // Event listeners
    conceptSelect.addEventListener('change', () => {
        const selectedConcept = conceptSelect.value;
        renderGraph(generateFilteredDotGraph(selectedConcept));
    });

    searchButton.addEventListener('click', () => {
        const query = searchInput.value;
        highlightParagraphs(query);
        // Optionally, filter the graph based on search results
        // renderGraph(generateFilteredDotGraph(null, query)); // You'll need to modify generateFilteredDotGraph to handle text search
    });

    // Initial setup
    generateParagraphAnalyses();
    renderGraph(generateDotGraph());
});