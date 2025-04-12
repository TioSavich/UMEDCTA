// brandom_lesson.js

document.addEventListener('DOMContentLoaded', () => {
    // --- Module Navigation ---
    const modules = document.querySelectorAll('.module');
    const prevButton = document.getElementById('prevButton');
    const nextButton = document.getElementById('nextButton');
    const moduleIndicator = document.getElementById('moduleIndicator');
    let currentModuleIndex = 0;
    let moduleInitializers = {}; // Store functions to initialize modules

    // --- Helper function for SVG (assuming it's used elsewhere too, place outside initializers) ---
    function getShapeSvg(shapeName) {
       let svg = `<svg viewBox="0 0 100 100" width="70" height="70" class="shape-viz">`;
       svg += `<title>${shapeName}</title>`; // Tooltip for SVG
       switch (shapeName) {
            case "Square": svg += `<rect x="10" y="10" width="80" height="80" fill="#cfe2ff" stroke="#0d6efd"/>`; break;
            case "Rectangle": svg += `<rect x="10" y="20" width="80" height="60" fill="#d1e7dd" stroke="#198754"/>`; break;
            case "Rhombus": svg += `<polygon points="50,5 95,50 50,95 5,50" fill="#f8d7da" stroke="#dc3545"/>`; break;
            case "Parallelogram": svg += `<polygon points="25,5 95,5 75,95 5,95" fill="#fff3cd" stroke="#ffc107"/>`; break;
            case "Kite": svg += `<polygon points="50,10 85,50 50,90 15,50" fill="#f3d7f8" stroke="#a30cff"/>`; break; // Adjusted Kite points slightly
            case "Trapezoid": svg += `<polygon points="30,10 70,10 100,90 0,90" fill="#e2e3e5" stroke="#6c757d"/>`; break;
            case "Quadrilateral": default: svg += `<polygon points="10,10 90,20 80,80 20,90" fill="#f8f9fa" stroke="#adb5bd"/>`; break;
       }
       // Adjust text position if needed
       svg += `<text x="50" y="115" text-anchor="middle" font-size="10px" style="font-family: sans-serif;">${shapeName}</text></svg>`;
       return svg.replace('<svg ', '<svg height="85" '); // Ensure text is visible
    }

    function showModule(index) {
        const totalModules = 7; // **** UPDATE THIS ****
        if (index < 0 || index >= totalModules) return;

        modules.forEach((module, i) => {
            module.style.display = 'none';
            module.classList.remove('current-module', 'hidden-module');
            if (i === index) {
                module.style.display = 'block';
                module.classList.add('current-module');
            } else {
                module.classList.add('hidden-module');
            }
        });

        currentModuleIndex = index;
        moduleIndicator.textContent = `Module ${index + 1} of ${totalModules}`; // **** UPDATE THIS ****
        prevButton.disabled = index === 0;
        nextButton.disabled = index === totalModules - 1; // **** UPDATE THIS ****

        if (typeof moduleInitializers[index] === 'function') {
            try {
                 moduleInitializers[index]();
                 moduleInitializers[index] = null; // Mark as run
            } catch (error) {
                 console.error(`Error initializing module ${index + 1}:`, error);
            }
        }

        // Scroll to top
        const targetModule = modules[index];
        if (targetModule) {
            const headerOffset = document.querySelector('header')?.offsetHeight || 60;
            const elementPosition = targetModule.getBoundingClientRect().top;
            const offsetPosition = elementPosition + window.pageYOffset - headerOffset - 20;
            window.scrollTo({ top: offsetPosition, behavior: 'smooth' });
         }
    }

    prevButton.addEventListener('click', () => showModule(currentModuleIndex - 1));
    nextButton.addEventListener('click', () => showModule(currentModuleIndex + 1));

    // --- Data Structures ---
    const shapeHierarchy = {
        "Square": { superclasses: ["Rectangle", "Rhombus"], properties: ["4 equal sides", "4 right angles", "opposite sides parallel", "diagonals bisect perpendicularly", "diagonals are equal"], incompatibilities: ["has obtuse angle", "unequal adjacent sides"] },
        "Rectangle": { superclasses: ["Parallelogram", /*"Isosceles Trapezoid" - Removed for simpler linear hierarchy in slider */], properties: ["4 right angles", "opposite sides parallel", "opposite sides equal", "diagonals are equal", "diagonals bisect each other"], incompatibilities: ["has acute angle (internal)", "unequal diagonals"] },
        "Rhombus": { superclasses: ["Parallelogram", "Kite"], properties: ["4 equal sides", "opposite sides parallel", "opposite angles equal", "diagonals bisect perpendicularly"], incompatibilities: ["has right angle but unequal adjacent sides"] },
        "Parallelogram": { superclasses: ["Trapezoid"], properties: ["opposite sides parallel", "opposite sides equal", "opposite angles equal", "diagonals bisect each other"], incompatibilities: ["only one pair parallel sides"] },
        "Kite": { superclasses: ["Quadrilateral"], properties: ["2 pairs adjacent equal sides", "one pair opposite angles equal", "diagonals perpendicular"], incompatibilities: ["opposite sides parallel", "all sides equal"] },
        // "Isosceles Trapezoid": { superclasses: ["Trapezoid"], properties: ["one pair parallel sides", "base angles equal", "diagonals are equal"], incompatibilities: ["is equilateral", "has perpendicular diagonals"] }, // Removed for linear slider
        "Trapezoid": { superclasses: ["Quadrilateral"], properties: ["at least one pair parallel sides"], incompatibilities: ["no parallel sides", "all sides equal"] },
        "Quadrilateral": { superclasses: [], properties: ["4 sides"], incompatibilities: ["is a triangle", "has 5 sides"] }
    };
    // Define the chain for sliders - ensure it matches hierarchy logic where needed
    const hierarchyChainForSlider = ["Quadrilateral", "Trapezoid", "Parallelogram", "Rectangle", "Square"]; // Weakest to Strongest


    function isSubclass(shapeA, shapeB) {
        if (shapeA === shapeB) return true;
        if (!shapeHierarchy[shapeA] || !shapeHierarchy[shapeB]) {
            // console.warn(`isSubclass: Shape definition missing for ${shapeA} or ${shapeB}`);
            return false;
        }
        let queue = [...(shapeHierarchy[shapeA].superclasses || [])];
        let visited = new Set([shapeA]);
        while (queue.length > 0) {
            const current = queue.shift();
            if (current === shapeB) return true;
            if (!visited.has(current) && shapeHierarchy[current]) {
                visited.add(current);
                if (shapeHierarchy[current].superclasses) {
                     queue.push(...shapeHierarchy[current].superclasses);
                }
            }
        }
        return false;
    }

    // --- Module Initializers ---

    // Module 1: Intro
    moduleInitializers[0] = function() {
        const vizSquare = document.getElementById('viz-square-m1');
        const vizRectangle = document.getElementById('viz-rectangle-m1');
        if (vizSquare) vizSquare.innerHTML = getShapeSvg("Square");
        if (vizRectangle) vizRectangle.innerHTML = getShapeSvg("Rectangle");
    };

        // *** REVISED Module 2: Quadrilateral Checklist Initializer ***
        moduleInitializers[1] = function setupQuadrilateralChecklist() {
            // Define shape data using 'false' for "rejects property / incompatibility holds"
            // Corresponds to Table 1 logic where X means incompatibility (rejects the "No..." property)
            const shapesDataM2 = [
                // Property keys match checkbox IDs r1-r6
                { name: "Square",       r1: false, r2: false, r3: false, r4: false, r5: false, r6: false }, // Rejects all "No..." props
                { name: "Rectangle",    r1: false, r2: true,  r3: false, r4: true,  r5: false, r6: false }, // Allows r2, r4
                { name: "Rhombus",      r1: false, r2: false, r3: false, r4: false, r5: false, r6: true },  // Allows r6
                { name: "Parallelogram",r1: false, r2: true,  r3: false, r4: true,  r5: false, r6: true },  // Allows r2, r4, r6
                { name: "Trapezoid",    r1: true,  r2: true,  r3: true,  r4: true,  r5: false, r6: true },  // Only rejects r5
                { name: "Kite",         r1: false, r2: false, r3: true,  r4: false, r5: true,  r6: true },  // Allows r3, r5, r6
                { name: "Quadrilateral",r1: true,  r2: true,  r3: true,  r4: true,  r5: true,  r6: true }   // Allows all
            ];
    
            // Map names to data for easier lookup later (e.g., in Module 4)
            window.shapeDataMap = shapesDataM2.reduce((acc, shape) => {
                acc[shape.name] = shape;
                return acc;
            }, {});
    
    
            const checkboxesContainer = document.getElementById('restrictionCheckboxesM2');
            if (!checkboxesContainer) {
                 console.error("Module 2 checkboxes container not found!");
                 return; // Exit if container is missing
            }
            const checkboxes = checkboxesContainer.querySelectorAll('input[type="checkbox"]');
            const shapesContainer = document.getElementById('shapesContainerM2'); // Use the new ID
    
            // --- Calculate Strength ---
            // Strength = number of properties the shape REJECTS (has 'false' for)
            function calculateStrength(shape) {
                let strength = 0;
                for (let i = 1; i <= 6; i++) {
                    if (shape[`r${i}`] === false) {
                        strength++;
                    }
                }
                return strength;
            }
    
            // Add strength to the main data structure for easy access
            shapesDataM2.forEach(shape => {
                shape.strength = calculateStrength(shape);
            });
    
    
            function getActiveRestrictions() {
                const active = {};
                checkboxes.forEach(cb => {
                    active[cb.dataset.propertyKey] = cb.checked;
                });
                return active;
            }
    
            function filterShapes() {
                const activeRestrictions = getActiveRestrictions();
                const targetValue = false; // A shape survives if it REJECTS the property (has 'false') when the restriction is active
    
                return shapesDataM2.filter(shape => {
                    for (const restrictionKey in activeRestrictions) {
                        // If the restriction checkbox IS CHECKED (activeRestrictions[restrictionKey] is true)...
                        if (activeRestrictions[restrictionKey]) {
                            // ...then the shape MUST have 'false' for this property to survive.
                            if (shape[restrictionKey] !== targetValue) {
                                return false; // Filter out this shape
                            }
                        }
                    }
                    return true; // Survived all active restrictions
                });
            }
    
            function updateShapesDisplay() {
                if (!shapesContainer) return; // Safety check
                const filteredShapes = filterShapes();
                shapesContainer.innerHTML = ''; // Clear previous shapes
                if (filteredShapes.length === 0) {
                    shapesContainer.innerHTML = '<p>No quadrilaterals match the current restrictions.</p>';
                } else {
                    // Sort shapes maybe by strength? (Optional)
                    // filteredShapes.sort((a, b) => b.strength - a.strength);
    
                    filteredShapes.forEach(shape => {
                        const div = document.createElement('div');
                        div.className = 'shape-item'; // Use a class if you want specific styling
                        div.style.textAlign = 'center'; // Center text below SVG
                        div.style.margin = '5px';
                        // Use the global SVG function and add strength display
                        div.innerHTML = getShapeSvg(shape.name).replace(/<\/svg>/, `
                            <text x="50" y="125" text-anchor="middle" font-size="10px" fill="#333" style="font-family: sans-serif;">
                                Strength: ${shape.strength}
                            </text>
                            </svg>`);
                        shapesContainer.appendChild(div);
                    });
                }
            }
    
            // Attach event listeners to checkboxes within this module
            checkboxes.forEach(cb => {
                cb.addEventListener('change', updateShapesDisplay);
            });
    
            // Initial display update for this module
            updateShapesDisplay();
        }; // End Module 2 Initializer


    // Module 3: Substitution Conceptual Intro
    moduleInitializers[2] = function() {
        // This module is now primarily text and static examples in the HTML.
        // No dynamic JS needed unless you add interactive highlighting later.
    };

    // Module 4: Polarity Demo
        // *** REVISED Module 4: Polarity Demo Initializer ***
    moduleInitializers[3] = function setupPolarityDemo() {
        // Ensure shape data is available from Module 2
        if (typeof window.shapeDataMap === 'undefined') {
            console.error("Shape data not initialized from Module 2. Run Module 2 first.");
            // Attempt to run Module 2 initializer if it hasn't run
            if (typeof moduleInitializers[1] === 'function') {
                console.warn("Attempting to initialize Module 2 now...");
                try {
                    moduleInitializers[1]();
                    moduleInitializers[1] = null; // Mark as run
                    if (typeof window.shapeDataMap === 'undefined') {
                         // Still failed
                         alert("Error: Module 2 data needed for Module 4. Please reload and navigate sequentially.");
                         return;
                    }
                 } catch(e) {
                      alert("Error initializing Module 2 data. Please reload and navigate sequentially.");
                      return;
                 }
            } else {
                 alert("Module 2 already initialized but data missing. Please reload.");
                 return;
            }
        }

        // Use unique IDs for Module 4 elements
        const fixedSelect = document.getElementById('fixedConceptSelectM4');
        const strengthSlider = document.getElementById('strengthSliderM4');
        const varConceptLabel = document.getElementById('variableConceptLabelM4');
        const varConceptLabelCond = document.getElementById('variableConceptLabelCondM4');
        const vizP_El = document.getElementById('vizPM4');
        const vizQ_R_El = document.getElementById('vizQM4'); // Renamed for clarity, shows Q
        const relationArrowEl = document.getElementById('relationArrowM4');
        const strengthPEl = document.getElementById('strengthPM4'); // Span for P strength
        const strengthQEl = document.getElementById('strengthQM4'); // Span for Q strength


        const baseInferEl = document.getElementById('baseInferM4');
        const converseInferEl = document.getElementById('converseInferM4');
        const contraInferEl = document.getElementById('contraInferM4');
        const inverseInferEl = document.getElementById('inverseInferM4');

        const propertyRSelect = document.getElementById('propertyRSelectM4');
        const propertyRLabel = document.getElementById('propertyRLabelM4');
        const condAntecedentEl = document.getElementById('condAntecedentM4');
        const condAntecedentStatusEl = document.getElementById('condAntecedentStatusM4');
        const condConsequentEl = document.getElementById('condConsequentM4');
        const condConsequentStatusEl = document.getElementById('condConsequentStatusM4');

        // Use the defined slider chain
        const sliderChain = hierarchyChainForSlider; // Uses the global constant

        // Populate fixed concept select
        fixedSelect.innerHTML = sliderChain.map(t => `<option value="${t}">${t}</option>`).join('');
        fixedSelect.value = "Rectangle"; // Default Q

        // Populate Property R select (remains the same)
        const availableProperties = {
            "HAS_4_SIDES": { value: "has 4 sides", description: "has 4 sides" },
            "HAS_PARALLEL_SIDES": { value: "at least one pair parallel sides", description: "at least one pair parallel sides" },
            "OPPOSITE_SIDES_PARALLEL": { value: "opposite sides parallel", description: "opposite sides parallel" },
            "HAS_4_RIGHT_ANGLES": { value: "4 right angles", description: "has 4 right angles" },
            "HAS_4_EQUAL_SIDES": { value: "4 equal sides", description: "has 4 equal sides" }
        };
        propertyRSelect.innerHTML = Object.values(availableProperties)
            .map(p => `<option value="${p.value}">${p.description}</option>`)
            .join('');
        propertyRSelect.value = "4 right angles"; // Default R

        // Set Slider Labels and Range
        document.getElementById('sliderMinLabelM4').textContent = sliderChain[0];
        document.getElementById('sliderMaxLabelM4').textContent = sliderChain[sliderChain.length - 1];
        strengthSlider.max = sliderChain.length - 1;
        strengthSlider.value = sliderChain.findIndex(s => s === "Square"); // Default P = Strongest

         // --- Validity Helper Functions (remain the same) ---
         function conceptEntailsProperty(conceptP, propertyR_value) {
              if (!shapeHierarchy[conceptP]) return false;
              let queue = [conceptP];
              let visited = new Set();
              while (queue.length > 0) {
                  let node = queue.shift();
                  if (visited.has(node)) continue;
                  visited.add(node);
                  if (shapeHierarchy[node]?.properties?.includes(propertyR_value)) {
                      return true;
                  }
                  if (shapeHierarchy[node]?.superclasses) {
                      shapeHierarchy[node].superclasses.forEach(sc => { if (!visited.has(sc)) queue.push(sc); });
                  }
              }
              return false;
          }

          function propertyEntailsConcept(propertyR_value, conceptP) {
              if (!shapeHierarchy[conceptP]) return false;
              let shapesThatGuaranteeR = [];
              for (const shape in shapeHierarchy) {
                  if (conceptEntailsProperty(shape, propertyR_value)) {
                      shapesThatGuaranteeR.push(shape);
                  }
              }
              if (shapesThatGuaranteeR.length === 0) return false;
              // Check if *all* shapes guaranteeing R are subclasses of conceptP
              return shapesThatGuaranteeR.every(shape => isSubclass(shape, conceptP));
          }

        function updatePolarityDemo() {
            const fixedConceptQ = fixedSelect.value;
            const sliderIndex = parseInt(strengthSlider.value);
            const variableConceptP = sliderChain[sliderIndex];
            const selectedPropertyR_value = propertyRSelect.value;
            const selectedPropertyR_desc = propertyRSelect.options[propertyRSelect.selectedIndex].text;

            // --- Get Strengths using Module 2 data ---
            const strengthP = window.shapeDataMap[variableConceptP]?.strength ?? '?';
            const strengthQ = window.shapeDataMap[fixedConceptQ]?.strength ?? '?';

            // Update labels and strengths
            varConceptLabel.textContent = variableConceptP;
            strengthPEl.textContent = strengthP;
            strengthQEl.textContent = strengthQ;
            varConceptLabelCond.textContent = variableConceptP;
            propertyRLabel.textContent = selectedPropertyR_desc;

            vizP_El.innerHTML = getShapeSvg(variableConceptP);
            vizQ_R_El.innerHTML = getShapeSvg(fixedConceptQ); // Display Q here

            const pImpliesQ = isSubclass(variableConceptP, fixedConceptQ);
            const qImpliesP = isSubclass(fixedConceptQ, variableConceptP);
            let relationSymbol = '❓';
            if (pImpliesQ && qImpliesP) { relationSymbol = '⇔ Equivalent'; }
            else if (pImpliesQ) { relationSymbol = `⇒ Stronger (S=${strengthP}) ⇒`; } // Show P is stronger
            else if (qImpliesP) { relationSymbol = `⇐ Weaker (S=${strengthP}) ⇐`; } // Show P is weaker
            else { relationSymbol = ' unrelated '; } // Added case for unrelated
            relationArrowEl.textContent = relationSymbol;

            // Update Base/Converse/Contra/Inverse validity displays (logic remains the same)
            baseInferEl.innerHTML = `Base (P ⇒ Q): If X is <span class="term">${variableConceptP}</span> then X is <span class="term">${fixedConceptQ}</span>? <span class="status-indicator ${pImpliesQ ? 'valid' : 'invalid'}">${pImpliesQ ? 'Valid' : 'Invalid'}</span>`;
            converseInferEl.innerHTML = `Converse (Q ⇒ P): If X is <span class="term">${fixedConceptQ}</span> then X is <span class="term">${variableConceptP}</span>? <span class="status-indicator ${qImpliesP ? 'valid' : 'invalid'}">${qImpliesP ? 'Valid' : 'Invalid'}</span>`;
            const notQImpliesNotP = pImpliesQ; // Validity matches base
            const notPImpliesNotQ = qImpliesP; // Validity matches converse
            contraInferEl.innerHTML = `Contrapositive (¬Q ⇒ ¬P): If X is not <span class="term">${fixedConceptQ}</span> then X is not <span class="term">${variableConceptP}</span>? <span class="status-indicator ${notQImpliesNotP ? 'valid' : 'invalid'}">${notQImpliesNotP ? 'Valid' : 'Invalid'}</span>`;
            inverseInferEl.innerHTML = `Inverse (¬P ⇒ ¬Q): If X is not <span class="term">${variableConceptP}</span> then X is not <span class="term">${fixedConceptQ}</span>? <span class="status-indicator ${notPImpliesNotQ ? 'valid' : 'invalid'}">${notPImpliesNotQ ? 'Valid' : 'Invalid'}</span>`;

            // --- Calculate Validity ONCE at the beginning of this section ---
            const ifPThenR_Valid = conceptEntailsProperty(variableConceptP, selectedPropertyR_value);
            const ifRThenP_Valid = propertyEntailsConcept(selectedPropertyR_value, variableConceptP);

            // --- Update Conditional Embedding displays (using the calculated values) ---

            // Rebuild Antecedent Element Content
            const antecedentSentenceHTML = `Antecedent Position: "If X is <span class='term'>${variableConceptP}</span>, then X ${selectedPropertyR_desc}"`;
            condAntecedentEl.innerHTML = antecedentSentenceHTML; // Set the sentence HTML
            // Use the already calculated 'ifPThenR_Valid'
            condAntecedentStatusEl.className = `status-indicator ${ifPThenR_Valid ? 'valid' : 'invalid'}`;
            condAntecedentStatusEl.textContent = ifPThenR_Valid ? 'Valid' : 'Invalid';
            condAntecedentEl.appendChild(condAntecedentStatusEl); // Append the status indicator

            // Rebuild Consequent Element Content
            const consequentSentenceHTML = `Consequent Position: "If X ${selectedPropertyR_desc}, then X is <span class='term'>${variableConceptP}</span>"`;
            condConsequentEl.innerHTML = consequentSentenceHTML; // Set the sentence HTML
            // Use the already calculated 'ifRThenP_Valid'
            condConsequentStatusEl.className = `status-indicator ${ifRThenP_Valid ? 'valid' : 'invalid'}`;
            condConsequentStatusEl.textContent = ifRThenP_Valid ? 'Valid' : 'Invalid';
            condConsequentEl.appendChild(condConsequentStatusEl); // Append the status indicator
        }

        // Event Listeners
        fixedSelect.addEventListener('change', updatePolarityDemo);
        strengthSlider.addEventListener('input', updatePolarityDemo);
        propertyRSelect.addEventListener('change', updatePolarityDemo);

        updatePolarityDemo(); // Initial call
    }; // End Module 4 Initializer

    // Module 5: Substitution Argument + Animation
    moduleInitializers[4] = function setupSubstitutionDemoViz() {
        const exampleSelect = document.getElementById('substExampleSelectViz');
        const frameSelect = document.getElementById('frameSelectViz');
        const frameVizEl = document.getElementById('frameViz');
        const exprA_VizEl = document.getElementById('exprA_Viz');
        const exprB_VizEl = document.getElementById('exprB_Viz');
        const animateBtn = document.getElementById('animateSubstButtonViz');
        const resetBtn = document.getElementById('resetSubstButtonViz');
        const baseSentenceEl = document.getElementById('baseSentenceViz');
        const resultSentenceEl = document.getElementById('resultSentenceViz');
        const infer1StatusEl = document.getElementById('infer1StatusViz');
        const infer2StatusEl = document.getElementById('infer2StatusViz');
        const analysisEl = document.getElementById('substAnalysisViz'); // The explanation area

        // Define frames with polarity info
        const frames = {
            simple_assertion: {
                template: "Shape S {expr}.",
                display: "Shape S _.",
                isInverting: false,
                property: null
            },
            conditional_antecedent: { // UPDATED
                template: "If Shape S {expr}, then S is a Rhombus.", // New Consequent
                display: "If Shape S _, then S is a Rhombus.", // New Display
                isInverting: true,
                property: "is a Rhombus" // Store the consequent property
            },
            negation: {
                template: "It is NOT the case that Shape S {expr}.",
                display: "It is NOT the case that Shape S _.",
                isInverting: true,
                property: null
            }
        };

        // Define examples with underlying rules
        const examples = {
            singularTerms: {
                exprA: "Mark Twain",
                exprB: "Samuel Clemens",
                isSymmetric: true,
                underlyingRule: "A ↔ B (Assumed Co-referential)",
                explanationTemplate: "Substituting co-referential terms ('${exprA}' ↔ '${exprB}'). Significance is SYMMETRIC."
            },
            predicatesStrongerToWeaker: {
                exprA: "is a Square",
                exprB: "is a Rectangle",
                isSymmetric: false,
                underlyingRule: "A ⇒ B (Square ⇒ Rectangle)",
                 explanationTemplate: "Substituting a stronger predicate ('${exprA}') with a weaker one ('${exprB}'). Significance is ASYMMETRIC."
            },
            predicatesWeakerToStronger: {
                 exprA: "is a Rectangle",
                 exprB: "is a Square",
                 isSymmetric: false,
                 underlyingRule: "A ⇏ B (Rectangle ⇏ Square), but B ⇒ A", // Note the base directionality
                 explanationTemplate: "Substituting a weaker predicate ('${exprA}') with a stronger one ('${exprB}'). Significance is ASYMMETRIC."
            }
        };

         let animationTimeout = null; // To clear existing timeouts on reset/change

        // --- Helper to build sentences with spans for animation ---
         function buildSentence(template, expression) {
              // Simple replacement for now, assuming one placeholder '{expr}'
              // Escape expression to prevent HTML issues if needed, though unlikely here
              const escapedExpr = expression; // Simplification
              const placeholder = "{expr}";
              const parts = template.split(placeholder);
              if (parts.length === 2) {
                  // Wrap the expression part in a span for animation targeting
                  return parts[0] + `<span class="substituted-part">${escapedExpr}</span>` + parts[1];
              }
              return template.replace(placeholder, `<span class="substituted-part">${escapedExpr}</span>`); // Fallback
         }

        // --- Setup the initial view ---
        function setupInitialView() {
            clearTimeout(animationTimeout); // Clear any pending animation
            animateBtn.disabled = false; // Re-enable button

            const selectedExampleKey = exampleSelect.value;
            const selectedFrameKey = frameSelect.value;
            const example = examples[selectedExampleKey];
            const frame = frames[selectedFrameKey];

            exprA_VizEl.textContent = example.exprA;
            exprB_VizEl.textContent = example.exprB;
            frameVizEl.textContent = frame.display;

             // Reset styling on expression boxes
             exprA_VizEl.classList.remove('highlight-replace', 'lift-out-anim', 'fade-out-anim');
             exprB_VizEl.classList.remove('highlight-incoming');
             exprA_VizEl.style.opacity = '1'; exprA_VizEl.style.transform = '';
             exprB_VizEl.style.opacity = '1'; exprB_VizEl.style.transform = '';


            // Build and display initial sentences
            baseSentenceEl.innerHTML = buildSentence(frame.template, example.exprA);
            // Reset result sentence visually, perhaps hide it initially or show placeholder
            resultSentenceEl.innerHTML = buildSentence(frame.template, '...'); // Placeholder
            resultSentenceEl.style.opacity = 0.5; // Dim it initially

            // Clear status indicators
            infer1StatusEl.textContent = ''; infer1StatusEl.className = 'status-indicator';
            infer2StatusEl.textContent = ''; infer2StatusEl.className = 'status-indicator';

            // Reset analysis text
            analysisEl.innerHTML = `<h4>4. Analysis: What Happens?</h4><p>Select an example and context, then click ▶️ Animate to see the substitution and evaluate the inferences.</p>`;

            // Ensure spans inside sentences are reset
            baseSentenceEl.querySelectorAll('.substituted-part, .substituting-part').forEach(span => {
                span.style.opacity = ''; span.style.transform = '';
                span.className = 'substituted-part'; // Ensure it starts as the base part
            });
             resultSentenceEl.querySelectorAll('.substituted-part, .substituting-part').forEach(span => {
                 span.textContent = '...';
                 span.style.opacity = '0'; span.style.transform = '';
                 span.className = 'substituted-part';
             });

        }


        // --- Run the animation ---
        function runSubstitutionAnimation() {
            clearTimeout(animationTimeout); // Clear previous timeouts
            animateBtn.disabled = true; // Disable button during animation

            const selectedExampleKey = exampleSelect.value;
            const selectedFrameKey = frameSelect.value;
            const example = examples[selectedExampleKey];
            const frame = frames[selectedFrameKey];

            const baseSpan = baseSentenceEl.querySelector('.substituted-part');
            const resultSpanTemplate = `<span class="substituting-part">${example.exprB}</span>`; // Prepare the incoming part

            // 1. Highlight the term to be substituted
            exprA_VizEl.classList.add('highlight-replace');
            if(baseSpan) baseSpan.style.backgroundColor = '#a8d5ff'; // Highlight in sentence

            animationTimeout = setTimeout(() => {
                // 2. Lift out the original term visually
                exprA_VizEl.classList.add('lift-out-anim');
                if(baseSpan) baseSpan.classList.add('lift-out-anim');

                animationTimeout = setTimeout(() => {
                    // 3. Fade out the original term
                    exprA_VizEl.classList.add('fade-out-anim');
                     if (baseSpan) {
                         baseSpan.classList.add('fade-out-anim');
                     }

                     // Prepare result sentence content *while* base fades
                     resultSentenceEl.innerHTML = baseSentenceEl.innerHTML.replace(/<span class="substituted-part lift-out-anim fade-out-anim"[^>]*>.*?<\/span>/, resultSpanTemplate);
                     resultSentenceEl.style.opacity = 1; // Make result sentence visible

                    animationTimeout = setTimeout(() => {
                         // 4. Highlight the incoming term
                         exprB_VizEl.classList.add('highlight-incoming');
                         const resultSpan = resultSentenceEl.querySelector('.substituting-part'); // Find the newly added span

                         animationTimeout = setTimeout(() => {
                              // 5. Move in the new term
                              exprB_VizEl.classList.remove('highlight-incoming'); // Remove highlight
                              if (resultSpan) {
                                  resultSpan.classList.add('move-in-anim'); // Trigger move-in style
                              }

                              // 6. Evaluate and display inferences AFTER animation settles
                             animationTimeout = setTimeout(() => {
                                  evaluateAndDisplayInferences(); // Calculate validity and update text
                                  animateBtn.disabled = false; // Re-enable button
                                  // Optional: Clean up animation classes on spans if needed, though reset handles it
                                  if(baseSpan) { baseSpan.className = 'substituted-part'; baseSpan.style = ''; }
                                  const finalResultSpan = resultSentenceEl.querySelector('.substituting-part');
                                  if(finalResultSpan) { finalResultSpan.className = 'substituted-part'; finalResultSpan.style = ''; } // Treat it as base now
                             }, 400); // Wait for move-in animation

                         }, 300); // Duration of incoming highlight

                    }, 300); // Duration of fade-out

                }, 300); // Duration of lift-out

            }, 200); // Initial highlight duration
        }


        // --- Evaluate Inferences (Simplified Logic - NEEDS Brandom's Logic) ---
         function evaluateAndDisplayInferences() {
             const selectedKey = exampleSelect.value;
             const selectedFrameKey = frameSelect.value;
             const example = examples[selectedKey];
             const frame = frames[selectedFrameKey];
             const termA = example.exprA;
             const termB = example.exprB;

             // --- Determine Underlying Material Implications ---
             let aMateriallyImpliesB, bMateriallyImpliesA;
             const isSymmetricExample = example.isSymmetric;

             if (isSymmetricExample) {
                 aMateriallyImpliesB = true; // By definition for co-referential terms
                 bMateriallyImpliesA = true;
             } else {
                 // Use the conceptual hierarchy for predicates
                 const conceptA = termA.startsWith("is a ") ? termA.substring(5) : termA;
                 const conceptB = termB.startsWith("is a ") ? termB.substring(5) : termB;
                 aMateriallyImpliesB = isSubclass(conceptA, conceptB); // e.g., Square is subclass of Rectangle
                 bMateriallyImpliesA = isSubclass(conceptB, conceptA); // e.g., Rectangle is NOT subclass of Square
             }

             // --- Determine Validity of the Substitution Inference (Base => Result) ---
             let infer1_valid = false;
             if (isSymmetricExample) {
                 infer1_valid = true; // Symmetric substitution is always valid in both directions *if terms co-refer*
             } else {
                 if (frame.isInverting) {
                     // Polarity inverted: Inference Base => Result is valid IF B materially implies A
                     infer1_valid = bMateriallyImpliesA;
                 } else {
                     // Normal polarity: Inference Base => Result is valid IF A materially implies B
                     infer1_valid = aMateriallyImpliesB;
                 }
             }

             // --- Determine Validity of the Reverse Substitution Inference (Result => Base) ---
             let infer2_valid = false;
             if (isSymmetricExample) {
                 infer2_valid = true; // Symmetric
             } else {
                 if (frame.isInverting) {
                      // Polarity inverted: Inference Result => Base is valid IF A materially implies B
                     infer2_valid = aMateriallyImpliesB;
                 } else {
                      // Normal polarity: Inference Result => Base is valid IF B materially implies A
                     infer2_valid = bMateriallyImpliesA;
                 }
             }

             // --- Update DOM elements ---
             infer1StatusEl.textContent = infer1_valid ? 'Valid' : 'Invalid';
             infer1StatusEl.className = `status-indicator ${infer1_valid ? 'valid' : 'invalid'}`;
             infer2StatusEl.textContent = infer2_valid ? 'Valid' : 'Invalid';
             infer2StatusEl.className = `status-indicator ${infer2_valid ? 'valid' : 'invalid'}`;

            // Update analysis text based on results
             let analysisText = `<h4>4. Analysis: What Happened?</h4>`;
             analysisText += `<p>Underlying Rule: <strong>${example.underlyingRule}</strong>. Context Polarity: <strong>${frame.isInverting ? 'Inverting' : 'Non-Inverting'}</strong>.</p>`;
             if (isSymmetricExample) {
                 analysisText += `<p>✅ With <strong>Symmetric Terms</strong>, the substitution is valid in <strong>both directions</strong> (${infer1_valid ? '✓' : '✗'} Base⇒Result, ${infer2_valid ? '✓' : '✗'} Result⇒Base), regardless of the context's polarity. This stability is key for terms referring to objects.</p>`;
             } else {
                 analysisText += `<p>⚠️ With <strong>Asymmetric Predicates</strong>:</p><ul>`;
                 analysisText += `<li>Base ⇒ Result validity (${infer1_valid ? '✓ Valid' : '✗ Invalid'}) ${frame.isInverting ? 'depends on the REVERSE material rule (B⇒A)' : 'depends on the FORWARD material rule (A⇒B)'} because the context is ${frame.isInverting ? 'INVERTING' : 'Non-Inverting'}.</li>`;
                 analysisText += `<li>Result ⇒ Base validity (${infer2_valid ? '✓ Valid' : '✗ Invalid'}) ${frame.isInverting ? 'depends on the FORWARD material rule (A⇒B)' : 'depends on the REVERSE material rule (B⇒A)'}.</li>`;
                 if (frame.isInverting && aMateriallyImpliesB !== bMateriallyImpliesA) {
                     analysisText += `<li style="color: purple; font-weight: bold;">🚨 Notice the flip! The valid inference direction changed compared to a simple context because this context is inverting.</li>`;
                 } else if (!frame.isInverting && aMateriallyImpliesB !== bMateriallyImpliesA) {
                     analysisText += `<li>This follows the basic asymmetric pattern, as the context is not inverting.</li>`;
                 }
                  analysisText += `</ul>`;
                  analysisText += `<p><strong>The Breakdown:</strong> If '${termA}'/'${termB}' were playing the basic 'Substituted-For' role but *had* this asymmetric rule, the rule wouldn't work consistently across all contexts. One rule would demand A⇒B in simple contexts but B⇒A in inverting ones!</p>`;
                  analysisText += `<p><strong>Brandom's Conclusion:</strong> Therefore, the basic 'Substituted-For' role *must* have SYMMETRIC significance (like terms). The asymmetric rules (like Square⇒Rectangle) belong to the 'Frame' role (predicates), and logic handles the polarity flips for those frames correctly.</p>`;
             }
             analysisEl.innerHTML = analysisText;
         }


        // Event Listeners
        exampleSelect.addEventListener('change', setupInitialView);
        frameSelect.addEventListener('change', setupInitialView);
        animateBtn.addEventListener('click', runSubstitutionAnimation);
        resetBtn.addEventListener('click', setupInitialView);

        // Initial setup
        setupInitialView(); // Make sure the view is correct on load
    };

    // Module 6: Matrix (Original Module 5)
    moduleInitializers[5] = function() { /* Module 6 (Original 5) - Likely static */ };

    // Module 7: Conclusion (Original Module 6)
    moduleInitializers[6] = function() { /* Module 7 (Original 6) - Likely static */ };

    // --- Initialize first module ---
    showModule(0);

}); // End DOMContentLoaded