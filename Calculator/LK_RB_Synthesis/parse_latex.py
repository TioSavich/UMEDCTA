import re
import json

def parse_latex(file_path):
    with open(file_path, 'r', encoding='utf-8') as f:
        content = f.read()

    # Find all sections, which represent strategies
    section_pattern = re.compile(r'\\section\{(.+?)\}(.+?)(?=\\section|\Z)', re.DOTALL)
    sections = section_pattern.findall(content)

    strategies = []
    for name, section_content in sections:
        name = name.strip().replace('\n', ' ')

        # List of sections to skip
        if name in ["Conceptual Dependency Graph (Narrative)",
                     "Temporal Dynamics Summary",
                     "Glossary of Symbols",
                     "Hermeneutic Calculator: Strategy Formalizations",
                     "Counting and Counting On"]: # Skip the first one as it's a primitive
            continue

        # Extract the description
        description_match = re.search(r'\\textbf\{Description\.\}\s*(.+?)(?=\\textbf|\\begin\{longtable\}|\\begin\{center\}|\Z)', section_content, re.DOTALL)
        description = "No description found."
        if description_match:
            description = description_match.group(1).strip()
            # Clean up LaTeX commands from description
            description = re.sub(r'\s*\\(.+?)\s*', '', description)
            description = re.sub(r'[\{\}\$]', '', description)


        # Extract cognitive steps from the Q = {...} definition
        cognitive_steps = []
        q_match = re.search(r'Q\s*=\s*\\\{(.+?)\\\}', section_content)
        if q_match:
            states_str = q_match.group(1)
            # Remove latex commands for states and clean up
            states_str = re.sub(r'q_\{(.+?)\}', r'q_\1', states_str)
            states = [s.strip().replace('\\', '') for s in states_str.split(',')]
            # We want the state names without the 'q_' prefix for the methods
            cognitive_steps = [s for s in states if s != 'q_accept']

        # Handle special cases
        if "Subtraction Chunking" in name:
            orientation_pattern = re.compile(r'\\textbf\{([ABC])\.\s*(.+?)\}.+?\(V = \\{(.+?)\\}\)', re.DOTALL)
            orientations = orientation_pattern.findall(section_content)
            for _letter, desc, registers in orientations:
                full_name = f"Subtraction Chunking ({desc.strip()})"
                # Infer steps from registers
                steps = ['q_init']
                reg_list = [r.strip() for r in registers.split(',')]
                if 'Chunk' in reg_list or 'S_{rem}' in reg_list:
                    steps.append('q_chunk')

                strategies.append({
                    "Name": full_name,
                    "Description": desc.strip().replace('.', ''),
                    "Cognitive_Steps": steps,
                    "Examples": []
                })
            continue

        if "Subtraction COBO / CBBO" in name:
            strategies.append({
                "Name": "Subtraction COBO (Missing Addend)",
                "Description": "Start at S, perform base jumps toward M (without overshoot), then ones; distance accumulated is D.",
                "Cognitive_Steps": ["q_init", "q_add_bases", "q_add_ones"],
                "Examples": []
            })
            strategies.append({
                "Name": "Subtraction CBBO (Counting Back)",
                "Description": "Start at M, subtract base units (from decomposed S) then ones; final position is D.",
                "Cognitive_Steps": ["q_init", "q_sub_bases", "q_sub_ones"],
                "Examples": []
            })
            continue

        # If we have a valid strategy, add it to the list
        if cognitive_steps:
             strategies.append({
                "Name": name,
                "Description": description,
                "Cognitive_Steps": cognitive_steps,
                "Examples": []
            })

    return strategies

if __name__ == "__main__":
    strategies = parse_latex('HC_GEM.tex')
    # Manually add the last few strategies that are not easily parsable
    strategies.append({
        "Name": "Commutative Reasoning (Multiplication Optimization)",
        "Description": "For A x B, evaluate heuristic difficulty of (A,B) vs (B,A); select orientation minimizing cognitive load, then perform iterative addition.",
        "Cognitive_Steps": ["q_evaluate", "q_repackage", "q_calc"],
        "Examples": []
    })
    strategies.append({
        "Name": "Coordinating Two Counts (C2C)",
        "Description": "Foundational multiplication: nested counting---items within group, groups within total.",
        "Cognitive_Steps": ["q_init", "q_checkG", "q_countItems", "q_nextGroup"],
        "Examples": []
    })
    strategies.append({
        "Name": "Conversion to Bases and Ones (CBO Multiplication)",
        "Description": "Redistribute units among groups so that many groups become exact base multiples, leaving a compact residual.",
        "Cognitive_Steps": ["q_init", "q_select_source", "q_transfer", "q_finalize"],
        "Examples": []
    })
    strategies.append({
        "Name": "Distributive Reasoning (Multiplication)",
        "Description": "Decompose S = S1 + S2, compute N*S1 and N*S2, then sum.",
        "Cognitive_Steps": ["q_split", "q_P1", "q_P2", "q_sum"],
        "Examples": []
    })
    strategies.append({
        "Name": "Dealing by Ones (Division -- Sharing)",
        "Description": "Partitive division: distribute single units round-robin into N groups until total T exhausted.",
        "Cognitive_Steps": ["q_init", "q_deal"],
        "Examples": []
    })
    strategies.append({
        "Name": "Inverse Distributive Reasoning (Division)",
        "Description": "Measurement division T / S: decompose T into known multiples of S.",
        "Cognitive_Steps": ["q_init", "q_search", "q_apply"],
        "Examples": []
    })
    strategies.append({
        "Name": "Using Commutative Reasoning (Division via Iterated Accumulation)",
        "Description": "For E / G: iteratively accumulate G until total E reached; iteration count is quotient.",
        "Cognitive_Steps": ["q_init", "q_iterate", "q_check"],
        "Examples": []
    })
    strategies.append({
        "Name": "Conversion to Groups Other than Bases (CGOB Division)",
        "Description": "Leverage base decomposition of dividend T and analysis of base/divisor relation.",
        "Cognitive_Steps": ["q_init", "q_analyze", "q_processBases", "q_combineR", "q_processR"],
        "Examples": []
    })


    with open('strategies.json', 'w') as f:
        json.dump(strategies, f, indent=4)
    print(f"Successfully parsed {len(strategies)} strategies and saved to strategies.json")
