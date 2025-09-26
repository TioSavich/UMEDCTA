"""
parse_metaphor_kb.py: Parses Metaphor_Knowledge_Base.md to generate cmt_data.json programmatically.
"""

import json
import re
import os

def parse_metaphor_kb(file_path):
    """
    Parses the Metaphor_Knowledge_Base.md file and extracts conceptual metaphors.
    """
    with open(file_path, 'r') as f:
        content = f.read()

    # Initialize data structure
    data = {
        "image_schemas": [
            {
                "name": "Container",
                "description": "In-out orientation, arising from the experience of putting objects in and taking them out."
            },
            {
                "name": "Part-Whole",
                "description": "Understanding an object as a whole composed of smaller constituent parts."
            },
            {
                "name": "Path",
                "description": "A schema involving a source, a destination, and a sequence of points in between."
            }
        ],
        "conceptual_metaphors": []
    }

    # Split content into sections for each metaphor
    sections = re.split(r'## \d+\. The (.+?) Metaphor', content)[1:]  # Skip the first empty element

    # Process each metaphor section (name, content) pairs
    for i in range(0, len(sections), 2):
        metaphor_name = sections[i].strip()
        section_content = sections[i + 1]

        # Extract source and target domains
        source_match = re.search(r'\*\s+\*\*Source Domain:\*\*\s*(.*?)\*\s+', section_content, re.DOTALL)
        target_match = re.search(r'\*\s+\*\*Target Domain:\*\*\s*(.*?)\*\s+', section_content, re.DOTALL)

        if not source_match or not target_match:
            continue

        source_domain = source_match.group(1).strip()
        target_domain = target_match.group(1).strip()

        # Extract key mappings
        mappings_match = re.search(r'\*\s+\*\*Key Mappings & Entailments:\*\*(.*?)\*\s+\*\*Mapped Strategies:\*\*', section_content, re.DOTALL)
        mappings_text = mappings_match.group(1) if mappings_match else ""

        # Simple mapping extraction - this could be improved
        mappings = {}
        mapping_lines = re.findall(r'\*\s+\*\*(.*?):\*\*\s*(.*?)(?=\n\*\s+\*\*|\n\*\s+\*\*|$)', mappings_text, re.DOTALL)
        for key, value in mapping_lines:
            # Extract the first sentence as a simple mapping
            first_sentence = value.split('.')[0].strip()
            if ':' in first_sentence:
                source, target = first_sentence.split(':', 1)
                mappings[source.strip()] = target.strip()
            else:
                mappings[key.strip()] = first_sentence

        # Determine image schema based on metaphor name
        if "Object Collection" in metaphor_name:
            image_schema = "Container"
        elif "Object Construction" in metaphor_name:
            image_schema = "Part-Whole"
        elif "Motion Along a Path" in metaphor_name:
            image_schema = "Path"
        else:
            image_schema = "Container"  # Default

        metaphor_data = {
            "name": metaphor_name,
            "source_domain": source_domain,
            "target_domain": target_domain,
            "image_schema": image_schema,
            "mappings": mappings
        }

        data["conceptual_metaphors"].append(metaphor_data)

    return data

def main():
    """
    Main function to parse the metaphor knowledge base and save to JSON.
    """
    script_dir = os.path.dirname(os.path.abspath(__file__))
    project_root = os.path.dirname(script_dir)

    kb_file = os.path.join(project_root, 'Metaphor_Knowledge_Base.md')
    output_file = os.path.join(project_root, 'eple/assets/data/processed/cmt_data.json')

    if not os.path.exists(kb_file):
        print(f"Error: Metaphor knowledge base file not found at '{kb_file}'.")
        return

    # Parse the knowledge base
    data = parse_metaphor_kb(kb_file)

    # Save to JSON
    with open(output_file, 'w') as f:
        json.dump(data, f, indent=2)

    print(f"Successfully generated conceptual metaphor data at: {output_file}")
    print(f"Parsed {len(data['conceptual_metaphors'])} conceptual metaphors.")

if __name__ == '__main__':
    main()