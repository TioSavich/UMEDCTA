from bs4 import BeautifulSoup
import os
import pandas as pd

def parse_strategies(raw_dir, output_path):
    # This mapping is derived from Metaphor_Knowledge_Base.md
    # It maps keywords from strategy names/descriptions to a conceptual metaphor.
    # This is a simplified approach for demonstration.
    metaphor_mapping = {
        "counting on": "Arithmetic is Motion Along a Path",
        "cobo": "Arithmetic is Motion Along a Path",
        "rounding": "Arithmetic is Motion Along a Path",
        "sliding": "Arithmetic is Motion Along a Path",
        "chunking": "Arithmetic is Object Construction",
        "decomposition": "Arithmetic is Object Construction",
        "distributive": "Arithmetic is Object Construction",
        "commutative": "Arithmetic is Object Collection",
        "coordinating two counts": "Arithmetic is Object Collection",
        "dealing by ones": "Arithmetic is Object Collection",
        "strategic counting": "Arithmetic is Object Collection",
        "c2c": "Arithmetic is Object Collection",
        "rmb": "Arithmetic is Object Construction",
        "cgob": "Arithmetic is Object Construction",
        "idp": "Arithmetic is Object Construction",
        "abao": "Arithmetic is Object Collection",
    }

    strategies = []
    for filename in os.listdir(raw_dir):
        if filename.endswith(".html"):
            filepath = os.path.join(raw_dir, filename)
            with open(filepath, 'r') as f:
                soup = BeautifulSoup(f.read(), 'lxml')
                
                name_tag = soup.find('h1')
                name = name_tag.get_text(strip=True) if name_tag else "Unknown Strategy"
                
                description_tag = soup.find('p')
                description = description_tag.get_text(strip=True) if description_tag else ""

                # Find the best metaphor
                annotation = "Arithmetic is Object Collection" # Default
                for keyword, metaphor in metaphor_mapping.items():
                    if keyword in name.lower() or keyword in description.lower():
                        annotation = metaphor
                        break

                strategies.append({
                    "id": os.path.splitext(filename)[0],
                    "name": name,
                    "description": description,
                    "metaphor_annotation": annotation
                })

    df = pd.DataFrame(strategies)
    df.to_csv(output_path, index=False)
    print(f"Parsed {len(strategies)} strategies and assigned metaphors. Saved to {output_path}")
