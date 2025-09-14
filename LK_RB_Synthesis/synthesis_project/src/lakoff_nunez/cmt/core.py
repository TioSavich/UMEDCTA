class ImageSchema:
    def __init__(self, name, description=""):
        self.name = name
        self.description = description

class ConceptualDomain:
    def __init__(self, name, description=""):
        self.name = name
        self.description = description

class ConceptualMetaphor:
    def __init__(self, name, source_domain: ConceptualDomain, target_domain: ConceptualDomain, mappings: dict, image_schema: ImageSchema = None):
        self.name = name
        self.source_domain = source_domain
        self.target_domain = target_domain
        self.mappings = mappings # Key-value pairs showing correspondence
        self.image_schema = image_schema
