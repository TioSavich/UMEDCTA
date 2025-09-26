"""
deontic_scorekeeper.py: Implements the core reasoning engine of the eple project.

The DeonticScorekeeper manages a set of practices and Meaning-Use Relations (MURs),
and can calculate the effective rules (inferences and incompatibilities) for any
practice, including those projected metaphorically.
"""

import copy
from eple.core.mua import PragmaticProjection
from eple.core.logic_terms import Atom, Incompatibility

class DeonticScorekeeper:
    """
    Manages practices and MURs to provide a complete view of the rules
    governing a conceptual domain.
    """
    def __init__(self, practices, murs):
        """
        Initializes the scorekeeper with a universe of practices and MURs.

        Args:
            practices (set): A set of all Practice objects.
            murs (set): A set of all MeaningUseRelation objects.
        """
        self.practices = {p.name: p for p in practices}
        self.murs = murs
        self._processed_practices = {}
        self._apply_projections()

    def _apply_projections(self):
        """
        Calculates the full set of incompatibilities for each practice,
        including those inherited via PragmaticProjection (metaphor).
        """
        # Start with the base rules for each practice
        for name, p in self.practices.items():
            self._processed_practices[name] = copy.deepcopy(p)

        # Augment with rules from metaphorical projections
        for mur in self.murs:
            if isinstance(mur, PragmaticProjection):
                source_practice = mur.P_base
                target_practice = mur.P_elaborated
                mappings = mur.mappings

                if not source_practice or not target_practice:
                    continue

                # Get the processed versions of the practices
                processed_target = self._processed_practices.get(target_practice.name)
                if not processed_target:
                    continue

                # Project incompatibilities
                projected_incompatibilities = self._project_rules(
                    source_practice.incompatibilities, mappings
                )
                processed_target.incompatibilities.update(projected_incompatibilities)

    def _project_rules(self, rules, mappings):
        """
        Translates a set of rules (incompatibilities) from a source domain
        to a target domain using predicate mappings.
        """
        projected_rules = set()
        for rule in rules:
            # Create a deep copy to avoid altering the original rule
            projected_rule = copy.deepcopy(rule)
            
            # The rule itself is an Incompatibility term. We need to traverse its arguments.
            if isinstance(projected_rule, Incompatibility) and hasattr(projected_rule, 'args'):
                for atom in projected_rule.args:
                    if isinstance(atom, Atom):
                        predicate_name = atom.predicate.name
                        if predicate_name in mappings:
                            # Replace the predicate's name with the target name
                            atom.predicate.name = mappings[predicate_name]
            
            projected_rules.add(projected_rule)
        return projected_rules

    def get_incompatibilities(self, practice_name):
        """
        Returns the full set of incompatibilities for a given practice,
        including any metaphorically projected ones.

        Args:
            practice_name (str): The name of the practice.

        Returns:
            set: A set of Incompatibility objects.
        """
        practice = self._processed_practices.get(practice_name)
        return practice.incompatibilities if practice else set()

    def get_practice(self, practice_name):
        """
        Returns the processed practice object.

        Args:
            practice_name (str): The name of the practice.

        Returns:
            Practice: The processed practice object, or None.
        """
        return self._processed_practices.get(practice_name)
