import unittest
import sys
import os

# Add the project root to the Python path to allow for absolute imports
project_root = os.path.abspath(os.path.join(os.path.dirname(__file__), '..', '..'))
sys.path.insert(0, project_root)

from eple.core.incompatibility_engine import DeonticScorekeeper
from eple.core.logic_terms import Predicate, Inference, Incompatibility

# Define some predicates for testing
P = Predicate('P')
Q = Predicate('Q')
R = Predicate('R')

class TestPractice:
    """
    A simple practice for testing the DeonticScorekeeper.
    It defines a simple set of rules:
    - P -> Q
    - Q -> R
    - P is incompatible with R
    """
    def __init__(self):
        self.inferences = [
            Inference(P(), Q()),
            Inference(Q(), R())
        ]
        self.incompatibilities = [
            Incompatibility(P(), R())
        ]

    def get_consequences(self, proposition):
        consequences = []
        for inference in self.inferences:
            if proposition in inference.antecedent:
                consequences.extend(inference.consequent)
        return consequences

    def get_all_inferences(self):
        return self.inferences

    def is_incoherent(self, propositions):
        for incomp in self.incompatibilities:
            if incomp.p1 in propositions and incomp.p2 in propositions:
                return True
        return False

class TestDeonticScorekeeper(unittest.TestCase):

    def setUp(self):
        self.practice = TestPractice()
        self.ds = DeonticScorekeeper(self.practice)
        self.ds.add_agent('Alice')

    def test_commitment_propagation(self):
        """Test that commitments propagate through the inference chain."""
        self.ds.add_commitment('Alice', P(), initial_entitlement=True)
        commitments = self.ds.get_commitments('Alice')
        self.assertIn(P(), commitments)
        self.assertIn(Q(), commitments)
        self.assertIn(R(), commitments)

    def test_entitlement_propagation(self):
        """Test that entitlements propagate through the inference chain."""
        self.ds.add_entitlement('Alice', P())
        entitlements = self.ds.get_entitlements('Alice')
        self.assertIn(P(), entitlements)
        self.assertIn(Q(), entitlements)
        self.assertIn(R(), entitlements)

    def test_incoherence_from_commitment_without_entitlement(self):
        """Test that committing to something without entitlement is incoherent."""
        # Alice commits to P, but is not entitled to it.
        self.ds.add_commitment('Alice', P())
        self.assertTrue(self.ds.is_incoherent('Alice'), "Should be incoherent if committed without entitlement")

    def test_incoherence_from_material_incompatibility(self):
        """
        Test that a commitment set is incoherent if it contains materially
        incompatible propositions, even if the agent is entitled to them.
        """
        # Alice is entitled to P, and thus to its consequences Q and R.
        self.ds.add_entitlement('Alice', P())

        # Alice commits to P. Her commitment set becomes {P, Q, R}.
        self.ds.add_commitment('Alice', P())

        # The practice states that P and R are incompatible. The scorekeeper
        # should detect this, even though she is entitled to all commitments.
        self.assertTrue(self.ds.is_incoherent('Alice'), "Should be incoherent due to material incompatibility (P and R)")

    def test_coherence_with_valid_commitment(self):
        """Test a fully coherent commitment."""
        # New practice where P -> Q, but no other rules.
        practice = TestPractice()
        practice.inferences = [Inference(P(), Q())]
        practice.incompatibilities = []
        ds = DeonticScorekeeper(practice)
        ds.add_agent('Bob')

        # Bob is entitled to P and commits to it.
        ds.add_entitlement('Bob', P())
        ds.add_commitment('Bob', P())

        # The final state should be coherent.
        # Commitments: {P, Q}, Entitlements: {P, Q}
        self.assertFalse(ds.is_incoherent('Bob'), "Should be coherent with a valid, entitled commitment")

if __name__ == '__main__':
    unittest.main()
