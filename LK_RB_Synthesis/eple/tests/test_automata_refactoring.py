import unittest
import sys
import os

# Add project root to the Python path to allow for absolute imports
project_root = os.path.abspath(os.path.join(os.path.dirname(__file__), '..', '..'))
sys.path.insert(0, project_root)

from src.automata.multiplication.SMR_MULT_Commutative_Reasoning import CommutativeReasoningMultiplicationAutomaton

class TestAutomataRefactoring(unittest.TestCase):

    def test_commutative_reasoning_automaton(self):
        """
        Tests that the refactored CommutativeReasoningMultiplicationAutomaton
        can be instantiated and run correctly.
        """
        inputs = {'A': 9, 'B': 3}
        automaton = CommutativeReasoningMultiplicationAutomaton(inputs)
        result = automaton.run()
        self.assertEqual(result, 27)

if __name__ == '__main__':
    unittest.main()
