"""
base.py: Defines the foundational (non-metaphorical) vocabulary and practices
for the domain of arithmetic.
"""

import sys
import os

# Add project root to path
sys.path.insert(0, os.path.abspath(os.path.join(os.path.dirname(__file__), '../../..')))

from eple.core.logic_terms import Predicate, Var
from eple.core.mua import Vocabulary, Practice

# --- Define Vocabulary ---

# Variables for term instantiation
x = Var('x')
y = Var('y')
z = Var('z')

# Define core arithmetic predicates
Add = Predicate('Add', 3)
Subtract = Predicate('Subtract', 3)
LessThan = Predicate('LessThan', 2)
IsNumber = Predicate('IsNumber', 1)

# Vocabulary for basic arithmetic
V_Arithmetic = Vocabulary(
    name="V_Arithmetic",
    predicates={Add, Subtract, LessThan, IsNumber}
)

# --- Define Practice ---

# For now, the base practice is just the vocabulary.
# In a fuller implementation, this would contain basic arithmetic rules,
# like the definitions of commutativity or associativity.
P_Arithmetic = Practice(
    name="P_Arithmetic",
    vocabulary=V_Arithmetic,
    inferences=set(),
    incompatibilities=set()
)

if __name__ == '__main__':
    print("--- Defining the Base Arithmetic Domain ---")
    print("\nVocabulary: V_Arithmetic")
    print(V_Arithmetic)
    print("\nPractice: P_Arithmetic")
    print(P_Arithmetic)
