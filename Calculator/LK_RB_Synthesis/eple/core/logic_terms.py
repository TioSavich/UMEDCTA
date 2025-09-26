# -*- coding: utf-8 -*-
"""
This file contains the core data structures for representing logical terms,
mirroring the structure of Prolog terms for the incompatibility semantics engine.
"""

class Term:
    """Base class for all logical terms."""
    def __init__(self, name, args=None):
        self.name = name
        self.args = args if args is not None else []

    def __eq__(self, other):
        return isinstance(other, self.__class__) and self.name == other.name and self.args == other.args
    def __hash__(self):
        return hash((self.__class__.__name__, self.name, tuple(self.args)))
    def __repr__(self):
        if not self.args:
            return str(self.name)
        return f"{self.name}({', '.join(map(repr, self.args))})"

class Var(Term):
    """Represents a variable in a logical expression."""
    def __init__(self, name):
        super().__init__(name)
    def __hash__(self):
        return hash((self.__class__.__name__, self.name))

class Atom(Term):
    """Represents an atomic value or constant."""
    def __init__(self, name):
        super().__init__(name)


class Predicate(Term):
    """Represents a predicate with a name and arity, or a predicate instance with arguments."""
    def __init__(self, name, arity_or_args=None):
        self.name = name
        if isinstance(arity_or_args, int):
            self.arity = arity_or_args
            self.args = []
        elif isinstance(arity_or_args, list):
            self.arity = len(arity_or_args)
            self.args = arity_or_args
        elif arity_or_args is None:
            self.arity = 0
            self.args = []
        else:
            self.args = list(arity_or_args)
            self.arity = len(self.args)

    def __call__(self, *args):
        return Predicate(self.name, list(args))

    def __hash__(self):
        if not self.args:
            return hash((self.__class__.__name__, self.name, self.arity))
        return hash((self.__class__.__name__, self.name, tuple(self.args)))

    def __eq__(self, other):
        if not isinstance(other, Predicate):
            return False
        if not self.args and not other.args:
            return self.name == other.name and self.arity == other.arity
        return self.name == other.name and self.args == other.args
    
    def __repr__(self):
        if not self.args and hasattr(self, 'arity'):
            return f"{self.name}/{self.arity}"
        return super().__repr__()

class Incompatibility(Term):
    """Represents an incompatibility between two predicates."""
    def __init__(self, p1, p2):
        self.p1 = p1
        self.p2 = p2
        self.args = [p1, p2]
        self.name = "incompatible"

    def __repr__(self):
        return f"¬({self.p1} & {self.p2})"

    @staticmethod
    def neg(p):
        """Syntactic sugar for incompatibility with itself, representing negation."""
        return Incompatibility(p, p)

class Inference(Term):
    """Represents an inference rule (antecedent -> consequent)."""
    def __init__(self, antecedent, consequent):
        self.antecedent = antecedent if isinstance(antecedent, list) else [antecedent]
        # Ensure consequent is also always a list for consistency
        self.consequent = consequent if isinstance(consequent, list) else [consequent]
        self.name = "inference"
        self.args = [self.antecedent, self.consequent]

    def __repr__(self):
        antecedent_repr = ', '.join(map(repr, self.antecedent))
        consequent_repr = ', '.join(map(repr, self.consequent))
        return f"[{antecedent_repr}] -> [{consequent_repr}]"

    def __hash__(self):
        # Hash based on the logical components, ensuring both lists are tuples
        return hash((self.__class__.__name__, tuple(self.antecedent), tuple(self.consequent)))

    def __eq__(self, other):
        if not isinstance(other, Inference):
            return False
        return self.antecedent == other.antecedent and self.consequent == other.consequent

class Sequent:
    """Represents a sequent P => C (Premises => Conclusions)."""
    def __init__(self, premises, conclusions):
        self.premises = premises
        self.conclusions = conclusions
    def __repr__(self):
        return f"{self.premises} => {self.conclusions}"

# Define common predicates and connectives for convenience
s = Predicate('s')
o = Predicate('o')
n = Predicate('n')
neg = Predicate('neg')
comp_nec = Predicate('comp_nec')
exp_nec = Predicate('exp_nec')
exp_poss = Predicate('exp_poss')
comp_poss = Predicate('comp_poss')
conj = Predicate('conj')

# Deontic Scorekeeping Predicates
Commits = Predicate('Commits')
Entitled = Predicate('Entitled')
