# -*- coding: utf-8 -*-
"""
This file contains the core logic engine based on incompatibility semantics.
It is responsible for tracking commitments and entitlements, and for proving
sequents based on a set of material inferences.
"""

import math
from fractions import Fraction
from itertools import product
from copy import deepcopy

from .logic_terms import Term, Var, Atom, Predicate, Sequent, s, o, n, neg, comp_nec, exp_nec, exp_poss, comp_poss, conj, Commits, Entitled

# =================================================================
# Part 1: Deontic Scorekeeper
# =================================================================

class DeonticScorekeeper:
    """
    Tracks the commitments and entitlements of various agents based on a set of
    material inference rules provided by a Practice.
    """

    def __init__(self, practice=None):
        """
        Initializes the scorekeeper.
        Args:
            practice: An object that defines the rules of material inference
                      (incompatibilities and entailments).
        """
        self.practice = practice
        # scores[agent] = {'commitments': {propositions}, 'entitlements': {propositions}}
        self.scores = {}

    def add_agent(self, agent_name):
        """Adds a new agent to the scorekeeping."""
        if agent_name not in self.scores:
            self.scores[agent_name] = {
                'commitments': set(),
                'entitlements': set()
            }
            print(f"Agent '{agent_name}' added.")

    def add_commitment(self, agent_name, proposition, initial_entitlement=False):
        """
        Adds a proposition to an agent's commitment set and calculates the consequences.
        If initial_entitlement is True, the agent is also granted entitlement.
        """
        if agent_name not in self.scores:
            self.add_agent(agent_name)
        
        if proposition not in self.scores[agent_name]['commitments']:
            self.scores[agent_name]['commitments'].add(proposition)
            print(f"Agent '{agent_name}' undertakes commitment: {proposition}")
            if initial_entitlement:
                self.add_entitlement(agent_name, proposition)
            self.update_commitments(agent_name)
            self.update_entitlements(agent_name)

    def add_entitlement(self, agent_name, proposition):
        """
        Adds a proposition to an agent's entitlement set.
        """
        if agent_name not in self.scores:
            self.add_agent(agent_name)

        if proposition not in self.scores[agent_name]['entitlements']:
            self.scores[agent_name]['entitlements'].add(proposition)
            print(f"Agent '{agent_name}' gains entitlement: {proposition}")
            self.update_entitlements(agent_name)

    def update_commitments(self, agent_name):
        """
        Recursively applies material inferences to an agent's commitment set
        until no new commitments are generated.
        """
        if not self.practice:
            return

        agent_scores = self.scores[agent_name]
        newly_added = True
        while newly_added:
            newly_added = False
            current_commitments = list(agent_scores['commitments'])
            
            # Apply entailment rules from the practice
            for p in current_commitments:
                consequences = self.practice.get_consequences(p)
                for c in consequences:
                    if c not in agent_scores['commitments']:
                        agent_scores['commitments'].add(c)
                        newly_added = True
                        print(f"  -> Consequence for '{agent_name}': {c}")

    def update_entitlements(self, agent_name):
        """
        Recursively applies material inferences to an agent's entitlement set.
        If an agent is entitled to the premises of an inference, they become
        entitled to the conclusion.
        """
        if not self.practice:
            return

        agent_scores = self.scores[agent_name]
        newly_added = True
        while newly_added:
            newly_added = False

            # Get all inferences from the practice
            inferences = self.practice.get_all_inferences()
            for inference in inferences:
                # Check if the agent is entitled to all premises
                is_entitled_to_all_premises = all(
                    premise in agent_scores['entitlements'] for premise in inference.antecedent
                )

                if is_entitled_to_all_premises:
                    for conclusion in inference.consequent:
                        if conclusion not in agent_scores['entitlements']:
                            agent_scores['entitlements'].add(conclusion)
                            newly_added = True
                            print(f"  -> Entitlement propagated for '{agent_name}': {conclusion}")

    def is_incoherent(self, agent_name):
        """
        Checks if an agent's commitment set is incoherent.
        Incoherence can arise from:
        1. Committing to P and not(P) (structural).
        2. Committing to a set of materially incompatible propositions.
        3. Committing to a proposition one is not entitled to.
        """
        if agent_name not in self.scores:
            return False
            
        commitments = self.scores[agent_name]['commitments']
        entitlements = self.scores[agent_name]['entitlements']

        # 3. Check for commitments without entitlement
        for p in commitments:
            if p not in entitlements:
                print(f"Incoherence found for '{agent_name}': Commitment to {p} without entitlement.")
                return True
        
        # 1. Check for structural incoherence (P and not P)
        for p in commitments:
            if neg(p) in commitments:
                print(f"Incoherence found for '{agent_name}': {p} and {neg(p)}")
                return True
        
        # 2. Check for material incoherence from the practice
        if self.practice:
            return self.practice.is_incoherent(commitments)

        return False

    def get_commitments(self, agent_name):
        return self.scores.get(agent_name, {}).get('commitments', set())

    def get_entitlements(self, agent_name):
        return self.scores.get(agent_name, {}).get('entitlements', set())

