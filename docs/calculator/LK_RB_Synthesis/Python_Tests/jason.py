import fractions
from typing import List, Tuple, Dict, Any

# =============================================================================
# I. Cognitive Material Representation (ContinuousUnit)
# =============================================================================

class ContinuousUnit:
    """
    Represents a continuous quantity (a 'stick') in Jason's cognition.
    It tracks not just the numerical value but the operational history
    that constitutes its meaning (U = (Q, H)).
    """
    def __init__(self, value: fractions.Fraction, history: str = "Reference Unit"):
        self.value = value
        self.history = history

    def __repr__(self):
        # Display the numerical value and its constructed derivation
        return f"Unit({self.value} derived from: '{self.history}')"

# =============================================================================
# II. Iterative Core: Explicitly Nested Number Sequence (ENS) Operations
# =============================================================================

class ENSOperations:
    """
    The fundamental, iterative core operations derived from Jason's ENS.
    """
    @staticmethod
    def partition(unit: ContinuousUnit, n: int) -> List[ContinuousUnit]:
        """
        [CORE::Partitioning]
        Divides a continuous unit into N equal parts.
        Cognitively: Applying the structure of the number sequence (1 to N)
        as a template onto the continuous material.
        Returns a list representing the structured whole (the collection of parts).
        """
        if n <= 0:
            raise ValueError("Partition must be a positive integer.")
        
        new_value = unit.value / n
        # The history of the part reflects its origin from the parent unit
        new_history = f"1/{n} part of ({unit.history})"
        
        return [ContinuousUnit(new_value, new_history) for _ in range(n)]

    @staticmethod
    def disembed(partitioned_whole: List[ContinuousUnit]) -> ContinuousUnit:
        """
        [CORE::Disembedding]
        Isolates a single unit part from the partitioned whole.
        Cognitively: Mentally isolating the unit fraction (1/N).
        """
        if not partitioned_whole:
            raise ValueError("Cannot disembed from an empty partition.")
        # In equi-partitioning, any part suffices.
        return partitioned_whole[0]

    @staticmethod
    def iterate(unit: ContinuousUnit, m: int) -> ContinuousUnit:
        """
        [CORE::Iterating]
        Repeats a unit M times.
        Cognitively: Treating the unit fraction as a countable composite unit
        and repeating it (Temporal Compression).
        """
        new_value = unit.value * m
        # The history reflects the iteration of the source part
        new_history = f"{m} iterations of [{unit.history}]"
        
        return ContinuousUnit(new_value, new_history)

# =============================================================================
# III. Strategic Shell: The Partitive Fractional Scheme (PFS)
# =============================================================================

class PartitiveFractionalScheme:
    """
    [SHELL::PFS]
    An automaton model of Jason's primary scheme for constructing proper fractions.
    It organizes the core ENS operations into a goal-directed sequence.
    M = (Q, V, delta, q0, F)
    """
    def __init__(self):
        self.Q = {'q_start', 'q_partition', 'q_disembed', 'q_iterate', 'q_accept'}
        self.F = {'q_accept'}
        self.V = {} # Variables/Registers
        self.trace = []

    def initialize(self, whole: ContinuousUnit, numerator: int, denominator: int):
        self.V = {
            'Whole': whole,
            'N': denominator,
            'M': numerator,
            'PartitionedWhole': None,
            'UnitFraction': None,
            'Result': None
        }
        self.current_state = 'q_start'
        self.trace = []
        self.log_state(f"PFS Initialized: Find {numerator}/{denominator} of {whole.value}")

    def log_state(self, action_description: str):
        # Logs the state transition and action for visualizing the choreography
        self.trace.append({
            'State': self.current_state,
            'Action': action_description,
        })

    def transition(self):
        """The transition function (delta) implemented as a state machine."""
        
        if self.current_state == 'q_start':
            self.current_state = 'q_partition'

        elif self.current_state == 'q_partition':
            self.log_state(f"[State: q_partition] Action: Partitioning Whole into {self.V['N']} parts.")
            # Action: Partition(Whole, N)
            self.V['PartitionedWhole'] = ENSOperations.partition(
                self.V['Whole'], self.V['N']
            )
            self.current_state = 'q_disembed'

        elif self.current_state == 'q_disembed':
            # Action: Disembed(PartitionedWhole)
            self.V['UnitFraction'] = ENSOperations.disembed(
                self.V['PartitionedWhole']
            )
            self.log_state(f"[State: q_disembed] Action: Disembedded Unit Fraction ({self.V['UnitFraction'].value}).")
            self.current_state = 'q_iterate'

        elif self.current_state == 'q_iterate':
            self.log_state(f"[State: q_iterate] Action: Iterating Unit Fraction {self.V['M']} times.")
            # Action: Iterate(UnitFraction, M)
            self.V['Result'] = ENSOperations.iterate(
                self.V['UnitFraction'], self.V['M']
            )
            self.current_state = 'q_accept'
        
    def run(self, whole: ContinuousUnit, numerator: int, denominator: int) -> ContinuousUnit:
        self.initialize(whole, numerator, denominator)
        while self.current_state not in self.F:
            self.transition()
        self.log_state("PFS Complete.")
        return self.V['Result']

# =============================================================================
# IV. Strategic Shell: The Fractional Composition Scheme (FCS)
# =============================================================================

class FractionalCompositionScheme:
    """
    [SHELL::FCS]
    Models the scheme developed after the "metamorphic accommodation" of recursive partitioning.
    This automaton demonstrates Fractal Architecture (FCS invokes PFS).
    It handles tasks like "A/B of C/D of a Whole".
    """
    def __init__(self):
        self.Q = {'q_start', 'q_inner_PFS', 'q_accommodate', 'q_outer_PFS', 'q_accept'}
        self.F = {'q_accept'}
        self.V = {}
        # The FCS contains the PFS as a subroutine (Fractal Elaboration)
        self.PFS = PartitiveFractionalScheme() 
        self.trace = []

    def initialize(self, whole: ContinuousUnit, outer_frac: Tuple[int, int], inner_frac: Tuple[int, int]):
        A, B = outer_frac
        C, D = inner_frac
        self.V = {
            'Whole': whole,
            'A': A, 'B': B, 'C': C, 'D': D,
            'IntermediateResult': None,
            'FinalResult': None
        }
        self.current_state = 'q_start'
        self.trace = []
        self.log_state(f"FCS Initialized: Find {A}/{B} of {C}/{D} of {whole.value}")

    def log_state(self, action_description: str, nested_trace=None):
        entry = {'State': self.current_state, 'Action': action_description}
        if nested_trace:
            entry['NestedTrace'] = nested_trace
        self.trace.append(entry)

    def transition(self):
        if self.current_state == 'q_start':
            self.current_state = 'q_inner_PFS'

        elif self.current_state == 'q_inner_PFS':
            self.log_state(f"[State: q_inner_PFS] Action: Calculating inner fraction ({self.V['C']}/{self.V['D']}).")
            # Action: Invoke PFS(Whole, C/D)
            self.V['IntermediateResult'] = self.PFS.run(
                self.V['Whole'], self.V['C'], self.V['D']
            )
            self.log_state(f"-> Intermediate Result: {self.V['IntermediateResult'].value}", self.PFS.trace)
            self.current_state = 'q_accommodate'

        elif self.current_state == 'q_accommodate':
            # CRITICAL STEP: Metamorphic Accommodation / Recursive Partitioning
            # The output of the previous operation (IntermediateResult) is re-assimilated 
            # as the input Whole for the next operation.
            self.log_state("[State: q_accommodate] METAMORPHIC ACCOMMODATION: Using IntermediateResult as new Whole.")
            self.V['NewWhole'] = self.V['IntermediateResult']
            self.current_state = 'q_outer_PFS'

        elif self.current_state == 'q_outer_PFS':
            self.log_state(f"[State: q_outer_PFS] Action: Calculating outer fraction ({self.V['A']}/{self.V['B']}) on new Whole.")
            # Action: Invoke PFS(NewWhole, A/B)
            self.V['FinalResult'] = self.PFS.run(
                self.V['NewWhole'], self.V['A'], self.V['B']
            )
            self.log_state(f"-> Final Result: {self.V['FinalResult'].value}", self.PFS.trace)
            self.current_state = 'q_accept'

    def run(self, whole: ContinuousUnit, outer_frac: Tuple[int, int], inner_frac: Tuple[int, int]) -> ContinuousUnit:
        self.initialize(whole, outer_frac, inner_frac)
        while self.current_state not in self.F:
            self.transition()
        self.log_state("FCS Complete.")
        return self.V['FinalResult']

# =============================================================================
# V. Demonstration and Testing
# =============================================================================

def print_trace(trace, indent=""):
    """Helper function to print the execution trace (choreography)."""
    for step in trace:
        print(f"{indent}State: {step['State']}, Action: {step['Action']}")
        if 'NestedTrace' in step:
            print(f"{indent}  [Begin Nested PFS Execution]")
            print_trace(step['NestedTrace'], indent + "    ")
            print(f"{indent}  [End Nested PFS Execution]")

def run_tests():
    print("=== JASON AUTOMATON MODEL TESTING ===")
    
    # Define the initial Whole (the reference 'stick')
    TheWhole = ContinuousUnit(fractions.Fraction(1, 1))

    # --- Test 1: Partitive Fractional Scheme (PFS) ---
    # Task: Construct 3/7 of the stick.
    print("\n" + "="*60)
    print("TEST 1: Construct 3/7 of the Whole (PFS)")
    print("="*60)
    PFS_Automaton = PartitiveFractionalScheme()
    result_pfs = PFS_Automaton.run(TheWhole, 3, 7)
    
    print("\nExecution Trace (Cognitive Choreography):")
    print_trace(PFS_Automaton.trace)

    print(f"\nRESULT (PFS): {result_pfs}")

    # --- Test 2: Fractional Composition Scheme (FCS) & Recursive Partitioning ---
    # Task: The pivotal novelty event: Find 3/4 of 1/4 of the stick.
    print("\n" + "="*60)
    print("TEST 2: Construct 3/4 of 1/4 of the Whole (FCS)")
    print("Modeling Metamorphic Accommodation (Recursive Partitioning)")
    print("="*60)
    FCS_Automaton = FractionalCompositionScheme()
    # Outer fraction: 3/4, Inner fraction: 1/4
    result_fcs = FCS_Automaton.run(TheWhole, (3, 4), (1, 4))

    print("\nExecution Trace (Cognitive Choreography):")
    print_trace(FCS_Automaton.trace)

    print(f"\nRESULT (FCS): {result_fcs}")

if __name__ == "__main__":
    # This allows the code to be executed via the tool interface.
    run_tests()