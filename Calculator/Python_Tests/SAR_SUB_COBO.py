import pandas as pd

class SubtractionIterativeAutomaton:
    """Base class for iterative subtraction strategies."""
    def __init__(self, M, S, Base=10):
        self.M = M # Minuend (Whole)
        self.S = S # Subtrahend (Known Part)
        self.BaseUnit = Base
        self.history = []
        self.state = 'q_start'
        self.Result = 0

        # Initialize registers for consistent history recording
        self.CurrentValue = 0
        
        if S > M:
            self.state = 'q_error'
            # Manually record error history as derived class registers may not be initialized yet
            self.history.append({'State': 'q_error', 'Interpretation': f"Error: Subtrahend ({S}) > Minuend ({M})."})

    def _record_history(self, interpretation, **kwargs):
        # Standardize history recording
        record = {'State': self.state, 'Interpretation': interpretation}
        # Include core registers if they exist in the specific strategy
        if hasattr(self, 'CurrentValue'):
            record['CV'] = self.CurrentValue
        if hasattr(self, 'Distance'):
            record['Dist'] = self.Distance
        if hasattr(self, 'BaseCounter'):
            record['BC'] = self.BaseCounter
        if hasattr(self, 'OneCounter'):
            record['OC'] = self.OneCounter
            
        record.update(kwargs) # Add any specific overrides
        self.history.append(record)

    def transition(self, next_state):
        self.state = next_state

    def run(self):
        while self.state not in ['q_accept', 'q_error']:
            executor = getattr(self, f"execute_{self.state}", self.execute_error)
            executor()
        return self.Result

    def execute_error(self):
        if self.state != 'q_error':
            self._record_history(f"Error: Entered unknown state {self.state}")
            self.transition('q_error')

    def execute_q_start(self):
        # Common start state
        self._record_history("Start.")
        self.transition('q_init')

    def display_history(self):
        print(f"\n--- Subtraction History ({self.M} - {self.S}) | Strategy: {self.strategy_name} ---")
        df = pd.DataFrame(self.history)
        if not df.empty:
             # Define desired column order and filter existing columns
            cols_order = ['State', 'Interpretation', 'CV', 'Dist', 'BC', 'OC']
            cols = [col for col in cols_order if col in df.columns]
            df = df[cols].fillna('')
        print(df.to_markdown(index=False))

# =============================================================================
# Strategy 1: COBO (Counting On - Missing Addend)
# =============================================================================

class COBO_MissingAddend(SubtractionIterativeAutomaton):
    """
    COBO (Counting On): Start at S, count up to M iteratively. Result is distance.
    Models Rita's strategy.
    """
    strategy_name = "COBO (Counting On - Missing Addend)"
    
    def __init__(self, M, S, Base=10):
        super().__init__(M, S, Base)
        self.Distance = 0
        self.Target = self.M

    def execute_q_init(self):
        self.CurrentValue = self.S
        self.Distance = 0
        self._record_history(f"Initialize at S ({self.S}). Target is M ({self.M}).")
        self.transition('q_add_bases')

    def execute_q_add_bases(self):
        """Iteratively add bases, checking for overshoot."""
        # Condition: Can add a base without overshooting M
        if self.CurrentValue + self.BaseUnit <= self.Target:
            self.CurrentValue += self.BaseUnit
            self.Distance += self.BaseUnit
            self._record_history(f"Count on by base (+{self.BaseUnit}). New Value={self.CurrentValue}.")
            # Stay in q_add_bases
        # Condition: Adding a base would overshoot
        else:
            self._record_history("Next base overshoots target. Switching to ones.")
            self.transition('q_add_ones')

    def execute_q_add_ones(self):
        """Iteratively add ones until M is reached."""
        # Condition: Not yet reached M
        if self.CurrentValue < self.Target:
            self.CurrentValue += 1
            self.Distance += 1
            self._record_history(f"Count on by one (+1). New Value={self.CurrentValue}.")
            # Stay in q_add_ones
        # Condition: Reached M
        else:
            self.Result = self.Distance
            self._record_history(f"Target reached. Result (Distance) = {self.Result}.")
            self.transition('q_accept')

# =============================================================================
# Strategy 2: CBBO (Counting Back - Take Away)
# =============================================================================

class CBBO_TakeAway(SubtractionIterativeAutomaton):
    """
    CBBO (Counting Back): Start at M, subtract S iteratively. Result is final position.
    Models the alternative strategy diagram.
    """
    strategy_name = "CBBO (Counting Back - Take Away)"
    
    def __init__(self, M, S, Base=10):
        super().__init__(M, S, Base)
        self.BaseCounter = 0
        self.OneCounter = 0

    def execute_q_init(self):
        self.CurrentValue = self.M
        # Decompose S into iterative counts
        self.BaseCounter = self.S // self.BaseUnit
        self.OneCounter = self.S % self.BaseUnit
        self._record_history(f"Initialize at M ({self.M}). Decompose S ({self.S}): {self.BaseCounter} bases, {self.OneCounter} ones.")
        self.transition('q_sub_bases')

    def execute_q_sub_bases(self):
        """Iteratively subtract bases."""
        if self.BaseCounter > 0:
            self.CurrentValue -= self.BaseUnit
            self.BaseCounter -= 1
            self._record_history(f"Count back by base (-{self.BaseUnit}). New Value={self.CurrentValue}.")
        else:
            self._record_history("Bases finished. Switching to ones.")
            self.transition('q_sub_ones')

    def execute_q_sub_ones(self):
        """Iteratively subtract ones."""
        if self.OneCounter > 0:
            self.CurrentValue -= 1
            self.OneCounter -= 1
            self._record_history(f"Count back by one (-1). New Value={self.CurrentValue}.")
        else:
            self.Result = self.CurrentValue
            self._record_history(f"Subtraction finished. Result (Final Position) = {self.Result}.")
            self.transition('q_accept')

# =============================================================================
# Testing (Example: 94 - 65)
# =============================================================================

M_test = 94
S_test = 65

# Test COBO (Rita's actual strategy)
print("=== Testing Rita's Strategy (COBO) ===")
cobo = COBO_MissingAddend(M=M_test, S=S_test)
cobo.run()
cobo.display_history()

# Test CBBO (The alternative strategy shown in the diagram)
print("\n=== Testing Alternative Strategy (CBBO) ===")
cbbo = CBBO_TakeAway(M=M_test, S=S_test)
cbbo.run()
cbbo.display_history()