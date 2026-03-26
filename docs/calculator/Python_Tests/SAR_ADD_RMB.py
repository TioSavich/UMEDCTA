import pandas as pd

class RMBAutomatonIterative:
    """
    A Register Machine model simulating the 'Rearranging to Make Bases' (RMB) strategy,
    based on algorithmic elaboration from counting primitives.
    """
    def __init__(self, A, B, Base=10):
        # Heuristic: Apply the strategy to the larger number.
        self.A = max(A, B)
        self.B = min(A, B)
        self.A_initial = self.A
        self.B_initial = self.B
        self.Base = Base
        
        # Registers for internal computation
        self.K = 0
        self.A_temp = 0 # Used for counting up A
        self.B_temp = 0 # Used for counting down B
        self.Result = 0
        
        # State
        self.state = 'q_start'
        self.history = []

    def _record_history(self, action, interpretation):
        self.history.append({
            'State': self.state,
            'Action': action,
            'Interpretation': interpretation,
            'A_reg': self.A,
            'B_reg': self.B,
            'K_reg': self.K,
            'A_temp': self.A_temp,
            'B_temp': self.B_temp,
        })

    def transition(self, next_state):
        self.state = next_state

    def run(self):
        # Transition from start directly to calculation
        if self.state == 'q_start':
            self.transition('q_calc_K')
        
        while self.state not in ['q_accept', 'q_error']:
            if self.state == 'q_calc_K':
                self.execute_calc_K()
            elif self.state == 'q_decompose_B':
                self.execute_decompose_B()
            elif self.state == 'q_recombine':
                self.execute_recombine()
            else:
                self.transition('q_error')
                break
        
        return self.Result

    def execute_calc_K(self):
        """q_calc_K: Calculate K needed to reach the base by counting up from A."""
        
        # Determine the target base
        if self.A % self.Base == 0 and self.A != 0:
             target_base = self.A
        else:
             target_base = ((self.A // self.Base) + 1) * self.Base

        if self.A_temp == 0:
            # Initialize
            self.A_temp = self.A
            self.K = 0
            self._record_history("Initialize K calc", f"Start counting up from A ({self.A}) to Target Base ({target_base}).")

        if self.A_temp < target_base:
            # Iterative counting up (Primitive operation)
            self.A_temp += 1
            self.K += 1
            self._record_history("A_temp += 1, K += 1", f"Count up: {self.A_temp}. Distance (K): {self.K}.")
        elif self.A_temp == target_base:
            self._record_history("Reached Target Base", f"K needed is {self.K}.")
            self.transition('q_decompose_B')

    def execute_decompose_B(self):
        """q_decompose_B: Decompose B by counting down K from B."""
        K_needed = self.K

        # Initialize B_temp if K>0 and this is the first entry into the state (A_temp > A)
        if self.K > 0 and self.B_temp == 0 and self.A_temp > self.A:
             self.B_temp = self.B
             self._record_history("Initialize B decomp", f"Start counting down K ({self.K}) from B ({self.B}).")

        if self.K > 0 and self.B_temp > 0:
            # Iterative counting down (Primitive operation)
            self.B_temp -= 1
            self.K -= 1
            self._record_history("B_temp -= 1, K -= 1", f"Transferred 1. B remainder: {self.B_temp}. K remaining: {self.K}.")
        elif self.K == 0:
            # Success: K has been transferred
            self.A = self.A_temp # A is now the target base
            self.B = self.B_temp # B is the remainder
            self._record_history("Decomp Complete", f"Transferred {K_needed}. New state: A={self.A}, B={self.B}.")
            self.transition('q_recombine')
        elif self.K > 0 and self.B_temp == 0:
            # Failure: B was insufficient
            self._record_history("Strategy Failed", f"B ({self.B_initial}) is too small to provide K ({K_needed}).")
            self.transition('q_error')

    def execute_recombine(self):
        """q_recombine: Combine the new A (base) and the remainder B."""
        # This step exploits the base structure (cognitively easy)
        self.Result = self.A + self.B
        self._record_history("Result = A + B", f"Combine rearranged numbers: {self.A} + {self.B} = {self.Result}.")
        self.transition('q_accept')

    def display_history(self):
        print(f"\n--- RMB Execution History ({self.A_initial} + {self.B_initial}) ---")
        df = pd.DataFrame(self.history)
        # Filter columns for cleaner display
        display_df = df[['State', 'Action', 'Interpretation', 'A_reg', 'B_reg', 'K_reg', 'A_temp', 'B_temp']]
        print(display_df.to_markdown(index=False))

# Example Test (Sarah's example: 8 + 5)
rmb_8_5 = RMBAutomatonIterative(A=8, B=5)
rmb_8_5.run()
rmb_8_5.display_history()