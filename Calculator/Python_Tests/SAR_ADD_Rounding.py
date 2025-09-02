import pandas as pd

class RoundingAdjustingAutomaton:
    """
    A Register Machine model simulating the 'Rounding and Adjusting' strategy.
    This model uses explicit states for initialization and iteration of subroutines 
    (Count Up To Base, COBO, Count Back) to ensure termination.
    """
    def __init__(self, A, B, Base=10):
        self.A_initial = A
        self.B_initial = B
        self.Base = Base

        # Heuristic: Apply the strategy to the number closer to the next base 
        # (i.e., the one with the largest remainder, favoring rounding up).
        A_rem = A % Base if A > 0 else 0
        B_rem = B % Base if B > 0 else 0

        if A_rem >= B_rem:
            self.Target = A
            self.Other = B
        else:
            self.Target = B
            self.Other = A
            
        # Main Registers
        self.K = 0             # Adjustment amount
        self.A_rounded = 0     # The rounded value of Target
        self.TempSum = 0       # Intermediate sum (A_rounded + Other)
        self.Result = 0        # Final result
        
        # Internal Registers for Iteration
        self.TargetBase = 0    # The goal base for rounding
        self.BaseCounter = 0   # Counter for COBO bases
        self.OneCounter = 0    # Counter for COBO ones
        # Note: K will be reused as the counter for adjustment (Count Back)
        
        self.state = 'q_start'
        self.history = []

    def _record_history(self, interpretation, highlight=False):
        self.history.append({
            'State': self.state, 'Interpretation': interpretation,
            'K_reg': self.K, 'A_rounded': self.A_rounded, 'TempSum': self.TempSum, 'Result': self.Result,
            'Highlight': highlight
        })

    def transition(self, next_state):
        self.state = next_state

    def run(self):
        while self.state not in ['q_accept', 'q_error']:
            # Execute the function corresponding to the current state
            executor = getattr(self, f"execute_{self.state}", self.execute_error)
            executor()
        return self.Result

    def execute_error(self):
        self._record_history(f"Error: Entered unknown state {self.state}")
        self.transition('q_error')

    # --- State Execution Methods ---

    def execute_q_start(self):
        """q_start: Read inputs and determine rounding target."""
        self._record_history(f"Inputs: {self.A_initial}, {self.B_initial}. Target for rounding: {self.Target}", highlight=True)
        self.transition('q_init_K')

    # Phase 1: Calculate K (Count Up To Base)
    def execute_q_init_K(self):
        """q_init_K: Initialize the 'Count Up To Base' subroutine."""
        self.K = 0
        self.A_rounded = self.Target # A_rounded acts as the accumulator for this phase

        # Determine the target base
        if self.Target <= 0:
             self.TargetBase = 0
        elif self.Target % self.Base == 0:
             self.TargetBase = self.Target
        else:
             # Calculate the next multiple of the base
             self.TargetBase = ((self.Target // self.Base) + 1) * self.Base
        
        self._record_history(f"Initializing K calculation. Counting from {self.Target} to {self.TargetBase}.")
        self.transition('q_loop_K')

    def execute_q_loop_K(self):
        """q_loop_K: Iteratively count up to the base."""
        # Condition: Loop Iteration
        if self.A_rounded < self.TargetBase:
            self.A_rounded += 1
            self.K += 1
            self._record_history(f"Counting Up: {self.A_rounded}, K={self.K}")
        # Condition: Loop Exit
        else:
            self._record_history(f"K needed is {self.K}. Target rounded to {self.A_rounded}.", highlight=True)
            self.transition('q_init_Add')

    # Phase 2: Addition (COBO)
    def execute_q_init_Add(self):
        """q_init_Add: Initialize the COBO subroutine."""
        self.TempSum = self.A_rounded
        # Decompose 'Other'
        self.BaseCounter = self.Other // self.Base
        self.OneCounter = self.Other % self.Base
        self._record_history(f"Initializing COBO: {self.A_rounded} + {self.Other}. (Bases: {self.BaseCounter}, Ones: {self.OneCounter})")
        self.transition('q_loop_AddBases')

    def execute_q_loop_AddBases(self):
        """q_loop_AddBases: COBO Phase 1: Add Bases."""
        # Condition: Loop Iteration
        if self.BaseCounter > 0:
            self.TempSum += self.Base
            self.BaseCounter -= 1
            self._record_history(f"COBO (Base): {self.TempSum}")
        # Condition: Loop Exit
        else:
            self._record_history("COBO Bases complete. Transitioning to Ones.")
            self.transition('q_loop_AddOnes')

    def execute_q_loop_AddOnes(self):
        """q_loop_AddOnes: COBO Phase 2: Add Ones."""
        # Condition: Loop Iteration
        if self.OneCounter > 0:
            self.TempSum += 1
            self.OneCounter -= 1
            self._record_history(f"COBO (One): {self.TempSum}")
        # Condition: Loop Exit
        else:
            self._record_history(f"{self.A_rounded} + {self.Other} = {self.TempSum}.", highlight=True)
            self.transition('q_init_Adjust')

    # Phase 3: Adjustment (Count Back)
    def execute_q_init_Adjust(self):
        """q_init_Adjust: Initialize the 'Count Back' subroutine."""
        self.Result = self.TempSum
        # We reuse the K register as the counter for how much to count back.
        self._record_history(f"Initializing Adjustment: Count back K={self.K}.")
        self.transition('q_loop_Adjust')

    def execute_q_loop_Adjust(self):
        """q_loop_Adjust: Iteratively count back K."""
        # Condition: Loop Iteration
        if self.K > 0:
            self.Result -= 1
            self.K -= 1 # Decrement K
            self._record_history(f"Counting Back: {self.Result}")
        # Condition: Loop Exit
        else:
            # Calculate the adjustment amount for the interpretation, as K is now 0.
            adjustment_amount = self.A_rounded - self.Target
            self._record_history(f"Subtracted Adjustment ({adjustment_amount}). Final Result: {self.Result}.", highlight=True)
            self.transition('q_accept')

    def display_history(self, summarized=True):
        print(f"\n--- Rounding and Adjusting Execution History ({self.A_initial} + {self.B_initial}) ---")
        df = pd.DataFrame(self.history)
        
        # Post-processing for display: Restore K value for the summary visualization.
        if not df.empty:
            # Find the maximum K value determined during the execution.
            k_determined = df['K_reg'].max()
            # Create a display column 'K' initialized with the determined value.
            df['K'] = k_determined
                
        display_cols_summary = ['State', 'Interpretation', 'K', 'A_rounded', 'TempSum', 'Result']
        display_cols_full = ['State', 'Interpretation', 'K_reg', 'A_rounded', 'TempSum', 'Result']

        if summarized:
             print("Summary Trace:")
             summary_df = df[df['Highlight'] == True]
             if not summary_df.empty:
                # Use the restored 'K' column for the summary
                print(summary_df[display_cols_summary].to_markdown(index=False))
             else:
                 print("Summary unavailable.")
        else:
            print("Full Iterative Trace (K_reg shows the live register value):")
            # Use the actual 'K_reg' column for the full trace
            print(df[display_cols_full].to_markdown(index=False))

# Test Case 1: Robert's example (8 + 5). Heuristic chooses 8.
print("--- Test Case 1: 8 + 5 ---")
ra_8_5 = RoundingAdjustingAutomaton(A=8, B=5)
ra_8_5.run()
ra_8_5.display_history(summarized=False)

# Test Case 2: 46 + 37. Heuristic chooses 37 (remainder 7 > remainder 6).
print("\n--- Test Case 2: 46 + 37 ---")
ra_46_37 = RoundingAdjustingAutomaton(A=46, B=37)
ra_46_37.run()
ra_46_37.display_history(summarized=True)

# Test Case 3: Case where K=0 (e.g., 10 + 5)
print("\n--- Test Case 3: 10 + 5 (No rounding needed) ---")
ra_10_5 = RoundingAdjustingAutomaton(A=10, B=5)
ra_10_5.run()
ra_10_5.display_history(summarized=True)