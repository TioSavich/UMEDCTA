import pandas as pd

class ConversionToGroupsAutomaton:
    """
    A Register Machine modeling the 'Conversion to Groups Other than Bases' division strategy.
    Models the cognitive process of utilizing the base structure of the dividend to divide by a non-base divisor.
    """
    strategy_name = "Conversion to Groups Other than Bases (CBO Division)"

    def __init__(self, T, S, Base=10):
        self.T = T # Dividend
        self.S = S # Divisor
        self.B = Base # Base
        
        # Registers
        self.T_Bases = 0
        self.T_Ones = 0
        self.Quotient = 0
        self.Remainder = 0
        
        # Derived Values (Analysis of B/S relationship)
        self.S_in_B = 0 # Groups of S within one B
        self.R_in_B = 0 # Remainder when B is divided by S

        self.state = 'q_init'
        self.history = []

        if S <= 0:
            self.state = 'q_error'
            self._record_history(f"Error: Divisor S must be positive.")

    def _record_history(self, interpretation, highlight=False):
        record = {
            'State': self.state, 'Interpretation': interpretation,
            'T_Bases': self.T_Bases, 'T_Ones': self.T_Ones, 
            'Quotient (Q)': self.Quotient, 'Remainder (R)': self.Remainder,
            'Highlight': highlight
        }
        self.history.append(record)

    def transition(self, next_state):
        self.state = next_state

    def run(self):
        while self.state not in ['q_accept', 'q_error']:
            executor = getattr(self, f"execute_{self.state}", self.execute_error)
            executor()
        return self.Quotient

    def execute_error(self):
        if self.state != 'q_error':
            self._record_history(f"Error: Entered unknown state {self.state}")
            self.transition('q_error')

    # --- State Execution Methods ---

    def execute_q_init(self):
        """Initialize registers and decompose T by Base B."""
        self.Quotient = 0
        self.Remainder = 0
        # Decompose T (Simplified model focusing on the highest power of the base and the remainder)
        self.T_Bases = self.T // self.B
        self.T_Ones = self.T % self.B
        
        interp = f"Initialize: {self.T}/{self.S} (Base {self.B}). Decompose T: {self.T_Bases} Bases + {self.T_Ones} Ones."
        self._record_history(interp, highlight=True)
        self.transition('q_analyze_base')

    def execute_q_analyze_base(self):
        """Analyze the relationship between B and S."""
        # Analyze B/S relationship (e.g., 10/8)
        self.S_in_B = self.B // self.S
        self.R_in_B = self.B % self.S
        
        interp = f"Analyze Base: One Base ({self.B}) = {self.S_in_B} group(s) of {self.S} + Remainder {self.R_in_B}."
        self._record_history(interp)
        self.transition('q_process_bases')

    def execute_q_process_bases(self):
        """Process all Bases simultaneously (Distributive logic)."""
        # This step relies on established multiplication practices.
        Q_from_bases = self.T_Bases * self.S_in_B
        R_from_bases = self.T_Bases * self.R_in_B
        
        self.Quotient += Q_from_bases
        self.Remainder += R_from_bases
        
        interp = f"Process {self.T_Bases} Bases: Yields {Q_from_bases} groups and {R_from_bases} remainder."
        self._record_history(interp, highlight=True)
        self.transition('q_combine_R')

    def execute_q_combine_R(self):
        """Combine remainder from Bases with initial Ones."""
        R_from_bases = self.Remainder
        R_from_ones = self.T_Ones
        self.Remainder += R_from_ones
        
        interp = f"Combine Remainders: {R_from_bases} (from Bases) + {R_from_ones} (from Ones) = {self.Remainder}."
        self._record_history(interp, highlight=True)
        self.transition('q_process_R')

    def execute_q_process_R(self):
        """Process the accumulated Remainder."""
        Q_from_R = self.Remainder // self.S
        R_final = self.Remainder % self.S
        
        self.Quotient += Q_from_R
        self.Remainder = R_final
            
        self._record_history(f"Process Remainder: Yields {Q_from_R} additional group(s).", highlight=True)
        self._record_history(f"Finished. Total Quotient = {self.Quotient}.", highlight=True)
        self.transition('q_accept')


    def display_history(self):
        print(f"\n--- {self.strategy_name} History ({self.T} / {self.S}) ---")
        df = pd.DataFrame(self.history)
        display_cols = ['State', 'Interpretation', 'Quotient (Q)', 'Remainder (R)']

        print("Summary Trace:")
        summary_df = df[df['Highlight'] == True]
        if not summary_df.empty:
            print(summary_df[display_cols].to_markdown(index=False))

# Test Case: Example from PDF (32 / 8)
T_test = 32
S_test = 8
cbo_div = ConversionToGroupsAutomaton(T=T_test, S=S_test)
cbo_div.run()
cbo_div.display_history()