import pandas as pd

class InverseDistributiveReasoningAutomaton:
    """
    A Register Machine modeling the 'Inverse of Distributive Reasoning' strategy for division.
    Decomposes the dividend using known multiples of the divisor.
    """
    strategy_name = "Inverse of Distributive Reasoning (Division)"

    def __init__(self, T, S, known_facts_db=None):
        self.T = T # Total (Dividend)
        self.S = S # Size (Divisor)
        
        # Registers
        self.Remaining = 0
        self.TotalQuotient = 0
        self.Partial_T = 0
        self.Partial_Q = 0

        # Knowledge Base (KB)
        # If no specific DB provided, use a default set of common facts (1x, 2x, 5x, 10x).
        self.KnownFactsDB = known_facts_db if known_facts_db else self._default_knowledge_base()
        self.KB = [] # Specific facts for the current divisor S

        self.state = 'q_init'
        self.history = []

        if S <= 0:
            self.state = 'q_error'
            self._record_history(f"Error: Divisor S must be positive.")

    def _default_knowledge_base(self):
        # Default facts often include multiples of 1, 2, 5, 10.
        facts = {}
        # Assuming a typical range for elementary multiplication facts
        for divisor in range(1, 13):
            facts[divisor] = []
            for multiplier in [1, 2, 5, 10]:
                multiple = divisor * multiplier
                facts[divisor].append((multiple, multiplier))
        return facts

    def _record_history(self, interpretation, highlight=False):
        record = {
            'State': self.state, 'Interpretation': interpretation,
            'Remaining (T)': self.Remaining, 
            # Display partials only if they are currently relevant/non-zero
            'Chunk (Partial T)': self.Partial_T if self.Partial_T > 0 else '',
            'Partial Q': self.Partial_Q if self.Partial_Q > 0 else '',
            'Total Quotient': self.TotalQuotient,
            'Highlight': highlight
        }
        self.history.append(record)

    def transition(self, next_state):
        self.state = next_state

    def run(self):
        while self.state not in ['q_accept', 'q_error']:
            executor = getattr(self, f"execute_{self.state}", self.execute_error)
            executor()
        return self.TotalQuotient

    def execute_error(self):
        if self.state != 'q_error':
            self._record_history(f"Error: Entered unknown state {self.state}")
            self.transition('q_error')

    # --- State Execution Methods ---

    def execute_q_init(self):
        """Initialize registers and load relevant known facts."""
        self.Remaining = self.T
        self.TotalQuotient = 0
        
        # Load facts relevant to the divisor S
        if self.S in self.KnownFactsDB:
            # Sort descending to prioritize larger multiples (Greedy heuristic)
            self.KB = sorted(self.KnownFactsDB[self.S], key=lambda x: x[0], reverse=True)
        
        self._record_history(f"Initialize: {self.T} / {self.S}. Loaded known facts for {self.S}.", highlight=True)
        self.transition('q_search_KB')

    def execute_q_search_KB(self):
        """Heuristically search for the largest known multiple <= Remaining."""
        
        # Reset partial registers before searching
        self.Partial_T = 0
        self.Partial_Q = 0

        found = False
        # Iterate through sorted KB (largest first)
        for multiple, factor in self.KB:
            if multiple <= self.Remaining:
                # Found a suitable fact
                self.Partial_T = multiple
                self.Partial_Q = factor
                found = True
                break
        
        if found:
            self._record_history(f"Found known multiple: {self.Partial_T} ({self.Partial_Q} x {self.S}).")
            self.transition('q_apply_fact')
        else:
            # Cannot find any more suitable facts (Remaining is 0 or a remainder exists)
            self._record_history(f"Decomposition complete. Total Quotient = {self.TotalQuotient}.", highlight=True)
            self.transition('q_accept')

    def execute_q_apply_fact(self):
        """Apply the fact: subtract the multiple (T), add the factor (Q)."""
        T_part = self.Partial_T
        Q_part = self.Partial_Q
        
        self.Remaining -= T_part
        self.TotalQuotient += Q_part
        
        self._record_history(f"Applied fact. Subtracted {T_part}. Added {Q_part} to Quotient.", highlight=True)
        # Loop back to search for the next fact
        self.transition('q_search_KB')


    def display_history(self, summarized=True):
        print(f"\n--- {self.strategy_name} History ({self.T} / {self.S}) ---")
        df = pd.DataFrame(self.history)
        display_cols = ['State', 'Interpretation', 'Remaining (T)', 'Chunk (Partial T)', 'Partial Q', 'Total Quotient']

        if summarized:
             print("Summary Trace:")
             summary_df = df[df['Highlight'] == True]
             if not summary_df.empty:
                print(summary_df[display_cols].to_markdown(index=False))
        else:
            print("Full Trace:")
            # Ensure columns exist before attempting to display
            if not df.empty:
                print(df[display_cols].to_markdown(index=False))

# Test Case: Example from PDF (56 / 8)
# We define the specific knowledge base implied by the transcript to accurately model the student.
STUDENT_KNOWLEDGE = {
    8: [
        (16, 2), # Two 8s = 16
        (40, 5), # Five 8s = 40
        (8, 1)   # Implicitly known
    ]
}

T_test = 56
S_test = 8
inv_dr = InverseDistributiveReasoningAutomaton(T=T_test, S=S_test, known_facts_db=STUDENT_KNOWLEDGE)
inv_dr.run()
inv_dr.display_history(summarized=True)