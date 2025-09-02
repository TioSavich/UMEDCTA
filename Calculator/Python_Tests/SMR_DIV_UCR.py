import pandas as pd

class CommutativeReasoningAutomaton:
    """
    An automaton simulating the 'Using Commutative Reasoning' division strategy.
    This models the cognitive process of transforming sharing division into 
    measurement division through iterative accumulation.
    """
    def __init__(self, E, G):
        """
        Initializes the automaton with inputs and memory registers.
        E: Total number of items (Dividend).
        G: Number of groups (Divisor).
        """
        self.E = E
        self.G = G
        # Memory Registers
        self.T = 0  # Accumulated total items distributed
        self.Q = 0  # Items per group (Quotient/Counter)
        # State
        self.state = 'q_start'
        self.history = []
        self._record_history("Initialization", f"Inputs received: E={self.E}, G={self.G}")

    def _record_history(self, action, interpretation):
        """Records the current state and registers for tracing execution."""
        self.history.append({
            'State': self.state,
            'T (Accumulated)': self.T,
            'Q (Per Group)': self.Q,
            'Action': action,
            'Interpretation': interpretation
        })

    def transition(self, next_state):
        """Transitions the automaton to the next state."""
        self.state = next_state

    def run(self):
        """Executes the automaton until an accept or error state is reached."""
        print(f"--- Starting Automaton Simulation (E={self.E}, G={self.G}) ---\n")

        while self.state not in ['q_accept', 'q_error']:
            if self.state == 'q_start':
                self.execute_start()
            elif self.state == 'q_initialize':
                self.execute_initialize()
            elif self.state == 'q_iterate':
                self.execute_iterate()
            elif self.state == 'q_check':
                self.execute_check()
            else:
                print(f"Error: Unknown state {self.state}")
                break
        
        print(f"\n--- Simulation Finished in state: {self.state} ---")
        if self.state == 'q_accept':
            return self.Q
        return None

    def execute_start(self):
        """q_start: Read inputs and move to initialize."""
        action = "Read E, Read G"
        interpretation = "Identify total items and number of groups."
        self._record_history(action, interpretation)
        self.transition('q_initialize')

    def execute_initialize(self):
        """q_initialize: Initialize registers T and Q."""
        # T and Q are already 0, record the action.
        action = "T = 0, Q = 0"
        interpretation = "Initialize distribution total and count per group."
        self._record_history(action, interpretation)
        self.transition('q_iterate')

    def execute_iterate(self):
        """q_iterate: Distribute one round (one item to each of the G groups)."""
        self.T += self.G
        self.Q += 1
        action = f"T = T + G ({self.G}), Q = Q + 1"
        interpretation = f"Distribute round {self.Q}. Total distributed: {self.T}."
        self._record_history(action, interpretation)
        self.transition('q_check')

    def execute_check(self):
        """q_check: Check if the total E has been reached."""
        if self.T < self.E:
            action = f"Check: T ({self.T}) < E ({self.E})"
            interpretation = "Total not yet reached; continue distributing."
            self._record_history(action, interpretation)
            self.transition('q_iterate')
        elif self.T == self.E:
            action = f"Check: T ({self.T}) == E ({self.E})"
            interpretation = f"Total reached. Problem solved. Output Q={self.Q}."
            self._record_history(action, interpretation)
            self.transition('q_accept')
        else:
            # This handles cases where E is not perfectly divisible by G
            action = f"Check: T ({self.T}) > E ({self.E})"
            interpretation = "Error: Accumulated total exceeded E. Not divisible."
            self._record_history(action, interpretation)
            self.transition('q_error')

    def display_history(self):
        """Displays the execution history using pandas for clear formatting."""
        print("\n--- Execution History ---")
        df = pd.DataFrame(self.history)
        # Display relevant columns, omitting the initial setup steps for brevity if desired
        # To see the full trace, simply print the df.
        # We will filter to show the iterative process clearly.
        
        # Display the summary table similar to Page 2 of the document
        print("\nIterative Distribution Summary:")
        iteration_history = df[df['State'] == 'q_iterate']
        summary_table = iteration_history[['Q (Per Group)', 'T (Accumulated)']]
        summary_table = summary_table.rename(columns={
            'Q (Per Group)': 'Number of cupcakes in each box',
            'T (Accumulated)': 'Number of cupcakes given out'
        })
        print(summary_table.to_markdown(index=False))
        
        # Display the full state transitions
        print("\nFull State Transition Trace:")
        print(df.to_markdown(index=True))


# Test the automaton with the example from the document: 56 cupcakes and 8 boxes.
E_input = 56
G_input = 8

automaton = CommutativeReasoningAutomaton(E=E_input, G=G_input)
result = automaton.run()

if result is not None:
    print(f"\nFinal Result: {E_input} items divided into {G_input} groups results in {result} items per group.")

automaton.display_history()