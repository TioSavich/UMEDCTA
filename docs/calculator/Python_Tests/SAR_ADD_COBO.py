import pandas as pd

class COBOAutomaton:
    """
    A Register Machine model simulating the 'Counting On By Bases and then Ones' (COBO) strategy.
    """
    def __init__(self, A, B, Base=10):
        self.A = A
        self.B = B
        self.BaseUnit = Base
        
        # Registers for internal computation
        self.Sum = 0
        self.BaseCounter = 0
        self.OneCounter = 0
        
        # State
        self.state = 'q_start'
        self.history = []

    def _record_history(self, action, interpretation):
        self.history.append({
            'State': self.state,
            'Sum': self.Sum,
            'BaseCounter': self.BaseCounter,
            'OneCounter': self.OneCounter,
            'Action': action,
            'Interpretation': interpretation,
        })

    def transition(self, next_state):
        self.state = next_state

    def run(self):
        while self.state not in ['q_accept', 'q_error']:
            if self.state == 'q_start':
                self.execute_start()
            elif self.state == 'q_initialize':
                self.execute_initialize()
            elif self.state == 'q_add_bases':
                self.execute_add_bases()
            elif self.state == 'q_add_ones':
                self.execute_add_ones()
            else:
                self.transition('q_error')
                break
        return self.Sum

    def execute_start(self):
        """q_start: Read inputs."""
        self._record_history(f"Read A={self.A}, B={self.B}", "Start.")
        self.transition('q_initialize')

    def execute_initialize(self):
        """q_initialize: Initialize Sum and decompose B."""
        self.Sum = self.A
        # Decomposition (Assuming this skill is prerequisite for COBO)
        self.BaseCounter = self.B // self.BaseUnit
        self.OneCounter = self.B % self.BaseUnit
        
        action = f"Sum=A; Decompose B ({self.B})"
        interpretation = f"Initialize Sum to {self.A}. {self.BaseCounter} Bases, {self.OneCounter} Ones."
        self._record_history(action, interpretation)
        # Proceed to the base addition phase
        self.transition('q_add_bases')

    def execute_add_bases(self):
        """q_add_bases: Iteratively add BaseUnits."""
        # Condition: BaseCounter > 0 (Loop Iteration)
        if self.BaseCounter > 0:
            prev_sum = self.Sum
            self.Sum += self.BaseUnit
            self.BaseCounter -= 1
            
            action = f"Sum += {self.BaseUnit}; BaseCounter -= 1"
            interpretation = f"Count on by base: {prev_sum} -> {self.Sum}."
            self._record_history(action, interpretation)
            # Stay in the same state
        # Condition: BaseCounter == 0 (Loop Exit)
        else:
            self._record_history("BaseCounter == 0", "All bases added. Transition to adding ones.")
            self.transition('q_add_ones')

    def execute_add_ones(self):
        """q_add_ones: Iteratively add Ones."""
        # Condition: OneCounter > 0 (Loop Iteration)
        if self.OneCounter > 0:
            prev_sum = self.Sum
            self.Sum += 1
            self.OneCounter -= 1
            
            action = "Sum += 1; OneCounter -= 1"
            interpretation = f"Count on by one: {prev_sum} -> {self.Sum}."
            self._record_history(action, interpretation)
            # Stay in the same state
        # Condition: OneCounter == 0 (Loop Exit)
        else:
            self._record_history("OneCounter == 0", "All ones added. Accept.")
            self.transition('q_accept')

    def display_history(self):
        print(f"\n--- COBO Execution History ({self.A} + {self.B}) ---")
        df = pd.DataFrame(self.history)
        print(df.to_markdown(index=False))

# Test the automaton with Lauren's example: 46 + 37.
cobo_automaton = COBOAutomaton(A=46, B=37)
result = cobo_automaton.run()
cobo_automaton.display_history()
print(f"\nFinal Result: {result}")