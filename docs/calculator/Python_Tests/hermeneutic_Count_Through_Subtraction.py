import pandas as pd
import math
import re

class Automaton:
    """
    Base class for all arithmetic strategy automata.
    Provides a common structure for running, recording history, and displaying results.
    """
    strategy_name = "Base Automaton"
    operation = None

    def __init__(self, Base=10, **kwargs):
        self.Base = Base
        self.state = 'q_start'
        self.history = []
        self.Result = 0

    def _record_history(self, interpretation, highlight=False, **kwargs):
        """Standardized history recording for all automata."""
        record = {'State': self.state, 'Interpretation': interpretation, 'Highlight': highlight}
        record.update(kwargs)
        self.history.append(record)

    def transition(self, next_state):
        """Transitions the automaton to the next state."""
        self.state = next_state

    def run(self):
        """
        Executes the automaton's state machine until it reaches an accept or error state.
        """
        while self.state not in ['q_accept', 'q_error']:
            executor = getattr(self, f"execute_{self.state}", self.execute_error)
            executor()
        return self.Result

    def execute_error(self):
        """Handles undefined states."""
        if self.state != 'q_error':
            self._record_history(f"Error: Entered unknown state {self.state}")
            self.transition('q_error')

    def display_history(self):
        """Displays the execution history using pandas for clear formatting."""
        print(f"\n--- Strategy: {self.strategy_name} ({self.op_string}) ---")
        if not self.history:
            print("No history was recorded.")
            return
            
        df = pd.DataFrame(self.history)
        
        # Define a standard column order
        cols_order = [
            'State', 'Interpretation', 'Sum', 'CV', 'Dist', 'BC', 'OC', 'S_Rem',
            'R_Tens', 'R_Ones', 'A_reg', 'B_reg', 'K_reg', 'K', 'K_M', 'K_S',
            'K_S_Rem', 'A_rounded', 'TempSum', 'Result'
        ]
        
        # Filter out columns that are not in the dataframe and the highlight column
        display_cols = [col for col in cols_order if col in df.columns]
        
        if not display_cols:
            print("No data to display.")
            return

        # For summarized view
        summary_df = df[df['Highlight'] == True]
        if not summary_df.empty:
            print("\nSummary Trace:")
            print(summary_df[display_cols].to_markdown(index=False))

        print("\nFull Iterative Trace:")
        print(df[display_cols].fillna('').to_markdown(index=False))

# --- ADDITION STRATEGIES ---

class COBOAdditionAutomaton(Automaton):
    strategy_name = "Counting On by Bases and then Ones (COBO)"
    operation = "+"

    def __init__(self, A, B, **kwargs):
        super().__init__(**kwargs)
        self.A = A
        self.B = B
        self.op_string = f"{A} + {B}"
        self.Sum = 0
        self.BaseCounter = 0
        self.OneCounter = 0

    def _record_history(self, interpretation, highlight=False):
        super()._record_history(interpretation, highlight, Sum=self.Sum, BC=self.BaseCounter, OC=self.OneCounter)
        
    def execute_q_start(self):
        self._record_history(f"Read A={self.A}, B={self.B}", highlight=True)
        self.transition('q_initialize')

    def execute_q_initialize(self):
        self.Sum = self.A
        self.BaseCounter = self.B // self.Base
        self.OneCounter = self.B % self.Base
        self._record_history(f"Initialize Sum to {self.A}. Decompose B: {self.BaseCounter} Bases, {self.OneCounter} Ones.", highlight=True)
        self.transition('q_add_bases')

    def execute_q_add_bases(self):
        if self.BaseCounter > 0:
            prev_sum = self.Sum
            self.Sum += self.Base
            self.BaseCounter -= 1
            self._record_history(f"Count on by base: {prev_sum} -> {self.Sum}.")
        else:
            self._record_history("All bases added. Transition to adding ones.")
            self.transition('q_add_ones')

    def execute_q_add_ones(self):
        if self.OneCounter > 0:
            prev_sum = self.Sum
            self.Sum += 1
            self.OneCounter -= 1
            self._record_history(f"Count on by one: {prev_sum} -> {self.Sum}.")
        else:
            self.Result = self.Sum
            self._record_history("All ones added. Accept.", highlight=True)
            self.transition('q_accept')

class RMBAdditionAutomaton(Automaton):
    strategy_name = "Rearranging to Make Bases (RMB)"
    operation = "+"

    def __init__(self, A, B, **kwargs):
        super().__init__(**kwargs)
        self.A_initial = A
        self.B_initial = B
        self.op_string = f"{A} + {B}"
        self.A = max(A, B)
        self.B = min(A, B)
        self.K = 0
        self.A_temp = 0
        self.B_temp = 0
    
    def _record_history(self, interpretation, highlight=False):
        super()._record_history(interpretation, highlight, A_reg=self.A, B_reg=self.B, K_reg=self.K, A_temp=self.A_temp, B_temp=self.B_temp)

    def execute_q_start(self):
        self._record_history(f"Start with A={self.A}, B={self.B}", highlight=True)
        self.transition('q_calc_K')
        
    def execute_q_calc_K(self):
        target_base = ((self.A // self.Base) + 1) * self.Base if self.A % self.Base != 0 else self.A
        if self.A_temp == 0:
            self.A_temp = self.A
            self._record_history(f"Start counting up from A ({self.A}) to Target Base ({target_base}).")

        if self.A_temp < target_base:
            self.A_temp += 1
            self.K += 1
            self._record_history(f"Count up: {self.A_temp}. Distance (K): {self.K}.")
        else:
            self._record_history(f"K needed is {self.K}.", highlight=True)
            self.transition('q_decompose_B')

    def execute_q_decompose_B(self):
        K_needed = self.K
        if self.K > 0 and self.B_temp == 0:
             self.B_temp = self.B
             self._record_history(f"Start counting down K ({self.K}) from B ({self.B}).")

        if self.K > 0 and self.B_temp > 0:
            self.B_temp -= 1
            self.K -= 1
            self._record_history(f"Transferred 1. B remainder: {self.B_temp}. K remaining: {self.K}.")
        elif self.K == 0:
            self.A = self.A_temp
            self.B = self.B_temp
            self._record_history(f"Transferred {K_needed}. New state: A={self.A}, B={self.B}.", highlight=True)
            self.transition('q_recombine')
        elif self.K > 0 and self.B_temp == 0:
            self._record_history(f"Strategy Failed: B ({self.B_initial}) is too small.", highlight=True)
            self.transition('q_error')

    def execute_q_recombine(self):
        self.Result = self.A + self.B
        self._record_history(f"Combine rearranged numbers: {self.A} + {self.B} = {self.Result}.", highlight=True)
        self.transition('q_accept')

class RoundingAdditionAutomaton(Automaton):
    strategy_name = "Rounding and Adjusting"
    operation = "+"
    
    def __init__(self, A, B, **kwargs):
        super().__init__(**kwargs)
        self.A_initial = A
        self.B_initial = B
        self.op_string = f"{A} + {B}"
        A_rem = A % self.Base
        B_rem = B % self.Base
        self.Target = A if A_rem >= B_rem else B
        self.Other = B if A_rem >= B_rem else A
        self.K = 0
        self.A_rounded = 0
        self.TempSum = 0
        self.TargetBase = 0
        self.BaseCounter = 0
        self.OneCounter = 0
        
    def _record_history(self, interpretation, highlight=False):
        super()._record_history(interpretation, highlight, K=self.K, A_rounded=self.A_rounded, TempSum=self.TempSum, Result=self.Result)
        
    def execute_q_start(self):
        self._record_history(f"Inputs: {self.A_initial}, {self.B_initial}. Target for rounding: {self.Target}", highlight=True)
        self.transition('q_init_K')

    def execute_q_init_K(self):
        self.TargetBase = ((self.Target // self.Base) + 1) * self.Base if self.Target % self.Base != 0 else self.Target
        self._record_history(f"Initializing K calculation. Counting from {self.Target} to {self.TargetBase}.")
        self.A_rounded = self.Target
        self.transition('q_loop_K')

    def execute_q_loop_K(self):
        if self.A_rounded < self.TargetBase:
            self.A_rounded += 1
            self.K += 1
            self._record_history(f"Counting Up: {self.A_rounded}, K={self.K}")
        else:
            self._record_history(f"K needed is {self.K}. Target rounded to {self.A_rounded}.", highlight=True)
            self.transition('q_init_Add')
            
    def execute_q_init_Add(self):
        self.TempSum = self.A_rounded
        self.BaseCounter = self.Other // self.Base
        self.OneCounter = self.Other % self.Base
        self._record_history(f"Initializing COBO: {self.A_rounded} + {self.Other}. (Bases: {self.BaseCounter}, Ones: {self.OneCounter})")
        self.transition('q_loop_AddBases')

    def execute_q_loop_AddBases(self):
        if self.BaseCounter > 0:
            self.TempSum += self.Base
            self.BaseCounter -= 1
            self._record_history(f"COBO (Base): {self.TempSum}")
        else:
            self.transition('q_loop_AddOnes')

    def execute_q_loop_AddOnes(self):
        if self.OneCounter > 0:
            self.TempSum += 1
            self.OneCounter -= 1
            self._record_history(f"COBO (One): {self.TempSum}")
        else:
            self._record_history(f"{self.A_rounded} + {self.Other} = {self.TempSum}.", highlight=True)
            self.transition('q_init_Adjust')

    def execute_q_init_Adjust(self):
        self.Result = self.TempSum
        self._record_history(f"Initializing Adjustment: Count back K={self.K}.")
        self.transition('q_loop_Adjust')

    def execute_q_loop_Adjust(self):
        if self.K > 0:
            self.Result -= 1
            self.K -= 1
            self._record_history(f"Counting Back: {self.Result}")
        else:
            adjustment_amount = self.A_rounded - self.Target
            self._record_history(f"Subtracted Adjustment ({adjustment_amount}). Final Result: {self.Result}.", highlight=True)
            self.transition('q_accept')

# --- SUBTRACTION STRATEGIES ---

class COBOSubtractionAutomaton(Automaton):
    strategy_name = "Counting On - Missing Addend (COBO)"
    operation = "-"

    def __init__(self, M, S, **kwargs):
        super().__init__(**kwargs)
        self.M = M
        self.S = S
        self.op_string = f"{M} - {S}"
        self.CurrentValue = S
        self.Distance = 0
        
    def _record_history(self, interpretation, highlight=False):
        super()._record_history(interpretation, highlight, CV=self.CurrentValue, Dist=self.Distance)
        
    def execute_q_start(self):
        self._record_history(f"Initialize at S ({self.S}). Target is M ({self.M}).", highlight=True)
        self.transition('q_add_bases')

    def execute_q_add_bases(self):
        if self.CurrentValue + self.Base <= self.M:
            self.CurrentValue += self.Base
            self.Distance += self.Base
            self._record_history(f"Count on by base (+{self.Base}). New Value={self.CurrentValue}.")
        else:
            self._record_history("Next base overshoots target. Switching to ones.")
            self.transition('q_add_ones')

    def execute_q_add_ones(self):
        if self.CurrentValue < self.M:
            self.CurrentValue += 1
            self.Distance += 1
            self._record_history(f"Count on by one (+1). New Value={self.CurrentValue}.")
        else:
            self.Result = self.Distance
            self._record_history(f"Target reached. Result (Distance) = {self.Result}.", highlight=True)
            self.transition('q_accept')

class CBBOSubtractionAutomaton(Automaton):
    strategy_name = "Counting Back by Bases and Ones (CBBO)"
    operation = "-"
    
    def __init__(self, M, S, **kwargs):
        super().__init__(**kwargs)
        self.M = M
        self.S = S
        self.op_string = f"{M} - {S}"
        self.CurrentValue = M
        self.BaseCounter = S // self.Base
        self.OneCounter = S % self.Base
        
    def _record_history(self, interpretation, highlight=False):
        super()._record_history(interpretation, highlight, CV=self.CurrentValue, BC=self.BaseCounter, OC=self.OneCounter)

    def execute_q_start(self):
        self._record_history(f"Initialize at M ({self.M}). Decompose S ({self.S}): {self.BaseCounter} bases, {self.OneCounter} ones.", highlight=True)
        self.transition('q_sub_bases')
        
    def execute_q_sub_bases(self):
        if self.BaseCounter > 0:
            self.CurrentValue -= self.Base
            self.BaseCounter -= 1
            self._record_history(f"Count back by base (-{self.Base}). New Value={self.CurrentValue}.")
        else:
            self._record_history("Bases finished. Switching to ones.")
            self.transition('q_sub_ones')

    def execute_q_sub_ones(self):
        if self.OneCounter > 0:
            self.CurrentValue -= 1
            self.OneCounter -= 1
            self._record_history(f"Count back by one (-1). New Value={self.CurrentValue}.")
        else:
            self.Result = self.CurrentValue
            self._record_history(f"Subtraction finished. Result = {self.Result}.", highlight=True)
            self.transition('q_accept')

class DecompositionSubtractionAutomaton(Automaton):
    strategy_name = "Decomposition (Borrowing)"
    operation = "-"

    def __init__(self, M, S, **kwargs):
        super().__init__(**kwargs)
        self.M = M
        self.S = S
        self.op_string = f"{M} - {S}"
        self.S_T = S // self.Base
        self.S_O = S % self.Base
        self.R_T = M // self.Base
        self.R_O = M % self.Base

    def _record_history(self, interpretation, highlight=False):
        super()._record_history(interpretation, highlight, R_Tens=self.R_T, R_Ones=self.R_O)
        
    def execute_q_start(self):
        self._record_history(f"Decompose M ({self.R_T}T+{self.R_O}O) and S ({self.S_T}T+{self.S_O}O).", highlight=True)
        self.transition('q_sub_bases')

    def execute_q_sub_bases(self):
        self.R_T -= self.S_T
        self._record_history(f"Subtract Bases. Result Tens: {self.R_T}", highlight=True)
        self.transition('q_check_ones')

    def execute_q_check_ones(self):
        if self.R_O >= self.S_O:
            self._record_history(f"Sufficient Ones ({self.R_O} >= {self.S_O}).")
            self.transition('q_sub_ones')
        else:
            self._record_history(f"Insufficient Ones ({self.R_O} < {self.S_O}). Need decomposition.", highlight=True)
            self.transition('q_decompose')

    def execute_q_decompose(self):
        if self.R_T > 0:
            self.R_T -= 1
            self.R_O += self.Base
            self._record_history(f"Decomposed 1 Ten. New state: {self.R_T}T, {self.R_O}O.", highlight=True)
            self.transition('q_sub_ones')
        else:
            self._record_history("Error: Cannot decompose from zero tens.")
            self.transition('q_error')

    def execute_q_sub_ones(self):
        self.R_O -= self.S_O
        self._record_history(f"Subtract Ones. Result Ones: {self.R_O}", highlight=True)
        self.Result = self.R_T * self.Base + self.R_O
        self._record_history(f"Final Result: {self.Result}", highlight=True)
        self.transition('q_accept')

# --- MAIN CALCULATOR ---

class HermeneuticCalculator:
    """
    A calculator that solves arithmetic problems by simulating various
    cognitive strategies modeled as automata.
    """
    def __init__(self):
        self.strategies = {
            "+": {
                "COBO": COBOAdditionAutomaton,
                "RMB": RMBAdditionAutomaton,
                "Rounding": RoundingAdditionAutomaton,
            },
            "-": {
                "COBO (Missing Addend)": COBOSubtractionAutomaton,
                "CBBO (Take Away)": CBBOSubtractionAutomaton,
                "Decomposition": DecompositionSubtractionAutomaton
            }
        }

    def list_strategies(self, operator):
        """Lists available strategies for a given operator."""
        if operator in self.strategies:
            return list(self.strategies[operator].keys())
        return []

    def calculate(self, num1, op, num2, strategy_name):
        """Calculates a result using a specified strategy."""
        if op not in self.strategies or strategy_name not in self.strategies[op]:
            print("Error: Invalid operator or strategy.")
            return

        automaton_class = self.strategies[op][strategy_name]
        
        if op == '+':
            automaton = automaton_class(A=num1, B=num2)
        elif op == '-':
            if num1 < num2:
                print("\nWarning: Minuend is less than subtrahend. This may cause strategy failure.")
            automaton = automaton_class(M=num1, S=num2)
        else:
            print("Operator not yet supported.")
            return

        print(f"\nInitializing calculator for {num1} {op} {num2} using '{strategy_name}' strategy...")
        result = automaton.run()
        automaton.display_history()
        print(f"\nFinal calculated result: {result}")
        return result

def main():
    """Main interactive loop for the calculator."""
    calculator = HermeneuticCalculator()
    print("Welcome to the Hermeneutic Calculator!")
    print("Enter a problem like '46 + 37' or 'exit'/'quit' to quit.")

    while True:
        try:
            user_input = input("\n> ").strip()
            if user_input.lower() in ['exit', 'quit']:
                break

            parts = re.match(r"(\d+)\s*([+\-*/])\s*(\d+)", user_input)
            if not parts:
                print("Invalid format. Please enter a problem like '46 + 37'.")
                continue

            num1, op, num2 = int(parts.group(1)), parts.group(2), int(parts.group(3))
            
            available_strategies = calculator.list_strategies(op)
            if not available_strategies:
                print(f"No strategies available for operator '{op}' yet.")
                continue

            print("\nAvailable strategies:")
            for i, name in enumerate(available_strategies):
                print(f"  {i+1}. {name}")

            choice = input("Choose a strategy (number): ")
            choice_idx = int(choice) - 1

            if 0 <= choice_idx < len(available_strategies):
                strategy = available_strategies[choice_idx]
                calculator.calculate(num1, op, num2, strategy)
            else:
                print("Invalid choice.")

        except (ValueError, IndexError):
            print("Invalid input. Please enter a valid number.")
        except Exception as e:
            print(f"An unexpected error occurred: {e}")

if __name__ == "__main__":
    main()

