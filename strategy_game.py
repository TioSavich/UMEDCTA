import sys
import os
import time
import random
import contextlib
import io
import pandas as pd

# Add the Python_Tests directory to sys.path so we can import the modules
current_dir = os.path.dirname(os.path.abspath(__file__))
strategies_dir = os.path.join(current_dir, 'Calculator', 'Python_Tests')
sys.path.append(strategies_dir)

# Context manager to suppress stdout during imports of scripts that run code on import
@contextlib.contextmanager
def suppress_stdout():
    s = io.StringIO()
    old_stdout = sys.stdout
    sys.stdout = s
    try:
        yield
    finally:
        sys.stdout = old_stdout

# Import the strategy modules safely
print("Loading Educational Modules...")
with suppress_stdout():
    try:
        import SAR_ADD_RMB
        import SAR_SUB_Sliding
        # We might need to copy the DPDA logic if counting_on_back is hard to import
        # But let's try importing it.
        import counting_on_back
    except ImportError as e:
        print(f"\nError loading modules: {e}")
        print("Make sure you are running this from the UMEDCTA root directory.")
        sys.exit(1)
    except Exception as e:
        # Some other error during execution of the scripts
        pass

print("Modules Loaded Successfully.")

class PedagogyQuest:
    def __init__(self):
        self.score = 0
        self.name = ""

    def clear_screen(self):
        os.system('cls' if os.name == 'nt' else 'clear')

    def type_text(self, text, speed=0.02, newline=True):
        for char in text:
            sys.stdout.write(char)
            sys.stdout.flush()
            time.sleep(speed)
        if newline:
            print()

    def get_input(self, prompt):
        print(f"\n{prompt}")
        return input("> ").strip()

    def start(self):
        self.clear_screen()
        self.type_text("Welcome to the UMEDCTA Pedagogical Simulator.")
        self.type_text("You are a Master Teacher training to diagnose and guide student thinking.")
        self.name = self.get_input("Enter your name, Professor:")
        
        while True:
            self.clear_screen()
            print(f"Professor {self.name} | Score: {self.score}")
            print("="*40)
            print("SELECT A MODULE:")
            print("1. The Robot Counter (Algorithmic Thinking)")
            print("2. Sarah's Addition (Rearranging to Make Bases)")
            print("3. Sam's Subtraction (Sliding/Constant Difference)")
            print("Q. Quit")
            
            choice = self.get_input("Choose a module:")
            
            if choice == '1':
                self.run_counting_level()
            elif choice == '2':
                self.run_rmb_level()
            elif choice == '3':
                self.run_sliding_level()
            elif choice.lower() == 'q':
                print("Class dismissed.")
                break
            else:
                print("Invalid selection.")
                time.sleep(1)

    # --- LEVEL 1: COUNTING (DPDA) ---
    def run_counting_level(self):
        self.clear_screen()
        self.type_text("MODULE 1: THE ROBOT COUNTER")
        self.type_text("A robot uses a stack of plates to count. H=Hundreds, T=Tens, U=Units.")
        self.type_text("It processes 'ticks' (count up) and 'tocks' (count down).")
        
        # Generate a problem
        start_val = random.randint(0, 20)
        ticks = random.randint(5, 15)
        direction = random.choice(['up', 'down'])
        
        # If down, make sure we don't go below zero for this simple level
        if direction == 'down' and ticks > start_val:
            start_val = ticks + random.randint(1, 10)
            
        self.type_text(f"\nScenario: The robot starts with {start_val}.")
        self.type_text(f"It receives {ticks} '{'tick' if direction == 'up' else 'tock'}' signals.")
        
        # Use the imported logic to get the real answer
        try:
            # The count_dpda function in counting_on_back.py takes (N, k, direction)
            # N is initial ticks, k is additional operations
            # So we simulate N=start_val, k=ticks
            correct_val = counting_on_back.count_dpda(start_val, ticks, direction)
            
            ans = self.get_input(f"What number will the robot display?")
            
            if ans.isdigit() and int(ans) == correct_val:
                self.type_text("Correct! The automaton state matches your prediction.")
                self.score += 10
            else:
                self.type_text(f"Incorrect. The robot displays {correct_val}.")
                self.type_text("Remember: The robot handles carries and borrows automatically via stack rules.")
                
        except Exception as e:
            self.type_text(f"Simulation Error: {e}")
            
        input("\nPress Enter to continue...")

    # --- LEVEL 2: ADDITION (RMB) ---
    def run_rmb_level(self):
        self.clear_screen()
        self.type_text("MODULE 2: REARRANGING TO MAKE BASES (RMB)")
        self.type_text("Student: Sarah. Strategy: Make 10 (or Base B).")
        
        base = 10
        A = random.randint(6, 9)
        B = random.randint(4, 9)
        # Ensure we actually cross a ten
        if A + B < 10: B = 10 - A + random.randint(1, 5)
        
        self.type_text(f"\nProblem: {A} + {B}")
        self.type_text(f"Sarah wants to keep the {A} and make it a {base}.")
        
        # Run simulation to get the "truth"
        rmb = SAR_ADD_RMB.RMBAutomatonIterative(A, B, Base=base)
        # We run it to populate history, but we want to step through it conceptually
        rmb.run()
        history = rmb.history
        
        # Step 1: Gap
        target_base = ((A // base) + 1) * base
        k_needed = target_base - A
        
        ans = self.get_input(f"How many does {A} need to become {target_base}?")
        if ans == str(k_needed):
            self.type_text("Correct. That is the Gap (K).")
            self.score += 5
        else:
            self.type_text(f"Not quite. {A} needs {k_needed} to reach {target_base}.")
            
        # Step 2: Decompose B
        b_rem = B - k_needed
        ans = self.get_input(f"If she takes {k_needed} from {B}, what is left?")
        if ans == str(b_rem):
            self.type_text("Correct. That is the Remainder.")
            self.score += 5
        else:
            self.type_text(f"No. {B} - {k_needed} = {b_rem}.")
            
        # Step 3: Result
        result = A + B
        self.type_text(f"So she has {target_base} + {b_rem}.")
        ans = self.get_input("Final Answer?")
        if ans == str(result):
            self.type_text("Excellent. You have successfully guided the student.")
            self.score += 10
        else:
            self.type_text(f"The answer is {result}.")
            
        input("\nPress Enter to continue...")

    # --- LEVEL 3: SUBTRACTION (SLIDING) ---
    def run_sliding_level(self):
        self.clear_screen()
        self.type_text("MODULE 3: SLIDING (CONSTANT DIFFERENCE)")
        self.type_text("Student: Sam. Strategy: Adjust both numbers to make subtraction easy.")
        
        M = random.randint(30, 90)
        S = random.randint(11, M - 10)
        # Ensure S is not a multiple of 10, to make it interesting
        if S % 10 == 0: S += random.randint(1, 9)
        if S > M: S = M - random.randint(1, 10) # Safety check
        
        self.type_text(f"\nProblem: {M} - {S}")
        self.type_text("Sam wants to slide the numbers so the subtrahend (bottom number) becomes a friendly base.")
        
        # Run simulation
        slider = SAR_SUB_Sliding.SlidingAutomaton(M, S)
        slider.run()
        
        # Step 1: Target
        target_s = ((S // 10) + 1) * 10
        k = target_s - S
        
        ans = self.get_input(f"What is the nearest higher multiple of 10 for {S}?")
        if ans == str(target_s):
            self.type_text("Correct.")
            self.score += 5
        else:
            self.type_text(f"Target is {target_s}.")
            
        # Step 2: Adjustment
        ans = self.get_input(f"How much do we add to both numbers (the slide)?")
        if ans == str(k):
            self.type_text("Correct. We slide up by {k}.")
            self.score += 5
        else:
            self.type_text(f"We need to add {k}.")
            
        # Step 3: New Problem
        new_m = M + k
        new_s = S + k
        self.type_text(f"New Problem: {new_m} - {new_s}")
        
        ans = self.get_input("Final Result?")
        if ans == str(new_m - new_s):
            self.type_text("Perfect. The distance remains constant.")
            self.score += 10
        else:
            self.type_text(f"Result is {new_m - new_s}.")
            
        input("\nPress Enter to continue...")

if __name__ == "__main__":
    game = PedagogyQuest()
    game.start()
