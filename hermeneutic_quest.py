import time
import random
import sys

# =============================================================================
# CORE ENGINE: Base Arithmetic Logic
# =============================================================================

class BaseInt:
    """Handles arithmetic and string representation for Base 5, 10, and 12."""
    def __init__(self, value, base=10):
        self.value = value
        self.base = base

    def __repr__(self):
        return self.to_string()

    def to_string(self):
        if self.value == 0: return "0"
        digits = []
        n = abs(self.value)
        while n:
            rem = int(n % self.base)
            if rem == 10: digits.append('T')
            elif rem == 11: digits.append('E')
            else: digits.append(str(rem))
            n //= self.base
        return "".join(digits[::-1])

    @staticmethod
    def from_string(s, base):
        s = str(s).upper()
        val = 0
        for char in s:
            if char == 'T': d = 10
            elif char == 'E': d = 11
            else: d = int(char)
            val = val * base + d
        return BaseInt(val, base)

    def __add__(self, other): return BaseInt(self.value + other.value, self.base)
    def __sub__(self, other): return BaseInt(self.value - other.value, self.base)
    def __lt__(self, other): return self.value < other.value
    def __eq__(self, other): return self.value == other.value

# =============================================================================
# STRATEGY AUTOMATA (Simplified for Gameplay)
# =============================================================================

class StrategyEngine:
    """Houses the logic for the specific N101 strategies."""
    
    @staticmethod
    def run_RMB(A, B, base):
        """Rearranging to Make Bases: A + B -> (A+K) + R"""
        # Logic from SAR_ADD_RMB.py
        target_base_val = ((A.value // base) + 1) * base
        K_val = target_base_val - A.value
        R_val = B.value - K_val
        
        return {
            "strategy": "RMB",
            "A": A, "B": B,
            "TargetBase": BaseInt(target_base_val, base),
            "Gap (K)": BaseInt(K_val, base),
            "Remainder (R)": BaseInt(R_val, base),
            "Result": BaseInt(target_base_val + R_val, base)
        }

    @staticmethod
    def run_Sliding(M, S, base):
        """Sliding: M - S -> (M+K) - (S+K)"""
        # Logic from SAR_SUB_Sliding.py
        # Target: Make S a base multiple
        target_S_val = ((S.value // base) + 1) * base
        K_val = target_S_val - S.value
        
        return {
            "strategy": "Sliding",
            "M": M, "S": S,
            "Gap (K)": BaseInt(K_val, base),
            "New S": BaseInt(target_S_val, base),
            "New M": BaseInt(M.value + K_val, base),
            "Result": BaseInt(M.value - S.value, base)
        }

    @staticmethod
    def calculate_heuristic(groups, items, base):
        """Logic from SMR_MULT_COMMUTATIVE_REASONING.py"""
        score = 0
        # Penalty if items are hard to count by
        if items.value not in [1, 2, 5, base, base//2]:
            score += 50
        # Penalty for number of iterations
        score += groups.value
        return score

# =============================================================================
# GAME INTERFACE
# =============================================================================

class HermeneuticGame:
    def __init__(self):
        self.score = 0
        self.level = 1
        self.base = 10 # Defaults to 10, changes per level

    def type_text(self, text, speed=0.01):
        for char in text:
            sys.stdout.write(char)
            sys.stdout.flush()
            time.sleep(speed)
        print()

    def header(self, title):
        print("\n" + "="*60)
        print(f" LEVEL {self.level}: {title}")
        print("="*60 + "\n")

    def get_input(self, prompt):
        return input(f"\n[You]: {prompt} ").strip().upper()

    def correct(self):
        print("\n>>> CORRECT! Strategy Validated. <<<")
        self.score += 10
        time.sleep(0.5)

    def fail(self, correct_answer):
        print(f"\n>>> INCORRECT. The logic required was: {correct_answer} <<<")
        time.sleep(1)

    # --- LEVEL 1: Ace of Bases ---
    def level_1_bases(self):
        self.base = 5
        self.header("THE ALIEN WORLD (Base 5)")
        self.type_text("Welcome, Professor. Your first task is to master the language of 'Hands'.")
        self.type_text("In Base 5, we count: 1, 2, 3, 4... and then?")
        
        ans = self.get_input("What comes after 4 in Base 5? (Type digits like '10')")
        if ans == "10": self.correct()
        else: self.fail("10 (One Hand)")

        self.type_text("\nGood. Now, predict the sequence boundary.")
        problem = BaseInt(24, 5) # 44 in base 5
        self.type_text(f"Current Number: {problem} (four hand four)")
        
        ans = self.get_input(f"What comes after {problem} in Base 5?")
        if ans == "100": self.correct()
        else: self.fail("100 (One Handred)")

        # Base 12 Check
        self.base = 12
        self.header("INTO THE DOZENS (Base 12)")
        self.type_text("Now entering Base 12. Remember: 9, T, E, 10...")
        
        prob_val = 11 # E
        b_prob = BaseInt(prob_val, 12)
        ans = self.get_input(f"What is one more than {b_prob}?")
        if ans == "10": self.correct()
        else: self.fail("10 (One Dozen)")

        self.level += 1

    # --- LEVEL 2: Addition (RMB) ---
    def level_2_addition(self):
        self.base = 10
        self.header("THE ART OF ASSEMBLY (RMB)")
        self.type_text("Your student, Sarah, wants to add 8 + 5.")
        self.type_text("She shouldn't just count on (9, 10, 11...).")
        self.type_text("Guide her to use 'Rearranging to Make Bases' (RMB).")
        
        A = BaseInt(8, 10)
        B = BaseInt(5, 10)
        
        self.type_text(f"\nProblem: {A} + {B}")
        
        # Step 1: Gap Finding
        target_base = 10
        k_correct = 2 # 8 needs 2 to make 10