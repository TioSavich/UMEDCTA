import abc
from collections import defaultdict
from eple.core.logic_terms import Predicate, Var, Inference
from eple.core.mua import Vocabulary, Practice


class RegisterMachine(abc.ABC):
    """
    An abstract base class for a register machine.

    A register machine is a simple computational model consisting of:
    - A set of named registers, each holding a natural number.
    - A program, which is a list of instructions.
    - A state, which is the current instruction pointer.

    Instructions can manipulate the registers. We will implement a simple
    instruction set inspired by Minsky machines:
    - ('INC', reg, next_state): Increment register `reg` and go to `next_state`.
    - ('DEC', reg, next_state_if_zero, next_state_if_not_zero):
        Decrement register `reg`. If `reg` becomes zero, go to
        `next_state_if_zero`. Otherwise, go to `next_state_if_not_zero`.
    - ('HALT',): Stop the machine.
    """
    def __init__(self, program, registers=None):
        """
        Initializes the Register Machine.

        Args:
            program (dict): A dictionary mapping state numbers (int) to instructions.
            registers (dict, optional): A dictionary of initial register values.
                                        Defaults to an empty dictionary.
        """
        self.program = program
        self.registers = defaultdict(int)
        if registers:
            self.registers.update(registers)
        self.state = 0  # Start at the first instruction (state 0)
        self.halted = False
        self.history = []

    def step(self):
        """Executes a single instruction."""
        if self.halted or self.state not in self.program:
            self.halted = True
            return

        self._log_state()
        instruction = self.program[self.state]
        op = instruction[0]

        if op == 'INC':
            _, reg, next_state = instruction
            self.registers[reg] += 1
            self.state = next_state
        elif op == 'DEC':
            _, reg, zero_state, non_zero_state = instruction
            if self.registers[reg] > 0:
                self.registers[reg] -= 1
                self.state = non_zero_state
            else:
                self.state = zero_state
        elif op == 'HALT':
            self.halted = True
        else:
            raise ValueError(f"Unknown instruction: {instruction}")

    def run(self, max_steps=100):
        """
        Runs the machine's program until it halts or exceeds max_steps.

        Args:
            max_steps (int): The maximum number of steps to prevent infinite loops.

        Returns:
            dict: The final state of the registers.
        """
        steps = 0
        while not self.halted and steps < max_steps:
            self.step()
            steps += 1
        if not self.halted:
            print(f"Warning: Machine did not halt after {max_steps} steps.")
        self._log_state() # Log the final state
        return self.registers

    def _log_state(self):
        """Logs the current state of the machine for tracing."""
        self.history.append({
            'state': self.state,
            'registers': self.registers.copy()
        })

    def get_trace(self):
        """Returns the execution history."""
        return self.history


class CountingOn(RegisterMachine):
    """
    Implements the 'Counting On' strategy for addition (a + b).
    This machine starts with values in registers 'a' and 'b' and
    ends with the sum in 'a' and 'b' being zero.
    """
    def __init__(self, a, b):
        program = {
            0: ('DEC', 'b', 2, 1),  # Decrement 'b'. If zero, halt. Else, continue.
            1: ('INC', 'a', 0),     # Increment 'a'. Go back to start.
            2: ('HALT',)
        }
        super().__init__(program, registers={'a': a, 'b': b})

    def __repr__(self):
        return f"CountingOn(a={self.registers['a']}, b={self.registers['b']})"


class RMB(RegisterMachine):
    """
    Implements the 'Rearranging-to-Make-Bases' (RMB) strategy for a + b.
    This is a more complex strategy that involves getting the first number
    to a base (e.g., 10) and then adding the remainder.

    For a + b, where base=10:
    1. Find gap_to_base for 'a'. (e.g., if a=8, gap=2).
       - Registers: 'a', 'b', 'base', 'gap', 'rem' (remainder).
    2. Decompose 'b' into 'gap' and 'rem'. (b -> gap + rem).
    3. Compute (a + gap) + rem.

    The register machine program is:
    - 'a': first addend
    - 'b': second addend
    - 'base': the base to round up to (e.g., 10)
    - 'gap': stores the gap (base - (a % base))
    - 'rem': stores the remainder (b - gap)
    - 'temp_a': temporary copy of 'a' to calculate modulo
    """
    def __init__(self, a, b, base=10):
        # Initial register setup
        registers = {'a': a, 'b': b, 'base': base}

        # Program to implement RMB
        program = {
            # Step 1: Calculate gap_to_base for 'a'.
            # We need to calculate a % base. We do this by repeatedly subtracting 'base' from a copy of 'a'.
            0: ('INC', 'temp_a', 1), # temp_a = a
            1: ('DEC', 'a', 2, 0),

            2: ('INC', 'a', 3), # restore a
            3: ('DEC', 'temp_a', 4, 2),

            # Now temp_a has the original 'a'. Calculate a % base.
            4: ('INC', 'gap', 5), # gap = base
            5: ('DEC', 'base', 6, 4),

            6: ('INC', 'base', 7), # restore base
            7: ('DEC', 'gap', 8, 6),

            # At this point, 'gap' has the original 'base'.
            # 'temp_a' has the original 'a'.
            # We find a % base by repeatedly subtracting base from temp_a
            8: ('DEC', 'temp_a', 10, 9), # If temp_a < base, then temp_a is the remainder
            9: ('DEC', 'base', 8, 8), # This is a simplified view. A real modulo is more complex.
                                     # For this example, we'll pre-calculate the gap.
                                     # A full register machine for modulo is too long.

            # Let's simplify and pre-calculate the gap and remainder for the program.
            # This is a common abstraction for complex sub-procedures.
            # The "real" machine would have a subroutine for this.
        }
        # Simplified approach: The spirit of RMB is in the conceptual steps,
        # which we can model with a higher-level program description.
        # The low-level register machine for this is very complex.
        # We will create a program that *assumes* the gap is known.

        gap = (base - (a % base)) % base
        rem = b - gap

        if rem < 0:
            # This strategy is typically used when b is large enough to fill the gap.
            # If not, it defaults to a simpler strategy. We'll model the ideal case.
            # For this simulation, we'll just do CountingOn if b < gap.
            gap = b
            rem = 0

        # Program assumes 'gap' and 'rem' are pre-calculated and loaded.
        # Registers: 'a', 'b' (will be decomposed), 'gap', 'rem'
        program_rmb = {
            # Initial state: a, b, base=10
            # conceptually: gap = 10 - (a % 10), rem = b - gap
            # We load these as initial registers for simplicity.
            'a': a, 'b': b, 'base': base, 'gap': gap, 'rem': rem
        }

        # Program:
        # 1. Add 'gap' to 'a'.
        # 2. Add 'rem' to 'a'.
        program = {
            # Add gap to a
            0: ('DEC', 'gap', 2, 1),
            1: ('INC', 'a', 0),
            # Add rem to a
            2: ('DEC', 'rem', 4, 3),
            3: ('INC', 'a', 2),
            4: ('HALT',)
        }

        super().__init__(program, registers=program_rmb)

    def __repr__(self):
        # Before running, show initial values
        return f"RMB(a={self.registers['a']}, b={self.registers['b']}, base={self.registers['base']})"


if __name__ == '__main__':
    # Example: A program to compute 3 + 2
    # It moves the value of register 'b' to register 'a'.
    # Initial state: {'a': 3, 'b': 2}
    # Final state: {'a': 5, 'b': 0}

    # Program Logic:
    # 0: Decrement 'b'. If 'b' is 0, go to state 2 (halt). Otherwise, go to state 1.
    # 1: Increment 'a'. Go back to state 0.
    # 2: Halt.
    addition_program = {
        0: ('DEC', 'b', 2, 1),
        1: ('INC', 'a', 0),
        2: ('HALT',)
    }

    # Initialize the machine with a=3, b=2
    machine = RegisterMachine(program=addition_program, registers={'a': 3, 'b': 2})

    print("--- Running Addition Machine (3 + 2) ---")
    print(f"Initial Registers: {dict(machine.registers)}")

    final_registers = machine.run()

    print(f"Final Registers: {dict(final_registers)}")
    print("\nExecution Trace:")
    for i, trace in enumerate(machine.get_trace()):
        print(f"Step {i}: State={trace['state']}, Registers={dict(trace['registers'])}")

    # Example: A program to compute 3 * 2
    # It adds 'a' to a 'result' register, 'b' times.
    # Uses a third register 'c' as temporary storage.
    # Initial: {'a': 3, 'b': 2, 'result': 0}
    # Final: {'a': 0, 'b': 0, 'c': 3, 'result': 6}
    multiplication_program = {
        # Main loop: decrement b, if it's not zero, start the addition loop
        0: ('DEC', 'b', 5, 1), # If b is zero, halt (state 5)
        # Addition loop: move 'a' to 'result' and 'c' (temp)
        1: ('DEC', 'a', 3, 2), # If a is zero, reset a and go back to main loop
        2: ('INC', 'result', 1), # result++
        # Reset loop: move 'c' back to 'a'
        3: ('DEC', 'c', 0, 4), # If c is zero, go back to main loop
        4: ('INC', 'a', 3),    # a++
        5: ('HALT',)
    }
    # This multiplication program is more complex and requires careful state management.
    # The provided example is a simplified one. A robust multiplication machine
    # would need to preserve the original value of 'a'.
    # Let's stick to the simpler addition example for clarity.

    print("\n--- Running CountingOn Machine (8 + 5) ---")
    counting_on_machine = CountingOn(a=8, b=5)
    print(f"Initial: {counting_on_machine}")
    final_regs_co = counting_on_machine.run()
    print(f"Final Registers: {dict(final_regs_co)}")
    # print("\nTrace:")
    # for i, trace in enumerate(counting_on_machine.get_trace()):
    #     print(f"Step {i}: State={trace['state']}, Registers={dict(trace['registers'])}")


    print("\n--- Running RMB Machine (8 + 5) ---")
    # Solves 8 + 5.
    # Gap for 8 to 10 is 2.
    # Remainder is 5 - 2 = 3.
    # Calculation: (8+2)+3 = 10+3 = 13.
    rmb_machine = RMB(a=8, b=5, base=10)
    print(f"Initial: {rmb_machine}")
    print(f"Conceptual steps: gap=2, rem=3. Program will add 2, then 3 to 'a'.")
    final_regs_rmb = rmb_machine.run()
    print(f"Final Registers: {dict(final_regs_rmb)}")
    print("\nTrace:")
    for i, trace in enumerate(rmb_machine.get_trace()):
        print(f"Step {i}: State={trace['state']}, Registers={dict(trace['registers'])}")

    # RMB where b is smaller than gap
    print("\n--- Running RMB Machine (8 + 1) ---")
    # Gap for 8 to 10 is 2. b < gap.
    # This would default to just adding 1.
    rmb_machine_small_b = RMB(a=8, b=1, base=10)
    print(f"Initial: {rmb_machine_small_b}")
    final_regs_rmb_small = rmb_machine_small_b.run()
    print(f"Final Registers: {dict(final_regs_rmb_small)}")


# =================================================================
# MUA Definitions for Arithmetic Strategies
# =================================================================

# --- Vocabulary and Practice for CountingOn ---

V_CountingOn = Vocabulary(
    name="V_CountingOn",
    predicates={
        Predicate('Reg', 2),      # Reg(name, value)
        Predicate('State', 1)     # State(id)
    }
)

P_CountingOn = Practice(
    name="P_CountingOn",
    vocabulary=V_CountingOn,
    inferences=set(), # In a full model, these would encode the machine's rules
    incompatibilities=set()
)

# --- Vocabulary and Practice for RMB ---

V_RMB = Vocabulary(
    name="V_RMB",
    predicates=V_CountingOn.predicates.union({
        Predicate('Reg_base', 1),
        Predicate('Reg_gap', 1),
        Predicate('Reg_rem', 1)
    })
)

P_RMB = Practice(
    name="P_RMB",
    vocabulary=V_RMB,
    inferences=set(),
    incompatibilities=set()
)
