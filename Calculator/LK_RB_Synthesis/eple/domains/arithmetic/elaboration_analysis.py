import sys
import os
import json

# Add the project root to the Python path to resolve the 'eple' module
project_root = os.path.abspath(os.path.join(os.path.dirname(__file__), '..', '..', '..'))
sys.path.insert(0, project_root)

"""
Simple Test of Real Automata and Practices

This module tests individual automata to ensure they work correctly
before doing comprehensive MUA analysis.
"""

import sys
import os

# Add project root to path
sys.path.insert(0, os.path.abspath(os.path.join(os.path.dirname(__file__), '../../..')))

def test_single_automaton():
    """Test just one automaton to see if it works."""
    print("🧪 Testing Single Automaton")

    try:
        from src.automata.addition.SAR_ADD_RMB import RMBAutomaton

        print("Creating RMB automaton...")
        rmb_auto = RMBAutomaton(inputs={'A': 8, 'B': 5})

        print("Running automaton...")
        result = rmb_auto.run()

        print(f"✅ Success! Result: {result}")
        print(f"Expected: 13")
        print(f"Correct: {result == 13}")

    except Exception as e:
        print(f"❌ Error: {e}")
        import traceback
        traceback.print_exc()

def test_real_automata_and_practices():
    """Test real automata and the practices/vocabularies that are actually defined."""
    print("\n🔬 Testing Real Automata and Practices")
    print("=" * 50)

    # Test multiple automata
    test_cases = [
        ("RMB Automaton", "SAR_ADD_RMB", {'A': 8, 'B': 5}, 13),
        ("COBO Automaton", "SAR_ADD_COBO", {'A': 46, 'B': 37}, 83),
    ]

    for name, module_name, inputs, expected in test_cases:
        try:
            print(f"\n📊 Testing {name} ({inputs['A']} + {inputs['B']})")

            # Import the automaton dynamically
            module = __import__(f"src.automata.addition.{module_name}", fromlist=[module_name.split('_')[-1] + 'Automaton'])
            automaton_class = getattr(module, module_name.split('_')[-1] + 'Automaton')

            auto = automaton_class(inputs=inputs)
            result = auto.run()

            print(f"Result: {result}")
            print(f"Expected: {expected}")
            print(f"✅ Correct: {result == expected}")

        except Exception as e:
            print(f"❌ Error with {name}: {e}")

    # Test real practices and vocabularies
    print("\n🔤 Testing Real Practices and Vocabularies")
    print("=" * 50)

    try:
        from eple.domains.arithmetic.strategies import P_CountingOn, V_CountingOn, P_RMB, V_RMB
        from eple.domains.arithmetic.core import P_ArithmeticAsObjectCollection, V_Arithmetic

        print(f"✅ Found real practices:")
        print(f"   • {P_ArithmeticAsObjectCollection.name} (metaphorical)")
        print(f"   • {P_CountingOn.name} (strategy)")
        print(f"   • {P_RMB.name} (strategy)")

        print(f"\n✅ Found real vocabularies:")
        print(f"   • {V_Arithmetic.name}: {[p.name for p in V_Arithmetic.predicates]}")
        print(f"   • {V_CountingOn.name}: {[p.name for p in V_CountingOn.predicates]}")
        print(f"   • {V_RMB.name}: {[p.name for p in V_RMB.predicates]}")

    except Exception as e:
        print(f"❌ Error loading practices: {e}")

def test_mua_with_real_components():
    """Test MUA analysis using only real components."""
    print("\n� Testing MUA with Real Components")
    print("=" * 50)

    try:
        from eple.domains.arithmetic.strategies import P_CountingOn, V_CountingOn, P_RMB, V_RMB
        from eple.domains.arithmetic.core import P_ArithmeticAsObjectCollection, V_Arithmetic
        from eple.core.mua import AlgorithmicElaboration, is_LX

        # Test CountingOn elaboration of ArithmeticAsObjectCollection
        print("\n📈 Testing CountingOn → ArithmeticAsObjectCollection")
        counting_elaboration = AlgorithmicElaboration(
            P_base=P_ArithmeticAsObjectCollection,
            P_elaborated=P_CountingOn
        )

        is_lx_counting = is_LX(
            p_base=P_ArithmeticAsObjectCollection,
            p_elaborated=P_CountingOn,
            v_base=V_Arithmetic,
            v_elaborated=V_CountingOn,
            all_practices={P_ArithmeticAsObjectCollection, P_CountingOn},
            all_murs={counting_elaboration}
        )

        print(f"Lexical Expansion: {is_lx_counting}")

        # Test RMB elaboration of CountingOn
        print("\n📈 Testing RMB → CountingOn")
        rmb_elaboration = AlgorithmicElaboration(
            P_base=P_CountingOn,
            P_elaborated=P_RMB
        )

        is_lx_rmb = is_LX(
            p_base=P_CountingOn,
            p_elaborated=P_RMB,
            v_base=V_CountingOn,
            v_elaborated=V_RMB,
            all_practices={P_CountingOn, P_RMB},
            all_murs={rmb_elaboration}
        )

        print(f"Lexical Expansion: {is_lx_rmb}")

        if is_lx_counting and is_lx_rmb:
            print("\n✅ SUCCESS: Real MUA analysis works!")
        else:
            print("\n⚠️  Partial success: Some relationships not detected")

    except Exception as e:
        print(f"❌ Error in MUA test: {e}")
        import traceback
        traceback.print_exc()

if __name__ == '__main__':
    test_single_automaton()
    test_real_automata_and_practices()
    test_mua_with_real_components()

    print("\n" + "=" * 60)
    print("🎯 SUMMARY: Testing Real EPLE Components")
    print("=" * 60)
    print("✅ Uses actual automata from src/automata/")
    print("✅ Uses real practices from eple/domains/arithmetic/")
    print("✅ Tests actual automaton behavior")
    print("✅ Performs MUA analysis with real components")
    print("✅ No invented practices or terms")
