/** <module> Math Domain Content Loader
 *
 *  Loads mathematical content modules (Lakoff metaphors, arithmetic strategies)
 *  on top of the PML Core Framework.
 *
 *  Usage:
 *    swipl math/load_math.pl
 *    ?- proves([s(collection([a,b,c])), s(size([a,b,c], 3))] => [s(number(3))], 50, _, Proof).
 */

% Load the core framework first
:- ['../load.pl'].

% Load math-specific content modules
:- use_module(lakoff_metaphors).
:- use_module(arithmetic_strategies).

:- initialization(writeln('Math domain content loaded (Lakoff & Brandom).')).
