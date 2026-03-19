# The Hermeneutic Calculator

A set of interactive HTML pages, each modeling one formal strategy from research on children's mathematical reasoning. The strategies come from Susan Jo Russell's work on addition, subtraction, multiplication, and division — each page walks through the inferential structure of a specific approach a student might use.

The calculator grew from a question in teacher education: what happens when you formalize a child's reasoning strategy as an explicit procedure? The formalization does not improve on the original reasoning; it makes visible what the reasoning was already doing. That difference between implicit and explicit inference is philosophically significant — it's related to what Brandom calls elaborating-explicating vocabulary — but the tools here are not implementations of that thesis. They're tools for noticing it.

The Ace of Bases companion (accessible from the main page) lets users explore number bases other than ten. The goal is not to teach base conversion but to make the contingency of base-ten notation tangible.

## What these tools do not claim

The strategies are modeled, not proven. A Prolog formalization of "counting on back" is not a theory of mathematical cognition — it's a representation that can be made to behave like the strategy under controlled conditions. The interesting thing is where the representation fails or oversimplifies. That breakdown is the point of contact with the manuscript's central argument.

## Files

- `index.html` — main calculator, links to all strategy pages
- `AceofBases/index.html` — base system explorer
- `strategy_styles.css` — shared stylesheet
- Individual strategy HTML files (SAR_ADD_*, SAR_SUB_*, SMR_*)
