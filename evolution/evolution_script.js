console.log("script.js loaded");

document.addEventListener('DOMContentLoaded', function() {
    // Centralized Data Structure for Concept Evolutions
    const conceptData = {
        "Consciousness": [
            {
                paragraph: 90,
                label: "Paragraph 90:\n• Introduced\n• Immediate knowledge as initial object\n• Immediate and receptive approach"
            },
            {
                paragraph: 116,
                label: "Paragraph 116:\n• Percipient, Thing is its object\n• Pure apprehension yields the True\n• Can be deceived\n• Aware of deception\n• Criterion of truth is self-identity\n• Dissimilarity is untruth in perceiving"
            },
            {
                paragraph: 117,
                label: "Paragraph 117:\n• Experiences contradictions in perceiving\n• Object presents as One, but universal property transcends it\n• Defines object as community, then excluding One, then medium, then withdraws to 'my' meaning\n• Thrown back to beginning, same cycle"
            },
            {
                paragraph: 118,
                label: "Paragraph 118:\n• Outcome of perception is dissolution/reflection into self\n• Perceiving is not pure apprehension but involves reflection\n• Return into self alters truth\n• Takes responsibility\n• Distinguishes apprehension of truth from untruth\n• Conscious of reflection, separates it from apprehension"
            },
            {
                paragraph: 119,
                label: "Paragraph 119:\n• Aware of Thing as One\n• Diversity of properties falls within consciousness\n• We are the universal medium keeping moments apart"
            },
            {
                paragraph: 121,
                label: "Paragraph 121:\n• Aware of reflection into self in perceiving\n• Opposite moment to Also, unity of Thing, excluding difference\n• Takes upon itself this unity\n• Introduces 'in so far'\n• Makes itself responsible for oneness"
            },
            {
                paragraph: 122,
                label: "Paragraph 122:\n• Alternately makes itself and Thing into One and Also\n• Finds its perceiving and the Thing itself reveal themselves in twofold way"
            },
            {
                paragraph: 123,
                label: "Paragraph 123:\n• Object is whole movement for it\n• Would have to assume responsibility for placing diversity in One\n• In so far as for itself, Thing not for other"
            },
            {
                paragraph: 129,
                label: "Paragraph 129:\n• Enters realm of Understanding"
            },
            {
                paragraph: 132,
                label: "Paragraph 132:\n• Arrived at thoughts in unconditioned universal\n• Object returned into itself, Notion in principle\n• Consciousness not yet Notion for itself\n• Shrinks from what emerged, takes it as objective essence"
            },
            {
                paragraph: 133,
                label: "Paragraph 133:\n• We must step into its place, be the Notion\n• Through awareness of object, becomes comprehending"
            },
            {
                paragraph: 143,
                label: "Paragraph 143:\n• Mediated relation to inner being, looks through play of Forces\n• Reflects itself out of movement as True, converts to objective inner\n• Distinguishes reflection of Things from its own reflection\n• Inner is extreme, but also the True, certainty of itself\n• Not conscious of this ground, being-for-self would be negative movement\n• Inner is Notion, but doesn't know nature of Notion"
            },
            {
                paragraph: 155,
                label: "Paragraph 155:\n• Not bare unity, but movement where distinction is made and cancelled\n• Change penetrated into supersensible world\n• Passed from inner being as object to Understanding, experiences change there"
            },
            {
                paragraph: 163,
                label: "Paragraph 163:\n• 'Explaining' affords self-satisfaction\n• Communing with itself, occupied only with itself"
            },
            {
                paragraph: 164,
                label: "Paragraph 164:\n• Apprehension of difference as it is in truth is for us\n• Notion of infinity is object for consciousness\n• Consciousness of difference that is immediately cancelled, self-consciousness\n• I distinguish myself from myself, aware what is distinguished is not different\n• I repel myself from myself, what is posited as unlike is not a distinction for me\n• Consciousness of other is necessarily self-consciousness\n• Self-consciousness is truth of previous shapes, but only for us\n• Self-consciousness has become for itself, not yet unity with consciousness in general"
            },
            {
                paragraph: 165,
                label: "Paragraph 165:\n• Closed in a unity with supersensible world through appearance\n• Inner being gazing into inner world, vision of undifferentiated selfsame being\n• Self-consciousness"
            }
        ],
        "Knowledge": [
            {
                paragraph: 90,
                label: "Paragraph 90:\n• Immediate knowledge\n• Knowledge of the immediate or of what simply is"
            }
        ],
        "SenseCertainty": [
            {
                paragraph: 91,
                label: "Paragraph 91:\n• Introduced\n• Appears richest, but is poorest\n• Truth is 'being' of the thing\n• Involves pure 'I' and pure 'This'\n• No complex mediation\n• Thing is, because it is"
            },
            {
                paragraph: 92,
                label: "Paragraph 92:\n• Pure being splits into 'I' and 'object'\n• Both 'I' and 'object' are mediated\n• 'I' has certainty through 'thing'; 'thing' through 'I'"
            },
            {
                paragraph: 93,
                label: "Paragraph 93:\n• Distinction between essence (object) and instance (I)\n• Object as essence and immediate\n• 'I' as unessential and mediated\n• Object remains; knowledge depends on object"
            },
            {
                paragraph: 94,
                label: "Paragraph 94:\n• Questions if object is the essence it's proclaimed to be\n• Examine object's presence in sense-certainty"
            },
            {
                paragraph: 95,
                label: "Paragraph 95:\n• Introduces 'This' as 'Now' and 'Here'\n• Dialectic of 'This' will be explored"
            },
            {
                paragraph: 96,
                label: "Paragraph 96:\n• True content is the universal"
            },
            {
                paragraph: 99,
                label: "Paragraph 99:\n• Pure being as essence, now understood as universal"
            },
            {
                paragraph: 100,
                label: "Paragraph 100:\n• Object no longer essential, now unessential\n• Certainty found in knowing ('I')\n• Expelled from object, driven back to 'I'"
            },
            {
                paragraph: 101,
                label: "Paragraph 101:\n• Force of truth now in the 'I'\n• Experiences same dialectic within 'I'"
            },
            {
                paragraph: 103,
                label: "Paragraph 103:\n• Essence neither in object nor in 'I'\n• Immediacy not of object nor of 'I'\n• Object and 'I' are universals\n• Whole of sense-certainty is its essence\n• Stands firm as immediacy, excludes opposition"
            },
            {
                paragraph: 104,
                label: "Paragraph 104:\n• Pure immediacy unconcerned with otherness\n• Self-identical relation, no distinction"
            },
            {
                paragraph: 105,
                label: "Paragraph 105:\n• Must enter same point of time/space\n• Become the singular 'I' that knows\n• Truth of immediate relation is truth of this 'I'"
            },
            {
                paragraph: 109,
                label: "Paragraph 109:\n• Dialectic is the history of its movement/experience\n• Natural consciousness reaches result but forgets\n• Assertion of 'Thises' as absolute truth refuted\n• Consciousness learns 'This' is a universal"
            },
            {
                paragraph: 111,
                label: "Paragraph 111:\n• Does not take over truth\n• Truth is universal, but it wants 'This'"
            },
            {
                paragraph: 130,
                label: "Paragraph 130:\n• Singular being vanishes and becomes sensuous universality"
            }
        ],
        "Now": [
            {
                paragraph: 95,
                label: "Paragraph 95:\n• Introduced as form of 'This'\n• Example: 'Now is Night'\n• Truth to be tested"
            },
            {
                paragraph: 96,
                label: "Paragraph 96:\n• Initial definition becomes stale\n• Preserves itself as a negative\n• Mediated, determined by what it is not\n• Indifferent to specific content\n• Identified as a universal"
            },
            {
                paragraph: 101,
                label: "Paragraph 101:\n• 'Now' is day because 'I' see it"
            },
            {
                paragraph: 106,
                label: "Paragraph 106:\n• Ceases to be in the act of pointing\n• 'Now' that is, is another 'Now'\n• To be no more just when it is\n• Pointed-out 'Now' has been, this is its truth"
            },
            {
                paragraph: 107,
                label: "Paragraph 107:\n• Pointing-out is a movement with three moments\n• True 'Now' is a simple day with many Nows\n• Pointing-out reveals 'Now' as a universal"
            }
        ],
        "Language": [
            {
                paragraph: 97,
                label: "Paragraph 97:\n• More truthful than sense-certainty\n• Expresses the universal\n• Refutes what we 'mean' to say\n• Impossible to express a sensuous being"
            },
            {
                paragraph: 110,
                label: "Paragraph 110:\n• Reverses meaning of what is said\n• Prevents what is meant from getting into words\n• Sensuous 'This' crumbles in language"
            }
        ],
        "This": [
            {
                paragraph: 97,
                label: "Paragraph 97:\n• Universal 'This' in language"
            },
            {
                paragraph: 110,
                label: "Paragraph 110:\n• When pointed out, 'Here' of other Heres\n• Simple togetherness of many Heres\n• A universal"
            },
            {
                paragraph: 113,
                label: "Paragraph 113:\n• Not 'This', or superseded\n• Determinate Nothing of the 'This'"
            }
        ],
        "Being": [
            {
                paragraph: 97,
                label: "Paragraph 97:\n• 'Being in general' in language"
            },
            {
                paragraph: 99,
                label: "Paragraph 99:\n• Not immediacy, but mediated\n• Defined as abstraction, or pure universal"
            },
            {
                paragraph: 113,
                label: "Paragraph 113:\n• A universal with mediation/negative\n• Expressed as differentiated property"
            }
        ],
        "Here": [
            {
                paragraph: 98,
                label: "Paragraph 98:\n• Introduced as second form of 'This'\n• Example: 'Here is the tree'\n• Truth vanishes when one turns around\n• Abides constant in the vanishing of objects\n• Indifferent to house or tree\n• Mediated simplicity, or universality"
            },
            {
                paragraph: 101,
                label: "Paragraph 101:\n• 'Here' is a tree because 'I' see it"
            },
            {
                paragraph: 108,
                label: "Paragraph 108:\n• 'Here' pointed out is not this 'Here', but many others\n• Vanishes in other Heres\n• What abides is a negative This, a complex of Heres\n• Pointing-out is a movement to universal 'Here'"
            }
        ],
        "Meaning": [
            {
                paragraph: 99,
                label: "Paragraph 99:\n• Left over in face of empty Now and Here\n• True content of sense-certainty is not universal for it"
            }
        ],
        "PointingOut": [
            {
                paragraph: 107,
                label: "Paragraph 107:\n• Reveals 'Now' as movement, not immediate\n• Experience of learning 'Now' is universal"
            },
            {
                paragraph: 108,
                label: "Paragraph 108:\n• Movement into universal 'Here'"
            }
        ],
        "Unutterable": [
            {
                paragraph: 110,
                label: "Paragraph 110:\n• The untrue, irrational, what is merely meant"
            }
        ],
        "Perception": [
            {
                paragraph: 111,
                label: "Paragraph 111:\n• Takes what is present as universal\n• Principle is universality\n• 'I' and object are universals\n• Emergence is logically necessitated\n• Object is togetherness of moments\n• Universal as principle is its essence"
            },
            {
                paragraph: 112,
                label: "Paragraph 112:\n• Contains negation, difference, manifoldness"
            },
            {
                paragraph: 130,
                label: "Paragraph 130:\n• Takes object as universal\n• Sophistry seeks to save moments from contradiction\n• Expedients are empty"
            }
        ],
        "Object": [
            {
                paragraph: 100,
                label: "Paragraph 100:\n• Now unessential in sense-certainty\n• Truth in being 'my' object"
            },
            {
                paragraph: 111,
                label: "Paragraph 111:\n• In perception, simple entity, the essence"
            },
            {
                paragraph: 112,
                label: "Paragraph 112:\n• Must express nature as mediated universal\n• Thing with many properties"
            },
            {
                paragraph: 116,
                label: "Paragraph 116:\n• The True and universal, self-identical"
            },
            {
                paragraph: 117,
                label: "Paragraph 117:\n• Initially One, then universal property\n• Perceived as community, then excluding One, then medium"
            },
            {
                paragraph: 127,
                label: "Paragraph 127:\n• Has essential property, simple being-for-self\n• Also contains diversity, necessary but not essential\n• Distinction is only nominal\n• Unessential cancels itself out"
            },
            {
                paragraph: 128,
                label: "Paragraph 128:\n• Opposite of itself in the same respect\n• For itself as for another, for another as for itself\n• Being-for-self is unessential"
            },
            {
                paragraph: 129,
                label: "Paragraph 129:\n• From sensuous being to universal\n• Universal conditioned by sensuous, splits into extremes\n• Extremes are 'being-for-self' burdened with 'being-for-another'\n• Now have unconditioned absolute universality"
            },
            {
                paragraph: 132,
                label: "Paragraph 132:\n• Developed through movement of consciousness\n• Reflection same on both sides"
            }
        ],
        "ActOfPerceiving": [
            {
                paragraph: 111,
                label: "Paragraph 111:\n• A movement, unessential, unstable"
            }
        ],
        "SenseKnowledge": [
            {
                paragraph: 112,
                label: "Paragraph 112:\n• Wealth belongs to perception"
            }
        ],
        "SenseElement": [
            {
                paragraph: 113,
                label: "Paragraph 113:\n• Present as a universal, not singular"
            }
        ],
        "Supersession": [
            {
                paragraph: 113,
                label: "Paragraph 113:\n• Twofold meaning: negating and preserving"
            }
        ],
        "Properties": [
            {
                paragraph: 113,
                label: "Paragraph 113:\n• Established simultaneously, negative of another\n• Related to themselves, indifferent\n• Simple universality distinct from them"
            },
            {
                paragraph: 114,
                label: "Paragraph 114:\n• If indifferent, not determinate\n• Determinate by differentiating and relating as opposites\n• As opposites, cannot be together in simple unity\n• Differentiation falls outside simple medium"
            },
            {
                paragraph: 120,
                label: "Paragraph 120:\n• Exist on their own in universal medium\n• Determinate properties exist in Thing itself, differentiating elements\n• Thing has a number of properties"
            }
        ],
        "Thinghood": [
            {
                paragraph: 113,
                label: "Paragraph 113:\n• Abstract universal medium\n• Simple togetherness of plurality\n• Many are simple universals"
            }
        ],
        "Also": [
            {
                paragraph: 113,
                label: "Paragraph 113:\n• The pure universal, medium, 'thinghood'"
            }
        ],
        "Medium": [
            {
                paragraph: 114,
                label: "Paragraph 114:\n• Not merely 'Also' (indifferent), but 'One' (excluding)"
            }
        ],
        "One": [
            {
                paragraph: 114,
                label: "Paragraph 114:\n• Moment of negation\n• Self-relation, excludes other\n• Determines 'thinghood' as Thing"
            }
        ],
        "Property": [
            {
                paragraph: 114,
                label: "Paragraph 114:\n• Determinateness one with immediacy of being\n• Immediacy, through unity with negation, is universality"
            }
        ],
        "Determinateness": [
            {
                paragraph: 114,
                label: "Paragraph 114:\n• As One, set free from unity with opposite"
            }
        ],
        "Thing": [
            {
                paragraph: 115,
                label: "Paragraph 115:\n• Completed as truth of perception\n• (a) Indifferent, passive universality, Also\n• (b) Negation, the One, excluding opposites\n• (c) Many properties, relation of first two\n• Sensuous universality a property when One and pure universality developed"
            },
            {
                paragraph: 119,
                label: "Paragraph 119:\n• Perceived as One\n• Properties recognized as reflections of consciousness"
            },
            {
                paragraph: 120,
                label: "Paragraph 120:\n• A One by being opposed to others\n• Determinate properties are in the Thing itself\n• The Also, universal medium of properties\n• Perceived as true when properties subsist apart"
            },
            {
                paragraph: 121,
                label: "Paragraph 121:\n• In so far as white, not cubical, etc.\n• Raised to genuine Also, collection of 'matters'\n• Instead of One, enclosing surface"
            },
            {
                paragraph: 122,
                label: "Paragraph 122:\n• Alternately made into One and Also\n• Exhibits itself specifically, but reflected out and back into itself\n• Contains in itself opposite truth"
            },
            {
                paragraph: 123,
                label: "Paragraph 123:\n• A One, reflected into itself, for itself and for another\n• Oneness contradicts diversity\n• Also and oneness fall within the Thing\n• Contradiction distributed between two objects\n• Self-identical, but disturbed by other Things"
            },
            {
                paragraph: 124,
                label: "Paragraph 124:\n• Different Things exist on their own\n• Each has essential difference in itself\n• Simple determinateness is essential character\n• Further manifoldness is unessential"
            },
            {
                paragraph: 125,
                label: "Paragraph 125:\n• Only a Thing if it does not relate to others\n• Relates through absolute character and opposition\n• Relation negates self-subsistence\n• Essential property is its undoing"
            },
            {
                paragraph: 126,
                label: "Paragraph 126:\n• Posited as being for itself, absolute negation of otherness\n• Self-related negation is suspension of itself\n• Has essential being in another Thing"
            }
        ],
        "Differences": [
            {
                paragraph: 115,
                label: "Paragraph 115:\n• Belonging to indifferent medium, universal, self-related\n• Belonging to negative unity, exclusive"
            }
        ],
        "BeingForSelf": [
            {
                paragraph: 130,
                label: "Paragraph 130:\n• Emerges as true singleness\n• Still conditioned, alongside opposed universality\n• Burdened with opposition"
            }
        ],
        "Truth": [
            {
                paragraph: 130,
                label: "Paragraph 130:\n• Proves to be the opposite of itself\n• Essence is universality devoid of distinctions"
            },
            {
                paragraph: 133,
                label: "Paragraph 133:\n• Follows its own essence, consciousness not involved"
            }
        ],
        "UnconditionedUniversal": [
            {
                paragraph: 132,
                label: "Paragraph 132:\n• If inert, would be extreme of being-for-self\n• Returned into itself from conditioned being-for-self\n• True object of consciousness, but just an object for it"
            },
            {
                paragraph: 134,
                label: "Paragraph 134:\n• Initially negative, consciousness negated one-sided Notions\n• Positive: unity of 'being-for-self' and 'being-for-another' posited\n• Content is likewise universal"
            },
            {
                paragraph: 135,
                label: "Paragraph 135:\n• Object for consciousness, distinction of form and content\n• No longer separated, self-superseding aspects"
            },
            {
                paragraph: 136,
                label: "Paragraph 136:\n• Plurality of diverse universals"
            }
        ],
        "Understanding": [
            {
                paragraph: 129,
                label: "Paragraph 129:\n• Realm of consciousness"
            },
            {
                paragraph: 133,
                label: "Paragraph 133:\n• Superseded its own untruth and of object\n• Result is Notion of True, but only implicit\n• Lets truth go its own way without knowing itself"
            },
            {
                paragraph: 148,
                label: "Paragraph 148:\n• Inner world initially universal, unfilled, in-itself\n• Connection with inner world through mediation\n• Movement fills out inner world for Understanding"
            },
            {
                paragraph: 150,
                label: "Paragraph 150:\n• Realm of laws is truth for Understanding\n• But only initial truth, doesn't fill out appearance\n• Imagines it has found universal law, but only found Notion of law\n• All reality is conformable to law"
            },
            {
                paragraph: 154,
                label: "Paragraph 154:\n• Has Notion of implicit difference\n• Asserts its own necessity"
            },
            {
                paragraph: 155,
                label: "Paragraph 155:\n• Sticks to inert unity of object, movement only within Understanding\n• Explanation explains nothing, repeats the same thing\n• Movement gives rise to nothing new in the Thing itself\n• Detects absolute flux, movement is opposite of itself\n• Posits a difference which it cancels as a difference\n• Same flux as play of Forces"
            },
            {
                paragraph: 156,
                label: "Paragraph 156:\n• Learns it is a law of appearance\n• Differences arise which are no differences\n• Selfsame repels itself, not selfsame is self-attractive"
            },
            {
                paragraph: 163,
                label: "Paragraph 163:\n• Movement is still necessity and movement of Understanding\n• Movement not Understanding's object\n• Objects are positive/negative electricity, distance, force of attraction, etc."
            },
            {
                paragraph: 164,
                label: "Paragraph 164:\n• Infinity becomes object of Understanding in contrary law\n• Understanding falls short of infinity, apportions difference to two worlds\n• Movement is a happening, selfsame and unlike are predicates\n• What is object in sensuous covering for Understanding is pure Notion for us"
            },
            {
                paragraph: 165,
                label: "Paragraph 165:\n• In inner world of appearance, comes to know nothing else but appearance\n• Not as play of Forces, but play of Forces in its universal moments\n• In fact, experiences only itself"
            }
        ],
        "Content": [
            {
                paragraph: 134,
                label: "Paragraph 134:\n• 'Being-for-self' and 'being-for-another' are content\n• Nature and essence is to be for itself and in relation to other\n• Truth is to be unconditionally universal"
            }
        ],
        "Moments": [
            {
                paragraph: 135,
                label: "Paragraph 135:\n• In shape of content, look like first presentation\n• Universal medium of 'matters' and One reflected into itself\n• Former is dissolution of independence, latter is being-for-self"
            }
        ],
        "Force": [
            {
                paragraph: 136,
                label: "Paragraph 136:\n• This movement is Force\n• Expression of Force: dispersal of 'matters'\n• Force proper: driven back into itself\n• Must express itself, remains within itself in expression\n• Understanding sustains different moments\n• Unconditioned universal, contains difference\n• Must be posited as substance of differences\n• Exists as exclusive One, unfolding of 'matters' is another essence\n• Also the whole, remains what it is according to Notion\n• Moments are independent\n• Movement is like movement of perceiving"
            },
            {
                paragraph: 137,
                label: "Paragraph 137:\n• One side of its Notion, substantial extreme, a One\n• Subsistence of unfolded 'matters' is other than Force\n• Expression appears as 'other' approaching and soliciting\n• Retract assertion that Force is a One and 'other' approaches externally\n• Itself the universal medium, has expressed itself\n• Exists as medium of unfolded 'matters', also a One\n• Oneness is other than Force, essence outside of it\n• 'Other' approaches, soliciting reflection into self\n• Supersedes its expression, is reflectedness-into-self\n• Oneness as 'other' vanishes, Force is driven back into itself"
            },
            {
                paragraph: 138,
                label: "Paragraph 138:\n• 'Other' soliciting proves to be itself Force\n• Two Forces present, same Notion, but in duality\n• Second Force appears as soliciting, universal medium\n• Second Force is universal medium only through being solicited\n• It solicits retraction only through being solicited"
            },
            {
                paragraph: 139,
                label: "Paragraph 139:\n• Interplay is being mutually opposed, being for one another\n• Absolute, immediate alternation of determinations\n• Soliciting Force posited as universal medium, solicited as driven back\n• But former is universal medium only through latter\n• Latter is actually the soliciting Force\n• First Force has determinateness only through other"
            },
            {
                paragraph: 140,
                label: "Paragraph 140:\n• Differences exhibited in twofold difference:\n  - Content: Force reflected into itself vs. medium of 'matters'\n  - Form: soliciting vs. solicited, active vs. passive\n• Content difference: distinguished in principle\n• Form difference: independent, opposed\n• Extremes are nothing in themselves, vanishing moments\n• Differences of content and form vanished in themselves\n• Active/soliciting/independent (form) = Force driven back into itself (content)\n• Passive/solicited/for another (form) = universal medium of 'matters' (content)"
            },
            {
                paragraph: 141,
                label: "Paragraph 141:\n• Notion becomes actual through its duplication into two Forces\n• Two Forces exist as independent essences, but their being is a movement towards each other\n• Being is pure positedness, a sheer vanishing\n• Not extremes with fixed substance, but only in middle term and contact\n• Repression within itself (being-for-self) and expression present\n• Moments not divided into independent extremes, each is solely through the other\n• No substances of their own\n• Notion preserves itself as essence in actuality\n• Exists in expression, which is supersession of itself\n• Actual Force free from expression is Force driven back into itself\n• Truth of Force is only the thought of it\n• Moments collapse into undifferentiated unity, its Notion qua Notion\n• Realization is loss of reality, becomes universality"
            },
            {
                paragraph: 142,
                label: "Paragraph 142:\n• First universal: Understanding's Notion, Force not yet for itself\n• Second: Force's essence in and for itself\n• Or, first: Immediate, actual object for consciousness\n• Second: negative of Force objective to sense, object for Understanding\n• First: Force driven back into itself, Force as Substance\n• Second: inner being of things qua inner, Notion of Force qua Notion"
            },
            {
                paragraph: 152,
                label: "Paragraph 152:\n• Abstraction absorbing differences\n• Simple electricity is Force, difference (pos/neg) is in law\n• Electricity not difference per se, said to have law of this mode of being\n• Property essential, but necessity is empty word\n• Notion indifferent to being\n• Being might mean actual existence, but definition doesn't contain necessity of existence\n• Basing necessity on determinateness through another is relapse to plurality of laws"
            }
        ],
        "Appearance": [
            {
                paragraph: 143,
                label: "Paragraph 143:\n• Middle term uniting Understanding and inner world\n• Developed being of Force, vanishing for Understanding\n• Not surface show, but totality of show, constitutes inner\n• Mediates consciousness"
            },
            {
                paragraph: 149,
                label: "Paragraph 149:\n• Absolute flux becomes simple difference\n• Difference expressed in the law, stable image of unstable appearance"
            },
            {
                paragraph: 165,
                label: "Paragraph 165:\n• Nothing to be seen behind curtain unless we go behind it\n• Cannot go straightway behind appearance\n• Knowledge of truth of appearance is result of complex movement\n• Modes of consciousness 'meaning', perceiving, and Understanding vanish"
            }
        ],
        "InnerTruth": [
            {
                paragraph: 144,
                label: "Paragraph 144:\n• Absolute universal, purged of antithesis\n• Opens up above sensuous world, is the true world\n• Permanent beyond, first appearance of Reason\n• Pure element in which truth has its essence"
            }
        ],
        "Syllogism": [
            {
                paragraph: 145,
                label: "Paragraph 145:\n• Extremes: inner being of Things and Understanding\n• Middle term: appearance\n• Movement yields further determination of inner world\n• Experience of close-linked unity of terms"
            }
        ],
        "InnerWorld": [
            {
                paragraph: 146,
                label: "Paragraph 146:\n• For consciousness, a pure beyond\n• Empty, nothingness of appearance, simple universal\n• Readily accepted as unknowable\n• No knowledge of it in its immediacy, not due to limited Reason\n• But because in the void nothing is known, beyond of consciousness"
            }
        ],
        "SupersensibleWorld": [
            {
                paragraph: 147,
                label: "Paragraph 147:\n• Comes from world of appearance, which is its essence and filling\n• Sensuous and perceived posited as it is in truth\n• Truth of sensuous and perceived is to be appearance\n• Supersensible is appearance qua appearance\n• Not the sensuous world as it exists for sense-certainty\n• World of appearance is sensuous world posited as superseded, inner world\n• Not appearance, but sensuous world as really actual"
            },
            {
                paragraph: 149,
                label: "Paragraph 149:\n• Inert realm of laws\n• Beyond perceived world, but present in it\n• Tranquil image"
            },
            {
                paragraph: 157,
                label: "Paragraph 157:\n• First: tranquil kingdom of laws, copy of perceived world\n• Changed into its opposite\n• Second: inverted world, inversion of the first\n• Inner world completed as appearance\n• First had counterpart in perceived world, lacked principle of change\n• Obtains principle of change as inverted world"
            }
        ],
        "PlayOfForces": [
            {
                paragraph: 148,
                label: "Paragraph 148:\n• Immediate for Understanding, simple inner world is True\n• Movement of Force is True as something simple\n• Force solicited by another is soliciting Force for that other\n• Interplay is immediate alternation, absolute interchange of determinateness\n• Ceases to be what it was on appearing, solicits other side\n• Two relations are one and the same\n• Difference of form same as difference of content\n• All distinction of separate Forces vanishes\n• Only difference as universal difference"
            },
            {
                paragraph: 156,
                label: "Paragraph 156:\n• Shows absolute transition and pure change\n• Selfsame (Force) splits into antithesis\n• Selfsame repels itself, repelled is self-attractive\n• Difference cancels itself"
            }
        ],
        "Law": [
            {
                paragraph: 148,
                label: "Paragraph 148:\n• Simple element in play of Force\n• What is true in it"
            },
            {
                paragraph: 150,
                label: "Paragraph 150:\n• Present in appearance, but not entire presence\n• Defect: difference is universal, indeterminate\n• Many laws, but plurality is a defect\n• Must let many laws collapse into one\n• When laws coincide, they lose specific character\n• Law becomes superficial, leaves out specific character"
            },
            {
                paragraph: 151,
                label: "Paragraph 151:\n• Pure Notion transcends law as such\n• Determinateness is vanishing moment\n• Notion of law turned against law itself\n• Difference in law taken up into universal\n• Parts of difference are determinate sides\n• Differences in law return to inner world as simple unity\n• Unity is inner necessity of law"
            },
            {
                paragraph: 152,
                label: "Paragraph 152:\n• Twofold: differences as independent moments, and simple withdrawal (Force)\n• Motion of falling: Force is gravity, law relates time and space as root and square\n• Given positive electricity, negative is given in principle\n• But that electricity divides is not in itself a necessity"
            },
            {
                paragraph: 156,
                label: "Paragraph 156:\n• Second law: opposite of first\n• Like becomes unlike, unlike becomes like\n• Second law is selfsameness of unlike, permanence of impermanence"
            },
            {
                paragraph: 157,
                label: "Paragraph 157:\n• Previously selfsame, now each of two worlds is opposite of itself\n• Selfsame repels itself, not selfsame posits itself as selfsame"
            }
        ],
        "UniversalAttraction": [
            {
                paragraph: 150,
                label: "Paragraph 150:\n• Expresses Notion of law itself\n• Everything has constant difference in relation to others\n• Directed against thoughtless contingency\n• Determinateness as sensuous independence"
            },
            {
                paragraph: 151,
                label: "Paragraph 151:\n• Pure Notion of law\n• Determinateness of specific law belongs to appearance/sensuous being"
            }
        ],
        "Motion": [
            {
                paragraph: 153,
                label: "Paragraph 153:\n• Must be split into time and space, or distance and velocity\n• Divided in itself, but parts don't express origin in One\n• Parts indifferent to one another, not related through essential nature\n• Necessity of division present, but not necessity of parts for one another\n• If thought of as simple essence or Force, it is gravity, but this does not contain these differences"
            }
        ],
        "Necessity": [
            {
                paragraph: 153,
                label: "Paragraph 153:\n• First necessity is sham, false\n• Motion not thought of as simple essence, but as already divided\n• Motion is only superficial relation of parts, not their essence"
            }
        ],
        "Difference": [
            {
                paragraph: 154,
                label: "Paragraph 154:\n• Not a difference in its own self\n• Either Force indifferent to division (law), or parts of law indifferent to one another\n• An inner difference\n• Falls within Understanding, not yet in the thing itself\n• Difference said to be not a difference of the thing itself, immediately cancelled"
            },
            {
                paragraph: 160,
                label: "Paragraph 160:\n• Think pure change, antithesis within antithesis, contradiction\n• Opposite is opposite of an opposite, other is present in it\n• Opposite here, other there, but opposite in-itself is opposite of itself\n• Has other immediately present in it"
            }
        ],
        "Explanation": [
            {
                paragraph: 154,
                label: "Paragraph 154:\n• This process is 'explanation'\n• Law enunciated, universal element distinguished as Force\n• But difference is said to be no difference\n• Example: lightning, law of electricity, Force as essence of law\n• Force constituted same as law, no difference between them\n• Difference of content withdrawn"
            },
            {
                paragraph: 163,
                label: "Paragraph 163:\n• Displays infinity, but as 'explanation' it stands forth freely\n• Finally an object for consciousness, consciousness is self-consciousness\n• Understanding's explanation is description of self-consciousness\n• Supersedes differences in law, posits them in single unity (Force)\n• Unifying is also sundering, creates new difference (Law and Force)\n• But this difference is no difference, lets Force be like Law"
            }
        ],
        "Change": [
            {
                paragraph: 156,
                label: "Paragraph 156:\n• Not yet change of the thing itself, pure change\n• Content of moments remains the same\n• Becomes law of inner world for Understanding"
            }
        ],
        "InvertedWorld": [
            {
                paragraph: 158,
                label: "Paragraph 158:\n• What is like in first world is unlike to itself, and vice-versa\n• Superficially, opposite of first: has first outside of it, repels it\n• One is appearance, other is in-itself\n• One is for an other, other is for itself"
            },
            {
                paragraph: 159,
                label: "Paragraph 159:\n• Antitheses of inner/outer, appearance/supersensible are not different actualities\n• Repelled differences not shared between two substances\n• If one side were world of perception, other would be inner world, imagined sense-world\n• But if in-itself is thought of as sensuous, then it is actual thing\n• Examples: sour/sweet, black/white, north/south pole, oxygen/hydrogen pole\n• Crime's inversion is in intention, but truth of intention is the act\n• Crime's inversion is in actual punishment, reconciliation of law and actuality\n• Actual punishment has inverted actuality, actualization of law, law becomes quiescent"
            },
            {
                paragraph: 160,
                label: "Paragraph 160:\n• Eliminate sensuous idea of fixing differences in different elements\n• Absolute Notion of difference as inner difference\n• Repulsion of selfsame from itself, likeness of unlike as unlike\n• Overarches other world, has it within it\n• Inversion of itself, itself and opposite in one unity\n• Difference as inner difference, or infinity"
            }
        ],
        "Infinity": [
            {
                paragraph: 161,
                label: "Paragraph 161:\n• Law completes itself into immanent necessity\n• Moments of appearance taken up into inner world\n• Simple character of law is infinity\n• (a) Self-identical, but also different in itself, self-repulsion\n• (b) Dirempted parts exhibit stable existence, indifferent without Notion of inner difference\n• (c) Through Notion of inner difference, unlike moments are a difference which is no difference, essence is unity\n• Stimulate each other into activity, posit themselves as not-being, suspend in unity\n• Two moments both subsist, implicit, opposites in themselves, each has 'other' within it, one unity"
            },
            {
                paragraph: 162,
                label: "Paragraph 162:\n• Simple essence of life, soul of the world, universal blood\n• Omnipresence undisturbed by difference, is every difference and their supersession\n• Pulsates within itself, does not move, inwardly vibrates, yet at rest\n• Self-identical, differences are tautological\n• Related only to itself, implies relationship to 'other', relation-to-self is self-sundering\n• Sundered moments are each an opposite of an other\n• Each is not opposite of 'other' but pure opposite, opposite of itself\n• Not an opposite at all, purely for itself, pure self-identical essence"
            },
            {
                paragraph: 163,
                label: "Paragraph 163:\n• Absolute unrest of pure self-movement\n• Being is opposite of its determinateness\n• Soul of all that has gone before\n• Freely and clearly shown in inner world"
            }
        ],
        "Unity": [
            {
                paragraph: 162,
                label: "Paragraph 162:\n• Said that difference cannot issue from it, but is itself one of two moments\n• Abstraction of simplicity over against difference\n• Saying it is abstraction implies it is dividing of itself\n• If unity is negative, opposed to something, has antithesis within it"
            }
        ],
        "SelfIdentity": [
            {
                paragraph: 162,
                label: "Paragraph 162:\n• Becoming self-identical is self-sundering\n• Opposes itself to self-sundering, becomes sundered moment"
            }
        ],
        "Science": [
            {
                paragraph: 164,
                label: "Paragraph 164:\n• Exposition of Notion belongs to Science"
            }
        ]
    };

    const conceptSelect = document.getElementById('concept-select');
    const graphContainer = document.getElementById('graph-container');
    const paragraphAnalysesContainer = document.querySelector('.paragraph-analyses');
    const searchInput = document.getElementById('search-input');
    const searchButton = document.getElementById('search-button');
    let activeConcept = null;

    // Populate concept select dropdown
    for (const concept in conceptData) {
        const option = document.createElement('option');
        option.value = concept;
        option.textContent = concept;
        conceptSelect.appendChild(option);
    }

    // Function to generate paragraph analyses in HTML
    function generateParagraphAnalyses() {
        paragraphAnalysesContainer.innerHTML = ''; // Clear existing content

        for (let paragraph = 90; paragraph <= 165; paragraph++) {
            const analysisDiv = document.createElement('div');
            analysisDiv.classList.add('paragraph-analysis');
            analysisDiv.id = `paragraph-${paragraph}`;
            analysisDiv.innerHTML = `<h3>Paragraph ${paragraph}</h3>`;

            // Add active concepts to the paragraph analysis
            const activeConceptsList = document.createElement('ul');
            for (const concept in conceptData) {
                const conceptEvolutions = conceptData[concept];
                if (conceptEvolutions.some(evol => evol.paragraph === paragraph)) {
                    const listItem = document.createElement('li');
                    listItem.textContent = concept;
                    activeConceptsList.appendChild(listItem);
                }
            }
            if (activeConceptsList.children.length > 0) {
                analysisDiv.innerHTML += `<h4>Concepts Active Here:</h4>`;
                analysisDiv.appendChild(activeConceptsList);
            }

            // Add more detailed analysis here if needed

            paragraphAnalysesContainer.appendChild(analysisDiv);
        }
    }

    // Function to generate .dot graph for a concept
    function generateDotGraph(concept) {
        try {
            // Sanitize labels to escape quotes and newlines
            const nodes = conceptData[concept].map(nodeData => {
                const sanitizedLabel = nodeData.label
                    .replace(/"/g, '\\"')
                    .replace(/\n/g, '\\n');
                return `    "${concept}_Para${nodeData.paragraph}" [label="${sanitizedLabel}"];`;
            }).join('\n');

            const edges = conceptData[concept].reduce((edges, nodeData, index) => {
                if (index < conceptData[concept].length - 1) {
                    const nextParagraph = conceptData[concept][index + 1].paragraph;
                    edges.push(`    "${concept}_Para${nodeData.paragraph}" -> "${concept}_Para${nextParagraph}";`);
                }
                return edges;
            }, []).join('\n');

            // Properly format the DOT graph with clear structure
            const dotTemplate = `digraph "${concept}Evolution" {
    rankdir=LR;
    node [shape=box, fontname="Arial", style=filled, fillcolor=white];
    edge [fontname="Arial"];
    label="${concept} Evolution";
    labelloc=t;

${nodes}

${edges}
}`;

            return dotTemplate;
        } catch (error) {
            console.error('Error generating DOT graph:', error);
            throw new Error(`Failed to generate graph for concept "${concept}": ${error.message}`);
        }
    }

    // Function to render the graph using Viz.js and enable pan/zoom
    function renderGraph(dot) {
        if (!dot) {
            console.error('No DOT graph provided');
            return;
        }

        console.log('Rendering DOT graph:', dot); // Debug log

        const viz = new Viz();
        viz.renderSVGElement(dot)
            .then(svg => {
                graphContainer.innerHTML = '';
                
                // Set initial size for the SVG
                svg.setAttribute('width', '100%');
                svg.setAttribute('height', '600px');
                svg.setAttribute('preserveAspectRatio', 'xMidYMid meet');
                
                graphContainer.appendChild(svg);
                
                // Initialize pan-zoom
                if (typeof svgPanZoom !== 'undefined') {
                    const panZoomInstance = svgPanZoom(svg, {
                        zoomEnabled: true,
                        controlIconsEnabled: true,
                        fit: true,
                        center: true,
                        minZoom: 0.1,
                        maxZoom: 10
                    });

                    // Resize handler
                    const resizeObserver = new ResizeObserver(() => {
                        panZoomInstance.resize();
                        panZoomInstance.fit();
                        panZoomInstance.center();
                    });
                    
                    resizeObserver.observe(graphContainer);
                }

                // Add hover effects
                svg.querySelectorAll('.node').forEach(node => {
                    node.addEventListener('mouseover', () => {
                        node.style.opacity = '0.8';
                    });
                    node.addEventListener('mouseout', () => {
                        node.style.opacity = '1';
                    });
                });
            })
            .catch(error => {
                console.error('Error rendering graph:', error);
                graphContainer.innerHTML = `
                    <div class="error-message">
                        <h3>Error rendering graph</h3>
                        <pre>${error.message}</pre>
                    </div>`;
            });
    }

    // Function to highlight paragraphs based on search query
    function highlightParagraphs(query) {
        const paragraphs = document.querySelectorAll('.paragraph-analysis');
        paragraphs.forEach(paragraph => {
            paragraph.classList.remove('highlight');
            if (paragraph.textContent.toLowerCase().includes(query.toLowerCase())) {
                paragraph.classList.add('highlight');
            }
        });

        // Highlight corresponding nodes in the graph (if a concept is selected)
    // Generate paragraph analyses on initial load
        if (activeConcept) {
            const svg = graphContainer.querySelector('svg');
            if (svg) {
                const nodes = svg.querySelectorAll('.node');
                nodes.forEach(node => {
                    node.classList.remove('highlight');
                    const title = node.querySelector('title').textContent;
                    const paragraphNumber = title.match(/Para(\d+)/)[1];
                    const paragraphElement = document.getElementById(`paragraph-${paragraphNumber}`);
                    if (paragraphElement && paragraphElement.classList.contains('highlight')) {
                        node.classList.add('highlight');
                    }
                });
            }
        }
    }

    // Event listener for concept selection
    conceptSelect.addEventListener('change', () => {
        activeConcept = conceptSelect.value;
        const dot = generateDotGraph(activeConcept);
        renderGraph(dot);
    });

    // Event listener for search button
    searchButton.addEventListener('click', () => {
        const query = searchInput.value;
        highlightParagraphs(query);
    });

    // Generate paragraph analyses on initial load
    generateParagraphAnalyses();

    // Optionally, select a default concept on load
    conceptSelect.value = "Consciousness"; // Or any other concept
    conceptSelect.dispatchEvent(new Event('change')); // Trigger change event to render graph
    generateParagraphAnalyses();

    // Optionally, select a default concept on load
    conceptSelect.value = "Consciousness"; // Or any other concept
    conceptSelect.dispatchEvent(new Event('change')); // Trigger change event to render graph
});