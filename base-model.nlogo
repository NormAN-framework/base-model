globals [
  hypothesis-value ;actual Boolean value of the instantiated hypothesis (world model)
  evidence-list ;List to store the actual values of the nodes designated as evidence (world model)
  optimal-posterior ;the optimal belief p(HYP| evidence list) about the hypothesis given access to all evidence nodes' actual values
  Bayes-net ;causal structure of the world model and of the agents' faithful subjective representations thereof
  hypothesis-node ;specification/ selection of the hypothesis nodes
  evidence-nodes  ;specification/ selection of the evidence nodes
  arguments ;list used to count the uses of each argument
  number-of-evidence ;number of pieces of evidence
  evidence-probabilities-list ;marginal probabilities of each piece of evidence given the actual value of the hypothesis
]

turtles-own [
  agent-evidence-list ;stores which parcels/pieces of evidence this agent has seen
  agent-belief ;current degree of belief in the hypothesis
  current-evidence ;list index of the currently handled piece of evidence
  last-heard ;the number of the last-heard piece of evidence
  draws ;How many more draws before this agent reaches max-draws?
  initial-draws-list ;used to store which pieces of evidence this agent pre-drew last time
  heard ;list of indexes of the pieces of evidence this agent has already encountered
  update-total ;list of evidence-strengths for this agent
  initial-belief ;this agent's prior about HYP from before they drew or heard any evidence
  recency-list ;list of evidence this agent has encountered, from least to most recent (no duplicated)
]

extensions [r Nw]

;******************************************************************
;MAIN MODEL FLOW **************************************************
;******************************************************************


to setup
  my-clear-all
  reset-ticks

  carefully [
    loadDAG ;all routines to do with loading or modifying DAGs can be found at the bottom of this code file
  ][
    print "EXCEPTION: Please ensure you are providing a valid R-file in the chosen location."
    print error-message
    stop
  ]

  if reset-world-? [
    carefully [
      reset-evidence
    ][
      print "EXCEPTION: please ensure that your custom settings for the Bayesian network are sound."
      print error-message
      stop
    ]
    set reset-agents-initial-evidence-? true ;a new world necessitates new initial draws
  ]

  compute-optimal-posterior

  if reset-social-network-? [
    clear-turtles
    carefully [
      if network = "wheel" [create-turtles number-of-agents create-network-wheel]
    ][
      print "EXCEPTION: please ensure that your wheel network contains at least three agents."
      stop
    ]
    if network = "complete" [create-turtles number-of-agents create-network-complete]
    if network = "small-world" [
       carefully [
      create-network-small-world
    ][
      print "EXCEPTION: To create a small-world network, please make sure that [k] is less than half of [number-of-agents]."
      print error-message
      stop
      ]
    ]
    if network = "null" [create-turtles number-of-agents create-network-null]

    set reset-agents-initial-evidence-? true ;a new network structure necessitates freshly initialized agents
  ]

  initializeAgents

  setupPlots
end

to go

  if check-stoping-conditions [stop] ;Optionally, the simulation stops when all agents know all the evidence or when max-ticks is reached.

  if show-me-? [
    show (word "--------------------------------------------")
    show (word "NEW TICK: " ticks)
    show (word "--------------------------------------------")
    show (word "Step 1: Inquiry")
    show (word "--------------------------------------------")
  ]

  ask turtles [
    if shouldInquire? [
      collectEvidence "random"
    ]
  ]

if show-me-? [
    show (word "--------------------------------------------")
    show (word "Step 2: COMMUNICATION")
    show (word "--------------------------------------------")
  ]

  ask turtles [
    if shouldShare? [
      if share = "impact" [impactShare]
      if share = "random" [randomShare]
      if share = "recent" [simpleRecentShare]
    ]

  ]

  plotArguments
  tick


 end

;******************************************************************
;INQUIRY AND REASONING SUBROUTINES ********************************
;******************************************************************

to compute-posterior
;This is called whenever an agent receives a new piece of evidence (parcel). The procedure computes the posterior belief in the hypothesis, given ALL currently recorded pieces of evidence.
  let b0 agent-belief
  let b1 0

  ;***********************************************************************************************************************************
  ;the following block creates a string that is later used to query the R instance

  let heard-sorted sort heard ;recall that heard just stores indexes, so sort applies

  let rQuery (word "cpquery("Bayes-net", event = ("Hypothesis-node" == 'yes'), evidence = (")

  let x 0 ;iterates through all eNodes
  let y 0 ;keeps track of the additions to the rQuery string
  while [x < number-of-evidence] [

    if member? x heard-sorted [
      set rQuery (word rQuery ""item x evidence-nodes" == '" item x agent-evidence-list"'")
      set y y + 1
      if y < length heard-sorted [set rQuery (word rQuery " & ")]
    ]
    set x x + 1
  ]

   set rQuery (word rQuery "))")

  if show-me-? [print (word "The rQuery for compute-optimal-posterior is " rQuery)]

  ;***********************************************************************************************************************************
  ;gRain
  let gRainQuery "ev <- setEvidence(gRainbn, nodes = c("

  set x 0 ;iterates through all eNodes
  set y 0 ;keeps track of the additions to the rQuery string
  while [x < number-of-evidence] [

    if member? x heard-sorted [
      set gRainQuery (word gRainQuery "'"item x evidence-nodes"'")
      set y y + 1
      if y < length heard-sorted [set gRainQuery (word gRainQuery " , ")]
    ]
    set x x + 1
  ]

  set gRainQuery (word gRainQuery "), , states = c(")

  set x 0 ;iterates through all eNodes
  set y 0 ;keeps track of the additions to the rQuery string


  while [x < number-of-evidence] [

    if member? x heard-sorted [
      set gRainQuery (word gRainQuery "'"item x agent-evidence-list"'")
      set y y + 1
      if y < length heard-sorted [set gRainQuery (word gRainQuery " , ")]
    ]
    set x x + 1
  ]
   set gRainQuery (word gRainQuery "))")


  ;if show-me-? [show  gRainQuery]
  r:eval gRainQuery

  let gRainQuery2 (word "matrix.cpt <- querygrain(ev, nodes = c('"hypothesis-node"'), type = 'joint')")
  ;if show-me-? [show gRainQuery2]
  r:eval gRainQuery2



  ;***********************************************************************************************************************************


  ;Here again, we repeat the query to counteract approximation errors.

  if approximation = "repeater" [
    repeat repeater [
      let m r:get rQuery
      set b1 (b1 + m)
    ]
    set b1 (b1 / repeater)
  ]

  if approximation = "seed"[
    r:eval (word "set.seed("seed") ")
    set b1 r:get rQuery
  ]

   if approximation = "gRain" [
   set b1 r:get (word "probability_H_true <- as.numeric(matrix.cpt['yes'])")

  ]

  if show-me-? [show (word "my posterior: " b1)]

  ;This computes the "relative" update: it records how the agent's belief has changed through this particular piece of evidence.
  let update-r b1 - b0
  ;set update-relative replace-item last-heard update-relative update-r

  compute-singular

  if show-me-? [show (word "update-total: " update-total)]

  set agent-belief  b1
  change-colors
end

to compute-singular ;It computes how much the last received piece of evidence would have changed the agent's belief had this been the first piece of evidence they heard (relative to the original prior).
;In this way, the agents record the isolated diagnosticity of the evidence.
;This is called whenever a new posterior is calculated.
  if last-heard != "none"
  [

    ;***********************************************************************************************************************************
    ;the following block creates a string that is later used to query the R instance
    let rQuery (word "cpquery("Bayes-net", event = ("Hypothesis-node" == 'yes'), evidence = (")
    set rQuery (word rQuery ""item last-heard evidence-nodes" == '" item last-heard evidence-list "' ))")

    ;***********************************************************************************************************************************

    ;gRain

    let gRainQuery (word "ev <- setEvidence(gRainbn, nodes = c('"item last-heard evidence-nodes"'),")
    set gRainQuery (word gRainQuery " states = c('" item last-heard evidence-list "'))")
    ;if show-me-?[show gRainQuery]
    r:eval gRainQuery

    let gRainQuery2 (word "matrix.cpt <- querygrain(ev, nodes = c('"hypothesis-node"'), type = 'joint')")
    ;if show-me-? [show gRainQuery2]
    r:eval gRainQuery2


    ;****************************************************************************************

    let r 0
    if approximation = "repeater"[
      repeat repeater [
        let v r:get rQuery
        set r (r + v)
      ]
      set r (r / repeater)
    ]
    if approximation = "seed"[
      r:eval (word "set.seed("seed") ")
      set r r:get rQuery
    ]
    if approximation = "gRain"[
      set r r:get (word "probability_H_true <- as.numeric(matrix.cpt['yes'])")
    ]

  if show-me-? [
  show (word "singular posterior: " rQuery ": " r)]
  set update-total replace-item last-heard update-total (r - initial-belief)
  ]
end

to change-colors
;changes the agents' colors, given their belief in the hypothesis node.
  if agent-belief = initial-belief [set color grey]
  if agent-belief > initial-belief [set color green]
  if agent-belief < initial-belief [set color red]
end

to collectEvidence [number] ;number is either the index of the piece of evidence to be collected, or the string "random"
  let i number

  if number = "random" [
    ;select the index of an hitherto unknown piece of evidence;
    let nIndices n-values length agent-evidence-list [ ? -> ifelse-value (item ? agent-evidence-list = "-") [?][false]]
    set nIndices filter [x -> x != false] nIndices
    set i one-of nIndices
  ]

  ;Computation and recording of new evidence

  set heard lput i heard

  set recency-list lput i recency-list

  set last-heard i

  if item i agent-evidence-list = "-"[
    set agent-evidence-list replace-item i agent-evidence-list item i evidence-list
  ]

  if show-me-? [show (word "I drew item " i "; my list: " agent-evidence-list)]
  if show-me-? and share = "recent" [show (word "recency-list: " recency-list)]
  compute-posterior
  set draws draws - 1

end

to-report shouldInquire? ;Each round, the agents may collect a new piece of evidence, but only given three conditions hold:

  ifelse draws > 0 [ ;Condition 1: They must still have "draws" (see interface).
    let p random-float 1
    ifelse p < curiosity [ ; Condition 2: They must be curious enough
      ifelse length heard  < number-of-evidence [ ;Condition 3: They must not know all the evidence already
        report true
      ][
        if show-me-? [show (word "I already know all pieces of evidence." )]
          report false
      ]
    ][
      if show-me-? [show (word "I am not curious this round.")]
      report false
    ]
  ][
    if show-me-? [show (word "I have no more draws left.")]
    report false
  ]

end

to compute-optimal-posterior

  let rQuery (word "cpquery("Bayes-net", event = ("Hypothesis-node" == 'yes'), evidence = (")



  let x 0
  while [x < number-of-evidence] [
    set rQuery (word rQuery "" item x evidence-nodes " == '" item x evidence-list "' ")
    set x x + 1
    if x < number-of-evidence [set rQuery (word rQuery " & ")]
  ]
  set rQuery (word rQuery "))")

  if show-me-? [print (word "The rQuery for compute-optimal-posterior is " rQuery)]

  ;******************************************************************
 let gRainQuery "ev <- setEvidence(gRainbn, nodes = c("

 set x 0
  while [x < number-of-evidence] [
      set gRainQuery (word gRainQuery "'"item x evidence-nodes"'")
      set x x + 1
      if x < number-of-evidence [set gRainQuery (word gRainQuery " , ")]
  ]

  set gRainQuery (word gRainQuery "), , states = c(")

  set x 0 ;iterates through all eNodes
  while [x < number-of-evidence] [


      set gRainQuery (word gRainQuery "'"item x evidence-list"'")
       set x x + 1
      if x < number-of-evidence [set gRainQuery (word gRainQuery " , ")]
  ]

  set gRainQuery (word gRainQuery "))")
  ;if show-me-? [ show gRainQuery]
  r:eval gRainQuery


  let gRainQuery2 (word "matrix.cpt <- querygrain(ev, nodes = c('"hypothesis-node"'), type = 'joint')")
  ;if show-me-? [ show gRainQuery2]
  r:eval gRainQuery2


  ;if show-me-? [ show (word "The gRain queries for the optimal posterior are " gRainQuery " and " gRainQuery2 ".")]


  ;******************************************************************

  if approximation = "repeater"[
    repeat repeater [
      let m r:get rQuery
      set optimal-posterior (optimal-posterior + m)
    ]
    set optimal-posterior (optimal-posterior / repeater)
  ]
  if approximation = "seed"[
    r:eval (word "set.seed("seed") ")
    set optimal-posterior r:get rQuery
  ]
  if approximation = "gRain"[

    set optimal-posterior r:get (word "probability_H_true <- as.numeric(matrix.cpt['yes'])")
  ]


  if show-me-? [show (word "The optimal posterior is " optimal-posterior)]
end

;******************************************************************
;COMMUNICATION SUBROUTINES ****************************************
;******************************************************************
to-report shouldShare?
  ;Three conditions need to obtain for agents to share evidence:
  ;1. They have already heard of a piece of evidence
  ;2. They need to be chatty enough
  ;3. They need to pass the conviction threshold
  ifelse length heard > 0 [
    let x random-float 1
    ifelse x < chattiness [

      let lowerBound (initial-belief - initial-belief * conviction-threshold)
      let upperBound (initial-belief + (1 - initial-belief) * conviction-threshold)

      ifelse (agent-belief < lowerBound OR agent-belief > upperBound)[
        report true
      ][
        if show-me-? [show (word "My belief does not pass the threshold.")]
        report false
      ]
    ][
      if show-me-? [show (word "I am not chatty right now.")]
      report false
    ]
  ][
    if show-me-? [show (word "I don't know anything, so I cannot share evidence.")]
    report false
  ]
end

to receiveShare [sharedPiece] ;sharedPiece is the list index the shared piece of evidence

  ;*****************************************************************************************************
  ;upon receiving a piece of evidence via communication, agents put it to the top of their simple-memory
  if   member? sharedPiece recency-list [
    set recency-list remove sharedPiece recency-list
  ]
  set recency-list lput sharedPiece recency-list
  if show-me-? and share = "recent" [show (word "recency-list: " recency-list)]
  ;*****************************************************************************************************

  ;Link-neighbors only accept the piece of evidence if they have not yet seen the parcel
  ifelse item sharedPiece [agent-evidence-list] of self != "-" [
    if show-me-? [show (word "I've heard " sharedPiece " before, thanks " [who] of myself". My list: " agent-evidence-list)]
  ][
    set agent-evidence-list replace-item sharedPiece agent-evidence-list (item sharedPiece evidence-list)
    set heard lput sharedPiece heard
    set last-heard sharedPiece
    compute-posterior


    if plotting-type = "received as novel" [ask myself [countArguments]]

    if show-me-? [show (word [who] of myself " told me " sharedPiece ", now I entertain:" agent-evidence-list " and believe " agent-belief"." )]
  ]
  if plotting-type = "sent to" [ask myself [countArguments]]
end

to randomshare
;This is the simplest of the sharing procedures: the agent chooses a random piece of evidence they have heard and shares it with their neighbours.
  let sharedPiece one-of heard
  if show-me-? [show (word "I want to communicate " sharedPiece)]
  set current-evidence sharedPiece
 if plotting-type = "uttered" [countArguments]


  ask link-neighbors [receiveShare sharedPiece]
end

to impactshare
;Strategic sharing procedures: the agent chooses the piece of evidence that most convinced them of their current position. They measure this by consulting ther "update-total" string.

  let sharedPiece "-"

  ;*************************************************************************************************************************************
  ;agents select the strongest positive piece of evidence if believing in HYP more than they started out, strongest negative piece of evidence otherwise
  ifelse agent-belief > initial-belief [
    let n max update-total
    set sharedPiece position n update-total
  ][
    let n min update-total
    set sharedPiece position n update-total
  ]
  ;*************************************************************************************************************************************

  if show-me-? [ show (word "I communicate " sharedPiece) ]
  set current-evidence sharedPiece
  if plotting-type = "uttered" [countArguments]

  ask link-neighbors [receiveShare sharedPiece]
end

to simplerecentshare

  ifelse length recency-list >= 1  [
    let sharedPiece "-"
    let n random-float 1

    ;*************************************************************************************************************************************
    ;agents have a 90% chance of sharing the last piece of evidence they encountered and–iff they have recently encountered more than once
    ;piece–a 10% chance of sharing the second to last

    ifelse n < 0.9 OR length recency-list = 1 [ ;HERE IS THE PROBABILITY OF PICKING THE MOST RECENT EVIDENCE
      set sharedPiece last recency-list
    ][
      let simple-memory-derivative (remove last recency-list recency-list)
      set sharedPiece one-of simple-memory-derivative
    ]
    ;*************************************************************************************************************************************

    if show-me-? [show (word "I want to communicate " sharedPiece)]

    set current-evidence sharedPiece
    if plotting-type = "uttered" [countArguments]
    if show-me-? [show (word "recency-list: " recency-list)]

    ask link-neighbors [receiveShare sharedPiece]

  ][
    if show-me-? [show (word "I don't remember anything from last round.")]
  ]
end

;******************************************************************
;NETWORK CREATION SUBROUTINES *************************************
;******************************************************************

to create-network-small-world
    nw:generate-watts-strogatz turtles links number-of-agents k rewiring-probability
  layout-circle sort turtles 10
end

to create-network-complete
  ask turtles [ create-links-with other turtles ]
  layout-circle sort turtles  10
end

to create-network-null
  layout-circle sort turtles  10
end

to create-network-cycle
  [turtle-list]
  let previous-turtle 0
  foreach turtle-list [ [cur-turtle] ->
    ask cur-turtle [
      ifelse previous-turtle != 0 [
        create-link-with previous-turtle
        set previous-turtle self
      ][
        create-link-with last turtle-list
        set previous-turtle self
      ]
    ]
  ]
  layout-circle sort turtles 10
end

to create-network-wheel
  let turtle-list sort turtles
  create-network-cycle but-first turtle-list
  ask first turtle-list [
    setxy 0 0
    create-links-with other turtles
  ]
end

;******************************************************************
;MISCELLANEOUS SUBROUTINES ****************************************
;******************************************************************

to initializeAgents

  ask turtles [

    ;******************************************************************************************************
    ;This is the agent's initial belief in the hypothesis, determined by the CPT.
    let rQuery (word "cpquery("Bayes-net", event = ("Hypothesis-node" == 'yes'), evidence = TRUE)")

    ;gRain
    let gRainQuery "evg <- setEvidence(gRainbn, nodes = TRUE, states = TRUE)"
    let gRainQuery2 (word "matrix.cpt <- querygrain(evg, nodes = c('"hypothesis-node"'), type = 'joint')")
    ;if show-me-? [show (word "gRainQueries are " gRainQuery " and " gRainQuery2 ".")]
    r:eval gRainQuery
    r:eval gRainQuery2

    let r 0
    if approximation = "repeater"[
      repeat repeater [let v r:get rQuery
        set r (r + v)]
      set r (r / repeater)]
    if approximation = "seed"[
      r:eval (word "set.seed("seed") ")
      let v r:get rQuery
      set r v ]
    if approximation = "gRain" [
      set r r:get (word "probability_H_true <- as.numeric(matrix.cpt['yes'])")
    ]

    set agent-belief r
    set initial-belief r
    ;******************************************************************************************************

    set agent-evidence-list []
    set heard [] ;Stores which pieces of evidence the agent has heard, in order of reception.
    set recency-list []
    set last-heard "none" ;Stores the piece of evidence the agent has last heard.
    set draws maximum-draws ;Agents may only "draw" from the evidence a limited amount of times. If maximum-draws = number-of-evidence, then full information is assured
    set update-total [] ;Stores the size of the "total" update: P(HYP|E_{i+1}) - P(HYP); where E_{i+1} is what the agent has just learned.

    repeat number-of-evidence [
      set agent-evidence-list lput "-" agent-evidence-list
      set update-total lput "-" update-total
    ]

    ;******************************************************************************************************
    ;Pre-draws
    ;[UI toggle] Either each agent performs random initial draws, or they re-draw their previous initial draws
    ifelse reset-agents-initial-evidence-? [
      repeat initial-draws [collectEvidence "random"]
      set initial-draws-list agent-evidence-list
    ][
      set agent-evidence-list initial-draws-list
      let x 0
      while [x < number-of-evidence] [
        if item x agent-evidence-list != "-" [collectEvidence x]
        set x x + 1
      ]
    ]
    ;******************************************************************************************************

    change-colors

    if show-me-? [show (word "my list " agent-evidence-list)]
  ]

end

to reset-evidence
  ;******************************************************************************************************
    ;This is the agent's initial belief in the hypothesis, determined by the CPT.
    let rQuery (word "cpquery("Bayes-net", event = ("Hypothesis-node" == 'yes'), evidence = TRUE)")


    ;gRain
    let gRainQuery "evg <- setEvidence(gRainbn, nodes = TRUE, states = TRUE)"
    let gRainQuery2 (word "matrix.cpt <- querygrain(evg, nodes = c('"hypothesis-node"'), type = 'joint')")
    ;if show-me-? [show (word "gRainQueries are " gRainQuery " and " gRainQuery2 ".")]
      r:eval gRainQuery
      r:eval gRainQuery2


    ;******************************************************************************************************
    let r 0
    if approximation = "repeater"[
      repeat repeater [let v r:get rQuery
        set r (r + v)]
      set r (r / repeater)]
    if approximation = "seed"[
      r:eval (word "set.seed("seed") ")
      let v r:get rQuery
      set r v ]

    if approximation = "gRain" [
   set r r:get (word "probability_H_true <- as.numeric(matrix.cpt['yes'])")

  ]





  let c random-float 1
  ifelse c < hypothesis-probability [set hypothesis-value "yes"][set hypothesis-value "no"]

  set evidence-probabilities-list []
  repeat number-of-evidence [
    set evidence-probabilities-list lput "-" evidence-probabilities-list
  ]
  set evidence-list []

  ;*********************************************************************************************************************************************
  ;For each piece of evidence, the rquery (using either seed or repeater) determines the conditional probability given the actual value of HYP
  ;Then, we use that probability to determine the actual value of each piece  of evidence
  let x 0
  while [x < number-of-evidence] [
    set r 0

    if approximation = "repeater" [
      repeat repeater[
      set r r + (r:get (word "cpquery(" Bayes-net ", event = ("item x evidence-nodes" == 'yes'), evidence = (" Hypothesis-node " == '" hypothesis-value "'))"))
    ]
    set r r / repeater
    ]

    if approximation = "seed" [
      r:eval (word "set.seed("seed") ")
      set r (r:get (word "cpquery(" Bayes-net ", event = ("item x evidence-nodes" == 'yes'), evidence = (" Hypothesis-node " == '" hypothesis-value "'))"))
    ]

   if approximation = "gRain"
    [

  r:eval  (word "ev <- setEvidence(gRainbn, nodes = c('"Hypothesis-node"'), states = c('"hypothesis-value"'))"  )
  r:eval (word "matrix.cpt <- querygrain(ev, nodes = c('"item x evidence-nodes"'), type = 'joint')")
 set r r:get (word "probability_H_true <- as.numeric(matrix.cpt['yes'])")

    ]

    let n random-float 1
    ifelse (n < r) [set evidence-list lput "yes" evidence-list][set evidence-list lput "no" evidence-list ]
    set evidence-probabilities-list (replace-item x evidence-probabilities-list (precision r 2))



    set x x + 1
  ]
  ;*********************************************************************************************************************************************



  if show-me-?[
    show  (word "Probabilities that evidence nodes are true, given truth/falsity of hypothesis: " evidence-probabilities-list)
    show (word "Evidence list: " evidence-list)
  ]

  set arguments [] ;resets arguments counter
  repeat number-of-evidence [
      set arguments lput 0 arguments
  ]

  if maximum-draws > number-of-evidence [
    set maximum-draws number-of-evidence
    print "Note: automatically adjusted [maximum-draws]."
  ]
  if initial-draws > maximum-draws [
    set initial-draws maximum-draws
    print "Note: automatically adjusted [initial-draws]."
  ]



 end

to my-clear-all
show (word "------------------------")
  show "**NEW RUN**"
  show (word "------------------------")

  ; manually clears the globals we want to reset, leaves retain-me alone.
  clear-ticks
  set optimal-posterior 0

  clear-patches
  clear-drawing
  clear-all-plots
  clear-output
end

to countArguments
  if current-evidence != "null" [set arguments replace-item current-evidence arguments (item current-evidence arguments + 1)] ;counts current evidence as an argument used
end

to setupPlots
  let x 0
  while [x < number-of-evidence] [
    create-temporary-plot-pen item x evidence-nodes
    ifelse x < 13 [set-plot-pen-color 5 + (x * 10)][set-plot-pen-color 0]
    set x x + 1
  ]
  update-plots
end

to plotArguments

  let x 0
  while [x < number-of-evidence] [
    set-current-plot-pen item x evidence-nodes
    plot item x arguments
    set x x + 1
  ]

  set arguments [] ;resets arguments counter
  repeat number-of-evidence [
    set arguments lput 0 arguments

    ]
end

to-report  check-stoping-conditions
  let stop? false

  if stop-at-full-information-? [
    if not any? turtles with [length heard  < number-of-evidence ][
      show "*** State of full information reached ***"
      set stop? true
    ]
  ]

  if stop-at-max-ticks-? [
    if ticks >= max-ticks [
      show "*** Upper tick-limit reached ***"
      set stop? true
    ]
  ]


  report stop?
end

;******************************************************************
;DAG-RELATED SUBROUTINES ******************************************
;******************************************************************

to loadDAG
  if approximation = "gRain" [r:eval "library(gRain)"]
  r:eval "library (bnlearn)" ;the R library that lets us use Bayes' nets

  if causal-structure = "big net"[LoadBigNet]
  if causal-structure = "small net"[LoadSmallNet]
  if causal-structure = "asia" [loadAsiaNet]
  if causal-structure = "alarm" [loadAlarmNet]
  if causal-structure = "Vole" [loadVoleNET]
  if causal-structure = "WetGrass" [loadWetGrassNet]
  if causal-structure = "SallyClark" [loadSallyClarkNet]

  if causal-structure = "custom" [
    ;######Upload your custom net here. Either use a whole file via using the UI and this line: <r:eval (word "source('" pathToCustomDAG "')")>
    ;OR copy paste the R script's lines here. Needed:  1. a dag, b. a probability distribution/data fitted onto the dag ("bn.fit")
    ;Make sure each line is embedded like so:  r:eval "R-SCRIPT-LINE". Make sure the syntax is correct (e.g., R is sensitive to small differences such as punctuation).
    ;#####################COPY-PASTE BEGIN


      r:eval (word "source('" path-to-custom-DAG "')")



    ;#####################COPY-PASTE END
    if approximation = "gRain" [r:eval "gRainbn <- compile(as.grain(bn))"]
    useCustomEvidenceAndHypothesisNodes
  ]


  set Bayes-net "bn"
  set number-of-evidence length evidence-nodes
end

to useCustomEvidenceAndHypothesisNodes
  set evidence-nodes []
  let tempString evidence-nodes-custom-DAG

  while [position "\n" tempString != FALSE] [
    let nextNode  (substring tempString 0 position "\n" tempString)
    set evidence-nodes lput nextNode evidence-nodes
    repeat (position "\n" tempString + 1) [set tempString (but-first tempString)]
  ]
  set evidence-nodes lput tempString evidence-nodes

  set hypothesis-node hypothesis-node-custom-DAG
end

to loadAsiaNet
   r:eval  "data(asia)"
   r:eval  "dag <- model2network('[A][S][T|A][L|S][B|S][D|B:E][E|T:L][X|E]')"
   r:eval  "bn <- bn.fit(dag, asia)"
    if approximation = "gRain" [r:eval "gRainbn <- compile(as.grain(bn))"]

  ifelse custom-evidence-and-hypothesis-? = false [
    set hypothesis-node "L"
    set evidence-nodes ["A" "S" "T" "B" "D" "X"]
  ][useCustomEvidenceAndHypothesisNodes]

end

to loadVoleNET
r:eval "dag1 <- model2network('[M1][M2][M3|M1:M2][Vole_present][H0|M3:Vole_present][H1|H0][H5][E6|H5][A3|H5:H1][E7|H5:A3:H1][A1][E1|Vole_present:A1][A2|H0][E2|A2:Vole_present][H3][E5|H3][H2|H0][H4|H3][E4|H4][E3|H2:H4][Auxilliary|H2:H4][Constraint|Auxilliary]')"
r:eval "cptM1 <- array(c(0.5,0.5), dim = 2, dimnames = list(M1 = c('no', 'yes')))"
r:eval "cptM2 <- array(c(0.5,0.5), dim = 2, dimnames = list(M2 = c('no', 'yes')))"
r:eval "cptVole_present <- array(c(0.5,0.5), dim = 2, dimnames = list(Vole_present = c('no', 'yes')))"
r:eval "cptA1 <- array(c(0.5,0.5), dim = 2, dimnames = list(A1 = c('no', 'yes')))"
r:eval "cptH3 <- array(c(0.5,0.5), dim = 2, dimnames = list(H3 = c('no', 'yes')))"
r:eval "cptH5 <- array(c(0.5,0.5), dim = 2, dimnames = list(H5 = c('no', 'yes')))"
r:eval "cptH1 <- array(c(1,0,0.5,0.5), dim = c(2,2), dimnames = list(H1 = c('no', 'yes'), H0 =c('no', 'yes')))"
r:eval "cptH2 <- array(c(0.9,0.1,0.3,0.7), dim = c(2,2), dimnames = list(H2 = c('no', 'yes'), H0 =c('no', 'yes')))"
r:eval "cptH2 <- array(c(0.9,0.1,0.3,0.7), dim = c(2,2), dimnames = list(H2 = c('no', 'yes'), H0 =c('no', 'yes')))"
r:eval "cptE5 <- array(c(0.99,0.01,0.1,0.9), dim = c(2,2), dimnames = list(E5 = c('no', 'yes'), H3 =c('no', 'yes')))"
r:eval "cptH4 <- array(c(1.0,0.0,0.1,0.9), dim = c(2,2), dimnames = list(H4 = c('no', 'yes'), H3 =c('no', 'yes')))"
r:eval "cptE4 <- array(c(0.99,0.01,0,1), dim = c(2,2), dimnames = list(E4 = c('no', 'yes'), H4 =c('no', 'yes')))"
r:eval "cptConstraint <- array(c(0.283,0.717,0.45,0.55,1,0), dim = c(2,3), dimnames = list(Constraint = c('no', 'yes'), Auxilliary = c('H2','H4','impossible')))"
r:eval "cptA2 <- array(c(0,1,0.9,0.1), dim = c(2,2), dimnames = list(A2 = c('no', 'yes'), H0 =c('no', 'yes')))"
r:eval "cptE6 <- array(c(0.99,0.01,0.1,0.9), dim = c(2,2), dimnames = list(E6 = c('no', 'yes'), H5 =c('no', 'yes')))"
r:eval "cptM3 <- array(c(0.9,0.1,0.3,0.7,0.4,0.6,0.2,0.8), dim = c(2,2,2),  dimnames = list(M3 = c('no','yes'),M2 = c('no', 'yes'),M1 = c('no', 'yes')))"
r:eval "cptH0 <- array(c(1.0,0.0,1.0,0.0,0.5,0.5,0.3,0.7), dim = c(2,2,2), dimnames = list(H0 = c('no','yes'),M3 = c('no', 'yes'),Vole_present = c('no', 'yes')))"
r:eval "cptE3 <- array(c(1,0,0.99,0.01,0,1,0,1), dim = c(2,2,2), dimnames = list(E3 = c('no','yes'), H4 = c('no', 'yes'), H2= c('no', 'yes')))"
r:eval "cptAuxilliary <-array(c(0,0,1,0,1,0,1,0,0,0,0,1), dim = c(3,2,2), dimnames = list(Auxilliary = c('H2','H4','impossible'), H4 = c('no', 'yes'), H2= c('no', 'yes')))"
r:eval "cptE1 <- array(c(0.5,0.5,0.5,0.5,1,0,0,1), dim = c(2,2,2), dimnames = list(E1 = c('no','yes'), Vole_present= c('no', 'yes'), A1= c('no', 'yes')))"
r:eval "cptE2 <- array(c(0,1,0.4,0.6,0,1,1,0), dim = c(2,2,2), dimnames = list(E2 = c('no','yes'), Vole_present= c('no', 'yes'), A2= c('no', 'yes')))"
r:eval "cptA3 <- array(c(0.9,0.1,0.001,0.999,1,0,0.3,0.7), dim = c(2,2,2),  dimnames = list(A3 = c('no','yes'), H1= c('no', 'yes'), H5= c('no', 'yes')))"
r:eval "cptE7 <- array(c(1,0,1,0,0.2,0.8,1,0,1,0,0.4,0.6,0.01,0.99,0,1),dim = c(2,2,2,2), dimnames = list(E7 = c('no','yes'), A3= c('no', 'yes'), H5= c('no', 'yes'),H1= c('no', 'yes')))"
r:eval "cpt1 <- list( H0 = cptH0, M1 = cptM1, M2 = cptM2, M3 = cptM3, Vole_present = cptVole_present, E1 = cptE1, A1 = cptA1, E2 = cptE2, A2 = cptA2, H1 = cptH1, H5 = cptH5, E6 = cptE6, E7 = cptE7, A3 = cptA3, H2 = cptH2, H3 = cptH3, H4 = cptH4, E3 = cptE3, E4 = cptE4, E5 = cptE5, Auxilliary = cptAuxilliary, Constraint = cptConstraint)"
r:eval "bn <- custom.fit(dag1, cpt1)"
 if approximation = "gRain" [r:eval "gRainbn <- compile(as.grain(bn))"]

  ifelse custom-evidence-and-hypothesis-? = false [
    set hypothesis-node "H0"
    set evidence-nodes ["E1" "E2" "E3" "E4" "E5" "E6" "E7"]
  ][
    useCustomEvidenceAndHypothesisNodes
  ]

end

to loadAlarmNet
  r:eval "data(alarm)"
  r:eval "dag <- model2network('[HIST|LVF][CVP|LVV][PCWP|LVV][HYP][LVV|HYP:LVF][LVF][STKV|HYP:LVF][ERLO][HRBP|ERLO:HR][HREK|ERCA:HR][ERCA][HRSA|ERCA:HR][ANES][APL][TPR|APL][ECO2|ACO2:VLNG][KINK][MINV|INT:VLNG][FIO2][PVS|FIO2:VALV][SAO2|PVS:SHNT][PAP|PMB][PMB][SHNT|INT:PMB][INT][PRSS|INT:KINK:VTUB][DISC][MVS][VMCH|MVS][VTUB|DISC:VMCH][VLNG|INT:KINK:VTUB][VALV|INT:VLNG][ACO2|VALV][CCHL|ACO2:ANES:SAO2:TPR][HR|CCHL][CO|HR:STKV][BP|CO:TPR]')"
  r:eval  "bn <- bn.fit(dag, alarm)"
  ;r:eval "gRainbn <- compile(as.grain(bn))"

  ifelse custom-evidence-and-hypothesis-? = false [
    set hypothesis-node "CVP"
    set evidence-nodes ["HIST" "CO"]
  ][
    useCustomEvidenceAndHypothesisNodes
  ]

end

to loadWetGrassNet

  r:eval "library (bnlearn)"

  r:eval "dag1 <- model2network('[Rain][Sprinkler][Watson|Rain][Holmes|Rain:Sprinkler]')"

  r:eval "cptRain <- array(c(0.2,0.8), dim = 2, dimnames = list(Rain = c('yes', 'no')))"
  r:eval "cptSprinkler <- array(c(0.1,0.9), dim = 2, dimnames = list(Sprinkler = c('yes', 'no')))"
  r:eval "cptWatson <- array(c(1.0,0.0,0.2,0.8), dim = c(2,2), dimnames = list(Watson = c('yes', 'no'),Rain = c('yes', 'no')))"
  r:eval "cptHolmes <- array(c(1.0, 0.0, 0.9, 0.1,1.0,0.0,0.0,1.0), dim = c(2,2,2), dimnames = list(Holmes = c('yes', 'no'), Rain = c('yes', 'no'), Sprinkler = c('yes', 'no')) )"
  r:eval "cpt1 <- list(Rain = cptRain,Sprinkler = cptSprinkler, Watson = cptWatson,  Holmes =cptHolmes)"
  r:eval "bn <- custom.fit(dag1, cpt1)"
  if approximation = "gRain" [r:eval "gRainbn <- compile(as.grain(bn))"]

  ifelse custom-evidence-and-hypothesis-? = false [
    set hypothesis-node "Sprinkler"
    set evidence-nodes ["Holmes" "Watson"]
  ][
    useCustomEvidenceAndHypothesisNodes
  ]

end

to loadSallyClarkNet

  ;NOTE: I changed SIDS to NO and MURDER to YES.
r:eval "dag1 <- model2network('[ChildACause][ChildBCause|ChildACause][Guilty|Findings][Findings|ChildACause:ChildBCause][ChildABruising|ChildACause][ChildADisease|ChildACause][ChildBBruising|ChildBCause][ChildBDisease|ChildBCause]')"
r:eval "cptChildACause <- array(c(0.921659,0.07834101), dim = 2, dimnames = list(ChildACause = c('no', 'yes')))"
r:eval "cptChildBCause <- array(c(0.9993604, 6.3959067E-4,1.4622862E-4, 0.9998538), dim = c(2, 2), dimnames = list(ChildBCause = c('no', 'yes'),ChildACause = c('no', 'yes')))"
r:eval "cptChildABruising <- array(c(0.99,0.01,0.95,0.05), dim = c(2, 2), dimnames = list(ChildABruising = c('no', 'yes'), ChildACause = c('no', 'yes')))"
r:eval "cptChildBBruising <- array(c(0.99,0.01,0.95,0.05), dim = c(2, 2), dimnames = list(ChildBBruising = c('no', 'yes'), ChildBCause = c('no', 'yes')))"
r:eval "cptChildADisease <- array(c(0.95,0.05,0.999,0.001), dim = c(2, 2), dimnames = list(ChildADisease = c('no', 'yes'),ChildACause = c('no', 'yes')))"
r:eval "cptChildBDisease <- array(c(0.95,0.05,0.999,0.001), dim = c(2, 2), dimnames = list(ChildBDisease = c('no', 'yes'),ChildBCause = c('no', 'yes')))"
r:eval "cptGuilty <- array(c(0.0,1.0,0.0,1.0,1.0,0.0), dim = c(2, 3), dimnames = list(Guilty = c('no', 'yes'), Findings = c('Both Murdered', 'Either Murdered', 'Neither Murdered')))"
r:eval "cptFindings <- array(c(0.0,0.0,1.0,0.0,1.0,0.0,0.0,1.0,0.0,1.0,0.0,0.0), dim = c(3,2,2), dimnames = list(Findings = c('Both Murdered', 'Either Murdered', 'Neither Murdered'), ChildBCause = c('no', 'yes'), ChildACause = c('no', 'yes')))"
r:eval "cpt1 <- list(ChildABruising = cptChildABruising, ChildADisease = cptChildADisease, ChildBBruising = cptChildBBruising, ChildBDisease = cptChildBDisease, ChildACause = cptChildACause, ChildBCause = cptChildBCause, Findings = cptFindings, Guilty = cptGuilty)"
r:eval "bn <- custom.fit(dag1, cpt1)"
   if approximation = "gRain" [r:eval "gRainbn <- compile(as.grain(bn))"]
  ifelse custom-evidence-and-hypothesis-? = false [
    set hypothesis-node "Guilty"
    set evidence-nodes ["ChildADisease" "ChildBDisease" "ChildABruising" "ChildBBruising"]
  ][
    useCustomEvidenceAndHypothesisNodes
  ]

end

to loadBigNet
  r:eval  "dag1 <- model2network('[A][B|A][C|A][D|A][one|B][two|B][three|B][four|C][five|C][six|C][seven|D][eight|D][nine|D]')"
  r:eval  "cptA <- array(c(0.5,0.5), dim = 2, dimnames = list(A = c('yes', 'no')))"
  r:eval  "cptB <- array(c(0.9, 0.1, 0.1, 0.9), dim = c(2, 2), dimnames = list(B = c('yes', 'no'), A = c('yes', 'no')))"
  r:eval  "cptC <- array(c(0.5, 0.5, 0.5, 0.5), dim = c(2, 2), dimnames = list(C = c('yes', 'no'), A = c('yes', 'no')))"
  r:eval  "cptD <- array(c(0.1, 0.9, 0.9, 0.1), dim = c(2, 2), dimnames = list(D = c('yes', 'no'), A = c('yes', 'no')))"
  r:eval  "cptone <- array(c(0.9, 0.1, 0.1, 0.9), dim = c(2, 2), dimnames = list(one = c('yes', 'no'), B = c('yes', 'no')))"
  r:eval  "cpttwo <- array(c(0.8, 0.2, 0.2, 0.8), dim = c(2, 2), dimnames = list(two = c('yes', 'no'), B = c('yes', 'no')))"
  r:eval  "cptthree <- array(c(0.7, 0.3, 0.3, 0.7), dim = c(2, 2), dimnames = list(three = c('yes', 'no'), B = c('yes', 'no')))"
  r:eval  "cptfour <- array(c(0.9, 0.1, 0.1, 0.9), dim = c(2, 2), dimnames = list(four = c('yes', 'no'), C = c('yes', 'no')))"
  r:eval  "cptfive <- array(c(0.8, 0.2, 0.2, 0.8), dim = c(2, 2), dimnames = list(five = c('yes', 'no'), C = c('yes', 'no')))"
  r:eval  "cptsix <- array(c(0.7, 0.3, 0.3, 0.7), dim = c(2, 2), dimnames = list(six = c('yes', 'no'), C = c('yes', 'no')))"
  r:eval  "cptseven <- array(c(0.9, 0.1, 0.1, 0.9), dim = c(2, 2), dimnames = list(seven = c('yes', 'no'), D = c('yes', 'no')))"
  r:eval  "cpteight <- array(c(0.8, 0.2, 0.2, 0.8), dim = c(2, 2), dimnames = list(eight = c('yes', 'no'), D = c('yes', 'no')))"
  r:eval  "cptnine <- array(c(0.7, 0.3, 0.3, 0.7), dim = c(2, 2), dimnames = list(nine = c('yes', 'no'), D = c('yes', 'no')))"
  r:eval  "cpt1 <- list(A = cptA, B = cptB, C = cptC, D = cptD, one = cptone, two = cpttwo, three = cptthree, four = cptfour, five = cptfive, six = cptsix, seven = cptseven, eight = cpteight, nine = cptnine)"
  r:eval  "bn <- custom.fit(dag1, cpt1)"
  if approximation = "gRain" [r:eval "gRainbn <- compile(as.grain(bn))"]

  ifelse custom-evidence-and-hypothesis-? = false [
    set hypothesis-node "A"
    set evidence-nodes [ "one" "two" "three" "four" "five" "six" "seven" "eight" "nine"]
  ][
    useCustomEvidenceAndHypothesisNodes
  ]
end

to loadSmallNet
  r:eval  "dag <- model2network('[V][CS|V][VT|V][M|CS][I|CS][WHO|VT][RS|VT]')"
  r:eval  "cptV <- array(c(0.5,0.5), dim = 2, dimnames = list(A = c('yes', 'no')))"
  r:eval  "cptCS <- array(c(0.9, 0.1, 0.1, 0.9), dim = c(2, 2), dimnames = list(CS = c('yes', 'no'), V = c('yes', 'no')))"
  r:eval  "cptVT <- array(c(0.8, 0.2, 0.2, 0.8), dim = c(2, 2), dimnames = list(VT = c('yes', 'no'), V = c('yes', 'no')))"
  r:eval  "cptI  <- array(c(0.7, 0.3, 0.3, 0.7), dim = c(2, 2), dimnames = list(I = c('yes', 'no'), CS = c('yes', 'no')))"
  r:eval  "cptM  <- array(c(0.8, 0.2, 0.2, 0.8), dim = c(2, 2), dimnames = list(M = c('yes', 'no'), CS = c('yes', 'no')))"
  r:eval  "cptWHO <- array(c(0.8, 0.2, 0.2, 0.8), dim = c(2, 2), dimnames = list(WHO = c('yes', 'no'), VT = c('yes', 'no')))"
  r:eval  "cptRS <- array(c(0.2, 0.8, 0.8, 0.2), dim = c(2, 2), dimnames = list(RS = c('yes', 'no'), VT = c('yes', 'no')))"
  r:eval  "cpt <- list(V = cptV, VT = cptVT, CS = cptCS, I = cptI, M = cptM, WHO = cptWHO, RS = cptRS)"
  r:eval  "bn <- custom.fit(dag, cpt)"
  if approximation = "gRain" [r:eval "gRainbn <- compile(as.grain(bn))"]

  ifelse custom-evidence-and-hypothesis-? = false [
    set evidence-nodes ["I" "M" "WHO" "RS"]
    set hypothesis-node "V"
  ][
    useCustomEvidenceAndHypothesisNodes
  ]
end
@#$#@#$#@
GRAPHICS-WINDOW
9
10
367
369
-1
-1
10.61
1
10
1
1
1
0
0
0
1
-16
16
-16
16
0
0
1
ticks
30.0

SLIDER
885
84
1040
117
number-of-agents
number-of-agents
1
1000
10.0
1
1
NIL
HORIZONTAL

BUTTON
380
171
457
205
setup
setup
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SWITCH
607
126
760
159
show-me-?
show-me-?
1
1
-1000

BUTTON
531
172
594
205
NIL
go
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
462
172
525
205
NIL
go
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

CHOOSER
885
37
1040
82
network
network
"wheel" "complete" "small-world" "null"
0

PLOT
848
377
1353
630
Histogram of beliefs
Degree of belief
Number of people
0.0
1.01
0.0
10.0
true
false
"" ""
PENS
"default" 0.01 1 -16777216 true "" "histogram [agent-belief] of turtles"

SLIDER
1056
175
1229
208
initial-draws
initial-draws
0
10
1.0
1
1
NIL
HORIZONTAL

CHOOSER
1056
211
1210
256
share
share
"random" "impact" "recent"
0

MONITOR
835
320
945
365
optimal-posterior
optimal-posterior
4
1
11

MONITOR
376
272
831
317
evidence-list
evidence-list
17
1
11

SLIDER
1250
286
1390
319
repeater
repeater
1
100
1.0
1
1
NIL
HORIZONTAL

SLIDER
1056
32
1228
65
chattiness
chattiness
0
1
1.0
0.01
1
NIL
HORIZONTAL

SLIDER
1056
69
1229
102
conviction-threshold
conviction-threshold
0
1
0.0
0.01
1
NIL
HORIZONTAL

SLIDER
1056
105
1228
138
curiosity
curiosity
0
1
1.0
0.01
1
NIL
HORIZONTAL

SLIDER
1056
140
1228
173
maximum-draws
maximum-draws
0
10
4.0
1
1
NIL
HORIZONTAL

TEXTBOX
1056
10
1206
28
Agent variables
11
0.0
1

SLIDER
885
117
1040
150
k
k
0
10
2.0
1
1
NIL
HORIZONTAL

SLIDER
885
152
1041
185
rewiring-probability
rewiring-probability
0
1
0.2
0.01
1
NIL
HORIZONTAL

CHOOSER
1251
241
1389
286
approximation
approximation
"gRain" "seed" "repeater"
0

PLOT
9
377
845
630
The rise and fall of arguments
ticks
# of usages
0.0
10.0
0.0
1.0
true
true
"" ""
PENS

SLIDER
1245
35
1465
68
hypothesis-probability
hypothesis-probability
0
1
1.0
0.01
1
NIL
HORIZONTAL

TEXTBOX
1245
10
1395
28
The World
11
0.0
1

MONITOR
835
273
945
318
hypothesis-value
hypothesis-value
17
1
11

CHOOSER
1246
75
1413
120
causal-structure
causal-structure
"big net" "small net" "asia" "alarm" "SallyClark" "Vole" "WetGrass" "custom"
2

INPUTBOX
250
748
786
808
path-to-custom-DAG
/this/is/the/file/path/to/where/you/stored/your/customDAG.R
1
0
String

TEXTBOX
256
646
861
751
If you want to load a [causal-structure] via an external R-file, please manually enter the [path-to-custom-DAG]. In addition, please decide upon a [hypothesis-node-custom-DAG], and upon [evidence-nodes-custom-DAG].\n\nIf you are using a pre-set [causal-structure], you may either use the preset [evidence-nodes] and [hypothesis-node], or enter your own selection and toggle [custom-evidence-and-hypothesis-?] on.
11
0.0
1

INPUTBOX
1250
172
1321
232
seed
4.0
1
0
Number

TEXTBOX
375
10
763
40
When first initializing the model or when switching [causal-structure], toggle on [reset-world-?] before using [setup] .
11
0.0
1

INPUTBOX
10
636
249
870
evidence-nodes-custom-DAG
E1\nE2\nE3\nE4
1
1
String

INPUTBOX
250
810
456
870
hypothesis-node-custom-DAG
H0
1
0
String

SWITCH
457
810
767
843
custom-evidence-and-hypothesis-?
custom-evidence-and-hypothesis-?
1
1
-1000

SWITCH
370
56
523
89
reset-world-?
reset-world-?
0
1
-1000

MONITOR
376
225
831
270
evidence-nodes
evidence-nodes
17
1
11

MONITOR
835
226
945
271
NIL
hypothesis-node
17
1
11

SWITCH
370
91
555
124
reset-social-network-?
reset-social-network-?
0
1
-1000

SWITCH
370
126
605
159
reset-agents-initial-evidence-?
reset-agents-initial-evidence-?
0
1
-1000

SWITCH
557
91
760
124
stop-at-full-information-?
stop-at-full-information-?
1
1
-1000

TEXTBOX
887
15
1037
33
Social network
11
0.0
1

MONITOR
376
320
831
365
evidence-probabilities-list
evidence-probabilities-list
3
1
11

TEXTBOX
1248
146
1384
167
BnLearn variables\n
11
0.0
1

CHOOSER
646
162
759
207
plotting-type
plotting-type
"uttered" "sent to" "received as novel"
0

INPUTBOX
785
100
860
160
max-ticks
10.0
1
0
Number

SWITCH
526
56
760
89
stop-at-max-ticks-?
stop-at-max-ticks-?
0
1
-1000

@#$#@#$#@
## WHAT IS IT?

NormAN 1.0 is the first implementation of the NormAN framework. NormAN – short for Normative Argument Exchange across Networks – is a framework for agent-based modelling of argument exchange in social networks, first presented in ’A Bayesian Agent-Based Framework for Argument Exchange Across Networks’. A detailed explanation of all model parameters can be found there. (Full reference: Assaad, L., Fuchs, R., Jalalimanesh, A., Phillips, K., Schöppl, L. & Hahn, U. (2023). ’A Bayesian Agent-Based Framework for Argument Exchange Across Networks’.) 

**This is the model’s process in a nutshell:** To argue about a central claim, agents exchange arguments for believing that claim to be true or false. What evidence exists is determined by the world and its causal structure: for example, if a patient has lung cancer, it is more likely than not that they have shortness of breath. NormAN uses this to generate ‘worlds’ in which argument exchange takes place. Agents can collect evidence from the world and they can communicate with one another by sharing that evidence as arguments across the social network.

To capture this process, NormAN has three components: a ground truth ‘world’ model, individual agents, and the social network across which these agents communicate. 

* The ground truth world determines the true state of the claim at issue, along with the evidence for it that could be discovered in principle. 

* Agents receive evidence about that world (through inquiry) and may communicate that evidence to others as arguments and receive it in turn. Agents aggregate all evidence/arguments that they have encountered to form a degree of belief in the target
claim. To this end, they use Bayes’ rule. 

* Communication, finally, takes place across a social network.

NormAN 1.0 offers the user the possibility to change each of these components and thus explore a variety of different argumentation scenarios. The model captures different communication styles, different world models and social networks. Issues that can be addressed with NormAN include (but are not limited to): polarization and the emergence of consensus, the diffusion of arguments across social networks, the truth-tracking potential of deliberative bodies, and many more.

## HOW TO USE IT?

Follow these steps to quickly initialize the model: 

1. When first opening the model, make sure ‘reset-world-?’
and ‘reset-social-network-?’ are on. 
2. The World: choose a ‘causal-structure’ (chooser). 
3. The social network:
choose a ‘social-network’ (chooser) and a ‘number-of-agents’ (slider). 
4. Click setup: the social network will appear in the interface, the right bottom monitor will show a histogram of agent-beliefs and the middle output will
show which pieces of evidence are true. 
5. Press ‘go’ to start the simulation 

By clicking ‘setup’, users can wholly re-initialize the model, or keep some model facets fixed. Switching ‘reset-world-?’ on resets the truth values of the evidence nodes when ‘ setup’ is pressed. ‘reset-social-network-?’ resets all agents and the social network that connects them. ‘reset-agents-initial-evidence-?’ resets the set of evidence that each agent starts with upon initialization (only relevant if initial-draws>0). Note that because these facets are interconnected, ‘reset-world-?’ triggers‘reset-agents-initial-evidence?’ and so does ‘reset-social-network-?’.

If you would like to monitor what each agent does each round, toggle on ‘show-me-?’: each agent will output their exact step-by-step behaviour as lines in the Command Center. 

The ‘plotting-type’ chooser gives three options for visualizing the frequency of shared arguments (in the plot entitled ‘The rise and fall of arguments’): ‘uttered’ tracks how many times a piece of evidence has been shared each round. ‘sent-to’ tracks how many agents an argument was sent to. ‘received-as-novel’ tracks how many times
an argument was received as a novelty by an agent.

## THINGS TO NOTICE

There are many things to notice. For example, notice how the dynamics of the evidence exchange (illustrated in the window ‘The rise and fall of arguments’) change when you alter the agents’ sharing rule (e.g., from impact to random). Also, observe how using less dense social networks (e.g., from complete to wheel) slows down the evolution of the agents’ beliefs.

## THINGS TO TRY

A phenomenon of interest in NormAN is whether agents reach a consensus (see window ‘Histogram of beliefs’). This will depend on all the factors mentioned above, and changing any model component can help or hinder consensus building. It is, therefore, a worthwhile endeavour to determine which parameter combinations lead to consensus.

## EXTENDING THE MODEL

As explained in ‘A Bayesian Agent-Based Framework for Argument Exchange Across Networks’ (Assaad et al., 2023), NormAN can (and should be) extended in many ways. One thing you can do easily is add new Bayesian networks to NormAN. Additional Bayesian networks must be written in bnlearn code (as in our code) and can be transferred to the NetLogo model via a path to an external R (bnlearn) file by entering the file path in ‘path-to-custom-dag’. Alternatively, you can "copy and paste" the bnlearn file into our NetLogo code (in the designated area at the bottom). Here is the step-by-step protocol to do this: 

1. Create bnlearn code of a Bayesian network.
2. In the UI: Select ‘causal-net = custom’. Specify which nodes ought to count as evidence (‘evidence-Nodes- Custom-DAG’) and hypothesis. Make sure you write each node’s name into one separate line (as in the preset). 
3. Go into the code. Copy and paste the code into the indicated slot (under "LoadCustomNet“). Make sure each line is embedded like so: r:eval "library(bnlearn)“. Make sure the syntax is correct (e.g., R is sensitive to differences such in punctuation, and NetLogo prefers ‘<-’ to ‘=’). Lastly, note that the evidence and hypothesis nodes must take the values ‘yes’ and ‘no’. In the preset ‘Sally Clark’ network, for instance, we changed the values ‘SIDS’ to ‘no’ and ‘murder’ to ‘yes’.

For those who are familiar with NetLogo, it should also be straightforwardly possible to add new communication rules and new social networks.

## CREDITS AND REFERENCES

**SOFTWARE**

* **NetLogo R extension**
Thiele, JC; Grimm, V (2010). “NetLogo meets R: Linking agent-based models with a toolbox for their analysis.” Environmental Modelling and Software, Volume 25, Issue 8: 972 - 974 [DOI: 10.1016/j.envsoft.2010.02.008]

* **bnlearn package**
Scutari M (2010). “Learning Bayesian Networks with the bnlearn R Package.” Journal of Statistical Software, 35(3), 1–22. doi:10.18637/jss.v035.i03.

**SOCIAL NETWORKS**

* **NetLogo Nw Extension and small-world networks**
<https://ccl.northwestern.edu/netlogo/docs/nw.htmlnw:generate-watts-strogatz> 
Wilensky, U. (2015). NetLogo Small Worlds model.http://ccl.northwestern.edu/netlogo/models/SmallWorlds. Center for Connected Learning and Computer-Based Modeling, Northwestern University, Evanston, IL.

* **Wheel and Cycle**
Frey, Daniel, and Dunja Šešelja. “Robustness and idealizations in agent-based models of scientific interaction.” The British Journal for the Philosophy of Science (2020).
https://github.com/daimpi/SocNetABM/tree/RobIdeal.

**BAYESIAN NETWORKS**

* **Alarm**
Accessed via bnlearn ‘Bayesian Network Repository’- www.bnlearn.com/bnrepository/ (updated Nov 2022). Originally found in: I. A. Beinlich, H. J. Suermondt, R. M. Chavez, and G. F. Cooper. The ALARM Monitoring System: A Case Study with Two Probabilistic Inference Techniques for Belief Networks. In Proceedings of the 2nd European Conference on Artificial Intelligence in Medicine, pages 247-256. Springer-Verlag, 1989.

* **Asia**
Accessed via bnlearn ‘Bayesian Network Repository’- www.bnlearn.com/bnrepository/ (updated Nov 2022). Originally found in: S. Lauritzen, and D. Spiegelhalter. Local Computation with Probabilities on Graphical Structures and their Application to Expert Systems (with discussion). Journal of the Royal Statistical Society: Series B (Statistical Methodology), 50(2):157-224, 1988.

* **Wet Grass**
Accessed via ‘agena.ai.modeller’ software (version 9336, www.agena.ai) model library. Originally found in: F. V. Jensen. Introduction to Bayesian Networks. Springer-Verlag, 1996.

* **Sally Clark**
Accessed via ‘agena.ai.modeller’ software (version 9336, www.agena.ai) model library. The AgenaRisk software contains a model library with executable versions of all models found in this book: F. Norman, and M. Neil. Risk assessment and decision analysis with Bayesian networks. Crc Press, 2018. Discussed in: N. Fenton. Assessing evidence and testing appropriate hypotheses. Sci Justice, 54(6):502–504, 2014. https://doi.org/10.1016/j.scijus.2014.10.007.

* **Vole**
Accessed via ‘agena.ai.modeller’ software (version 9336, www.agena.ai) model library. The AgenaRisk software contains a model library with executable versions of all models found in this book: F. Norman, and M. Neil. Risk assessment and decision analysis with Bayesian networks. Crc Press, 2018. Originally found in: Lagnado, D. A. “Thinking about evidence.” In Proceedings of the British Academy, vol. 171, pp. 183-223. Oxford, UK: Oxford University Press, 2011. Revised by: F. Norman, M. Neil, and D. A. Lagnado. A general structure for legal arguments about evidence using Bayesian networks. Cognitive science, 37(1): 61-102, 2013.

## HOW TO CITE
Assaad, L., Fuchs, R., Jalalimanesh, A., Phillips, K., Schöppl, L. & Hahn, U. (2023). “A Bayesian Agent-Based Framework for Argument Exchange Across Networks”.
@#$#@#$#@
default
true
0
Polygon -7500403 true true 150 5 40 250 150 205 260 250

airplane
true
0
Polygon -7500403 true true 150 0 135 15 120 60 120 105 15 165 15 195 120 180 135 240 105 270 120 285 150 270 180 285 210 270 165 240 180 180 285 195 285 165 180 105 180 60 165 15

arrow
true
0
Polygon -7500403 true true 150 0 0 150 105 150 105 293 195 293 195 150 300 150

box
false
0
Polygon -7500403 true true 150 285 285 225 285 75 150 135
Polygon -7500403 true true 150 135 15 75 150 15 285 75
Polygon -7500403 true true 15 75 15 225 150 285 150 135
Line -16777216 false 150 285 150 135
Line -16777216 false 150 135 15 75
Line -16777216 false 150 135 285 75

bug
true
0
Circle -7500403 true true 96 182 108
Circle -7500403 true true 110 127 80
Circle -7500403 true true 110 75 80
Line -7500403 true 150 100 80 30
Line -7500403 true 150 100 220 30

butterfly
true
0
Polygon -7500403 true true 150 165 209 199 225 225 225 255 195 270 165 255 150 240
Polygon -7500403 true true 150 165 89 198 75 225 75 255 105 270 135 255 150 240
Polygon -7500403 true true 139 148 100 105 55 90 25 90 10 105 10 135 25 180 40 195 85 194 139 163
Polygon -7500403 true true 162 150 200 105 245 90 275 90 290 105 290 135 275 180 260 195 215 195 162 165
Polygon -16777216 true false 150 255 135 225 120 150 135 120 150 105 165 120 180 150 165 225
Circle -16777216 true false 135 90 30
Line -16777216 false 150 105 195 60
Line -16777216 false 150 105 105 60

car
false
0
Polygon -7500403 true true 300 180 279 164 261 144 240 135 226 132 213 106 203 84 185 63 159 50 135 50 75 60 0 150 0 165 0 225 300 225 300 180
Circle -16777216 true false 180 180 90
Circle -16777216 true false 30 180 90
Polygon -16777216 true false 162 80 132 78 134 135 209 135 194 105 189 96 180 89
Circle -7500403 true true 47 195 58
Circle -7500403 true true 195 195 58

circle
false
0
Circle -7500403 true true 0 0 300

circle 2
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240

cow
false
0
Polygon -7500403 true true 200 193 197 249 179 249 177 196 166 187 140 189 93 191 78 179 72 211 49 209 48 181 37 149 25 120 25 89 45 72 103 84 179 75 198 76 252 64 272 81 293 103 285 121 255 121 242 118 224 167
Polygon -7500403 true true 73 210 86 251 62 249 48 208
Polygon -7500403 true true 25 114 16 195 9 204 23 213 25 200 39 123

cylinder
false
0
Circle -7500403 true true 0 0 300

dot
false
0
Circle -7500403 true true 90 90 120

face happy
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 255 90 239 62 213 47 191 67 179 90 203 109 218 150 225 192 218 210 203 227 181 251 194 236 217 212 240

face neutral
false
0
Circle -7500403 true true 8 7 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Rectangle -16777216 true false 60 195 240 225

face sad
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 168 90 184 62 210 47 232 67 244 90 220 109 205 150 198 192 205 210 220 227 242 251 229 236 206 212 183

fish
false
0
Polygon -1 true false 44 131 21 87 15 86 0 120 15 150 0 180 13 214 20 212 45 166
Polygon -1 true false 135 195 119 235 95 218 76 210 46 204 60 165
Polygon -1 true false 75 45 83 77 71 103 86 114 166 78 135 60
Polygon -7500403 true true 30 136 151 77 226 81 280 119 292 146 292 160 287 170 270 195 195 210 151 212 30 166
Circle -16777216 true false 215 106 30

flag
false
0
Rectangle -7500403 true true 60 15 75 300
Polygon -7500403 true true 90 150 270 90 90 30
Line -7500403 true 75 135 90 135
Line -7500403 true 75 45 90 45

flower
false
0
Polygon -10899396 true false 135 120 165 165 180 210 180 240 150 300 165 300 195 240 195 195 165 135
Circle -7500403 true true 85 132 38
Circle -7500403 true true 130 147 38
Circle -7500403 true true 192 85 38
Circle -7500403 true true 85 40 38
Circle -7500403 true true 177 40 38
Circle -7500403 true true 177 132 38
Circle -7500403 true true 70 85 38
Circle -7500403 true true 130 25 38
Circle -7500403 true true 96 51 108
Circle -16777216 true false 113 68 74
Polygon -10899396 true false 189 233 219 188 249 173 279 188 234 218
Polygon -10899396 true false 180 255 150 210 105 210 75 240 135 240

house
false
0
Rectangle -7500403 true true 45 120 255 285
Rectangle -16777216 true false 120 210 180 285
Polygon -7500403 true true 15 120 150 15 285 120
Line -16777216 false 30 120 270 120

leaf
false
0
Polygon -7500403 true true 150 210 135 195 120 210 60 210 30 195 60 180 60 165 15 135 30 120 15 105 40 104 45 90 60 90 90 105 105 120 120 120 105 60 120 60 135 30 150 15 165 30 180 60 195 60 180 120 195 120 210 105 240 90 255 90 263 104 285 105 270 120 285 135 240 165 240 180 270 195 240 210 180 210 165 195
Polygon -7500403 true true 135 195 135 240 120 255 105 255 105 285 135 285 165 240 165 195

line
true
0
Line -7500403 true 150 0 150 300

line half
true
0
Line -7500403 true 150 0 150 150

pentagon
false
0
Polygon -7500403 true true 150 15 15 120 60 285 240 285 285 120

person
false
0
Circle -7500403 true true 110 5 80
Polygon -7500403 true true 105 90 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 195 90
Rectangle -7500403 true true 127 79 172 94
Polygon -7500403 true true 195 90 240 150 225 180 165 105
Polygon -7500403 true true 105 90 60 150 75 180 135 105

plant
false
0
Rectangle -7500403 true true 135 90 165 300
Polygon -7500403 true true 135 255 90 210 45 195 75 255 135 285
Polygon -7500403 true true 165 255 210 210 255 195 225 255 165 285
Polygon -7500403 true true 135 180 90 135 45 120 75 180 135 210
Polygon -7500403 true true 165 180 165 210 225 180 255 120 210 135
Polygon -7500403 true true 135 105 90 60 45 45 75 105 135 135
Polygon -7500403 true true 165 105 165 135 225 105 255 45 210 60
Polygon -7500403 true true 135 90 120 45 150 15 180 45 165 90

sheep
false
15
Circle -1 true true 203 65 88
Circle -1 true true 70 65 162
Circle -1 true true 150 105 120
Polygon -7500403 true false 218 120 240 165 255 165 278 120
Circle -7500403 true false 214 72 67
Rectangle -1 true true 164 223 179 298
Polygon -1 true true 45 285 30 285 30 240 15 195 45 210
Circle -1 true true 3 83 150
Rectangle -1 true true 65 221 80 296
Polygon -1 true true 195 285 210 285 210 240 240 210 195 210
Polygon -7500403 true false 276 85 285 105 302 99 294 83
Polygon -7500403 true false 219 85 210 105 193 99 201 83

square
false
0
Rectangle -7500403 true true 30 30 270 270

square 2
false
0
Rectangle -7500403 true true 30 30 270 270
Rectangle -16777216 true false 60 60 240 240

star
false
0
Polygon -7500403 true true 151 1 185 108 298 108 207 175 242 282 151 216 59 282 94 175 3 108 116 108

target
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240
Circle -7500403 true true 60 60 180
Circle -16777216 true false 90 90 120
Circle -7500403 true true 120 120 60

tree
false
0
Circle -7500403 true true 118 3 94
Rectangle -6459832 true false 120 195 180 300
Circle -7500403 true true 65 21 108
Circle -7500403 true true 116 41 127
Circle -7500403 true true 45 90 120
Circle -7500403 true true 104 74 152

triangle
false
0
Polygon -7500403 true true 150 30 15 255 285 255

triangle 2
false
0
Polygon -7500403 true true 150 30 15 255 285 255
Polygon -16777216 true false 151 99 225 223 75 224

truck
false
0
Rectangle -7500403 true true 4 45 195 187
Polygon -7500403 true true 296 193 296 150 259 134 244 104 208 104 207 194
Rectangle -1 true false 195 60 195 105
Polygon -16777216 true false 238 112 252 141 219 141 218 112
Circle -16777216 true false 234 174 42
Rectangle -7500403 true true 181 185 214 194
Circle -16777216 true false 144 174 42
Circle -16777216 true false 24 174 42
Circle -7500403 false true 24 174 42
Circle -7500403 false true 144 174 42
Circle -7500403 false true 234 174 42

turtle
true
0
Polygon -10899396 true false 215 204 240 233 246 254 228 266 215 252 193 210
Polygon -10899396 true false 195 90 225 75 245 75 260 89 269 108 261 124 240 105 225 105 210 105
Polygon -10899396 true false 105 90 75 75 55 75 40 89 31 108 39 124 60 105 75 105 90 105
Polygon -10899396 true false 132 85 134 64 107 51 108 17 150 2 192 18 192 52 169 65 172 87
Polygon -10899396 true false 85 204 60 233 54 254 72 266 85 252 107 210
Polygon -7500403 true true 119 75 179 75 209 101 224 135 220 225 175 261 128 261 81 224 74 135 88 99

wheel
false
0
Circle -7500403 true true 3 3 294
Circle -16777216 true false 30 30 240
Line -7500403 true 150 285 150 15
Line -7500403 true 15 150 285 150
Circle -7500403 true true 120 120 60
Line -7500403 true 216 40 79 269
Line -7500403 true 40 84 269 221
Line -7500403 true 40 216 269 79
Line -7500403 true 84 40 221 269

wolf
false
0
Polygon -16777216 true false 253 133 245 131 245 133
Polygon -7500403 true true 2 194 13 197 30 191 38 193 38 205 20 226 20 257 27 265 38 266 40 260 31 253 31 230 60 206 68 198 75 209 66 228 65 243 82 261 84 268 100 267 103 261 77 239 79 231 100 207 98 196 119 201 143 202 160 195 166 210 172 213 173 238 167 251 160 248 154 265 169 264 178 247 186 240 198 260 200 271 217 271 219 262 207 258 195 230 192 198 210 184 227 164 242 144 259 145 284 151 277 141 293 140 299 134 297 127 273 119 270 105
Polygon -7500403 true true -1 195 14 180 36 166 40 153 53 140 82 131 134 133 159 126 188 115 227 108 236 102 238 98 268 86 269 92 281 87 269 103 269 113

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270
@#$#@#$#@
NetLogo 6.2.1
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
default
0.0
-0.2 0 0.0 1.0
0.0 1 1.0 0.0
0.2 0 0.0 1.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180
@#$#@#$#@
0
@#$#@#$#@
