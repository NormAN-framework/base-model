### PROJECT DESCRIPTION

This base model is the first implementation of the *NormAN* framework. NormAN – short for Normative Argument Exchange across Networks – is a framework for agent-based modelling of argument exchange in social networks, first presented in Assaad et.al, (2023). A detailed explanation of the base model and its parameters can be found there.

**Full reference:** Assaad, L., Fuchs, R., Jalalimanesh, A., Phillips, K., Schöppl, K. & Hahn, U. (2023). *“A Bayesian Agent-Based Framework for Argument Exchange Across Networks“*.

NormAN is a NetLogo model of argument exchange between Bayesian agents about a particular hypothesis. It comprises three components: a ‘world’ model, individual agents, and a social network. NormAN's base model offers the user the possibility to change each of these components and thus explore a variety of different argumentation scenarios.
* The world model consists of a Bayesian network which determines the true state of the hypothesis, along with the truth values of so-called evidence propositions connected to said hypothesis.
* Agents receive evidence about that world (through inquiry) and may communicate that evidence to others as arguments and receive it in turn. Agents aggregate all evidence/arguments that they have encountered to form a degree of belief in the target claim. To this end, they use Bayes’ rule.
* Communication, finally, takes place across a social network.

### PAPERS USING THIS FRAMEWORK
* Hahn, Assaad & Burton (2024) [*“Opinion Averaging versus Argument Exchange“*](https://escholarship.org/uc/item/9d14q4tv).
* Schöppl & Hahn (2024) [*“Exploring Effects of Self-Censoring through Agent-Based Simulation“*](https://escholarship.org/uc/item/9b32v6xc).
* Assaad & Hahn (2024) [*“Rational Polarization: Sharing Only One’s Best Evidence Can Lead to Group Polarization“*](https://escholarship.org/uc/item/6x80n8v4).
* Assaad, Fuchs, Phillips, Schöppl & Hahn (2025) [*“Capturing Argument in Agent-based Models“*](https://link.springer.com/article/10.1007/s11245-025-10215-2).
* Schöppl (2025) [*“Industry Influencing Collective Scientific Reasoning: An Agent-based Exploration“*](https://escholarship.org/uc/item/06t6b7v0).
* Hahn, Assaad & Schöppl (2025) [*“Flooding the Zone: An Agent-based Exploration“*](https://escholarship.org/uc/item/8z32x661).
* Assaad (2025) [*“Multi-Option Polarization: How Deliberating More Options Both Increases and Decreases Polarization“*](https://escholarship.org/uc/item/5dt8g8c1).

### HOW TO GET STARTED

This model was developed in **NetLogo 6.2.1** (freely available for download [here](https://ccl.northwestern.edu/netlogo/6.2.1/)) and relies on NetLogo’s R extension (for an explanation, see [here](https://ccl.northwestern.edu/netlogo/6.2.2/docs/r.html)). As of Nov 2023, it is important that one uses a *6.2.X* version of NetLogo because the R extension does not work reliably for versions *6.3* and above. Specifically, the model uses bnlearn, an R package for Bayesian network learning and inference (explained [here](https://www.bnlearn.com/)). The R Extension comes bundled with NetLogo *6* and requires a compatible R installation on your device. R is freely available [here](https://cran.r-project.org/), and you can learn more about the details of using R with NetLogo in NetLogo's [documentation](https://ccl.northwestern.edu/netlogo/6.2.2/docs/r.html) under Section *Installing*. Please do not hesitate to contact us should you have any problems running NormAN.

### HOW TO USE

Follow these steps to quickly initialize the model: 

1. When first opening the model, make sure ‘reset-world-?’ and ‘reset-social-network-?’ are on.
2. The World: choose a ‘causal-structure’ (chooser).
3. The social network: choose a ‘social-network’ (chooser) and a ‘number-of-agents’ (slider).
4. Click setup: the social network will appear in the interface, the right bottom monitor will show a histogram of agent-beliefs and the middle output will show which pieces of evidence are true.
5. Press ‘go’ to start the simulation. 
 
By clicking ‘setup’, users can wholly re-initialize the model, or keep some model facets fixed. Switching ‘reset-world-?’ on resets the truth values of the evidence nodes when ‘ setup’ is pressed. ‘reset-social-network-?’ resets all agents and the social network that connects them. ‘reset-agents-initial-evidence-?’ resets the set of evidence
that each agent starts with upon initialization (only relevant if initial-draws>0). Note that because these facets are interconnected, ‘reset-world-?’ triggers ‘reset-social-network-?’, and so does ‘reset-social-network-?’. 

If you would like to monitor what each agent does each round, toggle ‘show-me-?’ on each agent will output their exact step-by-step behaviour as lines in the Command Center. 

The ‘plotting-type’ chooser gives three options for visualizing the frequency of shared arguments (in the plot entitled ‘The rise and fall of arguments’): ‘uttered’ tracks how many times a piece of evidence has been shared each round. ‘sent-to’ tracks how many agents an argument was sent to. ‘received-as-novel’ tracks how many times an argument was received as a novelty by an agent.

### CREDITS AND REFERENCES
##### SOFTWARE

* **NetLogo R extension**
Thiele, JC; Grimm, V (2010). NetLogo meets R: Linking agent-based models with a toolbox for their analysis. Environmental Modelling and Software, Volume 25, Issue 8: 972 - 974 [DOI: 10.1016/j.envsoft.2010.02.008].

* **bnlearn package**
Scutari M (2010). “Learning Bayesian Networks with the bnlearn R Package.” Journal of Statistical Software, 35(3), 1–22. doi:10.18637/jss.v035.i03.

* **gRain package**
Højsgaard S (2012). “Graphical Independence Networks with the gRain Package for R.” Journal of Statistical Software, 46(10), 1–26. doi:10.18637/jss.v046.i10, https://www.jstatsoft.org/v46/i10/.

##### SOCIAL NETWORKS

* **NetLogo Nw Extension and small-world networks**
https://ccl.northwestern.edu/netlogo/docs/nw.htmlnw:generate-watts-strogatz. Wilensky, U. (2015). NetLogo Small Worlds model. http://ccl.northwestern.edu/netlogo/models/SmallWorlds. Center for Connected Learning and Computer-Based Modeling, Northwestern University, Evanston, IL.

* **Wheel and Cycle**
Frey, Daniel, and Dunja Šešelja. "Robustness and idealizations in agent-based models of scientific interaction." The British Journal for the Philosophy of Science (2020).
https://github.com/daimpi/SocNetABM/tree/RobIdeal.

##### BAYESIAN NETWORKS

* **Alarm**
Accessed via bnlearn ‘Bayesian Network Repository’- www.bnlearn.com/bnrepository/ (updated Nov, 2022). Originally found in: I. A. Beinlich, H. J. Suermondt, R. M. Chavez, and G. F. Cooper. The ALARM Monitoring System: A Case Study with Two Probabilistic Inference Techniques for Belief Networks. In Proceedings of the 2nd European Conference on Artificial Intelligence in Medicine, pages 247-256. Springer-Verlag, 1989.

* **Asia**
Accessed via bnlearn ‘Bayesian Network Repository’- www.bnlearn.com/bnrepository/ (updated Nov, 2022). Originally found in: S. Lauritzen, and D. Spiegelhalter. Local Computation with Probabilities on Graphical Structures and their Application to Expert Systems (with discussion). Journal of the Royal Statistical Society: Series B (Statistical Methodology), 50(2):157-224, 1988.

* **Wet Grass**
Accessed via ‘agena.ai.modeller’ software (version 9336, www.agena.ai) model library. Originally found in: F. V. Jensen. Introduction to Bayesian Networks. Springer-Verlag, 1996.

* **Sally Clark**
Accessed via ‘agena.ai.modeller’ software (version 9336, www.agena.ai) model library. The AgenaRisk software contains a model library with executable versions of all models found in this book: F. Norman, and M. Neil. Risk assessment and decision analysis with Bayesian networks. Crc Press, 2018. Discussed in: N. Fenton. Assessing evidence and testing appropriate hypotheses. Sci Justice, 54(6):502–504, 2014. https://doi.org/10.1016/j.scijus.2014.10.007.

* **Vole**
Accessed via ‘agena.ai.modeller’ software (version 9336, www.agena.ai) model library. The AgenaRisk software contains a model library with executable versions of all models found in this book: F. Norman, and M. Neil. Risk assessment and decision analysis with Bayesian networks. Crc Press, 2018. Originally found in: Lagnado, D. A. “Thinking about evidence.” In Proceedings of the British Academy, vol. 171, pp. 183-223. Oxford, UK: Oxford University Press, 2011. Revised by: F. Norman, M. Neil, and D. A. Lagnado. A general structure for legal arguments about evidence using Bayesian networks. Cognitive science, 37(1):61-102, 2013.
  
### CONTACT
[NormANFramework@gmail.com](mailto:NormANFramework@gmail.com)

### LICENSE
© 2023. This work is openly licensed via CC BY-NC 4.0 by Klee Schöppl, Ulrike Hahn, Leon Assaad, Kirsty Phillips, Rafael Fuchs and Ammar Jalalimanesh.
