# PowerBI-visuals-assorules
R-powered custom visual. Implements assosiation rules mining

![Association rules screenshot](https://az158878.vo.msecnd.net/marketing/Partner_21474836617/Product_42949680599/Asset_1ae51daa-9700-4e2d-b05d-f22655959eea/AssociationRulesscreenshot1.png)
# Overview
Association rules are if/then statements that help discovering interesting relations between variables in large databases. The simple example of an association rule is "If a customer buys a coffee, he is 80% likely to also purchase sugar". Such information can be used as the basis for decisions about promotional pricing or product placements. In addition to the above example from market basket analysis association rules are employed today in many application areas.

In this visual, the rules are automatically detected and visualized. User can control and sort the output rules using the best-known measures of significance: support, confidence and lift. This visual supports several methods for association rules visualization. You can control the algorithm parameters and the visual attributes to suit your needs.

Highlighted features:
* You may select items for Left Hand Side (LHS) and Right Hand Side (RHS) of the assosiation rules
* You may control output assosiation rules via thresholds
* Rules selection attribute allow to show only subset of rules to avoid clutter
* Four different types of rules visualization are provided: graph, parallel coordinates plot, table and scatter plot

R package dependencies(auto-installed): nloptr, seriation, gtools, caTools, lmtest, arules, arulesViz, grDevices, gridExtra, grid, methods

Supports R versions: R 3.3.1, R 3.3.0, MRO 3.3.1, MRO 3.3.0, MRO 3.2.2

See also [Association rules chart at Microsoft Office store](https://store.office.com/en-us/app.aspx?assetid=WA104380815&sourcecorrid=f46a638e-f733-4e6c-ad8c-c5cebbd40981&searchapppos=0&ui=en-US&rs=en-US&ad=US&appredirect=false)