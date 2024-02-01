## 1.15.1 (2024/02/01)
  - Adapt numerical features for prosody encoding

# 1.15.0 (2024/01/02)
  - remove ocaml version constraint "< 5"
  - Use Conll id in Graph.to_json
  - Handle Cxn coloring

## 1.14.1 (2023/12/03)
  - Change lexer for mSUD labels (like "mod/m")

# 1.14.0 (2023/11/11)
  - add disjunction in Node definition (feature request: https://github.com/grew-nlp/grew/issues/47)
  - Optimise request representation

## 1.13.1 (2023/09/16)
  - Requires ocaml < 5 (required by eliom-based grew tools)

# 1.13.0 (2023/09/16)
  - ⚠️ change cluster_item type

## 1.12.4 (2023/05/01)
  - Fix Grs.to_json (add needed quotes)
  - Fix html output in grew grep with empty nodes

## 1.12.3 (2023/04/28)
  - Fix Command.to_json

## 1.12.2 (2023/04/26)
  - Fix https://github.com/grew-nlp/grew/issues/38
  - Fix command "e.label = …" when called form AG

## 1.12.1 (2023/04/11)
  * generalise the "with" usage
  * Change Parseme colors
  * Detect repetitive usage of "add_node"

# 1.12.0 (2023/03/24)
  * ⚠️ library is renamed `grewlib` (old name was `libcaml-grew`)
  * use dune build system

## 1.11.1 (2023/03/10)
  * Bug fixes
  * "draw" loops are special features

# 1.11.0 (2023/01/22)
  * ⚠️ add "with" clauses (with is now a keyword)
  * non injnective matching with `N$`
  * few bug fixes

## 1.10.2 (2023/01/06)
  * change library dependences from libcaml-conll to conll

## 1.10.1 (2022/12/29)
  * change library dependences from libcaml-amr to amr

# 1.10.0 (2022/11/23)
  * Renaming from pattern to request
  * Renaming from Projection to the more generic Clustered 
  * New functions
    * Request.of_json
    * Matching.sub_graph
    * Grs.of_json
    * Grs.to_json
    * Corpus.update_graph
    * Corpus.of_conllx_corpus
    * Corpus.from_assoc_list
    * Corpus.search
    * Corpus.bounded_search
    * Corpus_desc.get_display
  * Removed functions
    * Grs.to_json_python
    * Graph.to_raw
    * Graph.of_gr
    * Graph.of_json_python
    * Graph.to_json_python
    * Graph.of_brown
    * Graph.of_gr
    * Graph.insert_proj

## 1.9.2 (2022/08/31)
  * Fix https://github.com/grew-nlp/grew/issues/17

## 1.9.1 (2022/07/08)
  * Add string as a possible feature value in pattern edges

# 1.9.0 (2022/06/29)
  * update Conll handling (libcaml-conll 1.13)
  * add subgraph function
  * minors bug fixes

## 1.8.1 (2022/04/03)
  * internal changes mainly relative to Grew-match

# 1.8.0 (2021/12/15)
  * Add command `prepend_feats` (See: https://grew.fr/doc/commands/#copy-several-features-from-one-node-to-another)
  * Add Python style string slicing (See: https://grew.fr/doc/commands/#add-or-update-a-node-feature)
  * Add several functions for other tools (ArboratorGrew, Grew-web…)

# 1.7.0 (2021/09/20)
  * Add syntax with regexp in node declaration

## 1.6.3 (2021/08/03)
  * Fix bug with lexicon folder finding

## 1.6.2 (2021/07/16)
  * add few functions
  * deal with `$` extension of Naija
  * stricter application of UPDATE_EDGE_FEAT

## 1.6.1 (2021/05/22)
  * add new commands `unorder` and `insert` (⚠️ in old GRS which used `unorder` or `insert` as identifier, a renaming is needed)

# 1.6.0 (2021/05/05)
  * new function Corpus.graph_of_sent_id
  * ⚠️ new type for Corpus.is_conll
  * get rid of of CoNLL implementation
  * ⚠️ stop backward compatibility `pos`, `cat` and `phon`

## 1.5.1 (2021/04/23)
  * Fix bug in the graph history tracking
  * Fix bug in default behaviour with unsafe command (https://gitlab.inria.fr/grew/libcaml-grew/-/issues/26)

# 1.5.0 (2021/03/16)
  * Change JSON encoding of graphs

## 1.4.3 (2021/02/26)
  * more general UPDATE_EDGE_FEAT handling (see https://grew.fr/gallery/update_edge_feature)

## 1.4.2 (2021/02/24)
  * Remove obsolete domain handling:
    * keywords “features” and “labels” obsolete
  * add whether handling for Grew-match
  * new fct: Graph.get_feature_values, Graph.get_history, Graph.get_relation and Graph.get_features
  * remove old Conll code

## 1.4.1 (2021/01/19)
  * add config in to_json_python (bug in grewpy)

# 1.4.0 (2020/10/02)
  * :warning: change pattern syntax (see https://grew.fr/doc/upgrade)
  * add new constraints in pattern syntax
  * new implementation of Conll handling

## 1.3.6.1 (2020/07/03)
  * remove cppo usage

## 1.3.6 (2020/05/05)
  * Fix bug in del_node implementation

## 1.3.5 (2020/03/12)
  * Avoid exception [Not_conll] in G_edge.to_dep & G_edge.to_dot

## 1.3.4 (2020/01/18)
  * add command append_feats (see https://gitlab.inria.fr/grew/grew/issues/12)
  * any sequence of "pattern" / "without" / "global" is admitted
  * add global constraints in meta information

## 1.3.3 (2019/10/22)
  * Various new functions from grew-match and grew_server
  * fix bug on checking duplicate definition in packages

## 1.3.2 (2019/08/27)
  * implement edge_del_feat
  * Fix various bugs

## 1.3.1 (2019/07/04)
  * add constraints with features in edges
  * add shift commands with features in edges
  * add UPDATE_EDGE_FEAT handling

# 1.3.0 (2019/06/24)
  * Add support of "@alpha" extension in edges
  * Add a default “empty.grs”
  * Review ordering of node
  * Add Multigraph management (for grew_server)

## 1.2.1 (2019/04/24)
  * Fix error in global pattern

# 1.2.0 (2019/03/26)
  * Edge label can be viewed as feature structure "x:y" <=> "1=x, 2=y"
  * Add global section in pattern (is_projective, is_cyclic, is_tree, is_forest)
  * Add ?get_url parameter to Graph.to_dot (AMR handling in Grew-match)
  * Add a notion of pivot node in pattern for Grew-match export
  * Add Libgrew.set_track_rules function

## 1.1.5 (2019/03/26)
  * move to opam2

## 1.1.4 (2019/03/06)
  * Fix bug introduced in previous version

## 1.1.3 (2019/03/04)
  * Fix bug in strategies implementation

## 1.1.2 (2019/02/10)
  * change json type
  * add cmxs target

## 1.1.1 (2018/11/23)
  * style for projection edges

# 1.1.0 (2018/11/22)
  * More general label_ident in parser (String are available everywhere)
  * Update to new MWE types from libcaml-conll

## 1.0.2 (2018/09/16)
  * Fix problem with nodes names in “grew grep”

## 1.0.1 (2018/09/10)
  * Fix error in MWE node construction

# 1.0.0 (2018/09/10)
  * /!\ Change lexical rules syntax and lexicon representation
  * Handling of Parseme's column 11
  * Large code cleaning
  * Fix https://gitlab.inria.fr/grew/grew/issues/4

# 0.49.0 (2018/07/05)
  * Add new syntax in pattern node "f=u/g=v" (mainly for MWEPOS handling in relations consistency checks)

# 0.48.0 (2018/06/19)
  * remove `conll_fields` mechanism (names of conll fields 2, 4 and 5 are `form`, `upos`, `xpos`)

## 0.47.2 (2018/05/04)
  * Deal with increasing Grs

## 0.47.1 (2018/03/16)
  * Fix bug in grs loading with relative path

# 0.47.0 (2018/03/13)
  * Add conll_fields in domain definition
  * graphs are managing the own domain (change types in library)
  * /!\ change type of function Graph.to_dep
  * add -safe_commands mode
  * more json

## 0.46.4 (2018/03/09)
  * Fix Not_found exception in conversion UD to Sequoia (fusion words)

## 0.46.3 (2018/01/03)
  * Fix #21580: out_edges not shifted by the shift command
  * Fix #21581: detection of duplicate identifiers inside packages
  * change `of_brown` function: `sent_id` goes in metadata

## 0.46.2 (2017/12/17)
  * Non strict rewrite by default (for demo)

## 0.46.1 (2017/12/17)
  * Fix semantics of Onf

# 0.46.0 (2017/12/14)
  * Remove Dep2pict dependency
  * Remove old code
  * Prepare new version of command history management

# 0.45.0 (2017/10/10)
  * Keep features defined in column 10 of conll (like SpaceAfter=No)
  * /!\ implicit nodes are allowed only in edge definitions
  * /!\ new grs syntax is required (old syntax can be used with -old_grs command line argument)

## 0.44.1 (2017/09/06)
  * Fix bug “undefined domain”

# 0.44.0 (2017/09/05)
  * /!\ new grs syntax (with package and strategies), see [https://grew.fr/grs](https://grew.fr/grs)
  * /!\ remove function `Rewrite.write_annot`
  * adapt to libcaml-conll 0.14.0

# 0.43.0 (2017/05/23)
  * /!\ syntax change: `confluent` --> `deterministic`
  * /!\ syntax change: `match` --> `pattern`
  * /!\ change shift semantics: edges with source and target nodes in the pattern are not concerned by the shifts
  * Fix [#2](https://gitlab.inria.fr/grew/libcaml-grew/issues/2) (Add a new syntax for `add_edge` command)

## 0.42.3 (2017/04/25)
  * Add dependence on yojson

## 0.42.2 (2017/04/25)
  * Fix ordering of dependencies in conll output

## 0.42.1 (2017/04/19)
  * Fix json export (missing commmands)

# 0.42.0 (2017/04/18)
  * Add json export

## 0.41.1 (2017/03/14)
  * adapt to new libraries versions

## 0.41 (2017/03/06)
  * adapt to libcaml-conll 0.12.0 (UD empty nodes)

## 0.40.1 (2017/01/17)
  * Fix a bug introduced in [01f7b589](https://gitlab.inria.fr/grew/libcaml-grew/commit/01f7b589e2024d7a86fbf8c52dc48d73b6e318d2) for handling the feature names ordering

# 0.40.0 (2016/11/10)
  * Change strategies syntax: `pick(S)`, `try(S)` and `S!``
  * Simplification of exported exceptions: only two exception are raised by libgrew: Error and Bug

# 0.39.0 (2016/11/02)
  * Add function `Rewrite.simple_rewrite` for applying strategies

## 0.38.3 (2016/10/21)
  * add dependency to libcaml-conll

## 0.38.2 (2016/10/21)
  * handling of extra features in column 10 of conlls

## 0.38.1 (2016/09/20)
  * Fix syntax error with pattern `match { X -> * }``

# 0.38.0 (2016/09/20)
  * add code for interaction with python
  * add function get_version

# 0.37.0 (2016/09/02)
  * add support for constituency trees
  * add definition of strategies (not completly implemented…)

## 0.36.2 (2016/08/30)
  * Remove dependencies amongst xml-light and camlp4

## 0.36.1 (2016/08/29)
  * Fix bug [#15](http://talc2.loria.fr:8888/bruno/grew/issues/15)

## 0.36 (2016/08/18)
  * Make domains optional

## 0.35 (2016/07/14)
  * add the debug_loop flag (when a loop is suspected graph is considered as a normal form, such that it is possible to see it in the GUI)
  * new functions in libgrew.mli for Grewpy:
    * `Graph.of_gr`
    * `Rewrite.get_graphs`
  * improve error reporting

## 0.34.4 (2016/05/19)
  Bug fix again… introduced in previous version

## 0.34.3 (2016/05/18)
  Bug fix introduced in previous version

## 0.34.2 (2016/05/18)
  Bug fix in node ordering in case of del_node

## 0.34.1 (2016/05/14)
  Bug fix in default values for depth bounds

## 0.34 (2016/05/10)
  NB: changes prefixed with "==>" belows breaks existing code!
  NB: changes prefixed with "-->" belows makes existing code deprecated
  * ==> new syntax:
       X<<Y for X is before Y in the linear order
       X<Y fot X is immediatly before Y in the linear order (diff from semantics introduced in 0.32)
  * ==> new_syntax for regular expression on label edges:
      NEW: X -[re"__your_regular_expression_here__"]-> Y
      OLD: X -[aux:*]-> Y is not supported anymore
  * ==> new_syntax for regular expression on featues:
      NEW: X.feat = re"__your_regular_expression_here__" (no whitespace between re and double quote)
      OLD: X.feat == "__your_regular_expression_here__" introduced in 0.32 is not supported anymore
  * --> remove lex_rule keyword: use "rule" instead
  * --> add_neighbour is now deprecated and new syntax is introduced for nodes creation:
      add_node X        Create a new node named X wihtout position constraint
      add_node X :> Y   Create a new node named X immediatly after Y
      add_node X :< Y   Create a new node named X immediatly before Y
  * improve search for a sequence:
      1) search for the sequence with the requested name
      2) search for the module with the requested name
      3) select the first sequence defined in the GRS file
  * domain can be declared in a external file
  * add functions Rewrite.set_max_depth_non_det and Rewrite.set_max_depth_det to the library
  * improve error handling
  * change precedence implementation
  * remove useless code

## 0.33.1 (2016/03/14)
  * fix Conll error locations

## 0.33 (2016/03/11)
  * Fix implicit nodes handling with matching item with regexp: N.lemma == "a.*er"
  * move conll handling code into an external library libcaml-conll

## 0.32 (2016/02/01)
  * extend syntax:
    - syntactic sugar: "A < B" is interpreted as "A.position < B.position"
    - complete pattern with implicit nodes
    - add new constraint with regexp
  * move to ocamlbuild
  * add "node_matching" function for the new "grep" mode

## 0.31 (2015/11/15)
  * take ranges into account in conlls (UD)
  * Code reorganisation

## 0.30 (2015/09/25)
  * Internal changes (adapt to new version of libcaml-dep2pict)

## 0.29 (2015/08/28)
  * renumbering of conll output
  * ignore UDT lines with "i-j" number in UDT Conll

## 0.28 (2015/08/12)
  * Add inequality constraints between numerical features and constants

## 0.27
  * add syntax for inequality on features

## 0.26 (2015/04/23)
  * The shift command can be parametrized by edge constraints
  * Negative edge constraint are available in out-edge (N -[^mod]-> *) and in-edge (* -[^mod]-> N) constraints

## 0.25 (2015/04/08)
  * add support for the [-full] mode
  * handling of "*" in pattern edge labels

## 0.24 (2014/11/24)
  * Changes in the interface:
    - the "loc" type is abstract and exported (changes the libgrew interface)
  * New features:
    - use a float in a command "node.feat = 123.456"
    - improve checking for consistency between rules and feature domain at GRS loading time
    - add the possibility to use parameters in the without part
  * Bug fixes:
    - take the feature "position" into account when the user chose features to display
    - accept colors like "#abc"
  * Misc
    - add files for Geshi
    - code reorganization
    - add activate mechanism (untested)

## 0.23 (2014-06-05) and earlier
  Please refer to git history