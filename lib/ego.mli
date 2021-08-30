(** Ego is an extensible egraph library for OCaml. The interface to
   Ego is loosely based on the Rust's egg library and reimplements
   their EClass analysis in pure OCaml.

    {{:#top}Ego} provides two interfaces to its equality saturation
    engine:
    
    1. {!Ego.Basic} - an out-of-the-box interface to pure equality
    saturation (i.e supporting only syntactic rewrites).

    2. {!Ego.Generic} - a higher order interface to equality saturation,
    parameterised over custom-user defined analyses.

    You may want to check out the {{:../index.html} quick start guide}.
*)

module Id : sig
  (** This module provides an implementation of an {i efficient}
      {b union-find} data-structure. It's main exported type,
      {!t}, is used to represent equivalence classes in the EGraph
      data-structures provided by {!Ego}. *)


  type t = private int
  (** An abstract datatype used to represent equivalence classes in
      {!Ego}. *)

end



module Basic: sig

  (** This module implements a {i fairly efficient}
      "syntactic-rewrite-only" EGraph-based equality saturation engine
      that operates over Sexps.

      The main interface to EGraph is under the module {!EGraph}.

      Note: This module is not safe for serialization as it uses
      {!Symbol.t} internally to represent strings, and so will be
      dependent on the execution context. If you wish to persist
      EGraphs across executions, check out the EGraphs defined in
      {!Ego.Generic} *)

  module Symbol : sig  
    (** Implements an efficient encoding of strings

        Note: Datatypes using this module are not safe for
        serialization as tag associated with each string dependent on
        the execution context.

        If you wish to persist EGraphs across executions, check out the
        EGraphs defined in {!Ego.Generic} *)

    type t = private int
    (** Abstract type providing an efficient encoding of some string value. *)

    val intern : string -> t
    (** [intern s] returns a symbol representing the string [s].  *)

    val to_string : t -> string
    (** [to_string t] returns the string associated with symbol [t]. *)
  end 

  module Query : sig
    (** This module encodes patterns (for both matching and
        transformation) over Sexprs and is part of {!Ego.Basic}'s API
        for expressing syntactic rewrites. *)

    type t
    (** Encodes a pattern over S-expressions. *)

    val pp: Format.formatter -> t -> unit
    (** [pp fmt s] pretty prints the query [s]. *)

    val show: t -> string
    (** [show s] converts the query [s] to a string *)

    val of_sexp : Sexplib0.Sexp.t -> t
    (** [of_sexp s] builds a pattern from a s-expression

        Note: Any atom prefixed with "?" will be treated as a pattern
        variable.

        For example, the following pattern will match any multiplication expressions:
        {[
          List [Atom "*"; Atom "?a"; Atom "?b"]
        ]}
    *)

    val to_sexp : t -> Sexplib0.Sexp.t
    (** [to_sexp s] converts a pattern back into an s-expression. This is idempotent with {!of_sexp}.  *)

  end

  module Rule : sig
    (** This module encodes syntactic rewrite rules over Sexprs and is part of {!Ego.Basic}'s API
        for expressing syntactic rewrites. *)

    type t
    (** Encodes a rewrite rule over S-expressions.  *)

    val pp: Format.formatter -> t -> unit
    (** [pp fmt r] pretty prints the rewrite rule [r]. *) 

    val show: t -> string
    (** [show r] converts the rewrite rule [r] to a string *)


    val make: from:Query.t -> into:Query.t -> t option
    (** [make ~from ~into] builds a syntactic rewrite rule from a
        matching pattern [from] and a result pattern [into].

        Iff [into] contains variables that are not bound in [from],
        then the rule is invalid, and the function will return [None]. *)

  end

  module EGraph : sig
    (** This module defines the main interface to the EGraph provided
        by {!Ego.Basic}.  *)

    type t
    (** Represents a syntactic-rewrite-only EGraph that operates over
        Sexps. *)

    val pp : ?pp_id:(Format.formatter -> Id.t -> unit) -> Format.formatter -> t -> unit
    (** [pp ?pp_id fmt graph] prints an internal representation of the
        [graph].

        {b Note}: This is primarily intended for debugging, and the
        output format is not guaranteed to remain consistent over
        versions. *)

    val pp_dot : Format.formatter -> t -> unit
    (** [pp_dot fmt graph] pretty prints [graph] in a Graphviz format. *)

    val init : unit -> t
    (** [init ()] creates a new EGraph.  *)

    val add_sexp : t -> Sexplib0.Sexp.t -> Id.t
    (** [add_sexp graph sexp] adds [sexp] to [graph] and returns the
        equivalence class associated with term. *)

    val to_dot : t -> Odot.graph
    (** [to_dot graph] converts [graph] into a Graphviz representation. *)

    val merge : t -> Id.t -> Id.t -> unit
    (** [merge graph id1 id2] merges the equivalence classes
        associated with [id1] and [id2].

        {b Note}: If you call {!merge} manually, you must call
        {!rebuild} before running any queries or extraction. *)

    val rebuild : t -> unit
    (** [rebuild graph] restores the internal invariants of the EGraph
        [graph].

        {b Note}: If you call {!merge} manually, you must call
        {!rebuild} before running any queries or extraction.  *)

    val extract: ((Id.t -> float) -> (Symbol.t * Id.t list) -> float) -> t -> Id.t -> Sexplib0.Sexp.t
    (** [extract cost_fn graph] computes an extraction function [Id.t
        -> Sexplib0.Sexp.t] to extract terms (specified by [Id.t]) from
        the EGraph.

        [cost_fn f (sym,children)] should assign costs to the node
        with tag [sym] and children [children] - it can use [f] to
        determine the cost of a child. *)

    val apply_rules : t -> Rule.t list -> unit
    (** [apply_rules graph rules] runs each of the rewrites in [rules]
        exactly once over the egraph [graph] and then returns. *)

    val run_until_saturation: ?fuel:int -> t -> Rule.t list -> bool
    (** [run_until_saturation ?fuel graph rules] repeatedly each one
        of the rewrites in [rules] until no further changes occur ({i
        i.e equality saturation }), or until it runs out of [fuel].

        It returns a boolean indicating whether it reached equality
        saturation or had to terminate early.  *)
  end

end

module Generic : sig

  (** This module implements a generic EGraph-based equality
      saturation engine that operates over arbitrary user-defined
      languages and provides support for extensible custom user-defined
      EClass analyses.

      The main interface to EGraph is provided by the functor {!Make}
      which constructs an EGraph given a {!LANGUAGE} and {!ANALYSIS},
      {!ANALYSIS_OPS}.

      You may want to check out the {{:../../index.html} quick start
      guide}. *)


  type ('node, 'analysis, 'data, 'permission) egraph
  (** A generic representation of an EGraph, parameterised over the
      language term types ['node], analysis state ['analysis] and data
      ['data] and read permissions ['permission]. *)

  module StringMap : Map.S with type key = string

  (** The module {!Query} encodes generic patterns (for both matching
     and transformation) over expressions and is part of
     {!Ego.Generic}'s API for expressing rewrites. *)
  module Query : sig

    type 'sym t
    (** Represents a query over expressions in a language with
        operators of type ['sym]. *)

    val of_sexp : (string -> 'a) -> Sexplib0.Sexp.t -> 'a t
    (** [of_sexp f s] constructs a query from a sexpression [s] using
        [f] to convert operator tags. *)

    val to_sexp : ('a -> string) -> 'a t -> Sexplib0.Sexp.t
    (** [to_sexp f q] converts a query [q] to a sexpression using [f]
        to convert operators in the query to strings. *)

    val pp : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit
    (** [pp f fmt q] pretty prints a query [q] using [f] to print the
        operators within the query. *)

    val show : (Format.formatter -> 'a -> unit) -> 'a t -> string
    (** [show f q] prints a query [q] to string using [f] to print
        operators within the query. *)

  end

  (** The module {!Scheduler} provides implementations of some generic
     schedulers for Ego's equality saturation engine.

     See {!Make.BuildRunner} on how to compose a schedule with an
     EGraph definition. *)
  module Scheduler : sig

    (** The module {!Backoff} implements an exponential backoff
        scheduler. The scheduler works by tracking a maximum match
        limit, and (BEB) banning rules which exceed their limit.  *)
    module Backoff : sig

      type t
      (** Represents the persistent state of the scheduler - it really
          just tracks the match limit and ban_length parameters chosen
          by for this particular instantiation. *)

      type data
      (** Represents the metadata about rules tracked by the
          scheduler.  *)

      val with_params : match_limit:int -> ban_length:int -> t
      (** [with_params ~match_limit ~ban_length] creates a new backoff
          scheduler with the threshold for banning rules set to
          [match_limit] and the length for which rules are banned set
          to [ban_length]. *)

      val default : unit -> t
      (** [default ()] returns a default instance of the backoff
          scheduler with the threshold for banning rules set to 1_000
          and the initial ban_length set to 5. *)

      (* */ *)

      val create_rule_metadata : t -> 'a -> data

      val should_stop : t -> int -> data Iter.t -> bool

      val guard_rule_usage :
        ('node, 'analysis, 'data, 'permission) egraph ->
        t -> data -> int ->
        (unit -> (Id.t * Id.t StringMap.t) Iter.t) ->
        (Id.t * Id.t StringMap.t) Iter.t

    end


    (** The module {!Simple} implements a scheduler that runs every
       rule each time - i.e applies no scheduling at all. This works
       fine for rewrite systems with a finite number of EClasses but
       can become a problem if the number of EClasses is too large or
       unbounded. *)
    module Simple : sig
      type t
      type data
      val init : unit -> data
      val create_rule_metadata : t -> 'b -> data
      val should_stop : t -> int -> data -> bool
      val guard_rule_usage :
        ('node, 'analysis, 'data, 'permission) egraph ->
        t ->
        data ->
        int ->
        (data -> (Id.t * Id.t StringMap.t) Iter.t) ->
        (Id.t * Id.t StringMap.t) Iter.t
    end
  end

  (** {1:permissions Read/Write permissions}

      For convenience, the operations over the EGraph are split into
      those which {b read and write} to the graph [rw t] and those that
      are {b read-only} [ro t]. When defining the analysis operations,
      certain operations assume that the graph is not modified, so
      these anotations will help users to avoid violating the internal
      invariants of the data structure.
  *)

  type rw
  (** Encodes a read/write permission for a graph.  *)

  type ro
  (** Encodes a read-only permission for a graph.  *)

  (** {1:interfaces Interfaces}  *)

  (** The {!LANGUAGE} module type represents the definition of an
      arbitrary language for use with an EGraph. *)
  module type LANGUAGE = sig

    type 'a shape
    (** Encodes the "shape" of an expression in the language over 
        sub-expressions of type ['a].  *)

    type op
    (** Represents the tags that discriminate the expression
        constructors of the language.  *)

    (** Represents concrete terms of the language by "tying-the-knot". *)
    type t = Mk of t shape [@@unboxed]

    val equal_op : op -> op -> bool
    (** [equal_op op1 op2] returns true if the operators [op1], [op2] are equal.   *)

    val pp_shape : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a shape -> unit
    (** [pp_shape f fmt s] pretty prints expressions of the language. *)

    val compare_shape : ('a -> 'a -> int) -> 'a shape -> 'a shape -> int
    (** [compare cmp a b] compares expressions [a] and [b] using [cmp]
        to compare subexpressions.  *)

    val op : 'a shape -> op
    (** [op expr] retrieves the tag that discriminates the shape of
        the expression [expr].  *)

    val children : 'a shape -> 'a list
    (** [children exp] returns the subexpressions of expression [exp]. *)

    val map_children : 'a shape -> ('a -> 'b) -> 'b shape
    (** [map_children exp f] maps the function [f] over the
        sub-expressions of the expression [exp] *)

    val make : op -> 'a list -> 'a shape
    (** [make op ls] constructs an expression from the tag [op] and
        children [ls].

        {b Note}: If called with invalid arity of arguments for the
        operator [op] the function may throw an error. *)

  end

  (** The module type {!ANALYSIS} encodes the data-types for an
      abstract EClass analysis  over EGraphs. *)
  module type ANALYSIS = sig

    type t
    (** Represents any persistent state that an analysis may need to
        track separately from each EClasses.

        {b Note}: Terms of this type must be mutated imperatively as
        the EGraph API doesn't provide any functions to functionally
        update the persisted state. *)

    type data
    (** Represents the additional analysis information that we will be
        attached to each EClass. *)

    val pp_data : Format.formatter -> data -> unit
    (** [pp_data fmt data] pretty prints [data] using the formatter [fmt]. *)

    val show_data : data -> string
    (** [show_data data] converts [data] into a string. *)

    val equal_data : data -> data -> bool
    (** [equal_data d1 d2] returns true iff [d1], [d2] are equal.  *)

    val default: data
    (** Represents a default abstract value for new nodes.  *)

  end

  (** The module type {!ANALYSIS_OPS} defines the main operations for
      an EClass analysis over an EGraph. *)
  module type ANALYSIS_OPS = sig

    type 'a t
    (** Represents the EGraph over which the analysis operates. *)

    type analysis
    (** Represents the persistent state of the analysis.  *)

    type node
    (** Represents expressions of the language over which the analysis
        operates. *)

    type data
    (** Represents the additional analysis information that we will be
        attached to each EClass. *)

    val make : ro t -> node -> data
    (** [make graph node] returns the analysis data for [node].

        This function is called whenever a new node is added and
        should generate a new abstract value for the node, usually
        using the abstract values of its children.

        {b Note}: In terms of abstract interpretation, this function
        can be thought of the "abstraction" function, mapping concrete
        terms to their corresponding values in the abstract domain. *)

    val merge : analysis -> data -> data -> data * (bool * bool)
    (** [merge st d1 d2] returns the analysis data that represents the
       combination of [d1] and [d2] and a tuple indicating whether the
       result differs from [d1] and or [d2].

       This function is called whenever two equivalance classes are
       merged and should produce a new abstract value that represents
       the merge of their corresponding abstract values.

        {b Note}: In terms of abstract interpretation, this function
       can be thought of the least upper bound (lub), exposing the
       semi-lattice structure of the abstract domain.  *)

    val modify : rw t -> Id.t -> unit
    (** [modify graph class] is used to introduce new children of an
        equivalence class whenever new information about its elements
        is found by the analysis.

        This function is called whenever the children or abstract
        values of an eclass are modified and may use the abstract value
        of its to modify the egraph.

        {b Note}: In terms of abstract interpretation, this function
        can be thought of the "abstraction" function, mapping concrete
        terms to their corresponding values in the abstract domain. *)

  end

  (** The module type {!COST} represents the definition of some
      arbitrary cost system for ranking expressions over some language.
  *)
  module type COST = sig

    type t
    (** Represents the type of a cost of a node.  *)

    type node
    (** Represents terms of the language  *)

    val compare : t -> t -> int
    (** [compare c1 c2] compares the costs [t1] and [t2]  *)

    val cost : (Id.t -> t) -> node -> t
    (** [cost f node] should assign costs to the node [node]. It can
        use the provided function [f] to determine the cost of a
        child. *)

  end

  (** The module type {!SCHEDULER} represents the definition of some
      scheduling system for ranking rule applications during equality
      saturation.

      See {!Scheduler} for some generic schedulers.
  *)
  module type SCHEDULER = sig

    type 'p egraph
    (** Represents an EGraph with read/write permissions
        ['p].  *)

    type t
    (** Represents any persistent state of the scheduler that must be
        maintained separately to its rules.  *)

    type data
    (** Represents metadata about a rule that the scheduler keeps
        track of in order to schedule rules.  *)

    type rule
    (** Represents the type of rules over which this scheduler operates  *)

    val default : unit -> t
    (** Create a default instance of the scheduler.  *)

    val should_stop: t -> int -> data Iter.t -> bool
    (** [should_stop scheduler iteration data] is called whenever the
        EGraph reaches saturation (with the rules that have been
        scheduled), and should return whether further iterations should
        be run (i.e we will be trying a different schedule) or whether
        we have actually truly reached saturation. *)

    val create_rule_metadata: t -> rule -> data
    (** [create_rule_metadata scheduler rule] returns the initial
        metadata for a rule [rule]. *)

    val guard_rule_usage:
      rw egraph -> t -> data -> int ->
      (unit -> (Id.t * Id.t StringMap.t) Iter.t) -> (Id.t * Id.t StringMap.t) Iter.t
      (** [guard_rule_usage graph scheduler data iteration
          gen_matches] is called before the execution of a particular
          rule (represented by the callback [gen_matches]), and should
          return a filtered set of matches according to the scheduling
          of the rule. *)

  end

  (** This module {!GRAPH_API} represents the interface through which
      EClass analyses can interact with an EGraph. *)
  module type GRAPH_API = sig

    type 'p t
    (** Represents an EGraph with read permissions ['p]. *)

    type data
    (** Represents the additional analysis information that we will be
        attached to each EClass. *)

    type analysis
    (** Represents the persistent state of the analysis.  *)

    type 'a shape
    (** Represents the shape of expressions in the language. *)

    type node
    (** Represents concrete terms of expressions in the language  *)

    val freeze : rw t -> ro t
    (** [freeze graph] returns a read-only reference to the EGraph.

        {b Note}: it is safe to modify [graph] after passing it to
        freeze, this method is mainly intended to allow using the
        read-only APIs of the EGraph when you have a RW instance of
        the EGraph. *)

    val class_equal : ro t -> Id.t -> Id.t -> bool
    (** [class_equal graph cls1 cls2] returns true if and only if
        [cls1] and [cls2] are congruent in the EGraph [graph].  *)

    val iter_children : ro t -> Id.t -> Id.t shape Iter.t
    (** [iter_children graph cls] returns an iterator over the
        children of the current EClass. *)

    val set_data : rw t -> Id.t -> data -> unit
    (** [set_data graph cls data] sets the analysis data for EClass
        [cls] in EGraph [graph] to be [data]. *)

    val get_data : ro t -> Id.t -> data
    (** [get_data graph cls] returns the analysis data for EClass
        [cls] in EGraph [graph]. *)

    val get_analysis: rw t -> analysis
    (** [get_analysis graph] returns the persistent analysis sate
        for an EGraph.  *)

    val add_node : rw t -> node -> Id.t
    (** [add_node graph term] adds the term [term] into the EGraph
        [graph] and returns the corresponding equivalence class. *)

    val merge : rw t -> Id.t -> Id.t -> unit
    (** [merge graph cls1 cls2] merges the two equivalence classes
        [cls1] and [cls2].  *)

  end

  (** This module type {!RULE} defines the rewrite interface for an
      EGraph, allowing users to express relatively complex
      transformations of expressions over some language. *)
  module type RULE = sig

    type t
    (** Represents rewrite rules over the language of the EGraph.  *)

    type query
    (** Represents a pattern over the language of the EGraph - it can
        either be used to {i match} and {i bind} a particular
        subpattern in an expression, or can be used to express the
        output schema for a rewrite.  *)

    type 'p egraph
    (** Represents an EGraph with read/write permissions
        ['p].  *)

    val make_constant : from:query -> into:query -> t
    (** [make_constant ~from ~into] creates a rewrite rule from a
        pattern [from] into a schema [into] that applies a purely
        syntactic transformation.  *)

    val make_conditional :
      from:query ->
      into:query ->
      cond:(rw egraph -> Id.t -> Id.t StringMap.t -> bool) ->
      t
    (** [make_conditional ~from ~into ~cond] creates a syntactic
        rewrite rule from [from] to [into] that is conditionally
        applied based on some property [cond] of the EGraph, the root
        eclass of the sub-expression being transformed and the eclasses
        of all bound variables. *)

    val make_dynamic :
      from:query ->
      generator:(rw egraph -> Id.t -> Id.t StringMap.t -> query option) -> t
      (** [make_dynamic ~from ~generator] creates a dynamic rewrite
          rule from a pattern [from] into a schema that is
          conditionally generated based on properties of the EGraph,
          the root eclass of the sub-expression being transformed and
          the eclasses of all bound variables *)

  end

  (** {1:constructors EGraph Constructors} *)

  (** This functor {!MakePrinter} allows users to construct EGraph
      printing utilities for a given {!LANGUAGE} and {!ANALYSIS}. *)
  module MakePrinter : functor (L : LANGUAGE) (A : ANALYSIS) -> sig

    (* val pp : Format.formatter -> (Id.t L.shape, A.t, A.data, 'b) egraph -> unit
     * (\** [pp fmt graph] pretty prints an internal representation of
     *     the graph.
     * 
     *     {b Note}: This is primarily intended for debugging, and the
     *     output format is not guaranteed to remain consistent over
     *     versions.  *\) *)

    val to_dot : (Id.t L.shape, A.t, A.data, 'b) egraph -> Odot.graph
    (** [to_dot graph] converts an EGraph into a Graphviz
        representation for debugging.  *)

  end

  (** This functor {!MakeExtractor} allows users to construct an
      EGraph extraction procedure for a given {!LANGUAGE} and {!COST}
      system. *)
  module MakeExtractor : functor
    (L : LANGUAGE)
    (E : COST with type node := Id.t L.shape) -> sig

    val extract : (Id.t L.shape, 'a, 'b, rw) egraph -> Id.t -> L.t
    (** [extract graph] computes an extraction function [Id.t ->
        Sexplib0.Sexp.t] to extract concrete terms of the language {!L}
        from their respective EClasses (specified by [Id.t]) from the
        EGraph according to the cost system {!E}.  *)

  end


  (** This functor {!Make} serves as the main interface to Ego's
      generic EGraphs, and constructs an EGraph given a {!LANGUAGE}, an
      {!ANALYSIS} and it's {!ANALYSIS_OPS}. *)
  module Make :
    functor
      (L : LANGUAGE)
      (A : ANALYSIS)
      (MakeAnalysisOps : functor
         (S : GRAPH_API with type 'p t = (Id.t L.shape, A.t, A.data, 'p) egraph
                         and type analysis := A.t
                         and type data := A.data
                         and type 'a shape := 'a L.shape
                         and type node := L.t) ->
         ANALYSIS_OPS with type  'p t = (Id.t L.shape, A.t, A.data, 'p) egraph
                       and type analysis := A.t
                       and type data := A.data
                       and type node := Id.t L.shape) ->
    sig

      type 'p t = (Id.t L.shape, A.t, A.data, 'p) egraph
      (** This type represents an EGraph parameterised over a
          particular language {!L} and analysis {!A}. *)

      (** This module {!Rule} defines the rewrite interface for the
          EGraph, allowing users to express relatively complex
          transformations of expressions of the Language {!L}. *)
      module Rule:
        RULE with type query := L.op Query.t
              and type 'a egraph := (Id.t L.shape, A.t, A.data, 'a) egraph


      val freeze : rw t -> ro t
      (** [freeze graph] returns a read-only reference to the EGraph.

          {b Note}: it is safe to modify [graph] after passing it to
          freeze, this method is mainly intended to allow using the
          read-only APIs of the EGraph when you have a RW instance of
          the EGraph. *)

      val init : A.t -> 'p t
      (** [init analysis] creates a new EGraph with an initial
          persistent analysis state of [analysis]. *)

      val class_equal: ro t -> Id.t -> Id.t -> bool
      (** [class_equal graph cls1 cls2] returns true if and only if
          [cls1] and [cls2] are congruent in the EGraph [graph].  *)

      val set_data : rw t -> Id.t -> A.data -> unit
      (** [set_data graph cls data] sets the analysis data for EClass
          [cls] in EGraph [graph] to be [data]. *)

      val get_data : _ t -> Id.t -> A.data
      (** [get_data graph cls] returns the analysis data for EClass
          [cls] in EGraph [graph]. *)

      val get_analysis: rw t -> A.t
      (** [get_analysis graph] returns the persistent analysis sate
          for an EGraph.  *)

      val iter_children : ro t -> Id.t -> Id.t L.shape Iter.t
      (** [iter_children graph cls] returns an iterator over the
          elements of an eclass [cls].  *)

      (* val pp : Format.formatter -> (Id.t L.shape, 'a, A.data, _) egraph -> unit
       * (\** [pp fmt graph] pretty prints an internal representation of
       *     the graph.
       * 
       *     {b Note}: This is primarily intended for debugging, and the
       *     output format is not guaranteed to remain consistent over
       *     versions.  *\) *)

      val to_dot : (Id.t L.shape, A.t, A.data, _) egraph -> Odot.graph
      (** [to_dot graph] converts an EGraph into a Graphviz
          representation for debugging.  *)

      val add_node : rw t -> L.t -> Id.t
      (** [add_node graph term] adds the term [term] into the EGraph
          [graph] and returns the corresponding equivalence class. *)

      val merge : rw t -> Id.t -> Id.t -> unit
      (** [merge graph cls1 cls2] merges the two equivalence classes
          [cls1] and [cls2].  *)

      val rebuild : rw t -> unit
      (** [rebuild graph] restores the internal invariants of the
          graph.

          {b Note}: If you call {!merge} manually (i.e outside of
          analysis functions), you must call {!rebuild} before running
          any queries or extraction. *)

      val find_matches : ro t -> L.op Query.t -> (Id.t * Id.t StringMap.t) Iter.t
      (** [find_matches graph query] returns an iterator over each
          match of the query [query] in the EGraph. *)

      val apply_rules : (Id.t L.shape, A.t, A.data, rw) egraph -> Rule.t list -> unit
      (** [apply_rules graph rules] runs each of the rewrites in [rules]
          exactly once over the egraph [graph] and then returns. *)

      val run_until_saturation:
        ?scheduler:Scheduler.Backoff.t ->
        ?node_limit:[`Bounded of int | `Unbounded] ->
        ?fuel:[`Bounded of int | `Unbounded] ->
        ?until:((Id.t L.shape, A.t, A.data, rw) egraph -> bool) ->
        (Id.t L.shape, A.t, A.data, rw) egraph -> Rule.t list -> bool
      (** [run_until_saturation ?scheduler ?node_limit ?fuel ?until
         graph rules] repeatedly each one of the rewrites in [rules]
         according to the scheduler [scheduler] until no further
         changes occur ({i i.e equality saturation }), or until it
         runs out of [fuel] (defaults to 30) or reaches a [node_limit]
         if supplied (defaults to 10_000) or some predicate [until] is
         satisfied.

         It returns a boolean indicating whether it reached equality
         saturation or had to terminate early.  *)

      (** The module {!BuildRunner} allows users to supply their own
          custom domain-specific scheduling strategies for equality
          saturation by supplying a corresponding Scheduling module
          satisfying {!SCHEDULER} *)
      module BuildRunner (S : SCHEDULER
                          with type 'a egraph := (Id.t L.shape, A.t, A.data, rw) egraph
                           and type rule := Rule.t) :
      sig 

        val run_until_saturation :
          ?scheduler:S.t ->
          ?node_limit:[`Bounded of int | `Unbounded] ->
          ?fuel:[`Bounded of int | `Unbounded] ->
          ?until:((Id.t L.shape, A.t, A.data, rw) egraph -> bool) ->
          (Id.t L.shape, A.t, A.data, rw) egraph -> Rule.t list -> bool
          (** [run_until_saturation ?scheduler ?node_limit ?fuel
             ?until graph rules] repeatedly each one of the rewrites
             in [rules] according to the scheduler [scheduler] until
             no further changes occur ({i i.e equality saturation }),
             or until it runs out of [fuel] (defaults to 30) or
             reaches some [node_limit] (defaults to 10_000) or some
             predicate [until] is satisfied.

              It returns a boolean indicating whether it reached
             equality saturation or had to terminate early.  *)

      end

    end

end
