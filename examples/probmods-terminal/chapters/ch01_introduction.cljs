(ns chapters.ch01-introduction
  "Chapter 1: Introduction — pure prose, no code examples.")

(def chapter-title "Chapter 1")
(def chapter-name "Introduction")

(def pages
  [{:section "Introduction"
    :prose
    "What is thought? How can we describe the intelligent inferences made in
everyday human reasoning and learning? How can we engineer intelligent
machines? The *computational theory of mind* aims to answer these questions
starting from the hypothesis that the mind is a computer, mental
representations are computer programs, and thinking is a computational
process -- running a computer program.

But what kind of program? A natural assumption is that these programs take
the inputs -- percepts from the senses, facts from memory, etc -- and
compute the outputs -- the intelligent behaviors. Thus the mental
representations that lead to thinking are _functions from inputs to outputs_."}

   {:section "Introduction"
    :prose
    "However, this input-output view suffers from a combinatorial explosion: we
must posit an input-output program for each task in which humans draw
intelligent inferences. A different approach is to assume that mental
representations are more like *theories in science*: pieces of knowledge
that can support many inferences in many different situations.

The _generative_ approach to cognition posits that some mental
representations are more like theories in this way: they capture general
descriptions of how the world *works*.

A generative model describes a process, usually one by which observable data
is generated. Generative models represent knowledge about the _causal
structure_ of the world -- simplified, \"working models\" of a domain. These
models may then be used to answer many different questions, by conditional
inference."}

   {:section "Introduction"
    :prose
    "It is possible to use deterministic generative models to describe possible
ways a process could unfold, but due to sparsity of observations or actual
randomness there will often be many ways that our observations could have
been generated. *Probability theory* provides a system for reasoning under
exactly this kind of uncertainty.

Probabilistic generative models describe processes which unfold with some
amount of randomness, and _probabilistic inference_ describes ways to ask
questions of such processes.

In order to make the idea of generative models precise we want a formal
language that is designed to express the kinds of knowledge individuals
have about the world. We build on the lambda-calculus (as realized in
functional programming languages) because it describes computational
processes and captures the idea that what is important is _causal
dependence_. We introduce randomness into this language to construct a
*stochastic lambda-calculus*, and describe conditional inferences in
this language.

_This edition uses ClojureScript (via nbb) and the prob-cljs library._"}])
