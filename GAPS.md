# prob-cljs vs webchurch: Gap Analysis

Systematic comparison of prob-cljs against [webchurch](https://github.com/probmods/webchurch) to catalog missing features.

## ERPs / Distributions

| Distribution | webchurch | prob-cljs | Gap |
|---|---|---|---|
| flip (Bernoulli) | sampling + scoring + proposal (negate) + enumerable | sampling only | **Missing: scorer, proposal, enumeration** |
| gaussian | sampling + scoring + drift proposal | sampling only | **Missing: scorer, drift proposal** |
| uniform (continuous) | sampling + scoring | sampling only | **Missing: scorer** |
| uniform-draw | sampling + scoring (via multinomial) + enumerable | sampling only | **Missing: scorer, enumeration** |
| multinomial | sampling + scoring + proposal (project-out) + enumerable | sampling only | **Missing: scorer, proposal, enumeration** |
| beta | sampling + scoring | sampling only | **Missing: scorer** |
| gamma | sampling + scoring | sampling only | **Missing: scorer** |
| dirichlet | sampling + scoring | sampling only | **Missing: scorer** |
| exponential | sampling + scoring | sampling only | **Missing: scorer** |
| random-integer | sampling + scoring + enumerable | sampling only | **Missing: scorer, enumeration** |
| sample-discrete | sampling + scoring | sampling only | **Missing: scorer** |
| **binomial** | full ERP with scoring | not implemented | **Missing entirely** |
| **poisson** | full ERP with scoring | not implemented | **Missing entirely** |

### Key structural gap

In webchurch, every ERP is a `RandomPrimitive` object with:
- `sample_impl(params)` -- forward sampling
- `logprob(val, params)` -- log-probability scoring
- `proposal(currval, params)` -- MH proposal kernel (default: resample from prior)
- `logProposalProb(currval, propval, params)` -- log-probability of proposal
- `nextVal(currval, params)` -- for enumerable ERPs: iterate over domain

In prob-cljs, ERPs are bare sampling functions with no scoring or proposal support. This is the single biggest gap -- it blocks real MH and real enumeration.

## Inference

| Capability | webchurch | prob-cljs | Gap |
|---|---|---|---|
| Rejection sampling | Trace-based, uses logprob-genlogprob ratio | Simple: run thunk, catch rejection exception | Works but no scoring |
| MH / MCMC | Real single-site trace MH with proposals + acceptance ratio | Repeated rejection sampling (not actual MH) | **Not real MH -- just repeated rejection** |
| Exact enumeration | Trace-based, iterates all ERP domain combinations | Approximate: runs 1000 rejection samples, aggregates | **Not real enumeration** |
| LARJ (reversible jump) | Full implementation with annealing for trans-dimensional models | not implemented | **Missing entirely** |
| Suwa-Todo (irreversible) | Full irreversible MCMC kernel for discrete variables | not implemented | **Missing entirely** |
| Marginalization | Nested inference as ERP via `marginalize()` | not implemented | **Missing entirely** |
| `condition` | Sets `conditionsSatisfied` flag on trace | Throws exception (rejection-style) | Functional but different mechanism |
| `factor` | Adds to trace logprob (exact) | Probabilistic rejection via `log(U) > logweight` | **Approximate, not exact** |
| `condition-equal` | Enumerates function + factors by probability | not implemented | **Missing** |
| Conditioning optimization | Compiler rewrites `(condition (= (erp) val))` to direct conditioning | none | Missing (less relevant without a compiler) |
| Structural vs non-structural ERPs | Supported, affects which MCMC moves are used | not implemented | **Missing** |
| Initialization modes | rejection / enumerate / lessdumb | none (forward sample only) | **Missing** |

### How webchurch MH actually works

1. Initializes a trace (via rejection or enumeration-based initialization)
2. At each step, `RandomWalkKernel` selects a single random variable uniformly at random from all free (unconditioned) variables
3. Proposes a new value using the ERP's `proposal()` method
4. Re-executes the program with that change via `traceUpdate`
5. Applies standard MH acceptance: `acceptThresh = nextTrace.logprob - currTrace.logprob + rvsPropLP - fwdPropLP`

### How webchurch enumeration actually works

Not continuation-based. Trace-based:
1. Creates an initial trace where all enumerable ERPs are initialized to the first value in their domain
2. Iterates through all combinations using `nextEnumState()` (odometer-style counter)
3. For each complete assignment, re-executes via `traceUpdate()` to compute log-probability
4. Only works for programs with finitely enumerable random choices (flip, multinomial)
5. Uses Kahan summation for numerical stability

## Trace / Address System

| Capability | webchurch | prob-cljs | Gap |
|---|---|---|---|
| Execution traces | `RandomExecutionTrace` with variable database | none | **Missing entirely** |
| Structural addresses | Call-site ID stack + loop counter | none | **Missing entirely** |
| `traceUpdate` (replay) | Re-execute program with changed variables | none | **Missing entirely** |
| Variable records | name, ERP, params, value, logprob, structural?, conditioned? | none | **Missing entirely** |

The trace system is the backbone of real MCMC. Each random choice gets a structural address from the call-site stack + loop counter. This allows MCMC to identify corresponding random choices across executions.

### Webchurch address construction

```
Call stack path (colon-separated call-site IDs) + "." + loop counter
Example: ":0:3:7.2" = third call at the stack path :0:3:7
```

### Webchurch variable record

```
name        -- address string
erp         -- the ERP object
params      -- parameters to the ERP
val         -- current value
logprob     -- log P(val | params)
active      -- touched during current traceUpdate?
structural  -- is this a structural choice?
conditioned -- is the value conditioned?
```

## Memoization

| Capability | webchurch | prob-cljs | Gap |
|---|---|---|---|
| `mem` | Deterministic memoization | Implemented | OK |
| `DPmem` | Dirichlet Process memoization (Chinese Restaurant Process) | not implemented | **Missing** |

DPmem is needed for nonparametric Bayesian models (infinite mixture models, HDP, etc.). It maintains per-argument-set tables of labels and counts, sampling from a multinomial over existing labels plus a "new table" option weighted by concentration parameter alpha.

## Builtins

| Category | webchurch | prob-cljs | Gap |
|---|---|---|---|
| List ops | Full suite | Full suite | OK |
| Math | Full suite | Full suite | OK |
| Comparison | Full suite | Full suite | OK |
| String ops | Full suite | Full suite | OK |
| Set ops | union, intersection, difference | Same | OK |
| `curry` / `uncurry` | Implemented | not implemented | Minor gap |
| `read-file` / `read-csv` / `write-csv` | Implemented | not implemented | Minor (users have `js/require "fs"`) |
| `set-seed` | PRNG seeding | not implemented | **Missing** |
| Visualization (`hist`, `density`, `scatter`, etc.) | Full Vega-based viz suite | not implemented | Missing (different context) |

## Priority Roadmap

### Critical (blocks correct probabilistic inference)

1. **ERP scoring** -- every ERP needs a `logprob` function, not just sampling. Without this, MH acceptance ratios can't be computed.
2. **Trace system** -- execution traces with structural addresses are the backbone of real MCMC. Without them, you can't replay programs with changed random choices.
3. **Real MH** -- current "mh-query" is just repeated rejection, not Metropolis-Hastings at all. Needs traces + proposals + acceptance.
4. **Real enumeration** -- needs `nextVal` on discrete ERPs to iterate over domains, not sampling approximation.

### Important (needed for expressive models)

5. **Binomial and Poisson** distributions
6. **DPmem** -- Dirichlet Process stochastic memoization for nonparametric Bayesian models
7. **Marginalization** -- nested queries (inference within inference)
8. **PRNG seeding** -- reproducibility

### Nice to have

9. LARJ (Locally Annealed Reversible Jump MCMC for trans-dimensional inference)
10. Suwa-Todo (irreversible MCMC kernel for discrete variables)
11. `condition-equal` (enumeration-based soft conditioning)
12. Visualization
13. `curry` / `uncurry`

## What webchurch does NOT have

For reference, these are things absent from webchurch that could be future differentiators for prob-cljs:

- Particle filtering / SMC
- Variational inference
- Hamiltonian Monte Carlo (HMC)
- Slice sampling
- Automatic differentiation
- Gradient-based methods of any kind

These are areas where prob-cljs could eventually go beyond webchurch (closer to WebPPL territory).
