[![Haskell CI](https://github.com/Matteo0301/Chimera/actions/workflows/haskell.yml/badge.svg)](https://github.com/Matteo0301/Chimera/actions/workflows/haskell.yml)
[![Haskell CI](https://github.com/Matteo0301/Chimera/actions/workflows/haskell.yml/badge.svg)](https://github.com/Matteo0301/Chimera/actions/workflows/haskell.yml)[![Scan code with HLint](https://github.com/Matteo0301/Chimera/actions/workflows/lint.yml/badge.svg)](https://github.com/Matteo0301/Chimera/actions/workflows/lint.yml)

# Chimera
An experimental chess engine written in haskell


## To-do list
## Board representation
- [ ] Quad-bitboards
- [ ] Also 8x8 or piece-list
- [ ] PEXT bitboards

## Move generation
- [ ] Take inspiration from Gigantua/DirGolem
- [ ] Staged move generation
- [ ] Different for quiescence/check/mate search

## Evaluation
- [ ] Stockfish HCE
- [ ] NNUE (with butterfly output for move ordering)

## NNUE
- [ ] Inspired by Stockfish
- [ ] HalfKP
- [ ] Relative HalfKP/HalfKA factorization
- [ ] Pytorch training
- [ ] Q-learning for final nodes

## Search
- [ ] Alpha-beta (PVS)
- [ ] Transposition table
- [ ] Opponent model (prune heavily on even plies)
- [ ] Quiescence search
- [ ] Iterative deepening
- [ ] Aspiration window
- [ ] MCTS with alpha-beta rollout
- [ ] PUCT
- [ ] Explore other selection strategies ([See this)
- [ ] Multi-PV (Also with MCTS)
- [ ] PDNS for mate searching in unbalanced (or unexpectedly ended) positions
- [ ] Parallel search: split tree between MCTS  threads (YBWC) and LazySMP for alpha-beta (but this is to be expanded)
- [ ] Retrograde analysis

## Selectivity
This part is still to be investigated
### Extensions
- [ ] Botvinnik-Markoff Extensions
- [ ] Capture Extensions
- [ ] Check Extensions
- [ ] Mate Threat Extensions
- [ ] One Reply Extensions
- [ ] Passed Pawn Extensions
- [ ] PV Extensions
- [ ] Recapture Extensions
- [ ] SEX Algorithms
- [ ] Singular Extensions
  
### Reductions
- [ ] Fail-High Reductions - FHR
- [ ] Late Move Reductions - LMR
- [ ] Null Move Reductions
- [ ] RankCut
- [ ] Razoring

### Pruning
- [ ] AEL-Pruning
- [ ] Delta Pruning
- [ ] Enhanced Forward Pruning
- [ ] Futility Pruning
- [ ] History Leaf Pruning
- [ ] Mate Distance Pruning
- [ ] Move Count Based Pruning (Late Move Pruning)
- [ ] Multi-Cut
- [ ] Null Move Pruning
- [ ] Parity Pruning
- [ ] ProbCut
- [ ] Reverse Futility Pruning
- [ ] Uncertainty Cut-Offs

## Move ordering
- [ ] Different ordering based on node type/horizon distance
- [ ] Oracle approach at root node (development bonus by Ronald De Man)
- [ ] Different ordering for quiescence/mate search
- [ ] PV-Move from the principal variation of the previous Iteration
- [ ] Hash Move Table, if available
- [ ] Internal Iterative Deepening
- [ ] MVV-LVA
- [ ] SEE
- [ ] Killer heuristic
- [ ] Relative History Heuristic (with butterfly/nnue interpolation)
- [ ] Mate Killers
- [ ] Countermove Heuristic
- [ ] Guard Heuristic
- [ ] Last Best Reply
- [ ] Threat Move from null move refutations
- [ ] Enhanced Transposition Cutoff (ETC)
- [ ] Refutation Table

## Algorithms
- [ ] Stockfish RNG
- [ ] De Bruijn bitscan (or hardware implementation)

## Tuning
- [ ] Texel's tuning method
- [ ] TDL for unsupervised learning option
- [ ] Evolutionary algorithm

## Behavior controllers
- [ ] Dynamism (by Komodo)
- [ ] Tactical/Strategical
- [ ] Contempt

## Opening book
- [ ] Own format
- [ ] Also for book learning

## Endgame bitbases
- [ ] Own format
- [ ] With Syzygy/Scorpio ideas