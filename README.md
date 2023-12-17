[![Check formatting](https://github.com/Matteo0301/Chimera/actions/workflows/format.yml/badge.svg)](https://github.com/Matteo0301/Chimera/actions/workflows/format.yml)[![Lint code](https://github.com/Matteo0301/Chimera/actions/workflows/lint.yml/badge.svg)](https://github.com/Matteo0301/Chimera/actions/workflows/lint.yml)[![Tests](https://github.com/Matteo0301/Chimera/actions/workflows/haskell-ci.yml/badge.svg)](https://github.com/Matteo0301/Chimera/actions/workflows/haskell-ci.yml)[![Static analysis](https://github.com/Matteo0301/Chimera/actions/workflows/stan.yml/badge.svg)](https://github.com/Matteo0301/Chimera/actions/workflows/stan.yml)


# Chimera
This is a chess engine written entirely in Haskell. The idea is to avoid, completely if possible, the use of other languages
(and of the FFI). The engine will rely on the classic alpha/beta algorithm, possibly extended to a MCTS with alpha/beta rollout. It will use magic bitboards (or PEXT bitboards if available). The goal is for me to explore all the possibilities offered by the language, especially when coming to the testing and benchmarking part. Because of this, the solutions implemented may not always be the cleanest or the most well engineered, but I will try to keep them efficient and readable.