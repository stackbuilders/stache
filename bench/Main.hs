{-# LANGUAGE CPP #-}

module Main (main) where

import Criterion.Main
import Text.Mustache.Parser
import Text.Mustache.Type
import Text.Mustache.Type

----------------------------------------------------------------------------
-- Configuration parameters

-- To configure the benchmark, build the benchmarks like this:
-- $ cabal build --ghc-options="-DBENCHMARK_STEPS=10 -DBENCHMARK_SIZE=1000"

-- | Number of step the benchmark will have.

benchSteps :: Int
#if BENCHMARK_STEPS
benchSteps = BENCHMARK_STEPS
#else
benchSteps = 5
#endif

-- | Number of iterations in a single benchmark.

benchSize :: Int
#if BENCHMARK_SIZE
benchSize = benchmark_SIZE
#else
benchSize = 100
#endif

----------------------------------------------------------------------------
-- Benchmarks

main :: IO ()
main = defaultMain [ parserBench, renderBench ]

parserBench :: Benchmark
parserBench = undefined
-- single text block of lorem ipsum or something like that
-- text with escaped var
-- text with unescaped var
-- text with unescaped var {{{}}}
-- text with section
-- inverted section with text
-- nested sections
-- text with partial
-- complex example with all types of parts

renderBench :: Benchmark
renderBench = undefined
-- single text block of lorpm ipsum or something like that
-- text with escaped var
-- text with unescaped var
-- text with unescaped var {{{}}}
-- text with section
-- inverted section with text
-- nested sections
-- text with partial
-- complex example with all types of parts
