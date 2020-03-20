# hfl-benchmark

Benchmark set for νHFL validity checker

## Usage

```
$ dune exec ./benchmark.exe -- CASE --script SCRIPT
```

- `CASE`: specify file in `./lists`
- `SCRIPT`: specify script in `./scripts`


## Benchmark set

|                             | hfl | ml  | hochc |
|-----------------------------|-----|-----|-------|
| mochi_test_web              | o   | o   | x     |
| non-termination             | o   | o   | x     |
| popl18                      | o   | o   | o     |
| test_safe_2019              | o   | o   | x     |
| test_safe_2019-adt          | o   | o   | x     |
| test_safe_2019-esop2017     | o   | o   | x     |
| test_safe_2019-fpice        | o   | o   | x     |
| test_safe_2019-mochi        | o   | o   | o     |
| test_safe_2019-termination  | o   | o   | x     |

(test_safe_2019-mochi = 旧horus-cps)
