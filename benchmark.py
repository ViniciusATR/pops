import time
import sys
import statistics
import itertools
from subprocess import run


def time_metrics(exec_times):

    d = {}

    for n, t in exec_times:
        d.setdefault(n, []).append(t)

    for n_worker in d:

        n = len(d[n_worker])
        cumsum = sum(d[n_worker])
        avg = cumsum / n

        stdev = statistics.stdev(d[n_worker])

        print(f"Avg time for {n_worker} workers: {avg} +- {stdev}")


def single_run(algo, seed, iteration, size, n_workers):

    start = time.time()

    run(
        [
            "stack",
            "exec",
            algo,
            f"{seed}",
            f"{iteration}",
            f"{size}",
            "+RTS",
            f"-N{n_workers}",
        ],
    )

    end = time.time()
    return end - start


def run_bench(algo):

    results = []
    for sc in scenarios:
        s, i, size, n = sc
        run_time = single_run(algo, s, i, size, n)
        results.append((n, run_time))
    return results


algo = sys.argv[1]
iterations = sys.argv[2]
pop_size = sys.argv[3]
seeds = [42, 21, 10, 1000, 90, 12, 23, 14, 999, 69]

if algo in ["ga", "pso", "optai"]:
    workers = [4]
else:
    workers = [2, 4, 6, 8, 16]

scenarios = list(itertools.product(*[seeds, [iterations], [pop_size], workers]))

res = run_bench(algo)
print(f"Benchmark for {algo} with {iterations} iterations and {pop_size} pop_size")
time_metrics(res)
