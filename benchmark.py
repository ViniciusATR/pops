import time
import statistics
import itertools
from subprocess import run


def time_metrics(exec_times):

    d = {}

    for size, n, t in exec_times:
        d.setdefault((size, n), []).append(t)

    reses = []
    for size, n_worker in d:

        n = len(d[(size, n_worker)])
        cumsum = sum(d[(size, n_worker)])
        avg = cumsum / n

        stdev = statistics.stdev(d[(size, n_worker)])

        reses.append(
            f"Avg time for {n_worker} workers and {size} pop_size: {avg} +- {stdev}\n"
        )

    return reses


def single_run(algo, seed, size, n_workers):

    start = time.time()

    res = run(
        [
            "stack",
            "exec",
            algo,
            f"{seed}",
            "100",
            f"{size}",
            "+RTS",
            f"-N{n_workers}",
        ],
        capture_output=True,
        encoding="utf-8"
    )

    end = time.time()
    return end - start, float(res.stdout)


def run_bench(algo):

    if algo in ["ga", "pso", "optai"]:
        workers = [4]
    else:
        workers = [2, 4, 6, 8]

    scenarios = list(itertools.product(*[seeds, pop_size, workers]))

    result = 999999.99999
    results = []
    for sc in scenarios:
        s, size, n = sc
        run_time, res_val = single_run(algo, s, size, n)
        if res_val < result:
            result = res_val
        results.append((size, n, run_time))
    return result, results


algos = ["ga", "parga", "pso", "parpso", "optai", "paroptai"]
pop_size = [100, 1000, 10000]
seeds = [42, 21, 10, 1000]

for algo in algos:
    value, res = run_bench(algo)
    print(f"Benchmark for {algo}")
    metrics = time_metrics(res)

    with open(f"metrics/{algo}_metrics.txt", "w") as f:
        f.write(f"Best achieved result: {value}\n")
        f.writelines(metrics)
