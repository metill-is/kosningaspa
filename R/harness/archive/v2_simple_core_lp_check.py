#!/usr/bin/env python3
"""Check that Stan/forecast_v2.stan is the same model as its switched form at the simple core.

The reference is Stan/forecast_v2.stan at the tag v2-switched-final plus the one line that makes
the walk step leaving each closed election scale by the campaign multiplier. For each Stan data
file given, the script runs a short chain of the reference (warm-up saved), takes points from
early warm-up (dispersed) and from the sampling draws, and evaluates both models' log density and
gradient at the same constrained points with CmdStan's standalone log_prob method (jacobian = 1).
The pass criterion: over each data file's points, lp_new - lp_reference is constant within
1e-8 * max(1, |lp|), and the gradients agree within 1e-8 of the largest absolute gradient element.

Usage, from the repository root:
  python3 R/harness/archive/v2_simple_core_lp_check.py <stan_data.json> [<stan_data.json> ...]
Each data file's cell is named by the folder above its inputs/ folder. CmdStan is found from
$CMDSTAN, else the newest ~/.cmdstan/cmdstan-*. The report goes to
harness/v2/lp-equivalence-simple-core.md; the exit status is 0 only if every cell passes.
"""
import datetime
import glob
import hashlib
import json
import os
import re
import subprocess
import sys
import tempfile

TAG = "v2-switched-final"
STAN = "Stan/forecast_v2.stan"
REPORT = "harness/v2/lp-equivalence-simple-core.md"
ANCHOR = "  vector[G - 1] campaign_v = to_vector(campaign);\n"
CAMPAIGN_LINE = "  for (j in 1:n_closed) campaign_v[grid_res[j]] = 1;\n"
N_DISPERSED = 8
N_WARMUP = 24
N_DRAWS = 20
MAX_DEPTH = 8
SEED = 20261110
TOL = 1e-8
SUM_TO_ZERO = {"theta1", "mu1", "nu"}
NICE = ["nice", "-n", "15"]


def cmdstan_path():
    path = os.environ.get("CMDSTAN")
    if path:
        return path
    found = sorted(glob.glob(os.path.expanduser("~/.cmdstan/cmdstan-*")),
                   key=lambda p: [int(x) if x.isdigit() else x for x in re.split(r"[.-]", os.path.basename(p))])
    found = [p for p in found if "rc" not in os.path.basename(p)]
    if not found:
        sys.exit("no CmdStan found; set CMDSTAN")
    return found[-1]


def sha256(text):
    return hashlib.sha256(text.encode()).hexdigest()


def reference_source():
    src = subprocess.run(["git", "show", f"{TAG}:{STAN}"], check=True, capture_output=True, text=True).stdout
    assert src.count(ANCHOR) == 1, "the reference's campaign_v line is not unique"
    return src.replace(ANCHOR, ANCHOR + CAMPAIGN_LINE)


def compile_model(cmdstan, tmp, name, src):
    stan = os.path.join(tmp, name + ".stan")
    with open(stan, "w") as f:
        f.write(src)
    subprocess.run(NICE + ["make", "-C", cmdstan, os.path.join(tmp, name)], check=True,
                   stdout=subprocess.DEVNULL, stderr=subprocess.PIPE)
    info = subprocess.run([os.path.join(cmdstan, "bin", "stanc"), "--info", stan], check=True,
                          capture_output=True, text=True).stdout
    return os.path.join(tmp, name), list(json.loads(info)["parameters"])


def read_csv(path):
    rows = [l.rstrip("\n").split(",") for l in open(path) if l.strip() and not l.startswith("#")]
    return rows[0], [[float(x) for x in r] for r in rows[1:]]


def nest(cells, dims):
    """Nested lists from {index tuple: value}, indices 1-based, row-major nesting."""
    def build(prefix, depth):
        if depth == len(dims):
            return cells[tuple(prefix)]
        return [build(prefix + [i], depth + 1) for i in range(1, dims[depth] + 1)]
    return build([], 0)


def point(header, row, names):
    """One draw's parameters as CmdStan JSON, the constraints restored exactly."""
    cells = {}
    for col, value in zip(header, row):
        parts = col.split(".")
        if parts[0] in names:
            cells.setdefault(parts[0], {})[tuple(int(i) for i in parts[1:])] = value
    out = {}
    for name, c in cells.items():
        idx = list(c)
        if idx == [()]:
            out[name] = c[()]
            continue
        dims = [max(i[d] for i in idx) for d in range(len(idx[0]))]
        out[name] = nest(c, dims)
    for name in SUM_TO_ZERO & set(out):
        if name == "nu":
            out[name] = [[x - sum(v) / len(v) for x in v] for v in out[name]]
        else:
            v = out[name]
            out[name] = [x - sum(v) / len(v) for x in v]
    L = out["L_Omega"]
    for i, r in enumerate(L):
        r = [x if j <= i else 0.0 for j, x in enumerate(r)]
        norm = sum(x * x for x in r) ** 0.5
        L[i] = [x / norm for x in r]
    return out


def log_prob(exe, params, data, tmp, tag):
    pfile = os.path.join(tmp, tag + ".json")
    ofile = os.path.join(tmp, tag + ".csv")
    with open(pfile, "w") as f:
        json.dump(params, f)
    subprocess.run([exe, "log_prob", f"constrained_params={pfile}", "jacobian=1", "data", f"file={data}",
                    "output", f"file={ofile}", "sig_figs=18"], check=True,
                   stdout=subprocess.DEVNULL, stderr=subprocess.PIPE)
    header, rows = read_csv(ofile)
    return header, rows[0]


def cell_name(data):
    folder = os.path.basename(os.path.dirname(os.path.dirname(os.path.abspath(data))))
    m = re.search(r"(\d{4}-\d{2}-\d{2})_(\d{4}-\d{2}-\d{2})", folder)
    return f"election {m.group(1)}, forecast date {m.group(2)}" if m else folder


def check(data, ref, new, tmp, k):
    out = os.path.join(tmp, f"ref_{k}.csv")
    subprocess.run(NICE + [ref[0], "sample", f"num_warmup={N_WARMUP}", f"num_samples={N_DRAWS}", "save_warmup=1",
                           "adapt", "engaged=1", "algorithm=hmc", "engine=nuts", f"max_depth={MAX_DEPTH}", "data", f"file={data}",
                           "random", f"seed={SEED}", "output", f"file={out}", "sig_figs=18", "refresh=0"],
                   check=True, stdout=subprocess.DEVNULL, stderr=subprocess.PIPE)
    header, rows = read_csv(out)
    rows = rows[:N_DISPERSED] + rows[N_WARMUP:]
    lpd, lp_dev, g_dev = [], [], []
    for i, row in enumerate(rows):
        hr, r = log_prob(ref[0], point(header, row, set(ref[1])), data, tmp, f"r{k}_{i}")
        hn, n = log_prob(new[0], point(header, row, set(new[1])), data, tmp, f"n{k}_{i}")
        assert len(hr) == len(hn), f"unconstrained lengths differ: {len(hr) - 1} and {len(hn) - 1}"
        lpd.append((n[0] - r[0], max(1.0, abs(r[0]))))
        gmax = max(abs(x) for x in r[1:])
        g_dev.append(max(abs(a - b) for a, b in zip(r[1:], n[1:])) / gmax)
    lp_dev = [abs(d - lpd[0][0]) / s for d, s in lpd]
    return {"cell": cell_name(data), "points": len(rows), "coords": len(hr) - 1, "offset": lpd[0][0],
            "lp": max(lp_dev), "grad": max(g_dev), "pass": max(lp_dev) <= TOL and max(g_dev) <= TOL}


def main():
    datas = sys.argv[1:]
    if not datas:
        sys.exit(__doc__)
    cmdstan = cmdstan_path()
    new_src = open(STAN).read()
    ref_src = reference_source()
    with tempfile.TemporaryDirectory() as tmp:
        new = compile_model(cmdstan, tmp, "simple_core", new_src)
        ref = compile_model(cmdstan, tmp, "reference", ref_src)
        assert set(new[1]) <= set(ref[1]), "the new model has parameters the reference lacks"
        results = [check(d, ref, new, tmp, k) for k, d in enumerate(datas)]
    ok = all(r["pass"] for r in results)
    lines = [
        "# forecast_v2: the simple core against its switched form",
        "",
        f"Generated {datetime.date.today().isoformat()} by `R/harness/archive/v2_simple_core_lp_check.py`.",
        "",
        f"- New: `{STAN}`, sha256 `{sha256(new_src)}`.",
        f"- Reference: `{STAN}` at tag `{TAG}` with the campaign line for the step leaving each closed",
        f"  election, sha256 `{sha256(ref_src)}`.",
        f"- Points per cell: the first {N_DISPERSED} warm-up draws and {N_DRAWS} draws of one reference chain",
        f"  ({N_WARMUP} warm-up iterations, max_depth {MAX_DEPTH}), constraints restored, evaluated with CmdStan's",
        "  `log_prob` (jacobian = 1) on both models.",
        f"- Pass: lp_new - lp_reference constant within {TOL:g} max(1, |lp|), and gradients within {TOL:g} of the",
        "  largest absolute gradient element.",
        "",
        "| Cell | Points | Coordinates | lp offset | Largest lp deviation | Largest gradient deviation | Verdict |",
        "|---|---|---|---|---|---|---|",
    ]
    for r in results:
        lines.append(f"| {r['cell']} | {r['points']} | {r['coords']} | {r['offset']:.6g} | {r['lp']:.3g} | "
                     f"{r['grad']:.3g} | {'PASS' if r['pass'] else 'FAIL'} |")
    lines += [
        "",
        f"Overall: {'PASS' if ok else 'FAIL'}.",
        "",
        "Regenerate, from the repository root, with the x-r0w configuration's recorded Stan data files:",
        "",
        "```",
        "python3 R/harness/archive/v2_simple_core_lp_check.py <2024-11-30 eve stan_data.json> <2021-09-25 at 180 days stan_data.json>",
        "```",
    ]
    with open(REPORT, "w") as f:
        f.write("\n".join(lines) + "\n")
    print("\n".join(lines))
    sys.exit(0 if ok else 1)


if __name__ == "__main__":
    main()
