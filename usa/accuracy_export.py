"""Assemble the live page's historical comparison of the model against polls and forecasters.

    python3 -I usa/accuracy_export.py <backtest dir> <accuracy dir> <out dir>

Reads the current model's combined backtest scores (<backtest dir>/scores.csv, from
usa/R/score_combined.R) and the fixed comparisons in <accuracy dir> (house.csv: the national
House vote 1998-2022 from the v8 history backtests; senate.csv: the v9 Senate backtests;
pres.csv: v9's presidential states against FiveThirtyEight, the Economist and Silver
Bulletin). Writes <out dir>/accuracy.json: each block names the model version it scores, so a
refit with new backtests replaces the "current" block and the older blocks say what they are.
Margins are D minus R as a share of the two-party vote, in points.
"""

import csv
import json
import pathlib
import sys


def rows(path):
    with open(path, encoding="utf-8") as f:
        return list(csv.DictReader(f))


def lab(m):
    return f"D+{m:.1f}" if m >= 0 else f"R+{-m:.1f}"


def num(x):
    try:
        return round(float(x), 4)
    except (TypeError, ValueError):
        return None


def main(bt: pathlib.Path, acc: pathlib.Path, out: pathlib.Path) -> None:
    cur = rows(bt / "scores.csv")
    current = {
        "model": cur[0]["model"] if cur else None,
        "what": "Combined House and Senate model, election-eve backtests: each forecast uses only data available before that election.",
        "cycles": [
            {
                "year": int(r["year"]),
                "national": {"actual": num(r["national_actual"]), "model": num(r["national_model"]), "polls": num(r["national_polls"]),
                             "model_q05": num(r["national_q5"]), "model_q95": num(r["national_q95"]), "pit": num(r["national_pit"])},
                "house_seats": {"actual": int(float(r["house_seats_actual"])), "model": num(r["house_seats_model"]),
                                "q05": num(r["house_seats_q5"]), "q95": num(r["house_seats_q95"]), "pit": num(r["house_seats_pit"])},
                "house_districts": {"model_mae": num(r["house_mae"]), "uniform_swing_mae": num(r["house_base_mae"]),
                                    "model_brier": num(r["house_brier"]), "uniform_swing_brier": num(r["house_base_brier"]),
                                    "cov90": num(r["house_cov90"]), "called": num(r["house_called"])},
                "senate": {"polled_races": int(r["senate_polled"]), "model_mae": num(r["senate_mae"]), "polls_mae": num(r["senate_polls_mae"]),
                           "model_called": int(r["senate_called"]), "polls_called": int(r["senate_polls_called"]), "races": int(r["senate_n"]),
                           "brier": num(r["senate_brier"]), "cov90": num(r["senate_cov90"])},
            }
            for r in sorted(cur, key=lambda r: r["year"])
        ],
    }
    house = rows(acc / "house.csv")
    long_run = {
        "model": "v8 (national generic-ballot model)",
        "what": "National House popular vote, election eve, absolute error in points: model against the final-60-day poll average.",
        "cycles": [
            {"year": int(y), "model_error": num(next(r["err"] for r in house if r["year"] == y and r["who"] == "Model")),
             "polls_error": num(next(r["err"] for r in house if r["year"] == y and r["who"] == "Poll average"))}
            for y in sorted({r["year"] for r in house})
        ],
    }
    senate = [{k: num(v) for k, v in r.items()} for r in rows(acc / "senate.csv")]
    pres = [{"year": int(r["year"]), "who": r["who"], "brier": num(r["brier"]), "mae_polled_states": num(r["mae_polled"]),
             "called": int(r["called"]), "states": int(r["n"])} for r in rows(acc / "pres.csv")]
    doc = {
        "current": current,
        "house_vote_long_run": long_run,
        "senate_v9": {"model": "v9 (Senate only, before the combined model)", "cycles": senate},
        "presidential_states": {"model": "v9", "what": "Presidential states 2008-2024 against published final forecasts; Brier on win probabilities (51 states), margin MAE on states with final-month polls.", "rows": pres},
        "notes": "Margins are two-party. FiveThirtyEight is Silver's model to 2020 (polls-only in 2016; its 2008 archive has no state probabilities) and Morris's in 2024.",
    }
    out.mkdir(parents=True, exist_ok=True)
    (out / "accuracy.json").write_text(json.dumps(doc, indent=1))
    # run_info's backtest note is computed from the same scores, so it never goes stale.
    ri = out / "run_info.json"
    if ri.exists() and current["cycles"]:
        parts = []
        for c in current["cycles"]:
            h, n, s = c["house_seats"], c["national"], c["senate"]
            inside = h["q05"] <= h["actual"] <= h["q95"]
            parts.append(f"{c['year']}: House {h['model']:.0f} D seats (90% {h['q05']:.0f}-{h['q95']:.0f}) vs {h['actual']} actual"
                         f"{'' if inside else ' (outside)'}; national {lab(n['model'])} vs {lab(n['actual'])}; Senate {s['model_called']}/{s['races']} called")
        info = json.loads(ri.read_text())
        info["backtest_note"] = f"Election-eve backtests of model {current['model']}: " + " | ".join(parts) + "."
        ri.write_text(json.dumps(info, indent=1))
    print(f"wrote {out / 'accuracy.json'}: current {current['model']} {len(current['cycles'])} cycles")


if __name__ == "__main__":
    main(*map(pathlib.Path, sys.argv[1:4]))
