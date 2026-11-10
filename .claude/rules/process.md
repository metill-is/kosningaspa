# Process rules that protect history

- **Merge with a merge commit, never a squash, when a test reads a commit by sha.**
  `tests/harness/test-r0-stan.R` and `tests/harness/test-r0-inputs.R` read production's files at
  `R0_PARENT_COMMIT` (`tests/harness/helper-root.R`) with `git show`. A squash that drops that
  commit from history breaks them.
- **A pinned test or its fixture changes only on Brynjólfur's yes.** Never delete or comment out
  a failing test: find out why it fails, then ask before changing it.
- **Pushes need a yes.** Commits stay local until Brynjólfur says to push.
- **2017 is a locked hold-out.** No forecast of the 2017 election is fitted or scored before
  `harness/DECISION.md` exists (`harness/README.md`, The freeze and the 2017 locks).
- **A rejected model variant is removed at a tag, not kept behind a switch.** Name the tag in
  the commit that removes it, so the record can still point at the code.
- **A new check names the mistake it would have caught.** Instruction files hold entry points
  and invariants, not function-by-function descriptions of the code.
