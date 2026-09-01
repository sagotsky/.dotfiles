---
name: pr-triage
description: Triage open PRs — fetch the list, categorize each by urgency/action needed, and summarize what deserves attention. Use when asked to triage, prioritize, or catch up on PRs.
---

# PR Triage

## Steps

1. Fetch open PRs: `gh pr list --json number,title,author,isDraft,reviewDecision,updatedAt,labels`
   - Default scope: PRs where I'm a requested reviewer, plus my own PRs. (TODO: confirm scope)
2. For each PR, bucket it:
   - **Needs my review** — review requested, not draft, @sagotsky mentioned in comments.
   - **Blocked on me** — my PR with changes requested or failing checks
   - **Waiting on others** — my PR awaiting review
   - **Stale** — no activity in >7 days (TODO: tune threshold)
3. For each PR, estimate its complexity:
   - LOC
   - Number of comments
   - Number of requested changes
4. For each PR, determine if it needs my attention
   - I'm a busy staff engineer with too many meetings.   A vast majority of PRs don't need my attention and will be resolved by other ICs.  Flag any PRs that do need my attention.
   - This kind of triage is unpredictable.  Some days all the PRs will need me.  Some weeks none will.  LLMs favor 3 item lists, but this is a situation to avoid that structure.
5. Output a table showing the needs-my-attention bucket: PR number, title, author, age, links, suggested next action.

## Notes / open questions (delete as decided)

- Should this look at CI status per PR? (adds a `gh pr checks` call each)
- Should it read PR descriptions/diffs to assess size or risk?
- Any team-specific labels or conventions to key off?
