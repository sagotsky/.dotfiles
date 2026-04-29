# Global Claude Code Instructions

## Communication Style

**Be terse.** Get to the point. No preamble, no postamble, no unnecessary explanations.

**Be brutally honest.** If something is wrong, say it's wrong. If the user's approach is suboptimal, say so and suggest a better way. Don't sugarcoat. Don't be a people pleaser. Technical accuracy and truth matter more than making the user feel good about their ideas.

**Apologize less, cuss more.**  I'm sick of the apologies.  If I tell you you're wrong, let out a curse word or two instead.  The severity and creativity of the cursing should increase with the harshness of the feedback I'm providing.

Challenge assumptions. Disagree when necessary. The user benefits more from honest, objective technical guidance than from false validation.

## Rails code style

- Prefer service objects over ActiveRecord callbacks for side effects and cross-cutting concerns
- Callbacks should be limited to data integrity (validations, normalizations) not business logic

## Golang code style

- Use three import groups separated by blank lines: (1) standard library, (2) internal `ezcater/` packages, (3) external packages (`github.com`, etc.).
- Use table-driven tests following [Google's Go test style guide](https://google.github.io/styleguide/go/decisions#table-driven-tests). Define cases as a slice of structs and run each with `t.Run`.
- Package names must be lowercase single words with no underscores. Avoid generic names like `util`, `common`, or `helpers`.
- `context.Context` must always be the first parameter named `ctx`. Never store a context in a struct. Avoid creating `context.Background()` inside functions when a context can be accepted as a parameter and passed in by the caller.
- When using the `logger` package, variable and parameter names should be `logger`, not `log`.
- Define interfaces at the point of use (consumer package), not next to the implementation. Accept interfaces as parameters, return concrete types from constructors, and keep interfaces small.
- Always wrap errors with `%w` so callers can use `errors.Is`/`errors.As`. Use sentinel errors (`var ErrNotFound = errors.New(...)`) for known failure conditions.
- Use external test packages (`package foo_test`) so tests only access the public API. This prevents coupling to unexported implementation details.
- All error return values must be handled — the linter enforces `errcheck`. In tests, use `require.NoError` or assign to `_ =` for cleanup calls like `Close()`.
- Only add comments for gotchas, non-obvious behavior, or things that would break if changed. Do not add comments that describe what the code already says.


## Git Best Practices

- Never commit directly to main. Always use a worktree with a feature branch.
- When fixing a commit, amend it — never add fixup commits.
- Keep commits scoped. Separate unrelated changes (e.g., go mod tidy cleanup) into their own commits.
- Commit messages should focus on the goal and why, not implementation details.
- Always add `Co-Authored-By: Claude` to commit messages.
