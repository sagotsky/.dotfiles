# Go Documentation Skill

When the user asks for documentation for any Go function, method, or type, provide the appropriate `go doc` command instead of fetching or explaining the docs.

**Format:**
- Functions: `go doc package.Function`
- Methods: `go doc package.Type.Method`
- Types: `go doc package.Type`
- Packages: `go doc package`

**Examples:**
- "show me sort.Slice docs" → `go doc sort.Slice`
- "fmt.Println docs" → `go doc fmt.Println`
- "docs for http.Server.ListenAndServe" → `go doc http.Server.ListenAndServe`

Always emit the command directly without preamble.
