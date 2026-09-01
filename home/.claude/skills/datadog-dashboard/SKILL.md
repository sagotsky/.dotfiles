---
name: datadog-dashboard
description: Generate a Datadog dashboard JSON from all metrics in the codebase
---

# Datadog Dashboard Generator

Generate a Datadog dashboard JSON file that visualizes all Datadog metrics found in the current repository.

## What This Does

1. Searches the entire codebase for Datadog metrics (statsd/DataDog client calls, metric names, types)
2. Extracts unique metric names and their types
3. Creates a Datadog dashboard JSON file that can be imported into Datadog
4. Organizes metrics into logical sections with appropriate visualizations

## Output

Creates a `datadog_dashboard.json` file in the project root that you can import directly into Datadog:
- Go to Dashboards → New Dashboard
- Click settings gear → "Import dashboard JSON"
- Paste the file contents

## Pre-Implementation Checklist

BEFORE generating the dashboard, verify these requirements:
- [ ] All metric queries will use `{*}` wildcard filters
- [ ] No `.as_count()` will be used with `by` clauses
- [ ] Counter metrics from `statsd.increment()` will NOT have `.as_count()`
- [ ] Gauge metrics with grouping will use `avg:metric{tags} by {tag}` NOT `sum:...as_count()`

## Implementation

1. **Explore the codebase** to find all Datadog metrics using an agent to search for:
   - Statsd client calls (statsd.gauge, statsd.increment, statsd.histogram, etc.)
   - Metric names and tags
   - Custom metrics being reported
   - Metric configuration or definitions

   Return a comprehensive list of all unique metric names with their types (gauge, counter, histogram, etc.) and what they measure.

2. **Generate the dashboard JSON** with:
   - All metric queries using `{*}` wildcard filters
   - Proper widget structure using only valid Datadog widget types
   - Valid types: `timeseries`, `bar_chart`, `note`, `query_value`, `heatmap`, etc.
   - Organized sections with section headers as `note` type widgets
   - Appropriate display types (line, bars, etc.) for each metric
   - Aggregation functions (avg, sum, p50, p95, p99) based on metric type

3. **Use `note` type widgets** for headers and markdown content (not `markdown` or `heading` types)

4. **Structure the dashboard** with correct Datadog API format:
   - Root level properties: title, description, layout_type, widgets
   - DO NOT wrap in a `definition` object at root level
   - Title describing the application
   - Description with purpose
   - `layout_type: "ordered"`
   - Widgets array with all charts
   - Each widget has a `definition` object containing type, title, and requests
   - Each request has `q` (query string) and `display_type`
   - Example structure:
     ```json
     {
       "title": "Dashboard Title",
       "description": "Description",
       "layout_type": "ordered",
       "widgets": [
         {
           "definition": {
             "type": "note",
             "content": "# Header"
           }
         },
         {
           "definition": {
             "title": "Chart Title",
             "type": "timeseries",
             "requests": [
               {
                 "q": "avg:metric.name{*}",
                 "display_type": "line"
               }
             ]
           }
         }
       ]
     }
     ```

5. **Write the file** to the project root as `datadog_dashboard.json`

## Metric Type Detection

**Identify metric types from the codebase:**

- **Counter metrics** (from `statsd.increment()`): Use `sum:metric{tags}` queries, NEVER `.as_count()`
  - Examples: `*.received`, `*.sent`, `*.error`, `*.retry`, `*.registered`, `*.published`, etc.
  - Pattern: Look for `.increment()` or `.count()` calls in code

- **Gauge metrics** (from `statsd.gauge()`): Use `avg:metric{tags}` or `sum:metric{tags}.as_count()` (only without grouping)
  - Examples: `*.pending`, `*.running`, `*.completed`, `*.active`, `*.open`, etc.
  - Pattern: Look for `.gauge()` calls in code

- **Timing/Histogram metrics** (from `statsd.timing()` or `.histogram()`): Use `avg:metric{tags}`
  - Examples: `*.latency.*`, `*.duration`, `*.time`, etc.
  - Pattern: Look for `.timing()` or `.histogram()` calls in code

- **Distribution metrics** (from `statsd.distribution()`): Use percentiles like `p50:`, `p95:`, `p99:`
  - Examples: `*.distribution.*`, used for calculating percentiles
  - Pattern: Look for `.distribution()` calls in code

**When generating queries:**
- Counter + no grouping: `sum:metric{tags}`
- Counter + grouping: `sum:metric{tags} by {grouping}` ← NO `.as_count()`
- Gauge + no grouping: `avg:metric{tags}` or `sum:metric{tags}.as_count()`
- Gauge + grouping: `avg:metric{tags} by {grouping}` ← NOT sum, NOT `.as_count()`
- Distribution: `p95:metric{tags}` (or p50, p99, etc.)

## Dashboard Import

Once generated, import the JSON into Datadog:
1. Create a new dashboard
2. Use the settings/import option
3. Paste the entire JSON content
4. Save and view your metrics dashboard

## Correct JSON Format

The dashboard JSON must use this simple format to import successfully:

```json
{
  "title": "Application Name - Metrics Dashboard",
  "description": "Description of metrics covered",
  "layout_type": "ordered",
  "widgets": [
    {
      "definition": {
        "type": "note",
        "content": "# Section Header"
      }
    },
    {
      "definition": {
        "title": "Metric Chart Title",
        "type": "timeseries",
        "requests": [
          {
            "q": "sum:metric.name{*}",
            "display_type": "line"
          }
        ]
      }
    }
  ]
}
```

**Critical formatting rules:**
- Root level has: title, description, layout_type, widgets (NO "definition" wrapper at root)
- Each widget request must use `"q"` key for the query string
- Query format: `"q": "sum:metric.name{*}"` (string, not nested object)
- Use `{*}` wildcard for all metric queries
- No `response_format`, `data_source`, `queries`, or `name` fields in requests
- Keep requests simple: just `q` and `display_type`

**Datadog Query Syntax Rules (CRITICAL):**
- **Counter metrics from statsd.increment():** NEVER use `.as_count()` - these are already counters
  - Correct: `sum:metric.name{tags}`
  - Correct with grouping: `sum:metric.name{tags} by {job_type}`
  - WRONG: `sum:metric.name{tags}.as_count()` ❌
- **`.as_count()` is ONLY for converting gauges to counts** and cannot be used with `by` clauses
  - Correct: `sum:gauge.metric{tags}.as_count()` (gauge only, no grouping)
  - WRONG: `sum:metric{tags}.as_count() by {grouping}` ❌
  - WRONG: `sum:counter.metric{tags}.as_count()` ❌
- For gauge metrics with grouping: use `avg:metric{tags} by {grouping}` (NOT sum, NOT .as_count())
- For distribution metrics: use `p50:metric{*}`, `p95:metric{*}`, `p99:metric{*}`, etc.
- When grouping by tags, place the `by {tags}` clause at the very end after all other modifiers

## Widget Type Constraints (IMPORTANT)

Different widget types have different request structures:

**timeseries, heatmap, query_value:**
- Support `display_type` in requests
- Example: `{"q": "metric{...}", "display_type": "line"}`

**bar_chart:**
- Do NOT include `display_type` in requests
- Example: `{"q": "metric{...}"}`
- The bar_chart widget type automatically handles visualization

**note:**
- No requests needed
- Use for section headers: `{"definition": {"type": "note", "content": "# Section Title"}}`

## Post-Implementation Validation (MANDATORY)

After generating the dashboard JSON, perform these checks BEFORE returning:

- [ ] **No `.as_count() by` pattern:** Search for this pattern - it's INVALID and will fail import
- [ ] **Query filters applied:** ALL metric queries use `{*}` wildcard
- [ ] **Counter syntax:** Counters use `sum:metric{*}` or `sum:metric{*} by {tag}` (NO `.as_count()`)
- [ ] **Gauge with grouping:** Uses `avg:metric{*} by {tag}` (NOT sum, NOT `.as_count()`)
- [ ] **Bar chart requests:** Have NO `display_type` field
- [ ] **JSON valid:** File parses as valid JSON

**Do NOT return the skill output until ALL checks pass.**

## Validation & Quality Checks

**Before finalizing the dashboard JSON, verify:**

1. **Syntax validation:** `jq empty datadog_dashboard.json`
2. **Check for `.as_count()` misuse:**
   - Search for `.as_count() by` ← WRONG, must be removed
   - Verify counter metrics don't have `.as_count()` at all
   - Pattern to search: `\\.as_count\\(\\).*by` - this is INVALID
3. **Bar chart issues:** `jq '.widgets[] | select(.definition.type == "bar_chart") | .definition.requests[0]'`
   - Remove any `display_type` fields from bar_chart requests
4. **Query format verification:**
   - All metric queries should use `{*}` wildcard
   - Grouping queries should end with `by {tag_name}`

**Query pattern validation checklist:**
- Counter + grouping: Must be `sum:metric{*} by {tag}` (no `.as_count()`)
- Gauge + grouping: Must be `avg:metric{*} by {tag}` (no sum, no `.as_count()`)
- No query should contain `.as_count() by` ← This is the most common error

## Common Import Errors

- **"unable to parse ... didn't consume all the text. The non-matching portion begins with 'by'"**
  - Cause: Using `.as_count() by {tags}` or `.as_count()` with grouping
  - Fix: Remove `.as_count()` entirely from queries with grouping
  - For counters: Just use `sum:metric{tags} by {tags}`

- "Additional properties are not allowed ('display_type' was unexpected)"
  - Cause: bar_chart widget has display_type
  - Fix: Remove the `display_type` field from bar_chart requests

- "Invalid widget definition"
  - Cause: Widget structure doesn't match its type's requirements
  - Fix: Check that the widget structure matches type's requirements (timeseries, query_value, note, etc.)

