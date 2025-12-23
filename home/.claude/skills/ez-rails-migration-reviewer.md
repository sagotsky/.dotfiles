a ez-rails Database Migration Reviewer

## Activation
When the user provides a GitHub PR URL for ezcater/ez-rails and mentions reviewing migrations:
- "review the migrations in <PR URL>"
- "check the migration in PR #1234"
- "migration review <PR URL>"

## Process

### 1. Fetch PR Data
Use gh CLI:
```bash
gh pr view <number> --repo ezcater/ez-rails --json files,diff
gh pr diff <number> --repo ezcater/ez-rails
```

Filter for files in `db/migrate/` directory.

### 2. Review Each Migration Against Best Practices

#### DDL Transactions
- ✅ Has `disable_ddl_transaction!` for concurrent operations
- ❌ Has `disable_ddl_transaction!` for simple column operations (shouldn't)
- **Required for:** Adding indexes concurrently, `algorithm: :concurrently`, `CREATE INDEX CONCURRENTLY`
- **NOT needed for:** Simple column additions, column type changes, adding foreign keys

#### Timeouts
- ✅ Has both `set_statement_timeout` and `set_lock_timeout`
- ✅ Uses readable format: `N.seconds.in_milliseconds` or `N.minutes.in_milliseconds`
- ✅ Lock timeout ≤ 30 seconds
- ❌ Missing timeouts
- ❌ Lock timeout > 30 seconds
- ❌ Uses raw numbers like `120000` instead of readable format

**Recommended values:**
- Statement timeout: 15-30s (simple ops), 2-5min (indexes), 10min (data updates)
- Lock timeout: 10s (default), max 30s

#### Indexes
- ✅ Uses `algorithm: :concurrently` for all index operations
- ✅ Wrapped `remove_index` in `safety_assured`
- ✅ Checks for existing indexes before operations (`index_exists?`)
- ❌ Missing `algorithm: :concurrently`
- ❌ Mixing `remove_index` and `add_index` in same migration
- ❌ Adding index on low-cardinality column without justification
- ❌ Index operations not split from column additions

**YAGNI principle for indexes:**
- Question if index is actually needed
- Low-cardinality columns (few distinct values) = poor candidates
- Postgres prioritizes FK indexes over single-column indexes
- Indexes slow writes and consume disk space

#### Foreign Keys
- ✅ Added with `validate: false`, validated in separate migration
- ❌ Adding and validating FK in same migration
- ❌ Using `safety_assured` for standard `add_foreign_key` (unnecessary)
- ❌ Using existence checks like `unless foreign_key_already_exists` (unnecessary)
- ❌ Missing foreign key definition on columns that reference other tables

#### Column Operations
- ✅ Simple column additions have appropriate timeouts
- ✅ UUIDs use `uuid` type not `string`
- ✅ External IDs use appropriate size (bigint for Zendesk, etc.)
- ✅ Integer `limit` values are appropriate (limit: 2 = 2 bytes, not 2 digits)
- ❌ Removing column without prior `ignored_columns` step (needs 3-step process)
- ❌ Making column NOT NULL without 4-step check constraint process
- ❌ Using `string` for UUID columns

#### Splitting Migrations
- ❌ Column addition + index creation in same migration (split into 2)
- ❌ FK addition + validation in same migration (split into 2)
- ❌ Multiple index operations on same table in one migration (split)
- ❌ Using `rename_table` (use create + drop pattern instead)
- ✅ Operations properly split with clear separation

**Standard patterns:**
- Add indexed FK column: 4 migrations (column → index → FK → validate)
- Table creation with indexes: 2 migrations (table → indexes)
- Remove column: 3 steps (ignore → deploy → remove)
- Make column NOT NULL: 4 migrations (constraint → validate → change null → remove constraint)

#### Data Migrations
- ✅ Small data updates use `safety_assured` with `execute` in migration
- ✅ Large backfills use maintenance tasks (not migrations)
- ❌ Large data updates in migration file (should be maintenance task)
- ❌ Data migration without appropriate timeouts

**Use maintenance tasks when:**
- Large datasets (millions of rows)
- Operations taking > 1 minute
- Need pause/resume capability
- Operations might fail on individual records

#### Idempotency
- ✅ Has existence checks (`index_exists?`, `column_exists?`, `foreign_key_exists?`)
- ✅ Safe to run multiple times
- ❌ Will fail if run twice

#### Safety Assured
- ✅ Used for `remove_index`
- ✅ Used for raw SQL `execute`
- ❌ Used unnecessarily for standard operations

### 3. Output Format

```markdown
## Migration Review for PR #<number>: <title>

### Files Reviewed
- `db/migrate/<timestamp>_<name>.rb`
- `db/migrate/<timestamp>_<name>.rb`

### ✅ Good Practices Found
- [List positive patterns observed]

### ⚠️ Critical Issues
[Issues that will cause outages or deployment problems]

### 🔧 Recommended Improvements
[Issues that should be fixed but won't cause immediate problems]

### ❓ Questions
[Things needing clarification]

### 📝 Suggested Changes

#### Issue: [Description]
**Current:**
```ruby
[current code]
```

**Suggested:**
```ruby
[improved code]
```

**Why:** [Explanation with reference to best practices]

---

### Additional Notes
[Any other relevant context]
```

### 4. Tone
- Direct and technical
- Focus on specific, actionable feedback
- Explain *why* something is a problem, not just *what*
- Include code examples for fixes
- Reference specific best practice patterns

## Best Practices Reference

### Core Principles
- **Zero-downtime only** - no long-running locks on high-traffic tables
- **Split operations** when necessary to avoid extended locks
- **Consider deployment** - old and new code must coexist during deploy

### DDL Transaction Rules
```ruby
# ✅ CORRECT - Concurrent index needs it
class AddIndexToCaterers < ActiveRecord::Migration[7.1]
  disable_ddl_transaction!
  set_statement_timeout(120.seconds.in_milliseconds)
  set_lock_timeout(10.seconds.in_milliseconds)

  def change
    add_index :caterers, :status, algorithm: :concurrently
  end
end

# ✅ CORRECT - Simple column doesn't need it
class AddStatusToCaterers < ActiveRecord::Migration[7.1]
  set_statement_timeout(30.seconds.in_milliseconds)
  set_lock_timeout(10.seconds.in_milliseconds)

  def change
    add_column :caterers, :status, :string
  end
end
```

### Index Best Practices
```ruby
# Always concurrent
class AddIndexToOrders < ActiveRecord::Migration[7.1]
  disable_ddl_transaction!
  set_statement_timeout(3.minutes.in_milliseconds)
  set_lock_timeout(10.seconds.in_milliseconds)

  def up
    remove_existing_index
    safety_assured do
      add_index :orders, :status, algorithm: :concurrently
    end
  end

  def down
    remove_existing_index
  end

  private

  def remove_existing_index
    safety_assured do
      if index_exists?(:orders, :status)
        remove_index :orders, :status, algorithm: :concurrently
      end
    end
  end
end
```

**Don't mix operations:**
```ruby
# ❌ BAD - remove and add in same migration
def change
  remove_index :orders, :old_status
  add_index :orders, :new_status, algorithm: :concurrently
end

# ✅ GOOD - split into 2 migrations
```

### Foreign Key Pattern
```ruby
# Migration 1: Add FK without validation
class AddForeignKeyToConnections < ActiveRecord::Migration[7.1]
  set_statement_timeout(30.seconds.in_milliseconds)
  set_lock_timeout(10.seconds.in_milliseconds)

  def change
    add_foreign_key :connections, :corp_accounts, validate: false
  end
end

# Migration 2: Validate FK (separate deploy)
class ValidateForeignKeyOnConnections < ActiveRecord::Migration[7.1]
  set_statement_timeout(30.minutes.in_milliseconds)
  set_lock_timeout(10.seconds.in_milliseconds)

  def change
    validate_foreign_key :connections, :corp_accounts
  end
end
```

**Don't overcomplicate:**
- No need for `safety_assured` wrapper
- No need for existence checks like `unless foreign_key_already_exists`
- These only needed for non-standard operations

### Column Removal (3-step)
```ruby
# Step 1: Ignore column (separate PR)
class Caterer < ApplicationRecord
  self.ignored_columns += [:old_status]
end

# Step 2: Deploy and verify

# Step 3: Remove column (separate PR)
class RemoveOldStatusFromCaterers < ActiveRecord::Migration[7.1]
  def change
    remove_column :caterers, :old_status
  end
end
```

**Why:** Rails maintains column cache - removing without ignoring first causes SQL errors during deploy.

### Making Column NOT NULL (4-step)
```ruby
# Step 1: Add constraint (not validated)
class AddGroupIdConstraint < ActiveRecord::Migration[7.1]
  def change
    add_check_constraint :modifiers, "group_id IS NOT NULL",
                         name: "modifiers_group_id_null",
                         validate: false
  end
end

# Step 2: Validate constraint
class ValidateGroupIdConstraint < ActiveRecord::Migration[7.1]
  def change
    validate_check_constraint :modifiers, name: "modifiers_group_id_null"
  end
end

# Step 3: Change column nullability
class ChangeGroupIdNullability < ActiveRecord::Migration[7.1]
  def change
    change_column_null :modifiers, :group_id, false
  end
end

# Step 4: Remove constraint
class RemoveGroupIdConstraint < ActiveRecord::Migration[7.1]
  def change
    remove_check_constraint :modifiers, name: "modifiers_group_id_null"
  end
end
```

**Why:** Even with backfilled data, adding validated constraint in one step causes locks.

### Column Type Rules
```ruby
# ❌ BAD - UUIDs as string
add_column :users, :scim_integration_id, :string

# ✅ GOOD - UUIDs as uuid
add_column :users, :scim_integration_id, :uuid

# ✅ External IDs as bigint
add_column :disputes, :zendesk_ticket_id, :bigint
```

**Integer limits:**
- `limit: 2` = 2 bytes (smallint: -32,768 to 32,767)
- `limit: 4` = 4 bytes (integer: -2B to 2B)
- `limit: 8` = 8 bytes (bigint: -9 quintillion to 9 quintillion)

### Adding Indexed FK Column (4 migrations)
```ruby
# Migration 1: Add column
class AddCorpAccountIdToConnections < ActiveRecord::Migration[7.1]
  set_statement_timeout(30.seconds.in_milliseconds)
  set_lock_timeout(10.seconds.in_milliseconds)

  def change
    add_column :connections, :corp_account_id, :bigint
  end
end

# Migration 2: Add index concurrently
class AddIndexToConnectionsCorpAccountId < ActiveRecord::Migration[7.1]
  disable_ddl_transaction!
  set_statement_timeout(2.minutes.in_milliseconds)
  set_lock_timeout(10.seconds.in_milliseconds)

  def change
    add_index :connections, :corp_account_id, algorithm: :concurrently
  end
end

# Migration 3: Add FK without validation
class AddForeignKeyToConnectionsCorpAccountId < ActiveRecord::Migration[7.1]
  set_statement_timeout(30.seconds.in_milliseconds)
  set_lock_timeout(10.seconds.in_milliseconds)

  def change
    add_foreign_key :connections, :corp_accounts, validate: false
  end
end

# Migration 4: Validate FK
class ValidateForeignKeyConnectionsCorpAccountId < ActiveRecord::Migration[7.1]
  set_statement_timeout(30.minutes.in_milliseconds)
  set_lock_timeout(10.seconds.in_milliseconds)

  def change
    validate_foreign_key :connections, :corp_accounts
  end
end
```

### Table Creation with Indexes (2 migrations)
```ruby
# Migration 1: Create table
class CreateMarketplaceSearchMenus < ActiveRecord::Migration[7.1]
  def change
    create_table :marketplace_search_menus do |t|
      t.uuid :caterer_uuid, null: false
      t.foreign_key :caterers, column: :caterer_uuid, primary_key: :uuid, validate: false
      t.date :start_date, null: false
      t.boolean :vegan, default: false, null: false
      t.tsvector :full_m_item_vector, null: false, default: ""
      t.timestamps
    end
  end
end

# Migration 2: Add indexes concurrently
class AddIndexesToMarketplaceSearchMenus < ActiveRecord::Migration[7.1]
  disable_ddl_transaction!
  set_statement_timeout(120.seconds.in_milliseconds)
  set_lock_timeout(10.seconds.in_milliseconds)

  def change
    add_index :marketplace_search_menus, :caterer_uuid, algorithm: :concurrently
    add_index :marketplace_search_menus, [:start_date, :end_date], algorithm: :concurrently
  end
end
```

### Avoid rename_table
```ruby
# ❌ BAD - even on empty tables
def change
  rename_table :uploaded_files, :onboarding_uploaded_files
end

# ✅ GOOD - create + drop pattern
# Migration 1: Create new table
class CreateOnboardingUploadedFiles < ActiveRecord::Migration[7.1]
  def change
    create_table :onboarding_uploaded_files do |t|
      # ... columns ...
    end
  end
end

# Update model to point to new table
class GuidedOnboarding::UploadedFile < ApplicationRecord
  self.table_name = "onboarding_uploaded_files"
end

# Migration 2 (follow-up PR): Drop old table
class DropUploadedFiles < ActiveRecord::Migration[7.1]
  def change
    drop_table :uploaded_files
  end
end
```

**Why:** Causes Ruby class loading issues during deployment.

### Data Migrations
**Small updates in migration:**
```ruby
class BackfillRoasTarget < ActiveRecord::Migration[7.1]
  set_statement_timeout(10.minutes.in_milliseconds)
  set_lock_timeout(30.seconds.in_milliseconds)

  def up
    safety_assured do
      execute <<~SQL
        UPDATE supplier_marketing_topsort_campaigns
        SET roas_target = 5.5
        WHERE campaign_type = 'autobidding'
          AND roas_target IS NULL
          AND deleted_at IS NULL;
      SQL
    end
  end

  def down
    # Usually irreversible
  end
end
```

**Large backfills use maintenance tasks:**
```bash
bin/rails g backfill FooBar::Baz
```

Features:
- Web UI at `/backfills`
- Slack notifications to `#pb-a-ez-rails`
- Progress tracking
- Pause/resume capability
- Rate limiting

### Common Gotchas
1. **structure.sql** - Review carefully, revert unrelated changes
2. **Investigate failures** - Understand why migration failed before recreating
3. **pganalyze** - Use to inspect table stats, index usage, column cardinality
4. **Validation timeouts** - Keep conservative (15s, not 30 minutes)

### Migration Safety Checklist
- [ ] Uses appropriate timeouts (statement and lock)
- [ ] Concurrent operations have `disable_ddl_transaction!`
- [ ] Indexes added with `algorithm: :concurrently`
- [ ] Foreign keys added with `validate: false`, validated separately
- [ ] Operations split appropriately (no mixing column + index)
- [ ] Migration is idempotent (safe to run multiple times)
- [ ] `safety_assured` only used when necessary
- [ ] PR description includes benchmarks and risk assessment
- [ ] Tested locally with scrubbed production database
- [ ] Deployed to dev1 for verification

## Important Notes
- **Only review ezcater/ez-rails migrations**
- If PR has no migration files, state that clearly
- Don't review non-migration changes unless explicitly asked
- Focus on safety and zero-downtime concerns
- Be direct and technical in feedback
