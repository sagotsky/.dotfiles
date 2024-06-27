class EnumToCte
  def self.call(...) = new(...).call

  def initialize(model_name, enum_name)
    model = model_name.constantize
    @enum_name = enum_name.to_s.pluralize
    @enum_hash = model.send(@enum_name)
  end

  def call
    puts <<~SQL

      WITH #{@enum_name} AS (
        SELECT column1 as name, column2 as id
        FROM ( VALUES
          #{ enum_to_sql_values }
        )
      )

    SQL
  end

  private

  def enum_to_sql_values
    @enum_hash.map do |name, id|
      "('#{name}', #{id})"
    end.join(", ")
  end
end

if ARGV.size != 2
  puts <<~EOF

    Usage:

    rails r enum_to_cte.rb YourModel enum_name

  EOF
  exit 1
end

EnumToCte.call(ARGV[0], ARGV[1])
