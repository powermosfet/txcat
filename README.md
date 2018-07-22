# Transaction Categorizer

Analyze CSV bank statements from Sparebank 1

By having a JSON file with regex -> category mappings,
categorize the transactions and create a report to get
an overview of your spendings

By combining `--format "ledger <account>"` and `--printAll` the output can be saved as a ledger file or piped into hledger for analysis

```
$ txcat --help
txcat - a regex-based transaction categorizer

Usage: txcat [--config PATH] [-m|--month MM] [-y|--year YYYY]
             [-p|--printCategory ARG] [--printAll] INPUTFILES...
             [--format FORMAT]
  Process and categorize CSV transactions from the bank

Available options:
  --config PATH            path to config
                           file (default: "/Users/asmundberge/.txcat.json")
  -m,--month MM            only process transactions from month MM
  -y,--year YYYY           only process transactions from year YYYY
  -p,--printCategory ARG   Print all transactions of a given category
  --printAll               Print all transactions instead of report
  --format FORMAT          output format for transactions (default: OneLine)
  -h,--help                Show this help text
```
