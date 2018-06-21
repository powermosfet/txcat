# Transaction Categorizer

Analyze CSV bank statements

By having a JSON file with regex -> category mappings,
categorize the transactions and create a report to get
an overview of your spendings

```
$ txcat --help
txcat - a regex-based transaction categorizer

Usage: txcat [--config PATH] [-m|--month MM] [-y|--year YYYY]
             [-p|--print-category ARG] INPUTFILES...
  Process and categorize CSV transactions from the bank

Available options:
  --config PATH            path to config
                           file (default: "/Users/asmundberge/.txcat.json")
  -m,--month MM            only process transactions from month MM
  -y,--year YYYY           only process transactions from year YYYY
  -p,--print-category ARG  Print all transactions of a given category
  -h,--help                Show this help text
```
