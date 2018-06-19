# Transaction Categorizer

Analyze CSV bank statements

By having a JSON file with regex -> category mappings,
categorize the transactions and create a report to get
an overview of your spendings

```
$ txcat --help
txcat - a regex-based transaction categorizer

Usage: txcat [--config PATH] [--month MM] INPUTFILES...
  Process and categorize CSV transactions from the bank

Available options:
  --config PATH            path to config
                           file (default: "/Users/asmundberge/.txcat.json")
  --month MM               only process given month
  -h,--help                Show this help text
```
