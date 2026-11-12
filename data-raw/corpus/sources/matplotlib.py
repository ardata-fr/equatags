# Expressions of the list math_tests of matplotlib (lib/matplotlib/tests/
# test_mathtext.py), read without running the file (ast). Writes
# sources/matplotlib.tsv: index, expression (with its $ delimiters and text).
# Run: python3 -I sources/matplotlib.py  (from data-raw/corpus)
import ast, csv
tree = ast.parse(open("sources/raw/matplotlib_test_mathtext.py", encoding="utf-8").read())
items = None
for node in tree.body:
    target = None
    if isinstance(node, ast.Assign) and isinstance(node.targets[0], ast.Name):
        target = node.targets[0].id
    elif isinstance(node, ast.AnnAssign) and isinstance(node.target, ast.Name):
        target = node.target.id
    if target == "math_tests":
        items = ast.literal_eval(node.value)
with open("sources/matplotlib.tsv", "w", newline="", encoding="utf-8") as f:
    w = csv.writer(f, delimiter="\t")
    w.writerow(["index", "expression"])
    for i, x in enumerate(items):
        if x is not None:
            w.writerow([i, x])
print(len(items), "items")
