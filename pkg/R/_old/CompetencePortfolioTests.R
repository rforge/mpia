
# -  -  -  -  -  -  -  -  -  -  -  -
# test CompetencePortfolio

cat("\n\n\n")
cat("-  -  - CompetencePortfolio tests -  -  -\n")


pf = new("CompetencePortfolio", ppl)
pf$performances()

cat("Found the following performance collections:\n")
cat(pf$all())
cat("\n")

cat("Looking at the position for collection 'generic' and comparing it with position a from above:\n")
pos = pf$position("generic")
proximity(pos, a)
