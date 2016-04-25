
library(mpia)

#source("../tests/tests.R")
#setwd("~/Documents/werkstatt/mpia-package/mpia-package/pkg/mpia/tests")
#source("dependencies.R")
#source("sources.R")

# -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
# DEMOS

demo("sna-lsa-mpia")
demo("math-foundations")
demo("placement")
demo("essay-scoring")

# -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
# DOMAIN / DOMAINMANAGER / VISUALISER

source("testbank/DomainManagerTests.R")

# -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
# VISUALISER

source("testbank/VisualiserTests.R") # will be a bit slower, since DomainManagerTests overwrote all geodata

# -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
# PERFORMANCE

source("testbank/PerformanceTests.R")

# -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
# PERSON

source("testbank/PersonTests.R")

# -  -  -  -  -  -  -  -  -  -  -  -
# INTROSPECTION

source("testbank/introspectionTests.R")

# -  -  -  -  -  -  -  -  -  -  -  -
# PROXIMITY

source("testbank/PersonGenerateTraces.R")
source("testbank/ProximityTests.R")

# -  -  -  -  -  -  -  -  -  -  -  -
# HUMANRESOURCEMANAGER

source("testbank/PersonGenerateTraces.R")
source("testbank/HumanResourceManagerTests.R")

# -  -  -  -  -  -  -  -  -  -  -  -
# PLOTTING / GROUP / COMPETENCE ANALYSIS

#source("../tests/personTracesExtendedLandauer.R")
source("testbank/plotTests.R")

