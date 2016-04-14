
cat("\n\n-  -  - proximity tests -  -  -\n")

cat("position tests:\n")

round(cosine(hans),1)

cosine(fw)
cosine(hans)

cosine(hans, fw)

cat("\nposition tests III: Performance vs. Performance\n")

proximity ( Performance("this is a test", domain=d), Performance("this is a test", domain=d) )
proximity ( Performance("This is a test", domain=d), Performance("test management business", domain=d))
proximity ( Performance("This is a test", domain=d), Performance("management business retail", domain=d))

cat("\nposition tests position a,b,c\n")

fw = new("Person", domain=d, name="fridolin")

fw$read("this is a demo text in business for management purposes", purpose="exam")
fw$read("this is a second text for demonstration of purposes in management all in business", purpose="exam 2", when = "2013-01-04 11:11:11")
fw$read("documents can be very different from each other, no matter what domain", purpose="exam", label="label1.txt")
fw$read("everything can be very different from each other, no matter what domain", purpose="exam", when="2013-03-03 11:11:02")
fw$write("latent semantic analysis is a strange thing to do. strange awkward and probably forbidden.", purpose="exam")
fw$write("this is yet another test")
fw$read("this is yet another test")


a = hans$position()
cat(paste("   a$position mean = ", mean(a$getMeaningVector()), "\n", sep=""))
b = fw$position(when = "2013-03-04 11:11:11")
cat(paste("   b$position mean = ", mean(b$getMeaningVector()), "\n", sep=""))
c = fw$position()
cat(paste("   c$position mean = ", mean(c$getMeaningVector()), "\n", sep=""))

cat(">> positions a,b,c calculated. <<\n\n")

cat("starting to compare a,b,c.\n")

cat(">> a,a: proximity(hans$position(),hans$position()) =\n")
cos = proximity(a,a)
cat(paste( " ", round(cos,1), "\n", sep=""))

cat(">> a,b: proximity(hans$position(),fw$position(when)) =\n")
cos = proximity(a,b)
cat(paste( " ", round(cos,1), "\n", sep=""))

cat(">> b,c: proximity(fw$position(when),fw$position()) =\n")
cos = proximity(b,c)
cat(paste( " ", round(cos,1), "\n", sep=""))


cat("\nposition tests II: proximity (fw,hans)\n")

proximity(fw,hans)

cat("\nproximity tests done\n")
