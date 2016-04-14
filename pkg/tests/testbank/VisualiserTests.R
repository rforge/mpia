
# -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
# testing Visualiser

cat ("- - - - - - - - - - - - - - - - - - - - - \n")
cat ("testing Visualiser\n\n")

cat("~~~ TEST upgrade domain 'generic' to latest code:\n")
tryCatch({

	   dmgr = DomainManager()
	   d = dmgr$get("generic")
	   ids = dmgr$upgrade(force=TRUE)
   
	},
	error=function(e) stop ("~~~ !!! FAILED !!!\n"),
	finally=cat("~~~ passed. \n")
)
cat("\n")

cat("~~~ TEST generate PDF file with term map (toponymy only):\n")
tryCatch({
   
      d = dmgr$get("generic")
   
   	d$visualiser$newDevice(name="termplot", pdf=TRUE, filename="~/Documents/werkstatt/mpia-package/plot-termplot.pdf")
      d$visualiser$toponymy(method="all", add=FALSE)
   	d$visualiser$closeDevice()
   
   	dmgr$materialise(d$signature)
   
   },
   error=function(e) stop ("~~~ !!! FAILED !!!\n"),
   finally=cat("~~~ passed. \n")
)
cat("\n")


cat("~~~ TEST whether materialisation of geo data worked:\n")
tryCatch({
   
      d = dmgr$get("generic")
      dmgr$flush(d)
      d = dmgr$get("generic")
      if (is.null(d$visualiser$netcoords)) stop ("netcoords not available!")
      
   },
   error=function(e) stop ("~~~ !!! FAILED !!!\n"),
   finally=cat("~~~ passed. \n")
)
cat("\n")

cat("~~~ TEST generate PDF file with default map and toponymy (mountain+gridprestige):\n")
tryCatch({
   
      d$visualiser$newDevice(name="map", pdf=TRUE, filename="~/Documents/werkstatt/mpia-package/plot-map.pdf")
      d$visualiser$plotMap(rotated=TRUE, method="persp")
      d$visualiser$toponymy(method="mountains")
      d$visualiser$toponymy(method="gridprestige", gridsize=c(5,5))
      d$visualiser$closeDevice()
   
   },
   error=function(e) stop ("~~~ !!! FAILED !!!\n"),
   finally=cat("~~~ passed. \n")
)
cat("\n")

cat("~~~ TEST plot() with persp and toponyny() mountains + gridprestige:\n")
tryCatch({

   	dev.new()
   	#d$visualiser$newDevice()
   
      plot(d, rotated=FALSE)
      toponymy(d, method="mountains")
      toponymy(d, method="gridprestige", gridsize=c(5,5))
   
	},
	error=function(e) stop ("~~~ !!! FAILED !!!\n"),
	finally=cat("~~~ passed. \n")
)
cat("\n")

cat("~~~ TEST plot with contour and toponyny mountains + gridprestige:\n")
tryCatch({
   
	   d$visualiser$plotMap(method="contour")
	   d$visualiser$toponymy(method="mountains")
	   d$visualiser$toponymy(method="gridprestige", gridsize=c(5,5))
   
	},
	error=function(e) stop ("~~~ !!! FAILED !!!\n"),
	finally=cat("~~~ passed. \n")
)
cat("\n")

cat("~~~ TEST plot with topographic and toponyny mountains, add person path and position:\n")
tryCatch({
   
	   plot(d, method="topographic")
	   toponymy(d, method="mountains")

   	fridolin=Person(name="fridolin")
      fridolin$write("The strategy framework adopts a combination of both a rational approach to strategy and a process perspective. The rational part is that there is usually a logical and sequential process through the three stages of analysing, choosing and implementing. You have to collect the data and analyse it first, before you can choose a suitable strategy. Then, of course, you need to have chosen your strategy before you can begin implementing it. So far, this is logical, rational and linear: you do stage one, followed by stage two, followed by stage three.However, the reason that the centre of the framework is presented as three interconnected circles is that, in practice, this is not a tidy straight line. Instead, the three stages in the process are all interrelated and iterative. In other words, these three stages need to be performed again and again.Indeed, the process is iterative because it builds on existing organisational knowledge that may emerge gradually over time, and often only when managers begin to ask the right questions. Mintzberg (1987) has argued in his paper ‘Crafting strategy’ that strategies need not be either deliberate or emergent; they are more often both together, for this reason:Purely deliberate strategy precludes learning once the strategy is formulated: emergent strategy fosters it. We can learn from an evolving situation and that learning can be incorporated into an evolving emergent strategy.", purpose="b835", label="Chapter 2: strategy framework & process")
      
      fridolin$write("The first stage of the interrelated strategy process is analysing. It involves examining a variety of frameworks that allow you to develop an understanding of the opportunities and threats facing your organisation from its external environment, as well as an appreciation of the strengths and weaknesses of your organisation in terms of resources and capabilities. You will also need to identify who your primary and secondary stakeholders are, to understand what they may want from the organisation and how to respond to their demands.", purpose="b835", label="Chapter 2.1: analysing")
      
      fridolin$write("The third stage of the strategy process is ‘implementing’. It is often the most difficult stage. Many strategies fail not because the analysis was poor or the strategy inappropriate, but because the implementation of the strategy was badly carried out. Implementing is the stage at which the strategy is translated into action within the organisation. It is therefore the part of strategy in which all levels of the organisation need to become involved and where an understanding by each individual of their role and contribution in the organisation, can contribute to more effective implementation and coordination.The backbone of any implementation process is matching it to the design of the organisational structure and using appropriate management systems for coordination and control. Grant (2010, p. 174) calls these ‘the fundamentals of strategy implementation’. Different types of organisational structure are suited to particular businesses and particular tasks. The organisational structure and control systems suited to a global accounting firm are unlikely to be appropriate for a global fast food chain or an internet start-up. It is also to be expected that the mix of resources required in those organisations will be different, and their cultures too. Managing all these aspects of an organisation is critical for effective strategy implementation, yet all of them are extremely hard to manage or to change.Alongside all these basic aspects of the organisation that need to be managed during implementation, there are also the external forces for change that may sweep in at any moment and cause your wonderful new strategy to become obsolete. These are the four ‘pressures’. We discuss these as the next part of our strategy framework.", purpose="b835", label="Chapter 2.2: implementing")

   	plot(fridolin)
   	plot(performances(fridolin), label=FALSE)
   
	},
	error=function(e) stop ("~~~ !!! FAILED !!!\n"),
	finally=cat("~~~ passed. \n")
)
cat("\n")

cat("~~~ TEST plot with wireframe:\n")
tryCatch({
   
   	d$visualiser$plotMap(method="wireframe")
   
	},
	error=function(e) stop ("~~~ !!! FAILED !!!\n"),
	finally=cat("~~~ passed. \n")
)
cat("\n")

cat("~~~ TEST provoke toponymy error (with wireframe):\n")
tryCatch({
   
   	d$visualiser$toponymy()
   
	},
	error=function(e) cat("~~~ passed. \n"),
	finally=function(e) stop("~~~ !!! FAILED !!!\n")
)
cat("\n")




