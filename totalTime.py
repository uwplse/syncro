import sys

time = 0.0
with open(sys.argv[1]) as f:
    for line in f:
    	if len(line.split(" ")) > 0 and line.split(" ")[0] == "Took":
    		time += float(line.split(" ")[1][:-2])

print("Total time take " + sys.argv[1].rsplit('/', 1)[-1][:-4] + " = " + str(time) + "ms")

