#!/lfs1/projects/amb-verif/anaconda3/bin/python

import matplotlib
#matplotlib.use('TkAgg') # <-- THIS MAKES IT FAST!
import matplotlib.pyplot as plt
import numpy as np
import datetime
outfile="187.png"
lablestr="187"

infile='mycounts.txt'
yyyymmdd='20180101' #fake date

lineList = [line.rstrip() for line in open(infile)]
while("" in lineList) :
  lineList.remove("") 
#print(*lineList, sep = "\n") 
first=lineList[0][:4]
last=lineList[-1][:4]
#print(first,last)

### generate a HHMM list every 1 minute
time1 = datetime.datetime.strptime(yyyymmdd+first,"%Y%m%d%H%M")
time2 = datetime.datetime.strptime(yyyymmdd+last,"%Y%m%d%H%M")
cur = time1; delta=datetime.timedelta(minutes=1)
dateList=[time1.strftime("%H%M")]
while cur < time2:
  cur += delta
  dateList.append(cur.strftime("%H%M"))
#print(*dateList, sep="\n")

### to get the counts for each HHMM
# dictKnt={}
# for s in dateList:
#   dictKnt[s]=0
dictKnt={s:0 for s in dateList}
for s in lineList:
  key=s[:4];value=int(s[5:].strip())
  dictKnt[key]=value
#print(dictKnt)

labels=[s[2:] for s in dictKnt.keys()]
counts=list(dictKnt.values())

x = np.arange(len(labels))  # the label locations
width = 1.0  # the width of the bars

fig, ax = plt.subplots()
rects1 = ax.bar(x, counts, width, label=lablestr)

# Add some text for labels, title and custom x-axis tick labels, etc.
ax.set_ylabel('obs#')
ax.set_title('obs counts')
ax.set_xticks(x)
ax.set_xticklabels(labels)
for label in ax.xaxis.get_ticklabels():
  if int(label.get_text())%5 != 0:
    label.set_visible(False)

ax.legend()

plt.savefig(outfile)
