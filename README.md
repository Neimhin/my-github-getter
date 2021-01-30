# GitHub API app
## Software Engineering

The idea for this app was to crawl through github users and for a given user get all their repos and then find the user who had the highest number of commits in that set of repos and then recurse for that user, thus producing a chain of highest committers for a given user.
Note that eventually one reaches a user who is the highest committer in their own set of repos and thus the crawl terminates.

The usernames for which these chains will be generated are specified in a a list in `src/Main.hs`.

To run the program run `stack run`.

Gathering lots of these chains gives us a graph which is saved into `alltogether.json`. The file can be loaded into the visualization folder with:
```powershell
cp alltogether.json d3\
```

To display the graph vsually in your browser run
```powershell
.\run-vis.ps1
```
and point your browser to `localhost:8000`.

The visualization is a force directed graph based on Mike Bostock's visualization of co-occurence of characters in Les Misérables (https://gist.github.com/mbostock/4062045).

# Issues
The crawl is extremely slow. I tried to make a branch where the api calls are run concurrently, but it is not running correctly or producing correct results.
