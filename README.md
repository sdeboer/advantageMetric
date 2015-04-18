# Team Contribution

A proposed way of measuring team coordination and map awareness through
measuring synchronicity of obtaining an advantage.


## Run Time

(Potentially) See it in action here: http://team.psiinteractive.com/

(If I still have the server running, I'll keep it up for a couple of weeks anyway)

To run this locally you need to do something along the lines of:

```shell
# do what you need to in order to get erlang + rebar and ruby + bundler installed
cd advantageMetric/server
rebar get-deps
rebar compile && RIOT_API_KEY=<<YOUR KEY>> CLIENT_DOMAIN="localhost:4567" ./run_server

# separate shell
cd advantageMetric/client
bundle install
bundle exec middleman -e development
# point browser at http://localhost:4567/
```

## How to use it

Type in a summoner name.  Hit return.

Type in another in the second box.  Hit return.

## API Challenge response

API Challenge description : https://developer.riotgames.com/terms#statement

When the original API Challenge was given it was stated to be in concert
with the announced N.U.R.F. changes.  Which in my consideration, if at
all playable, would have required the utmost in team synchronization of
skill shots in order to achieve any meaningful result.

This led me to thinking more deeply about team coordination and how to 
measure it and what the value of it might be...

## Rationale

The current used statistics of KDA, GPM, CSD, etc. can be good measures of
individual performance, but even these can fail in this regard.  KDA is
probably the most obvious example of this.

* *High KDA* : a player with a high KDA could be running around the map hunting
	for kills and being generally greedy.  Making it impossible for their team to
	win once team fights start.
* *Low KDA* : a player with low KDA could be setting up and making plays for
	their team through sacrificing their own benefit.

It is also the case that a team can collectively have gained a gold advantage
but still is unable to win because they lack team coordination.

## Proposal

In my opinion one of the main ways teams win is through managing
the whole map to achieve many actions at once.  For example synchronizing a
bot tower kill while also warding top.  These positive actions then lead into
another action.  Chain more positive actions together in a row and a huge
advantage is opened up as compared to the opposing team.

This stat calculation breaks game events into "streaks" as a list of positive
events that have no gap greater than 20 seconds between any two of them.
Each positive event is given a different amount of points that is collected
together as score for that streak.  Adding all of these points together gives
the total team coordination score for the match.

I think that this value may have some real value once it gets normalized in
some manner (probably based on some non-linear game length function).  That
would allow for directly comparing one team to another in their coordination
ability.

At the moment though the main use of this value is to provide a baseline
for calculating the individual contribution.  This is calculated as a
percentage contribution to the overall team score at that time.  The 
intention here is that even if a team is doing badly a single player
may have coordinated their actions with other team members very well
and can recognized as such.

The chart shows their contribution over time, where the overall individual
score (shown in the subtitle) is the average of comparing their total to their teams.

*Note* It is entirely possible for people on the losing team to have
a higher team contribution percentage than people on the winning team.

## Player Contribution

The key indicator of a player contributing to a streak is being listed
as having done the kill, place the ward, or being listed as an assist.

It is important to note that a streak can include placing a ward, and
even if that is the only thing the player did during the streak they
will receive the full points for the streak.

Players are also included in the streak if there is a ParticipantFrame
in the 10 seconds on either side of the streak in which they are within
1,500 units of any position within the streak.  The assumption here
being that they were applying positional pressure or support to the
action even if they don't end up a killer/assister.  (Ideally this
would have been based on the player's position at the actual time
of the event, but that isn't provided in the post-match data)

Any event that is a single event, even destroying a tower, does not
contribute to the scores.

## Next Steps

A couple of weeks of hobby time isn't much...

* All regions
* Better name
* Cosmetics...all of them.
* Getting normal games to work
* Handling other game modes
* Storing result data such that comparisons could be made to various
	different conditions:
	* Average of your ELO
	* Average for a given position
	* Average for a given champ
	* etc.
* Ability to drill down into streaks
* Getting some further derived stats like
	* Differential scores
	* Differences between queue types
	* Correlation between position, winning/losing team, and contribution
	* Successful initiations
	* Clean up scores
	* Streak ordering and whether there are particularly
	successful patterns
* A bunch of other things...

I'd also like to consider how to remove the "game winning push"
especially the winning team has a huge advantage over the other
team.  This last push can artificially pad all statistics in LoL.

## Constants

_These are defined in objectives.erl_

* *Gap Between Streaks* 20 seconds
* *Pre/Post Streak Gap for Positional Pressure* 10 seconds
* *Placing a Ward* 1 point
* *Killing a Ward* 2 points
* *Champion Kill* 3 points
* *Dragon* 3
* *Baron* 5
* *Tower* 10

## Limitations

* Errors on the server are not reported in the interface at all
* CSS is sloppy, the right column will move to left when there is
	no content, that is when you search for a new player.
* There is no loading indicator after you hit return/submit the
	form.
