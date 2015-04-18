# Team Contribution

A proposed way of measuring team coordination and map awareness through
measuring synchronicity of obtaining an advantage.

## Rationale

The current used statistics of KDA, GPM, CSD, etc. can be good measures of
individual performance, but even these can fail in this regard.  KDA is
probably the most obvious example of this.

** High KDA : a player with a high KDA could be running around the map hunting
	for kills and being generally greedy.  Making it impossible for their team to
	win once team fights start.
** Low KDA : a player with low KDA could be setting up and making plays for
	their team through sacrificing their own benefit.

It is also the case that a team can collectively have gained a gold advantage
but still is unable to win because they lack team coordination.

## Proposal

In my opinion one of the main ways professional teams win is through managing
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
score is the average of comparing their total to their teams.

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
