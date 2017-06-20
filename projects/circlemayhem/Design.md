# Design Doc for Circle Mayhem

## Enemies

There are 3 types of enemy circles.

Normal: these have an initial random speed and direction, and bounce around the world.

Homing: these have a fixed speed which is slightly faster than the player, and move towards the player.

Random: these are like Normal most of the time, but randomly change their direction.
There is some fixed probability every some (TBD) period that they change direction.

Should these enemies have more than 1 HP? Let's start with no, just 1 HP.

How should spawning work?
Fixed, periodic spawning? Maybe up to some fixed upper limit?

## Player

The player shoots small circles at some fixed speed.

The player starts with some amount of HP.
Shooting enemies gains HP at some rate. Should it be linear? Sublinear?

Should the radius of the player reflect HP? This seems like an interesting design.
It acts as a natural balancing force.
Easier to dodge at low HP. Harder to dodge at high HP.

Should the player just be constantly shooting and only the direction is controlled?

