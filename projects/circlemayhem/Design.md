# Design Doc for Circle Mayhem

## Circle Entities

The main theme is that every entity that is drawn in the game is a circle. So enemies are circles, the player is a circle, bullets are circles. Maybe this will be relaxed in the future to other shapes like ovals or rectangles.

The exception to this are UI elements like score and time.

### Visual Representation

Circles can be rings, where the border is some color and the inside is transparent, or filled, or filled and bordered.

Animations may consists of changing radii, or changing border or fill colors. For example, enemies may fade in by changing the opacity of their color.

### Physics

The player circle will be controlled through the keyboard and mouse. The keyboard controls the movement. For now, there will be no momentum, so the player circle only moves on key downs. The mouse controls the aiming of the bullets. Bullets autofire, so the player doesn't need to hold down a mouse button

Enemy circles will have different movement patters. The simplest is a random initial velocity, and then the circle will bounce around the screen.

## Enemies

There are 3 types of enemy circles.

**Normal:** these have an initial random speed and direction, and bounce around the world.

**Homing:** these have a fixed speed which is slightly faster than the player, and move towards the player.

**Random:** these are like Normal most of the time, but randomly change their direction.
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

