// game.js for Perlenspiel 2.3

/*
   Perlenspiel is a scheme by Professor Moriarty (bmoriarty@wpi.edu).
   Perlenspiel is Copyright © 2009-12 Worcester Polytechnic Institute.
   This file is part of Perlenspiel.

   Perlenspiel is free software: you can redistribute it and/or modify
   it under the terms of the GNU Lesser General Public License as published
   by the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   Perlenspiel is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
   GNU Lesser General Public License for more details.

   You may have received a copy of the GNU Lesser General Public License
   along with Perlenspiel. If not, see <http://www.gnu.org/licenses/>.
   */

// The following comment lines are for JSLint. Don't remove them!

/*jslint nomen: true, white: true */
/*global PS */

// All of the functions below MUST exist, or the engine will stop and complain!

var FACING = {};
FACING.inward = true;
FACING.outward = false;

var COLOR = {};
COLOR.player = PS.COLOR_RED;
COLOR.blank = PS.COLOR_WHITE;
COLOR.wall  = PS.COLOR_BLUE;

var fade_color = function(color) {
   if (color == COLOR.wall)
      return PS.COLOR_GRAY_LIGHT;
   else
      return color;
};

var g_map = {
   w: 3,
   h: 3,
   d: 3,
   grids: [
      [
         [COLOR.wall, COLOR.wall, COLOR.wall],
         [COLOR.blank, COLOR.blank, COLOR.blank],
         [COLOR.blank, COLOR.blank, COLOR.blank],
      ],
      [
         [COLOR.blank, COLOR.wall, COLOR.blank],
         [COLOR.blank, COLOR.blank, COLOR.blank],
         [COLOR.blank, COLOR.blank, COLOR.blank],
      ],
      [
         [COLOR.blank, COLOR.blank, COLOR.blank],
         [COLOR.blank, COLOR.blank, COLOR.blank],
         [COLOR.wall, COLOR.wall, COLOR.wall],
      ],
   ],
};

var map_value_at = function(x, y, z) {
   return g_map.grids[z][y][x];
};

var orient_x_to_view = function(x) {
   if (g_player.facing == FACING.inward) {
      return x;
   } else {
      return g_map.w - x - 1;
   }
};

var draw_map_at = function(uncorrected_x, y) {
   var x = orient_x_to_view(uncorrected_x);
   var map_value = map_value_at(x, y, g_player.z);
   if (map_value == COLOR.blank) {
      var background_z = g_player.facing == FACING.inward ?
         g_player.z - 1 : g_player.z + 1;
      if (background_z >= 0 && background_z < g_map.d) {
         PS.BeadColor(x, y, fade_color(map_value_at(x, y, background_z)));
      } else {
         PS.BeadColor(x, y, fade_color(COLOR.wall));
      }
   } else {
      PS.BeadColor(x, y, map_value);
   }
};

var draw_map = function() {
   for (var x = 0; x < g_map.w; x++) {
      for (var y = 0; y < g_map.h; y++) {
         draw_map_at(x, y);
      }
   }
};

var g_player = {
   x: 1,
   y: 1,
   z: 1,
   facing: FACING.inward,
};

var player_change_facing = function() {
   g_player.facing = g_player.facing == FACING.inward ?
      FACING.outward : FACING.inward;
};

var player_change_depth = function() {
   if (g_player.facing == FACING.inward) {
      if (g_player.z > 0 &&
          map_value_at(g_player.x, g_player.y, g_player.z - 1) == COLOR.blank)
         g_player.z--;
   } else {
      if (g_player.z < g_map.d - 1 &&
          map_value_at(g_player.x, g_player.y, g_player.z + 1) == COLOR.blank)
         g_player.z++;
   }
};

var player_move = function(dx, dy) {
   var x = g_player.x + dx;
   var y = g_player.y + dy;
   if (x < 0 || x >= g_map.w || y < 0 || y >= g_map.h)
      return;

   if (map_value_at(x, y, g_player.z) == COLOR.blank) {
      draw_map_at(g_player.x, g_player.y);
      g_player.x = x;
      g_player.y = y;
      draw_player();
   }
};

var draw_player = function() {
   PS.BeadColor(orient_x_to_view(g_player.x), g_player.y, COLOR.player);
};

var redraw = function() {
   draw_map();
   draw_player();
};

// PS.Init ()
// Initializes the game
// This function normally includes a call to PS.GridSize (x, y)
// where x and y are the desired initial dimensions of the grid
// options = a table with optional parameters; see documentation for details
PS.Init = function (options)
{
   "use strict";

   PS.GridSize (g_map.w, g_map.h);
   PS.BeadFlash(PS.ALL, PS.ALL, false);

   redraw();
};

// PS.Click (x, y, data)
// This function is called whenever a bead is clicked
// It doesn't have to do anything
// x = the x-position of the bead on the grid
// y = the y-position of the bead on the grid
// data = the data value associated with this bead, 0 if none has been set
// options = a table with optional parameters; see documentation for details

PS.Click = function (x, y, data, options)
{
   "use strict";

   // put code here for bead clicks
};

// PS.Release (x, y, data)
// This function is called whenever a mouse button is released over a bead
// It doesn't have to do anything
// x = the x-position of the bead on the grid
// y = the y-position of the bead on the grid
// data = the data value associated with this bead, 0 if none has been set
// options = a table with optional parameters; see documentation for details

PS.Release = function (x, y, data, options)
{
   "use strict";

   // Put code here for when the mouse button is released over a bead
};

// PS.Enter (x, y, button, data)
// This function is called whenever the mouse moves over a bead
// It doesn't have to do anything
// x = the x-position of the bead on the grid
// y = the y-position of the bead on the grid
// data = the data value associated with this bead, 0 if none has been set
// options = a table with optional parameters; see documentation for details

PS.Enter = function (x, y, data, options)
{
   "use strict";

   // Put code here for when the mouse enters a bead
};

// PS.Leave (x, y, data)
// This function is called whenever the mouse moves away from a bead
// It doesn't have to do anything
// x = the x-position of the bead on the grid
// y = the y-position of the bead on the grid
// data = the data value associated with this bead, 0 if none has been set
// options = a table with optional parameters; see documentation for details

PS.Leave = function (x, y, data, options)
{
   "use strict";

   // Put code here for when the mouse leaves a bead
};

// PS.KeyDown (key, shift, ctrl)
// This function is called whenever a key on the keyboard is pressed
// It doesn't have to do anything
// key = the ASCII code of the pressed key, or one of the following constants:
// Arrow keys = PS.ARROW_UP, PS.ARROW_DOWN, PS.ARROW_LEFT, PS.ARROW_RIGHT
// Function keys = PS.F1 through PS.F1
// shift = true if shift key is held down, false otherwise
// ctrl = true if control key is held down, false otherwise
// options = a table with optional parameters; see documentation for details

PS.KeyDown = function (key, shift, ctrl, options)
{
   "use strict";
   var ascii = function(a) {
      return a.charCodeAt(0);
   };

   if (key == ascii('W') || key == PS.ARROW_UP)
      player_move(0, -1);
   else if (key == ascii('S') || key == PS.ARROW_DOWN)
      player_move(0, 1);

   if (g_player.facing == FACING.inward) {
      if (key == ascii('A') || key == PS.ARROW_LEFT)
         player_move(-1, 0);
      else if (key == ascii('D') || key == PS.ARROW_RIGHT)
         player_move(1, 0);
   } else {
      if (key == ascii('A') || key == PS.ARROW_LEFT)
         player_move(1, 0);
      else if (key == ascii('D') || key == PS.ARROW_RIGHT)
         player_move(-1, 0);
   }

   if (key == ascii('Q'))
      player_change_facing();
   else if (key == ascii(' '))
      player_change_depth();

   redraw();
};

// PS.KeyUp (key, shift, ctrl)
// This function is called whenever a key on the keyboard is released
// It doesn't have to do anything
// key = the ASCII code of the pressed key, or one of the following constants:
// Arrow keys = PS.ARROW_UP, PS.ARROW_DOWN, PS.ARROW_LEFT, PS.ARROW_RIGHT
// Function keys = PS.F1 through PS.F12
// shift = true if shift key is held down, false otherwise
// ctrl = true if control key is held down, false otherwise
// options = a table with optional parameters; see documentation for details

PS.KeyUp = function (key, shift, ctrl, options)
{
   "use strict";

   // Put code here for when a key is released
};

// PS.Wheel (dir)
// This function is called whenever the mouse wheel moves forward or backward
// It doesn't have to do anything
// dir = PS.FORWARD if mouse wheel moves forward, PS.BACKWARD if backward
// options = a table with optional parameters; see documentation for details

PS.Wheel = function (dir, options)
{
   "use strict";

   // Put code here for when mouse wheel is moved
};

// PS.Tick ()
// This function is called on every clock tick
// if a timer has been activated with a call to PS.Timer()
// It doesn't have to do anything
// options = a table with optional parameters; see documentation for details

PS.Tick = function (options)
{
   "use strict";

   // Put code here to handle clock ticks
};
