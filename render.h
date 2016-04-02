/* Copyright (C) 2016 Andrey Fainer <fandrey@gmx.com>

This file is part of Empic.

Empic is free software: you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation, either version 3 of the License, or (at your
option) any later version.

Empic is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with Empic.  If not, see <http://www.gnu.org/licenses/>. */

typedef enum {
  ZOOM_FIT,
  ZOOM_FIT_BIG,
  ZOOM_FIT_SMALL
} zoom_fit_t;

void move_view( float x, float y );
void move_view_delta( float x, float y );
void zoom_view( float z );
void zoom_view_frac( float z );
void zoom_view_fit( zoom_fit_t fit );
void rotate_view( float angle );
void rotate_view_delta( float angle );

void update_viewport();

int init_render();
int render();

int load_image( const char * file );
