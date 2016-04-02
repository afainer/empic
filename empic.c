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

#include <SDL2/SDL.h>

#include "render.h"

SDL_Window * window = NULL;

static int quit = 0, do_render = 0;

int create_window()
{
  window = SDL_CreateWindow( "Empic", 0, 0, 800, 600,
                             SDL_WINDOW_OPENGL |
                             SDL_WINDOW_SHOWN |
                             SDL_WINDOW_RESIZABLE );

  if( !(window && SDL_GL_CreateContext( window )) )
    return 0;

  return 1;
}

void destroy_window()
{
  if( SDL_GL_GetCurrentContext() )
    SDL_GL_DeleteContext( SDL_GL_GetCurrentContext() );

  if( window )
    SDL_DestroyWindow( window );
}

void cleanup_and_quit()
{
  destroy_window();
  SDL_Quit();
}

void process_window_event( SDL_Event * event )
{
  switch( event->window.event )
    {
    case SDL_WINDOWEVENT_SHOWN:
    case SDL_WINDOWEVENT_EXPOSED:
    case SDL_WINDOWEVENT_MAXIMIZED:
    case SDL_WINDOWEVENT_RESTORED:
    case SDL_WINDOWEVENT_RESIZED:
    case SDL_WINDOWEVENT_SIZE_CHANGED:
      update_viewport();
      do_render = 1;
    }
}

void process_key_event( SDL_Event * event )
{
  switch( event->key.keysym.sym )
    {
    case SDLK_q:
      quit = 1;
      break;
    case SDLK_EQUALS:
    case SDLK_KP_PLUS:
      zoom_view_frac( 1.1f );
      break;
    case SDLK_MINUS:
    case SDLK_KP_MINUS:
      zoom_view_frac( 0.9f );
      break;
    case SDLK_0:
      zoom_view_fit( ZOOM_FIT_BIG );
      break;
    case SDLK_1:
      zoom_view( 1.0f );
      break;
    case SDLK_RIGHT:
      move_view_delta( 10, 0 );
      break;
    case SDLK_LEFT:
      move_view_delta( -10, 0 );
      break;
    case SDLK_UP:
      move_view_delta( 0, 10 );
      break;
    case SDLK_DOWN:
      move_view_delta( 0, -10 );
      break;
    case SDLK_SLASH:
      rotate_view( 0 );
      break;
    case SDLK_COMMA:
      rotate_view( -90 );
      break;
    case SDLK_PERIOD:
      rotate_view( 90 );
      break;
    case SDLK_SEMICOLON:
      rotate_view_delta( -10 );
      break;
    case SDLK_QUOTE:
      rotate_view_delta( 10 );
      break;
    default:;
    }

  do_render = 1;
}

void process_events()
{
  SDL_Event event;
  if( !SDL_WaitEvent( &event ) )
    return;

  switch( event.type )
    {
    case SDL_QUIT:
      quit = 1;
      break;
    case SDL_WINDOWEVENT:
      process_window_event( &event );
      break;
    case SDL_KEYDOWN:
        process_key_event( &event );
      break;
    }
}

int main( int argc, char ** argv )
{
  if( argc < 2 )
    return 0;

  if( SDL_Init( SDL_INIT_VIDEO ) < 0 )
    {
      SDL_Log( "Couldn't init: %s\n", SDL_GetError() );
      cleanup_and_quit();
      return 1;
    }

  if( !create_window() )
    {
      SDL_Log( "Couldn't create window: %s\n", SDL_GetError() );
      cleanup_and_quit();
      return 2;
    }

  if( !init_render() )
    {
      cleanup_and_quit();
      return 3;
    }

  if( !load_image( argv[ 1 ] ) )
    {
      cleanup_and_quit();
      return 4;
    }

  /* Simulate keydown for ZOOM_FIT_BIG */
  SDL_Event key;
  key.type = SDL_KEYDOWN;
  key.key.keysym.sym = SDLK_0;
  SDL_PushEvent( &key );

  while( !quit )
    {
      process_events();

      if( do_render )
        {
          do_render = 0;
          if( !render() )
            {
              cleanup_and_quit();
              return 5;
            }
        }
    }

  cleanup_and_quit();

  return 0;
}
